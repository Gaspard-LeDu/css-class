################################
## Title: week5.R
## Author: Marshall A. Taylor
################################

### BEGIN ###

# Necessary packages
install.packages("pacman")
pacman::p_load(tidyverse, 
               glmnet, 
               caret,
               e1071,
               nnet,
               keras,
               tensorflow,
               tibble,
               knitr,
               install = T)

# Load data - you will probably need to change the file path
acc_data <- readRDS("accidents_2025.rds")

# Create train and test splits, stratified by target
# 80/20 as a general rule of thumb
set.seed(8675309)
train_ind <- createDataPartition(acc_data$grav, p = 0.8, list = FALSE)
# Seperating the rows
train_data <- acc_data[train_ind, ]
test_data  <- acc_data[-train_ind, ]
# It matters that distribution proportions are somewhat similar (among both 80/20 there should be 20% deaths for example)

# Check proportions -- they should be roughly equal
table(train_data$grav) |> prop.table()
table(test_data$grav) |> prop.table()


# Models (e1071 package)
# Naïve Bayes
nb_model <- naiveBayes(grav ~ ., data = train_data) # the "." refers to everything else (be careful using this because you might have useless admin data)
nb_predictions <- predict(nb_model, newdata = test_data) # Uses all the priors& likelihood onto the test_data for prediction
nb_cm <- confusionMatrix(nb_predictions, test_data$grav, mode = "everything") # making the confusion matrix
nb_cm

# Multinomial logistic regression
multinom_model <- multinom(grav ~ ., data = train_data, maxit = 100) # Maximizes log-likelihood, given a certain amount of itertions
summary(multinom_model)
multinom_predictions <- predict(multinom_model, newdata = test_data)
lr_cm <- confusionMatrix(multinom_predictions, test_data$grav, mode = "everything")

# Penalized multinomial logistic regression with lasso penalty
x_train <- model.matrix(grav ~ . - 1, data = train_data)
y_train <- train_data$grav
x_test <- model.matrix(grav ~ . - 1, data = test_data)

# Find the optimal lambda (penalty weighting factor)
set.seed(8675309)
cv_lasso <- cv.glmnet(x_train, y_train, family = "multinomial", alpha = 1)
coef(cv_lasso, s = "lambda.min")[[1]] #see the coefficients (weights) for 
#class #1 -- notice how some are attenuated down to zero? That's feature selection!



# Predict using the lambda that gave the minimum cross-validation error
lasso_predictions <- predict(cv_lasso, newx = x_test, 
                             s = "lambda.min", type = "class")
lasso_predictions <- as.factor(lasso_predictions)


plr_cm <- confusionMatrix(lasso_predictions, test_data$grav, mode = "everything")




# Multilater perceptron (neural networks)
# install_keras() - might have to restart R if you need to run this line
# Needs standardization

x_train_nn <- scale(model.matrix(grav ~ . - 1, data = train_data))
x_test_nn  <- scale(model.matrix(grav ~ . - 1, data = test_data))


y_train_nn <- to_categorical(as.numeric(train_data$grav) - 1, num_classes = 4)
y_test_nn  <- to_categorical(as.numeric(test_data$grav) - 1, num_classes = 4)

# From 2000xp to 2000x64 to 2kx32 to 2kx4
model <- keras_model_sequential() %>% 
  layer_dense(units = 64, activation = "relu", input_shape = ncol(x_train_nn)) |> 
  layer_dropout(rate = 0.3) |>
  layer_dense(units = 32, activation = "relu") |>
  layer_dense(units = 4, activation = "softmax")
# Fully connected network, every regression gets every feature

model |> compile(
  loss = "categorical_crossentropy",
  optimizer = tf$keras$optimizers$legacy$RMSprop(),
  # optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)


history <- model |> fit(
  x_train_nn, y_train_nn, 
  epochs = 30, 
  batch_size = 128, 
  validation_split = 0.2
)


nn_pred_probs <- model |> predict(x_test_nn)
nn_predictions_indices <- apply(nn_pred_probs, 1, which.max)


target_levels <- levels(test_data$grav)


nn_predictions <- factor(target_levels[nn_predictions_indices], 
                         levels = target_levels)


nn_cm <- confusionMatrix(nn_predictions, test_data$grav, mode = "everything")




# Compare and contrast the models
extract_cm_metrics <- function(cm, model_name) {
  
  by_class <- as.data.frame(cm$byClass)
  
  class_labels <- gsub("Class: ", "", rownames(by_class))
  
  accuracy <- cm$overall["Accuracy"]
  
  macro_f1 <- mean(by_class$F1, na.rm = TRUE)
  
  tibble(
    Model = model_name,
    Class = class_labels,
    Precision = by_class$Precision,
    Recall = by_class$Recall,
    F1 = by_class$F1,
    Accuracy = accuracy,
    Macro_F1 = macro_f1
  )
}


comparison_table <- bind_rows(
  extract_cm_metrics(nb_cm,  "1. Naïve Bayes"),
  extract_cm_metrics(lr_cm,  "2. Logistic Regression"),
  extract_cm_metrics(plr_cm, "3. Lasso Penalty"),
  extract_cm_metrics(nn_cm,  "4. Neural Network")
) |> 
  arrange(Class, Model)


kable(comparison_table, digits = 3, align = "llrrrrr", 
      caption = "Model Comparison: Traffic Accident Severity")




### END ###
