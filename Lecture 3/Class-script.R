################################
## Title: week3.R
## Author: Marshall A. Taylor
################################

### BEGIN ###

# Necessary packages
# install.packages("pacman")
install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse,
               textclean,
               textstem,
               stringi,
               tidytext,
               text2map,
               topicmodels,
               topicdoc,
               Matrix,
               forcats,
               LDAvis,
               servr,
               install = T)

install.packages("remotes")
install.packages("text2map")
library(remotes)
library(text2map.corpora)
remotes::install_gitlab("culturalcartography/text2map.corpora")

#Load and clean data
data("corpus_envsociology")

df <- corpus_envsociology |> 
  mutate(text = replace_curly_quote(abstract),
         text = stri_trans_general(text, id = "Any-Latin; Latin-ASCII"),
         text = replace_contraction(text),
         text = tolower(text),
         text = gsub("[[:punct:]]+", " ", text),
         text = gsub("[[:digit:]]+", " ", text),
         text = gsub("\\s+", " ", text)) |>
  mutate(lemma = lemmatize_strings(text)) |>
  rowid_to_column(var = "doc_id")


#Create the DTM
dtm <- df |> 
  dtm_builder(text, title) |> 
  dtm_stopper(stop_list = get_stoplist("snowball2014"))

dim(dtm)


#Estimate some topics (k = 6)
opts <- list(seed = 59801)
lda_env <- LDA(dtm, k = 6, control = opts)


#Evaluate
terms(lda_env, 5)

install.packages("reshape2")
library(reshape2)
tidy(lda_env, matrix = "beta") |>
  group_by(topic) |>
  slice_max(beta, n = 5) |>
  arrange(topic, desc(beta)) |>
  filter(topic == 1)

topics(lda_env)[1]

tidy(lda_env, matrix = "gamma") |>
  mutate(document = fct_inorder(document)) |>
  arrange(document, topic) |>
  slice(1:5)


#Determine k
k_topic <- seq(2, 10, by = 2)
opts <- list(seed = 59801)

lda_envs <- lapply(k_topic, function(k) LDA(dtm, k, control = opts))
diag_list <- lapply(lda_envs, topic_diagnostics, dtm, 5)
names(diag_list) <- k_topic
df_diag <- bind_rows(diag_list, .id = "K")

df_diag |>
  group_by(K) |>
  summarize(coherence = mean(topic_coherence), 
            exclusivity = mean(topic_exclusivity)) |>
  ggplot(aes(x = coherence, 
             y = exclusivity)) +
  geom_text(aes(label = K)) +
  labs(x = "Semantic Coherence",
       y = "Exclusivity")


#Interpreting topics, beyond just highest prob
lda_env <- lda_envs[4][[1]]

json <- createJSON(phi = exp(lda_env@beta),
                   theta = lda_env@gamma,
                   doc.length = rowSums(
                     as.matrix(dtm)),
                   term.frequency = colSums(
                     as.matrix(dtm)),
                   vocab = colnames(
                     as.matrix(dtm)),
                   reorder.topics = F)

serVis(json, out.dir = "vis", 
       open.browser = interactive())


#"Using" topics
pacman::p_load(mclogit,
               marginaleffects,
               install = T)

topic_names <- c(
  "topic_1" = "Water",
  "topic_2" = "Global Political Economy and Carbon Emissions",
  "topic_3" = "Environmental Risk",
  "topic_4" = "Social Dimensions of Climate Change",
  "topic_5" = "Sociology of the Environment",
  "topic_6" = "Climate Policy",
  "topic_7" = "Public Attitudes and Environmental Concerns",
  "topic_8" = "Gender Dynamics"
)

topic_colors <- c(
  "Water"                                          = "#F8766D",
  "Global Political Economy and Carbon Emissions"  = "#B79F00",
  "Environmental Risk"                             = "#00BA38",
  "Social Dimensions of Climate Change"            = "#00BFC4",
  "Sociology of the Environment"                   = "#619CFF",
  "Climate Policy"                                 = "#F564E3",
  "Public Attitudes and Environmental Concerns"    = "#E58700",
  "Gender Dynamics"                                = "#C77CFF" 
)

theta_mat <- posterior(lda_env)$topics
colnames(theta_mat) <- paste0("topic_", 1:ncol(theta_mat))
env_data <- bind_cols(corpus_envsociology, as.data.frame(theta_mat))
topic_cols <- colnames(theta_mat)

fml <- as.formula(
  paste0("cbind(", paste(topic_cols, collapse = ", "), ") ~ journal_rank + year")
)

fit_reg <- mblogit(fml, data = env_data)
unique_ranks <- sort(unique(env_data$journal_rank))

plot_data <- predictions(
  fit_reg,
  newdata = datagrid(
    journal_rank = unique_ranks,
    year = mean(env_data$year, na.rm = T)
  )) |>
  as_tibble() |>
  mutate(
    topic = factor(topic_names[group], levels = unname(topic_names)),
    journal_rank_factor = factor(journal_rank))

plot <- ggplot(plot_data, aes(x = journal_rank, y = estimate, color = topic, fill = topic)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ topic, ncol = 4) +
  scale_color_manual(values = topic_colors) +
  scale_fill_manual(values = topic_colors) +
  # Show clean numeric breaks on x-axis without dropping any data points
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Journal Prestige Rank",
    y = "Predicted Topic Probability") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 8),
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank())

plot

### END ###