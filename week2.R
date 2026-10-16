################################
## Title: week2.R
## Author: Marshall A. Taylor - test
################################

### BEGIN ###

# Necessary packages
# install.packages("pacman")
pacman::p_load(igraph,
               tidyverse,
               sna,
               ergm,
               install = T)


# usethis::create_github_token()
# gitcreds::gitcreds_set()
# Sys.setenv(GITHUB_PAT = "token__you_just_made_here")
remotes::install_github("jason-morgan/ina")
library(ina)

#Load data
data(cosponsorship)

sen.net <- senlist$net


#CUG Test
#Graph transitivity
set.seed(1234)
cug.test(sen.net,
         FUN = gtrans,
         cmode = "dyad.census",
         reps = 1500)

#Mean indegree
in.degree <- function(x) {
  mean(degree(x, cmode = "indegree"))
}

set.seed(1234)
cug.test(sen.net,
         FUN = in.degree,
         cmode = "edges",
         reps = 1500)

#Reciprocity
set.seed(1234)
cug.test(sen.net,
         FUN = grecip,
         cmode = "edges",
         reps = 1500)


#QAP (specifically, MRQAP)
#dwnom[,1]---aka, dwnom$ideo1---is that row's (senator's) score on the first 
#DW-NOMINATE dimension, where more negative scores indicate a more economically 
#progressive senator and more positive scores indicate a more economically 
#conservative senator. The function below creates an "ideological distance" matrix
#where each ij cell indicates how similar the two senators are in terms of 
#their dispostion toward economic conservatism.
ideo.econ.dist <- as.matrix(dist(dwnom[,1]))

#This function also creates a matrix from the economic conservatism variable.
#But this time, we are simply repeating each senator's score on the variable
#for each possible connection in the adjacency matrix, including a self-loop.
#This is simply taking a column vector (a variable) and making it of the same
#dimension as the adjacency matrix, sen.net.
ideo.econ.send <- matrix(dwnom[,1], nrow(dwnom),nrow(dwnom), byrow = F)

#This function does the same thing, but now repeating the ideology scores
#down the column for the senator as opposed to the row for that senator.
#In effect, ideo.econ.send gives us the cosponsoring senator's economic conservatism
#score and ideo.econ.rec gives us the original sponsoring senator's economic 
#conservatism score.
ideo.econ.rec <- matrix(dwnom[,1], nrow(dwnom),nrow(dwnom), byrow = T)

#This is another ideological distance matrix, but this time on the second
#DW-NOMINATE dimension---which is broadly interpreted as social conservatism.
ideo.social.dist <- as.matrix(dist(dwnom[,2]))

#Same thing as ideo.econ.send, but for the social conservatism score.
ideo.social.send <- matrix(dwnom[,2], nrow(dwnom),nrow(dwnom), byrow = F)

#Same thing as ideo.econ.rec, but for the social conservatism score.
ideo.social.rec <- matrix(dwnom[,2], nrow(dwnom),nrow(dwnom), byrow = T)

#Put the covariate matrices together
cov <- list(ideo.econ.dist = ideo.econ.dist,
            ideo.econ.send = ideo.econ.send,
            ideo.econ.rec = ideo.econ.rec,
            ideo.social.dist = ideo.social.dist,
            ideo.social.send = ideo.social.send,
            ideo.social.rec = ideo.social.rec)

#And the test
set.seed(5)
qap.results <- netlm(sen.net, cov, nullhyp = "qap", reps = 100)
qap.results

#Compare QAP p-values to asymptotically-derived p-values
ols.results <- netlm(sen.net, cov, nullhyp = "classical")
ols.results


#Basic ERGM
#Data
data("Krackhardt")

advice.net <- Krackhardt

#Model
set.seed(510)
ergm.model <- ergm(advice.net ~ edges + mutual + transitiveties +
                     nodeicov("Age") + nodeocov("Age") +
                     nodematch("Level"), control = control.ergm(
                       MCMC.samplesize = 5000,
                       MCMC.burnin = 10000,
                       MCMLE.maxit = 10), verbose = T)
summary(ergm.model)

#Some predited log odds and probabilities
df <- data.frame(trans = rep(1:20, 2), 
                 homoph = rep(0:1, each = 20),
                 log.odds = rep(NA, 40),
                 prob = rep(NA, 40))

for (i in 1:40) {
  
  df[i,3] <- 0.900 + (0.101 * 1) + (0.169 * df[i,1]) + (-0.006 * 50) + 
    (-0.021 * 50) + (-0.445 * df[i,2])
  
  df[i,4] <- exp(df[i,3])/(1 + exp(df[i,3]))
  
}

ggplot(df, aes(x = trans, y = prob, color = factor(homoph))) +
  geom_line() +
  labs(x = "Number of New Closed Triads",
       y = "Predicted Probability of Advice-Seeking",
       color = "Same position in firm's hierarchy?") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold")) +
  scale_color_manual(breaks = c(0,1),
                     values = c("#1696d2","#fdbf11"),
                     labels = c("No","Yes"))

### END ###