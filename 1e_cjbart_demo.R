set.seed(89)
library(ggplot2)
library(cjbart)

subjects <- 100
rounds <- 5
profiles <- 2
obs <- subjects*rounds*profiles

fake_data <- data.frame(A = sample(c("a1","a2","a3"), obs, replace = TRUE),
                        B = sample(c("b1","b2","b3"), obs, replace = TRUE),
                        C = sample(c("c1","c2","c3"), obs, replace = TRUE),
                        D = sample(c("d1","d2","d3"), obs, replace = TRUE),
                        E = sample(c("e1","e2","e3"), obs, replace = TRUE),
                        
                        covar1 = rep(runif(subjects, 0 ,1),
                                     each = rounds),
                        covar2 = rep(sample(c(1,0),
                                            subjects,
                                            replace = TRUE),
                                     each = rounds),
                        
                        id1 = rep(1:subjects, each=rounds),
                        stringsAsFactors = TRUE)

fake_data$Y <- ifelse(fake_data$E == "e2",
                      rbinom(obs, 1, fake_data$covar1),
                      sample(c(0,1), obs, replace = TRUE))
