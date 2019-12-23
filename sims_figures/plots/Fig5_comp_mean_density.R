### This file includes source code to produce Figure 5 in Chpater 6. 
library(tidyverse)
library(here)
library(rmutil)
source(here("algorithms/alg5_EXPQ.R"))

noisy_mean <- function(X, epsilon, range) {
  return(mean(X) + rlaplace(1,0, range/(epsilon * length(X))))
}


med_mat <- matrix(rep(NA, 500000), ncol = 5)
e1 <- .25
e2 <- .1

for (i in 1:100000) {
  data <- rnorm(500, 0, 1)
  med_mat[i, 1] <- mean(data)
  med_mat[i, 2] <- noisy_mean(data, e1, 10)
  med_mat[i, 3] <- noisy_mean(data, e2, 10)
  med_mat[i, 4] <- priv_median_c(data, .5, e1, -5, 5)
  med_mat[i, 5] <- priv_median_c(data, .5, e2, -5, 5)
}

med_df <- data.frame(value = c(med_mat[,1], med_mat[,2], med_mat[,3], med_mat[,4], med_mat[,5]),
                     type = c(rep("Mean", 100000), 
                              rep("Lap ep = 0.25", 100000), 
                              rep("Lap ep = 0.1", 100000),
                              rep("Exp ep = 0.25", 100000), 
                              rep("Exp ep = 0.1", 100000)))


med_df %>% 
  ggplot(aes(x = value, col = type)) +
  geom_density() +
  xlim(c(-1,1)) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) 

ggsave(here("paper", "plots", "comp_mean_density.png"))