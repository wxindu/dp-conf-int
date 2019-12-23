### This file includes source code to produce the coverage plot that make comparison among the proposed algorithms. 
### Both Figure 2 in Chapter 5 and Figure 9,10 in the Appendix are produced using the code in this file. 
### The figures included in the paper were generated with larger number of iterations than included here. 
library(tidyverse)
library(scales)

# source(here("sims_figures/simulate.r"))

## Generate dataset for the complete coverage plot that compares our algorithms with varied range
## and epsilon values, sample size set to be 1000. 
# ns <- 1000
# algs <- c("pub", "priv", "abs", "exp", "med_dev", "double")
# system.time(data <- simulate(algs, ns, es = c(.01, .1, .5, 1),
#                              ranges = c(2, 6, 12, 32), 
#                              as = seq(from = .01, to = .5, length.out = 25), 1500))
# 
# write_csv(data, "data/ours_full_varied_range.csv")

## Generate dataset for the complete coverage plot that compares our algorithms with varied 
## sample sizes and epsilon values, range set to be [-6, 6]
# ns <- c(250, 1000, 2000, 5000)
# system.time(data <- simulate(algs, ns, 
#                              es = c(.01, .1, .5, 1), 
#                              ranges = 6, as = seq(from = .01, to = .5, length.out = 25), 1500))
# 
# write_csv(data, "data/ours_full_range.csv")

## Import datasets
data_range_e <- read_csv("data/ours_full_varied_range.csv")
data_n_e <- read_csv("data/ours_full_range.csv")

## Modify details for pretty plots
cols <- c("abs" = "#dbd114", 
          "exp" = "#07ab30", 
          "double" = "#19c0f7", 
          "med_dev" = "#f09890", 
          "pub" = "#ff2bd5", 
          "priv" = "#a36d6a")
names <- c("abs"= "Noisy Absolute Deviation", 
           "exp"= "Centered Quantiles", 
           "double" = "Symmetric Quantiles",
           "med_dev" = "Median of Deviations",
           "priv" = "Noisy Mean and Variance", 
           "pub" = "Public")
data_range_e$e1 <- factor(as.factor(data_range_e$e), labels = c(expression(paste(epsilon, " = 0.01")), 
                                                expression(paste(epsilon, " = 0.1")), 
                                                expression(paste(epsilon, " = 0.5")), 
                                                expression(paste(epsilon, " = 1"))))
data_n_e$e1 <- factor(as.factor(data_n_e$e), labels = c(expression(paste(epsilon, " = 0.01")), 
                                                  expression(paste(epsilon, " = 0.1")), 
                                                  expression(paste(epsilon, " = 0.5")), 
                                                  expression(paste(epsilon, " = 1"))))
data_range_e$range1 <- factor(as.factor(data_range_e$range), 
                      labels = c(expression(paste("X"["min"], ", ", "X"["max"], " = -2, 2")), 
                                 expression(paste("X"["min"], ", ", "X"["max"], " = -6, 6")), 
                                 expression(paste("X"["min"], ", ", "X"["max"], " = -12, 12")), 
                                 expression(paste("X"["min"], ", ", "X"["max"], " = -32, 32"))))
data_n_e$n1 <- factor(as.factor(data_n_e$n), labels = c(expression(paste("n", " = 250")), 
                                                    expression(paste("n", " = 1000")), 
                                                    expression(paste("n", " = 2000")), 
                                                    expression(paste("n", " = 5000"))))

## Generate the full 4*4 coverage plot for our algorithms, with range and epsilon varied. 
ours_cov_range_e <- data_range_e %>% 
  ggplot(aes(y = coverage, x = a, col = alg)) + 
  facet_grid(as.factor(range1) ~as.factor(e1), labeller = label_parsed) + 
  geom_line() + 
  scale_color_manual(values = cols, 
                     labels = names) +
  labs(x = "Significance Level \u03b1", 
       y = "Coverage", 
       col = "Algorithm") +
  theme(legend.position = "bottom", 
        text = element_text(10))+
  theme(panel.background = element_rect(fill = "white", colour = "grey70"), 
        panel.grid.major = element_line(colour = "grey90"), 
        panel.grid.minor = element_line(colour = "grey90"), 
        strip.background = element_rect(colour = "grey70", fill = "grey90"), 
        legend.key = element_rect(colour = "white", fill = "white"), 
        legend.box.background = element_rect(colour = "grey80"), 
        legend.box.margin = margin(6, 6, 6, 6))

## Generate the full 4*4 coverage plot for our algorithms, with sample size and epsilon varied. 
ours_cov_n_e <- data_n_e %>%
  ggplot(aes(y = coverage, x = a, col = alg)) + 
  facet_grid(as.factor(n1) ~as.factor(e1), labeller = label_parsed) + 
  geom_line() + 
  scale_color_manual(values = cols, 
                     labels = names) +
  labs(x = "Significance Level \u03b1", 
       y = "Coverage", 
       col = "Algorithm") +
  theme(legend.position = "bottom", 
        text = element_text(10))+
  theme(panel.background = element_rect(fill = "white", colour = "grey70"), 
        panel.grid.major = element_line(colour = "grey90"), 
        panel.grid.minor = element_line(colour = "grey90"), 
        strip.background = element_rect(colour = "grey70", fill = "grey90"), 
        legend.key = element_rect(colour = "white", fill = "white"), 
        legend.box.background = element_rect(colour = "grey80"), 
        legend.box.margin = margin(6, 6, 6, 6))

## Generate the example coverage plot (that's included in the main text) for our algorithms, with range and epsilon varied. 
ours_cov_range_e_ex <- data_range_e %>% 
  filter(e == 0.1 & range == 6) %>%
  ggplot(aes(y = coverage, x = a, col = alg)) + 
  facet_grid(as.factor(range1) ~ as.factor(e1), labeller = label_parsed) + 
  geom_line() + 
  scale_color_manual(values = cols, 
                     labels = names) +
  labs(x = "Significance Level \u03b1", 
       y = "Coverage", 
       col = "Algorithm") +
  theme(legend.position = "bottom", 
        text = element_text(10))+
  theme(panel.background = element_rect(fill = "white", colour = "grey70"), 
        panel.grid.major = element_line(colour = "grey90"), 
        panel.grid.minor = element_line(colour = "grey90"), 
        strip.background = element_rect(colour = "grey70", fill = "grey90"), 
        legend.key = element_rect(colour = "white", fill = "white"), 
        legend.box.background = element_rect(colour = "grey80"), 
        legend.box.margin = margin(6, 6, 6, 6)) 
 
 # ggsave("ours_cov_n_e.png", path = "paper/plots", width = 8.12, height = 8.12, ours_cov_n_e, device = "png")
 # ggsave("ours_cov_range_e_ex.png", path = "paper/plots", ours_cov_range_e_ex, device = "png")
 # ggsave("ours_cov_range_e.png", path = "paper/plots", width = 8.12, height = 8.12, ours_cov_range_e, device = "png")
