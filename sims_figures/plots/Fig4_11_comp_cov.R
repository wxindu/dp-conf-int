### This file includes source code to produce the coverage plot that make comparison among the proposed
### and existing algorithms. 
### Both Figure 4 in Chapter 5 and Figure 11 in the Appendix are produced using the code in this file. 
### The figures included in the paper were generated with larger number of iterations than included here. 
library(tidyverse)
library(scales)

# source(here("sims_figures/simulate.r"))
 
## Generate dataset for the full coverage plots that compares our algorithms to existing ones, 
## with ranges and epsilon varied, sample size set to be 1000
# ns = c(1000)
# algs <- c("pub", "vadhan", "honaker", "boot", "abs", "double")
# system.time(data <- simulate(algs, ns, es = c(.01, .1, .5, 1), ranges = c(2, 6, 12, 32), as = seq(from = .01, to = .5, length.out = 25), 1500))
# 
# write_csv(data, "data/comp_full_cov.csv")

## Import dataset
data <- read_csv("data/comp_full_cov.csv")

## Modify details for pretty plot
cols <- c("abs" = "#dbd114", 
          "boot" = "#0f4f50", 
          "double" = "#19c0f7", 
          "honaker" = "#65C282", 
          "pub" = "#ff2bd5", 
          "vadhan" = "#cc6600")
names <- c("abs"= "Noisy Absolute Deviation", 
           "boot"= "Bootstrapping", 
           "double" = "Symmetric Quantiles",
           "honaker" = "Honaker",
           "vadhan" = "Vadhan", 
           "pub" = "Public")
data$e1 <- factor(as.factor(data$e), labels = c(expression(paste(epsilon, " = 0.01")), 
                                                expression(paste(epsilon, " = 0.1")), 
                                                expression(paste(epsilon, " = 0.5")), 
                                                expression(paste(epsilon, " = 1"))))
data$range1 <- factor(as.factor(data$range), 
                      labels = c(expression(paste("X"["min"], ", ", "X"["max"], " = -2, 2")), 
                                 expression(paste("X"["min"], ", ", "X"["max"], " = -6, 6")), 
                                 expression(paste("X"["min"], ", ", "X"["max"], " = -12, 12")), 
                                 expression(paste("X"["min"], ", ", "X"["max"], " = -32, 32"))))

## Generate the full 4*4 coverage plot (our best vs. existing)
comp_cov <- data %>% 
  filter(ns == 1000) %>%
  ggplot(aes(y = coverage, x = a, col = alg)) + 
  facet_grid(as.factor(e1) ~ as.factor(range1), labeller = label_parsed) + 
  geom_line()+ 
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

## Generate the example coverage plot (our best vs. existing) (used in main text)
comp_cov_ex <- data %>% 
  filter(ns == 1000) %>%
  filter(e == 0.1 & range == 6) %>%
  ggplot(aes(y = coverage, x = a, col = alg)) + 
  facet_grid(as.factor(range1)~as.factor(e1), labeller = label_parsed) + 
  geom_line()+ 
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

# ggsave("comp_cov.png", path = "paper/plots", width = 8.12, height = 8.12, comp_cov, device = "png")
# ggsave("comp_cov_ex.png", path = "paper/plots", comp_cov_ex, width = 6.55, height = 7.5, device = "png")
