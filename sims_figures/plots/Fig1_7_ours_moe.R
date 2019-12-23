### This file includes source code to produce the MoE plot that make comparison among the proposed algorithms. 
### Both Figure 1 in Chapter 5 and Figure 7 in the Appendix are produced using the code in this file. 
### The figures included in the paper were generated with larger number of iterations than included here. 
library(tidyverse)
library(scales)

# source(here("sims_figures/simulate.r"))

## Generate dataset for the complete moe plot that compares our algorithms with varied range and 
## epsilon values, significance level set to be 0.05. 
# p <- .65
# ns <- round(seq(from = 50^p, to = 10000^p, length.out = 25)^(1/p))
# algs <- c("pub", "priv", "abs", "exp", "med_dev", "double")
# system.time(data <- simulate(algs, ns, es = c(.01, .1, .5, 1), ranges = c(2, 6, 12, 32), as = .05, 200))
# 
# write_csv(data, "data/ours_full_plot.csv")

## Import dataset
# data <- read_csv(here("data", "ours_full_plot.csv"))

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
data$e1 <- factor(as.factor(data$e), labels = c(expression(paste(epsilon, " = 0.01")), 
                                                expression(paste(epsilon, " = 0.1")), 
                                                expression(paste(epsilon, " = 0.5")), 
                                                expression(paste(epsilon, " = 1"))))
data$range1 <- factor(as.factor(data$range), 
                      labels = c(expression(paste("X"["min"], ", ", "X"["max"], " = -2, 2")), 
                                 expression(paste("X"["min"], ", ", "X"["max"], " = -6, 6")), 
                                 expression(paste("X"["min"], ", ", "X"["max"], " = -12, 12")), 
                                 expression(paste("X"["min"], ", ", "X"["max"], " = -32, 32"))))
data$range_ex <- factor(as.factor(data$range), 
                      labels = c(expression(paste("X"["min"], ", ", "X"["max"], " = -2, 2")), 
                                 expression(paste("a) X"["min"], ", ", "X"["max"], " = -6, 6")), 
                                 expression(paste("X"["min"], ", ", "X"["max"], " = -12, 12")), 
                                 expression(paste("b) X"["min"], ", ", "X"["max"], " = -32, 32"))))

## Generate the full moe plot for our algorithms with range and epsilon varied
ours_moe <- data %>% 
  ggplot(aes(y = moe, x = n, col = alg)) + 
  facet_grid(as.factor(range1) ~ as.factor(e1), labeller = label_parsed) + 
  geom_line() + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values = cols, 
                     labels = names) +
  labs(x = "Sample Size", 
       y = "Margin of Error", 
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

## Generate the example moe plot used in the main text
ours_moe_ex <- data %>% 
  filter(e == 0.1 & range %in% c(6, 32))  %>%
  ggplot(aes(y = moe, x = n, col = alg)) + 
  facet_wrap(~as.factor(range_ex), ncol = 1, labeller = label_parsed) + 
  geom_line() + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values = cols, 
                     labels = names) +
  labs(x = "Sample Size", 
       y = "Margin of Error", 
       col = "Algorithm") +
  theme(legend.position = "right",
        panel.background = element_rect(fill = "white", colour = "grey70"), 
        panel.grid.major = element_line(colour = "grey90"), 
        panel.grid.minor = element_line(colour = "grey90"),  
        strip.background = element_rect(colour = "grey70", fill = "grey90"), 
        legend.key = element_rect(colour = "white", fill = "white"), 
        legend.box.background = element_rect(colour = "grey80"), 
        legend.box.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size = 11))

# ggsave("ours_moe.png", path = here("paper", "plots"), ours_moe, device = "png",
#        width = 8.12, height = 8.12)
# ggsave("ours_moe_ex.png", path = here("paper", "plots"), ours_moe_ex, device = "png",
#        width = 6.55, height = 7.5)
