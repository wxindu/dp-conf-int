library(tidyverse)
library(scales)
library(here)

# source(here("code/figures/simulate.r"))
# 
# ## Generate dataset for the complete coverage plot that compares our algorithms with varied range and epsilon values, sample size set to be 1000. 
# ns <- 1000
# algs <- c("pub", "priv", "abs", "exp", "med_dev", "double")
# system.time(data <- simulate(algs, ns, es = c(.01, .1, .5, 1), ranges = c(5, 8, 18), as = seq(from = .01, to = .5, length.out = 25), center = 3, 600))
# 
# write_csv(data, "data/ours_non0center_cov.csv")

## Import datasets
data <- read_csv("data/ours_non0center_cov.csv")

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
                      labels = c(expression(paste("X"["min"], ", ", "X"["max"], " = -5, 5")), 
                                 expression(paste("X"["min"], ", ", "X"["max"], " = -8, 8")), 
                                 expression(paste("X"["min"], ", ", "X"["max"], " = -18, 18"))))


## Generate the full 4*4 coverage plot for our algorithms, with range and epsilon varied. 
ours_non0center_cov <- data %>% 
  filter(alg != "abs") %>%
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

ggsave("ours_non0center_cov_noabs.png", path = "paper/plots", width = 8.12, height = 8.12, ours_non0center_cov, device = "png")
