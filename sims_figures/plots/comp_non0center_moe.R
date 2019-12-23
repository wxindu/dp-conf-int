library(tidyverse)
library(scales)
library(here)

# source(here("code/figures/simulate.r"))
# 
# ## Generate dataset for the full moe plot for comparing our best algorithms to existing algorithms, 
# ## with range and epsilon varied and significance level set to be 0.05
# p <- .65
# ns <- round(seq(from = 50^p, to = 10000^p, length.out = 25)^(1/p))
# algs <- c("pub", "vadhan", "honaker", "boot", "abs", "double")
# system.time(data <- simulate(algs, ns, es = c(.01, .1, .5, 1), ranges = c(5, 8, 18), as = .05, center = 3, 100))
# 
# write_csv(data, "data/comp_non0center_moe.csv")

## Import data
data <- read_csv("data/comp_non0center_moe.csv")

## Modify details for pretty plots
cols <- c("abs" = "#dbd114", 
          "boot" = "#0f4f50", 
          "double" = "#19c0f7", 
          "honaker" = "#65C282", 
          "pub" = "#ff2bd5", 
          "vadhan" = "#cc6600")
names <- c("abs"= "Noisy Absolute Deviation", 
           "boot"= "Brawner", 
           "double" = "Symmetric Quantiles",
           "honaker" = "D'Orazio",
           "vadhan" = "Karwa", 
           "pub" = "Public")
data$e1 <- factor(as.factor(data$e), labels = c(expression(paste(epsilon, " = 0.01")), 
                                                expression(paste(epsilon, " = 0.1")), 
                                                expression(paste(epsilon, " = 0.5")), 
                                                expression(paste(epsilon, " = 1"))))
data$range1 <- factor(as.factor(data$range), 
                      labels = c(expression(paste("X"["min"], ", ", "X"["max"], " = -5, 5")), 
                                 expression(paste("X"["min"], ", ", "X"["max"], " = -8, 8")), 
                                 expression(paste("X"["min"], ", ", "X"["max"], " = -18, 18"))))

## Generate full 3*3 moe plot (our best vs. existing)
comp_non0center_moe <- data %>% 
  ggplot(aes(y = moe, x = n, col = alg)) + 
  geom_line() + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x))) + 
  facet_grid(as.factor(e1) ~ as.factor(range1), labeller = label_parsed) + 
  scale_color_manual(values = cols, 
                     labels = names) +
  labs(x = "Sample Size", 
       y = "Margin of Error", 
       col = "Algorithm") +
  theme(legend.position = "bottom") +
  theme(panel.background = element_rect(fill = "white", colour = "grey70"), 
        panel.grid.major = element_line(colour = "grey90"), 
        panel.grid.minor = element_line(colour = "grey90"), 
        strip.background = element_rect(colour = "grey70", fill = "grey90"), 
        legend.key = element_rect(colour = "white", fill = "white"), 
        legend.box.background = element_rect(colour = "grey80"), 
        legend.box.margin = margin(6, 6, 6, 6))

ggsave("comp_non0center_moe.png", path = "paper/plots", width = 8.12, height = 8.12, comp_non0center_moe, device = "png")

