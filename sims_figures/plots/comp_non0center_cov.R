library(tidyverse)
library(scales)
library(here)

# source(here("code/figures/simulate.r"))
# 
# ## Generate dataset for the full coverage plots that compares our algorithms to existing ones,
# ## with ranges and epsilon varied, sample size set to be 1000
# ns = c(1000)
# algs <- c("pub", "vadhan", "honaker", "boot", "abs", "double")
# system.time(data <- simulate(algs, ns, es = c(.01, .1, .5, 1), ranges = c(5, 8, 18), as = seq(from = .01, to = .5, length.out = 25), center = 3, 600))
# 
# write_csv(data, "data/comp_non0center_cov.csv")

data <- read_csv("data/comp_non0center_cov.csv")

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
                      labels = c(expression(paste("X"["min"], ", ", "X"["max"], " = -5, 5")), 
                                 expression(paste("X"["min"], ", ", "X"["max"], " = -8, 8")), 
                                 expression(paste("X"["min"], ", ", "X"["max"], " = -18, 18"))))

## Generate the full 3*3 coverage plot (our best vs. existing)
comp_non0center_cov <- data %>% 
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

ggsave("comp_non0center_cov.png", path = "paper/plots", width = 8.12, height = 8.12, comp_non0center_cov, device = "png")
