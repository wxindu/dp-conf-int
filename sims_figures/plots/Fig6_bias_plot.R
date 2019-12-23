### This file includes source code to produce the bias plot as Figure 6 in Appendix.
library(here)
source(here("sims_figures", "plots", "discussion_helper_functions.R"))

epsilons <- c(.01, .1, .5, 1)
ns <- c(250, 500, 1000)
n_e_df <- expand.grid(ns, epsilons)
quantiles <- seq(.05, .95, .05)


final_mat <- quantiles
for (i in 1:length(n_e_df$Var1)) {
  print(i)
  n <- n_e_df[i,1]
  ep <- n_e_df[i,2]
  m_values <- round(n*quantiles)
  vec <- rep(NA, length(m_values))
  for (j in 1:length(m_values)) {
    vec[j] <- calculate_bias_recip(n, 0, 1, -5, 5, ep, m_values[j])
  }
  final_mat <- cbind(final_mat, vec)
}

bias_ratio <- data.frame(bias_recip = c(final_mat[,2],
                                        final_mat[,3],
                                        final_mat[,4],
                                        final_mat[,5],
                                        final_mat[,6],
                                        final_mat[,7],
                                        final_mat[,8],
                                        final_mat[,9],
                                        final_mat[,10],
                                        final_mat[,11],
                                        final_mat[,12],
                                        final_mat[,13]),
                         epsilon = c(rep(.01, 57), rep(.1, 57), rep(.5, 57), rep(1, 57)),
                         n = rep(c(rep(250, 19), rep(500, 19), rep(1000, 19)), 4),
                         alpha = rep(seq(.05,.95, .05), 12))

bias_ratio$e1 <- factor(as.factor(bias_ratio$epsilon), labels = c(expression(paste(epsilon, " = 0.01")), 
                                                                  expression(paste(epsilon, " = 0.1")), 
                                                                  expression(paste(epsilon, " = 0.5")), 
                                                                  expression(paste(epsilon, " = 1"))))

bias_ratio %>%
  ggplot(aes(y = bias_recip, x = alpha, color = as.factor(n))) +
  geom_line() +
  facet_wrap(~e1, labeller = label_parsed)  +
  labs(y = "Bias (Difference)", 
       col = "n", 
       x = "Quantile Being Estimated") +
  theme(legend.position = "bottom")+
  theme(panel.background = element_rect(fill = "white", colour = "grey70"), 
        panel.grid.major = element_line(colour = "grey90"), 
        panel.grid.minor = element_line(colour = "grey90"), 
        strip.background = element_rect(colour = "grey70", fill = "grey90"), 
        legend.key = element_rect(colour = "white", fill = "white"), 
        legend.box.background = element_rect(colour = "grey80"), 
        legend.box.margin = margin(6, 6, 6, 6))

ggsave(here("paper", "plots", "bias_plot_copy.png"))