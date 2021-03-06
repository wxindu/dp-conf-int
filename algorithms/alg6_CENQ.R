######
# This file includes the code to output a private confidence interval, using private mean 
# and variance estimators generated by Algorithm 6 Centered quantiles and simulation
# as introduced in Algorithm 3. 
######

#install.packages("rmutil")
#install.packages("here")
library(rmutil)
library(here)
source(here("algorithms/alg5_EXPQ.R"))


############
# The function priv_exp_ci() takes up to 7 parameters and returns a differentially private
# confidence interval: 
#   db:     Normally distributed sample data as a vector of numbers. 
#   a:      Significance level (set a = 0.05, the function outputs 95% confidence interval).
#   e:      Privacy parameter epsilon.
#   xmin:   Given lower bound of the data range.
#   xmax:   Given upper bound of the data range.
#   b:      Choice of quantile for standard deviation estimate. Set in default to the optimized value. 
#   p:      Allocation of the privacy parameter epsilon. 100p% of the privacy budget is consumed
#             by generating the private mean; the rest is consumed by generating the private
#             variance. Set in default to the optimized value. 
############
priv_exp_ci <- function(db, a, e, xmin, xmax, b = .65, p = .5) {
  n <- length(db)
  db <- sort(db)
  m <- priv_median_c(db, .5, p*e, xmin, xmax, TRUE)
  top <- priv_median_c(db, b,  (1 - p)*e, xmin, xmax, TRUE)
  std <- max(0, (top - m)/qnorm(b))
  
  sims <- 1:1000
  for(i in 1:1000) {
    sim_db <- rnorm(n, m, std)
    sims[i] <- priv_median_c(sim_db, .5, p*e, xmin, xmax)
  }
  
  moe <- diff(as.double(quantile(sims, c(a/2, 1 - a/2))))/2
  return(c(m - moe, m + moe))
}
