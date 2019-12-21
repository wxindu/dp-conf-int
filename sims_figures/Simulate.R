######
# This file includes the code to perform simulations using the 3 existing and 5
# proposed algorithmsa on differentially private confidence intervals. Two functions
# are included in this file, simulate_single() which perform a single simulation on
# selected algorithm, and simulate() which performs parallel simulation for specified
# number of iterations. 
######

library(here)
library(tidyverse)
library(parallel)
library(scales)
source(here("algorithms/public_CI.R"))
source(here("algorithms/alg2_NOISYVAR.R"))
source(here("algorithms/alg4_NOISYMAD.R"))
source(here("algorithms/alg5_EXPQ.R"))
source(here("algorithms/alg6_CENQ.R"))
source(here("algorithms/alg7_SYMQ.R"))
source(here("algorithms/alg8_MOD.R"))
source(here("algorithms/replications/alg0_Brawner_Honaker.R"))
source(here("algorithms/replications/alg0_Orazio_Honaker.R"))
source(here("algorithms/replications/alg1_Vadhan.R"))

############
# The function simulate_single() takes up to 7 parameters: 
#   alg:    Name of the algorithm; 
#               "pub" - Public Confidence Interval
#               "priv" - NOISYVAR
#               "abs" - NOISMAD
#               "exp" - CENQ
#               "double" - SYMQ
#               "med_dev" - MOD
#               "pub" - Public Confidence Interval
#   a:      Significance level (set a = 0.05, the function outputs 95% confidence interval).
#   e:      Privacy parameter epsilon.
#   xmin:   Given lower bound of the data range.
#   xmax:   Given upper bound of the data range.
#   b:      Choice of the lower quantile. The upper quantile will be indicated by 1-b. 
#           (e.g. If want to use the first and third quartiles, do b = 0.25). Set in default
#           to the optimized value. 
############
simulate_single <- function(alg, n, e, r, a, ave, center) {
  moe <- 1:ave
  cov <- 0
  
  if(alg == "pub") {
    for(i in 1:ave) {
      ci <- pub_ci(rnorm(n, center), a)
      moe[i] = diff(ci)/2
      ci[1] <- min(r, max(ci[1], -r))
      ci[2] <- min(r, max(ci[2], -r))
      if(ci[1] < center & center < ci[2]) {
        cov <- cov + 1
      }
    }
  } else if(alg == "priv") {
    for(i in 1:ave) {
      ci <- priv_ci(rnorm(n, center), a, e, -r, r)
      moe[i] = diff(ci)/2
      ci[1] <- min(r, max(ci[1], -r))
      ci[2] <- min(r, max(ci[2], -r))
      if(ci[1] < center & center < ci[2]) {
        cov <- cov + 1
      }
    }
  } else if(alg == "abs") {
    for(i in 1:ave) {
      ci <- priv_abs_sd_ci(rnorm(n, center), a, e, -r, r)
      moe[i] = diff(ci)/2
      ci[1] <- min(r, max(ci[1], -r))
      ci[2] <- min(r, max(ci[2], -r))
      if(ci[1] < center & center < ci[2]) {
        cov <- cov + 1
      }
    }
  } else if(alg == "exp") {
    for(i in 1:ave) {
      ci <- priv_exp_ci(rnorm(n, center), a, e, -r, r)
      moe[i] = diff(ci)/2
      ci[1] <- min(r, max(ci[1], -r))
      ci[2] <- min(r, max(ci[2], -r))
      if(ci[1] < center & center < ci[2]) {
        cov <- cov + 1
      }
    }
  } else if(alg == "double") {
    for(i in 1:ave) {
      ci <- priv_double_ci(rnorm(n, center), a, e, -r, r)
      moe[i] = diff(ci)/2
      ci[1] <- min(r, max(ci[1], -r))
      ci[2] <- min(r, max(ci[2], -r))
      if(ci[1] < center & center < ci[2]) {
        cov <- cov + 1
      }
    }
  } else if(alg == "med_dev") {
    for(i in 1:ave) {
      ci <- priv_exp_ci_abs(rnorm(n, center), a, e, -r, r)
      moe[i] = diff(ci)/2
      ci[1] <- min(r, max(ci[1], -r))
      ci[2] <- min(r, max(ci[2], -r))
      if(ci[1] < center & center < ci[2]) {
        cov <- cov + 1
      }
    }
  } else if(alg == "vadhan") {
    for(i in 1:ave) {
      ci <- priv_vadhan_ci(rnorm(n, center), a, e, .2, 5, -r, r)
      moe[i] = diff(ci)/2
      ci[1] <- min(r, max(ci[1], -r))
      ci[2] <- min(r, max(ci[2], -r))
      if(ci[1] < center & center < ci[2]) {
        cov <- cov + 1
      }
    }
  } else if(alg == "honaker") {
    for(i in 1:ave) {
      ci <- priv_interval_orazio(rnorm(n, center), a, e, -r, r, 5)
      moe[i] = diff(ci)/2
      ci[1] <- min(r, max(ci[1], -r))
      ci[2] <- min(r, max(ci[2], -r))
      if(ci[1] < center & center < ci[2]) {
        cov <- cov + 1
      }
    }
  } else if(alg == "boot") {
    for(i in 1:ave) {
      ci <- construct_boot_ci(rnorm(n, center), 50, e, 2*r, a, .05)
      moe[i] = diff(ci)/2
      ci[1] <- min(r, max(ci[1], -r))
      ci[2] <- min(r, max(ci[2], -r))
      if(ci[1] < center & center < ci[2]) {
        cov <- cov + 1
      }
    }
  } else if(alg == "ourboot") {
    for(i in 1:ave) {
      ci <- our_boot_ci(rnorm(n, center), a, e, 100, -r, r)
      moe[i] = diff(ci)/2
      ci[1] <- min(r, max(ci[1], -r))
      ci[2] <- min(r, max(ci[2], -r))
      if(ci[1] < center & center < ci[2]) {
        cov <- cov + 1
      }
    }
  }
  print(as.character(alg))
  print(c(n, e, r, a))
  return(list(moe = mean(moe), var = var(moe), cov = cov/ave))
}

simulate <- function(algs, ns, es, ranges, as, ave, cores = 8L, center = 0) {
  data <- expand.grid(alg = algs, n = ns, e = es, range = ranges, a = as)
  results <- mcmapply(simulate_single, data$alg, data$n, data$e, data$range, data$a, ave, center, mc.cores = cores)
  # results <- mapply(simulate_single, data$alg, data$n, data$e, data$range, data$a, ave, center)
  data <- bind_cols(data, coverage = as.double(unlist(results["cov",])), var = as.double(unlist(results["var",])), moe = as.double(unlist(results["moe",])))
  return(data)
}