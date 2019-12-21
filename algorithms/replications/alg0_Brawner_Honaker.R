######
# This file includes the code for the algorithm proposed by Brawner and Honaker, as
# discussed in Section 3.3 in the paper. 
######

library(tidyverse)

compute_epsilon <- function(rho, delta = 1e-6) {
  rho + 2 * sqrt(rho * log((pi * rho)/delta))
}

partition_bootstrap <- function(data) {
  N <- length(data)
  draws <- rmultinom(1,N,rep(1/N, N))
  partitions <- list()
  for (i in 1:8) {
    partitions[[i]] <- rep(NA, round(N/10))
  }
  for (i in 1:8) {
    partitions[[i]] <- data[draws == (i-1)]
  }
  return(partitions)
}

define_p_i <- function(i, N) {
  choose(N, i) * ((1/N)^i) * (1-1/N)^(N - i)
}

define_rho_i <- function(i, rho, N) {
  (i * rho) / define_p_i(i, N) 
}

define_sigma_i <- function(sensitivity, i, rho, N) {
  p_i <- define_p_i(i, N)
  i * p_i * (sensitivity ^ 2 / (2 * rho))
}

calculate_partition_mean <- function(X_i, i, rho, range, N) {
  sensitivity <- range / N
  sigma_i <- define_sigma_i(sensitivity, i, rho, N)
  
  m_i <- i * sum(X_i) / N
  m_i + rnorm(1, 0, sqrt(sigma_i))
}

bootstrap_priv_mean <- function(data, rho, range) {
  N <- length(data)
  partitions <- partition_bootstrap(data)
  M_i_vec <- rep(0, N)
  for (i in 0:7) {
    M_i_vec[(i+1)] <- calculate_partition_mean(partitions[[i+1]], i, rho, range, N)
  }
  sum(M_i_vec)
}

estimate_var <- function(k_boot_means, alpha_prime, N, rho, range) {
  sensitivity <- range / N
  k <- length(k_boot_means)
  c_a_prime <- qchisq(alpha_prime, k - 1)
  var(k_boot_means) - (sensitivity^2)/(2*rho)*((k*c_a_prime)/(k-1) - 1)
}


epsilon_rho_equiv <- data.frame(epsilons = c(.01, .1, .2, .25, .5, .75, 1, 2), 
                                rhos = c(.000009, .0004, .0012, .0018, .0062, .013, .022, .075))

construct_boot_ci <- function(data, k, epsilon, range, alpha, alpha_prime) {
  rho <- epsilon_rho_equiv$rhos[epsilon_rho_equiv$epsilons == epsilon]
  N <- length(data)
  boot_vec <- rep(NA, k) 
  
  for (i in 1:k) {
    boot_vec[i] <- bootstrap_priv_mean(data, rho/k, range)
  }
  
  mean_est <- mean(boot_vec)
  var_est <- max(0, estimate_var(boot_vec, alpha_prime, N, rho, range))
  se_est <- sqrt(var_est)
  z <- qnorm(1-alpha/2)
  c(mean_est - z * se_est, mean_est + z * se_est)
}


avg_cover_boot <- function(reps, n, epsilon, alpha, range) {
  cov_vec <- rep(NA, reps)
  moe_vec <- rep(NA, reps)
  for (i in 1:reps) {
    interval <- construct_boot_ci(rnorm(n), 50, epsilon, range,  alpha, alpha_prime = .05)
    cov_vec[i] <- coverage(interval, 0)
    moe_vec[i] <- diff(interval)
  }
  return(c(mean(cov_vec, na.rm = T), mean(moe_vec, na.rm = T)))
  
}
