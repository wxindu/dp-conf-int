### This is a help file for figure 6. 
library(tidyverse)
library(here)
library(scales)
source(here("algorithms", "alg5_EXPQ.R"))

# Calculate expected pdf for ith order stat i.e. pdf of X_(i)

ith_order_pdf <- function(x, i, n, mu = 0, sigma = 1) {
  i * choose(n, i) * (pnorm(x, mu, sigma)^(i-1)) * ((1 - pnorm(x, mu, sigma)) ^ (n - i)) * dnorm(x, mu, sigma)
}

# Function to be integrated to calculate ith expected value 

temp <- function(x, i, n, mu, sigma) {
  x * ith_order_pdf(x, i, n, mu, sigma)
}

# Function to calculate expected difference between X_(i) and X_(i + 1), 

expected_value_diff_ith <- function(i, n, mu = 0, sigma = 1, xmin, xmax) {
  vec <- rep(NA, length(i))
  for (j in 1:length(i)) {
    if (i[j] == 0) {
      vec[j] <- abs(xmin - integrate(temp, -Inf, Inf,  i = 1, n = n, mu = mu, sigma = sigma)[[1]])
    } else if (i[j] == n) {
      vec[j] <- abs(xmax - integrate(temp, -Inf, Inf,  i = n, n = n, mu = mu, sigma = sigma)[[1]])
    } else {
      int1 <- integrate(temp, -Inf, Inf,  i = i[j], n = n, mu = mu, sigma = sigma, rel.tol = .000001, abs.tol = 0.00001) 
      int2 <- integrate(temp, -Inf, Inf,  i = (i[j] + 1), n = n, mu = mu, sigma = sigma,  rel.tol = .000001, abs.tol = .00001)
      vec[j] <- abs(int1[[1]] - int2[[1]])
    }
    
  }
  vec
}


### NOT USED 
# Would calculate E[X_(i)]
expected_value_ith <- function(i, n, mu = 0, sigma = 1, xmin, xmax) {
  vec <- rep(NA, length(i))
  for (j in 1:length(i)) {
    if (i[j] == 0) {
      vec[j] <- mean(c(xmin, integrate(temp, -Inf, Inf,  i = 1, n = n, mu = mu, sigma = sigma)[[1]]))
    } else if (i[j] == n) {
      vec[j] <- mean(c(xmax, integrate(temp, -Inf, Inf,  i = n, n = n, mu = mu, sigma = sigma)[[1]]))
    } else {
      int1 <- integrate(temp, -Inf, Inf,  i = i[j], n = n, mu = mu, sigma = sigma) 
      int2 <- integrate(temp, -Inf, Inf,  i = (i[j] + 1), n = n, mu = mu, sigma = sigma)
      vec[j] <- mean(c(int1[[1]], int2[[1]]))
    }
    
  }
  vec
}
 
# Expected bin index of exponential mechanism

calculate_exp_bin <- function(n, mu, sigma, xmin, xmax, epsilon, m) {
  ui_s <- c((1):m - m, m - ((m):n))
  phi <- sum(expected_value_diff_ith(0:(n), n, mu, sigma, xmin, xmax) * exp((ui_s * epsilon)/2))
  sum((1:(n+1)) * expected_value_diff_ith(0:n, n, mu, sigma, xmin, xmax) * exp((ui_s * epsilon)/2) / phi) 
}


# Expected value of exponential mechanism

calculate_expect_value <- function(n, mu, sigma, xmin, xmax, epsilon, m) {
  ui_s <- c((1):m - m, m - ((m):n))
  phi <- sum(expected_value_diff_ith(0:(n), n, mu, sigma, xmin, xmax) * exp((ui_s * epsilon)/2))
  sum(expected_value_ith(0:(n), n, mu, sigma, xmin, xmax) * expected_value_diff_ith(0:n, n, mu, sigma, xmin, xmax) * exp((ui_s * epsilon)/2) / phi)
}



calculate_bias_recip <- function(n, mu, sigma, xmin, xmax, epsilon, m) {
  expect_val <- calculate_expect_value(n, mu, sigma, xmin, xmax, epsilon, m)
  true_value <- qnorm((m/n), mu, sigma)
  true_value - expect_val
}

# Returns probabily vector for exponential mechanism
priv_median_exp_helper <- function(db, e, q, xmin, xmax) {
  n <- length(db)
  probs <- 1:(n + 1)
  db <- sort(db)
  qi <- round((n - 1)*q + 1)
  
  utility <- 1 - qi
  probs[1] <- (db[1] - xmin)*exp(e*utility/2)
  i <- 2
  
  while (i <= qi) {
    utility <- i - qi
    probs[i] <- (db[i] - db[i - 1])*exp(e*utility/2)
    i <- i + 1
  }
  
  i <- qi + 1
  while (i <= n) {
    utility <- qi - (i - 1)
    probs[i] <- (db[i] - db[i - 1])*exp(e*utility/2)
    i <- i + 1
  }
  
  utility <- qi - n
  probs[n + 1] <- (xmax - db[n])*exp(e*utility/2)
  probs <- probs/sum(probs)
  
  r <- runif(1)
  
  return(list(probs, db))
}

# Expected probabilities for each bin to be selected

calculate_exp_probs <- function(n, mu, sigma, xmin, xmax, epsilon, m) {
  ui_s <- c((1):m - m, m - ((m):n))
  phi <- sum(expected_value_diff_ith(0:(n), n, mu, sigma, xmin, xmax) * exp((ui_s * epsilon)/2))
  expected_value_diff_ith(0:n, n, mu, sigma, xmin, xmax) * exp((ui_s * epsilon)/2) / phi
}

# Utility helper
calculate_util <- function(n, mu, sigma, xmin, xmax, epsilon, m) {
  ui_s <- c((1):m - m, m - ((m):n))
  exp((ui_s * epsilon)/2)
}


calculate_exp_order_stat <- function(n,  mu, sigma, xmin, xmax, m) {
  vec <- rep(NA, n)
  for (i in 1:n) {
    vec[i] <- integrate(temp, -Inf, Inf,  i = i, n = n, mu = mu, sigma = sigma)[[1]]
  }
  return(vec)
}

get_bin <- function(x, bins) {
  for(i in 1:length(bins)) {
    if (x < bins[i+1]) {
      break
    }
  }
  return(i)
}

# Put together the data frame
create_df_data <- function(n, mu, sigma, xmin, xmax, epsilon, m) {
  order_vals <- c(xmin, calculate_exp_order_stat(n, mu, sigma, xmin, xmax, m), xmax)
  probs <- calculate_exp_probs(n, mu, sigma, xmin, xmax, epsilon, m)
  util <- calculate_util(n, mu, sigma, xmin, xmax, epsilon, m)
  util <- c(util, util[n+1])
  probs <- c(probs, probs[n+1])
  probs_pdf <- 1/  sum(probs * (order_vals[2:(n+2)] - order_vals[1:(n+1)]))
  return(data.frame(probs = probs, values = order_vals, util = util, index = 0:(n+1), probs_pdf = probs* probs_pdf))
}









