######
# This file includes our own algorithm to generate confidence intervals in the public setting. 
######

# function for sample variance
variance <- function(x, m) {
  return((1/(length(x) - 1))*sum((x - m)^2))
}

# function to produce confidence intervals in the public setting
pub_ci <- function(db, a) {
  m <- mean(db)
  radius <- sqrt(variance(db, m)/length(db))*qnorm(1 - a/2)
  return(c(m - radius, m + radius))
}