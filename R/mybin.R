#' Simulate a Binomial Distribution
#'
#' This function simulates repeated binomial trials and plots
#' the distribution of successes.
#'
#' @param iter Number of iterations
#' @param n Number of trials per iteration
#' @param p Probability of success
#'
#' @return A barplot of simulated proportions and a table of frequencies
#' @export
#'
#' @examples
#' mybin_package(iter = 1000, n = 10, p = 0.5)
mybin_package <- function(iter = 100, n = 10, p = 0.5) {
  sam.mat <- matrix(NA, nr = n, nc = iter, byrow = TRUE)
  succ <- c()
  for (i in 1:iter) {
    sam.mat[, i] <- sample(c(1, 0), n, replace = TRUE, prob = c(p, 1 - p))
    succ[i] <- sum(sam.mat[, i])
  }
  succ.tab <- table(factor(succ, levels = 0:n))
  barplot(succ.tab / iter, col = rainbow(n + 1),
          main = "Binomial Simulation",
          xlab = "Number of Successes")
  return(succ.tab / iter)
}
