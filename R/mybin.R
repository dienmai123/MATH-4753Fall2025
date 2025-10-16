#' Simulate a Binomial Distribution
#'
#' Simulate repeated Binomial(n, p) trials and plot the distribution of
#' the number of successes.
#'
#' @param iter Number of iterations (simulations).
#' @param n    Number of trials in each iteration.
#' @param p    Probability of success.
#'
#' @return A named numeric vector of simulated proportions for k = 0, 1, ..., n.
#' @examples
#' \donttest{
#' mybin_package(iter = 1000, n = 10, p = 0.5)
#' }
#' @export
mybin_package <- function(iter = 100, n = 10, p = 0.5) {
  # basic checks
  stopifnot(length(iter) == 1L, iter > 0, iter == as.integer(iter))
  stopifnot(length(n) == 1L, n >= 0, n == as.integer(n))
  stopifnot(length(p) == 1L, is.finite(p), p >= 0, p <= 1)

  # simulate the number of successes per iteration
  succ <- stats::rbinom(n = iter, size = n, prob = p)

  # tabulate counts for all k in 0..n (include empty categories)
  k_vals <- 0:n
  tab <- table(factor(succ, levels = k_vals))
  prop <- as.numeric(tab) / iter
  names(prop) <- as.character(k_vals)

  # plot a barplot of proportions
  graphics::barplot(
    height   = prop,
    names.arg = k_vals,
    col      = grDevices::rainbow(length(k_vals)),
    xlab     = "Number of successes (k)",
    ylab     = "Proportion",
    main     = sprintf("Binomial(%d, %.2f) - %d simulations", n, p, iter)
  )

  prop
}
