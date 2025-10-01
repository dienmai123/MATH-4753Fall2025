#' Birthday problem: Probability of at least one shared birthday
#'
#' Computes the probability that, among \code{n} people, at least two
#' share the same birthday. Assumes 365 equally likely days and independence.
#'
#' @param n Integer vector of group sizes.
#' @param days Number of equally likely birthdays (default 365).
#'
#' @return A numeric vector of probabilities between 0 and 1.
#'
#' @examples
#' birthday(20:25)
#' birthday(c(1, 23, 50))
#'
#' @export
birthday <- function(n, days = 365) {
  n <- as.integer(n)

  # quick boundary cases
  out <- rep(NA_real_, length(n))
  too_small <- n <= 1L
  too_large <- n > days

  out[too_small] <- 0.0
  out[too_large] <- 1.0

  idx <- which(!too_small & !too_large)
  if (length(idx)) {
    k <- n[idx]
    # log(P(no shared)) = log(days!) - log((days - k)!) - k * log(days)
    log_p_no <- lfactorial(days) - lfactorial(days - k) - k * log(days)
    p_no <- exp(log_p_no)
    out[idx] <- pmin(pmax(1 - p_no, 0), 1)
  }

  out
}
