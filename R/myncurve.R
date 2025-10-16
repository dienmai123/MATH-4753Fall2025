#' Draw a normal curve and shade P(X <= a)
#'
#' Plot N(mu, sigma^2), shade the left tail up to `a`, and return the
#' probability P(X <= a) along with the inputs.
#'
#' @param mu    Numeric scalar. Mean of the normal distribution.
#' @param sigma Numeric scalar (> 0). Standard deviation.
#' @param a     Numeric scalar. Cutoff for the shaded region.
#'
#' @return A named list with elements `mu`, `sigma`, and `probability`.
#'
#' @examples
#' \donttest{
#' # Example run
#' res <- myncurve(mu = 10, sigma = 5, a = 6)
#' res$probability
#' }
#'
#' @export
myncurve <- function(mu, sigma, a) {
  # validate inputs
  stopifnot(is.numeric(mu),    length(mu)    == 1L)
  stopifnot(is.numeric(sigma), length(sigma) == 1L, sigma > 0)
  stopifnot(is.numeric(a),     length(a)     == 1L)

  # show the curve over mu +/- 3*sigma
  xl <- mu - 3 * sigma
  xr <- mu + 3 * sigma

  # grid and density values
  xs <- seq(xl, xr, length.out = 1000)
  ys <- stats::dnorm(xs, mean = mu, sd = sigma)

  # title in a simple style
  ttl <- sprintf("Normal Curve (mu = %g , sigma = %g )", mu, sigma)

  # base plot with a blue outline
  graphics::plot(xs, ys, type = "l", lwd = 2, col = "blue",
                 xlab = "x", ylab = "Density", main = ttl)

  # shade from the left edge to a (clipped at xr so it stays on the plot)
  right <- min(a, xr)
  if (right > xl) {
    xshade <- seq(xl, right, length.out = 600)
    yshade <- stats::dnorm(xshade, mean = mu, sd = sigma)
    graphics::polygon(c(xl, xshade, right),
                      c(0,  yshade, 0),
                      col = grDevices::rgb(0.5, 0.8, 0.95, 0.5),
                      border = NA)
    graphics::lines(xs, ys, lwd = 2, col = "blue")
  }

  # vertical line at a
  graphics::abline(v = a, col = "gray40", lty = 2, lwd = 2)

  # compute and return probability
  prob <- stats::pnorm(a, mean = mu, sd = sigma)
  list(mu = mu, sigma = sigma, probability = prob)
}
