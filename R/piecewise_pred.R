#' Piecewise prediction function
#'
#' Predicts values from a piecewise regression model
#' where the slope changes at a given breakpoint (default = 18).
#'
#' @param x Numeric vector of predictor values.
#' @param coef Numeric vector of model coefficients (from lm()).
#' @param breakpoint The value of x where the slope changes. Default is 18.
#'
#' @return Numeric vector of predicted values.
#' @examples
#' coef <- c(4.5, 1.2, -0.3)
#' piecewise_pred(20, coef)
#'
#' @export
piecewise_pred <- function(x, coef, breakpoint = 18){
  coef[1] + coef[2]*x + coef[3]*pmax(x - breakpoint, 0)
}
