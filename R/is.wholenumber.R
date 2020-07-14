#' Checks if a numeric input is a whole number
#'
#' @param x a numeric value
#' @return  TRUE if x is a whole number and FALSE if it is not.

is.wholenumber <- function(x) {
  abs(x - round(x)) < .Machine$double.eps^0.5
}
