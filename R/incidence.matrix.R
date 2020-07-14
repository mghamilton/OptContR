#' Generates an incidence matrix
#'
#' @param row_values vector of values equal in length to row_names.  Element values must be present in col_names.
#' @param row_names vector of row names
#' @param col_names vector of column names
#' @return  An incidence matrix with 1 in elements where row_values match col_names

incidence.matrix <- function(row_values, row_names, col_names) {
  i_mat <- matrix(rep(row_values, length(col_names)) ==
                    rep(col_names, each = length(row_values)),
                  ncol = length(col_names)) * 1
  rownames(i_mat) <- row_names
  colnames(i_mat) <- col_names
  return(i_mat)
}
library(compiler)
incidence.matrix <- cmpfun(incidence.matrix)

