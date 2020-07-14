#' get.A_bar_bb.meuwissen
#'
#' @param groups
#' @param ped
#' @param A_cand_fams
#' @param J
#' @return A_bar_bb
#' @return A_cand_in_A_bar_bb

get.A_bar_bb.meuwissen <- function(groups, ped, A_cand_fams, J) {

  A_cand_in_A_bar_bb <- A_cand_fams[rownames(J),rownames(J)]
 # A_bar_bb <- crossprod(J,A_cand_in_A_bar_bb %*% J)
  J <- as.matrix(J)
  A_bar_bb <- t(J) %*% A_cand_in_A_bar_bb %*% J

  return(list(A_bar_bb=A_bar_bb,
              A_cand_in_A_bar_bb=A_cand_in_A_bar_bb))
}
