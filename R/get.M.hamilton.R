#' get.M.hamilton
#'
#' @param c1_candidates
#' @param c2_candidates
#' @param A_cand_fams
#' @param L
#' @return M_1
#' @return M_2
#' @return A_1bf
#' @return A_2bf

get.M.hamilton <- function(c1_candidates, c2_candidates, A_cand_fams, L) {

  #M_1
  A_1bf <- A_cand_fams[c1_candidates[,"id"], rownames(L)]

  M_1 <- A_1bf %*% L
  M_1 <- cbind(M_1, M_1)
  colnames(M_1) <- paste0(rep(c("male_", "female_"), each = ncol(L)), rep(1+as.numeric(colnames(L)),2))

  #M_2
  A_2bf <- A_cand_fams[c2_candidates[,"id"], rownames(L)]

  M_2 <- A_2bf %*% L
  M_2 <- cbind(M_2, M_2)
  colnames(M_2) <- colnames(M_1)

  return(list(M_1 = M_1,
              M_2 = M_2,
              A_1bf = A_1bf,
              A_2bf = A_2bf))
}

