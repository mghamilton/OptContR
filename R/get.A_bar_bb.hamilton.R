#' get.A_bar_bb.hamilton
#'
#' @param group_contbn
#' @param fams
#' @param A_cand_fams
#' @return L

get.A_bar_bb.hamilton <- function(group_contbn, fams, A_cand_fams) {

  #generate family relationship matrix
  A_fams <- A_cand_fams[fams[,"family_id"], paste0(fams[,"family_id"],"__2")]
  colnames(A_fams) <- rownames(A_fams)

  L <- get.L(group_contbn = group_contbn, fams = fams)

  #current A_fams
  A_fams_in_A_bar_bb <- A_fams[rownames(A_fams) %in% rownames(L), colnames(A_fams) %in% rownames(L)]

#  A_bar_bb <- crossprod(L, A_fams_in_A_bar_bb %*% L)
  A_bar_bb <- t(L) %*% A_fams_in_A_bar_bb %*% L
  A_bar_bb <- cbind(A_bar_bb, A_bar_bb)
  A_bar_bb <- rbind(A_bar_bb, A_bar_bb)
  colnames(A_bar_bb) <- paste0(rep(c("male_", "female_"), each = ncol(L)), rep(1+as.numeric(colnames(L)),2))
  rownames(A_bar_bb) <- colnames(A_bar_bb)

  return(list(L = L,
              A_bar_bb = A_bar_bb,
              A_fams = A_fams,
              A_fams_in_A_bar_bb = A_fams_in_A_bar_bb))
}

