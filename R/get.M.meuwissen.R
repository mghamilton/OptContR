#' get.M.meuwissen
#'
#' @param ped_b
#' @param A_cand_fams
#' @param c1_candidates
#' @param c2_candidates
#' @param group_contbn
#' @return J
#' @return A_1b
#' @return A_2b
#' @return M_1
#' @return M_2

get.M.meuwissen <- function(ped_b, A_cand_fams, c1_candidates, c2_candidates, group_contbn) {
  #returns J, M_1 and M_2

  A_1b <- A_cand_fams[c1_candidates[,"id"],ped_b[,"id"]]

  A_2b <- A_cand_fams[c2_candidates[,"id"],ped_b[,"id"]]

  #generate J matrices
  J <- incidence.matrix(row_values = ped_b$sex_age,
                        row_names = ped_b$id,
                        col_names = group_contbn[group_contbn[,"age"] != max(group_contbn[,"age"]),"sex_age"])
  J <- J * matrix(rep(1 / colSums(J), each = nrow(J)), nrow = nrow(J))
  J[is.na(J[,])] <- 0

  M_1 <- A_1b %*% J
  M_2 <- A_2b %*% J

  return(list(J = J,
              A_1b = A_1b,
              A_2b = A_2b,
              M_1 = M_1,
              M_2 = M_2))
}
