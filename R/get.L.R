#' data_checks
#'
#' @param group_contbn
#' @param fams
#' @return L

get.L <- function(group_contbn, fams) {
  #generate L
  n_age_classes <- max(group_contbn$age)
  current_sel_rounds <- (max(fams$selection_round) - n_age_classes + 2):max(fams$selection_round)
  current_fams <- fams[fams[,"selection_round"] %in% current_sel_rounds,"family_id"]
  L <- matrix(0, nrow = length(current_fams), ncol = length(current_sel_rounds))
  rownames(L) <- current_fams
  colnames(L) <- current_sel_rounds

  for(sr in current_sel_rounds) {
    tmp <- fams[fams[,"selection_round"] == sr,"family_id"]
    L[rownames(L) %in% tmp, as.character(sr)] <- 1/(sum(rownames(L) %in% tmp))
    rm(tmp)
  }
  colnames(L) <- ncol(L):1
  return(L)
}
library(compiler)
get.L <- cmpfun(get.L)
