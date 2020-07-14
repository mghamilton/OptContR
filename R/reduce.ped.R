#' Removes unncessary rows from a 3-column pedigree file by retaining only ancestors of specified individuals
#'
#' @param ped A three column pedigree file including candidate individuals only
#' @param indivs List of individuals for which ancestors will be retained
#' @return A a 3-column pedigree file retaining only ancestors of specified individuals (and the individuals themselves).

reduce.ped <- function(ped,indivs) {
  ped[,1] <- as.character(ped[,1])
  ped[,2] <- as.character(ped[,2])
  ped[,3] <- as.character(ped[,3])
  indivs  <- as.character(indivs)

  if(sum(!indivs %in% ped[,1]) > 0) {
    stop("\'indivs\' in input to \'reduce.ped\' function are not in the first column of the \'ped\' input.")
  }

  indivs_new <- indivs
  n_indivs <- length(indivs)
  n_indivs_new <- n_indivs + 1

  while(n_indivs_new > n_indivs) {
    indivs <- indivs_new
    indivs_new <- unique(c(indivs,ped[ped[,1] %in% indivs,2],ped[ped[,1] %in% indivs,3]))
    n_indivs <- length(indivs)
    n_indivs_new <- length(indivs_new)
  }
  ped <- ped[ped[,1] %in% indivs_new,]

  return(ped)
}
library(compiler)
reduce.ped <- cmpfun(reduce.ped)





