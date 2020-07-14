#' Generates D and Z matrices
#'
#' @param ped A three column pedigree file including candidate individuals only
#' @param A_parents A numerator relationship matrix including parents only
#' @return Z
#' @return D_diag
#' @return D_inv_diag
#' @return F_bar

get.D.and.Z <- function(ped, A_parents) {

  parents_of_cand <- unique(c(ped[,2], ped[,3]))
  A_parents <- A_parents[rownames(A_parents) %in% parents_of_cand,
                         colnames(A_parents) %in% parents_of_cand]

  F_bar <- ((diag(A_parents[ped[,2], ped[,2]])+
               diag(A_parents[ped[,3], ped[,3]]))/2) - 1
  D_diag <- 0.5 * (1- F_bar)
  names(D_diag) <- ped[,1]
  D_inv_diag <- 1 / D_diag
  names(D_inv_diag) <- ped[,1]

  Z <- incidence.matrix(row_values = ped[,2],
                        #  col_values = parents_of_cand,
                        row_names = ped[,1],
                        col_names = colnames(A_parents)) * 0.5 +
    incidence.matrix(row_values = ped[,3],
                     # col_values = parents_of_cand,
                     row_names = ped[,1],
                     col_names = colnames(A_parents)) * 0.5

  #Identify individuals that are both parents and candidates and replace elements in Z and D_inv_diag (see page 3217 of Hinrich et al (2009))
  tmp <- rownames(Z)[rownames(Z) %in% colnames(Z)]
  Z[rownames(Z) %in% tmp,] <- 0
  if(length(tmp) != 1) {
    diag(Z[rownames(Z) %in% tmp,colnames(Z) %in% tmp]) <- 1
  } else {
    Z[rownames(Z) %in% tmp,colnames(Z) %in% tmp] <- 1
  }
  D_diag[names(D_diag) %in% tmp] <- 10^-8
  D_inv_diag[names(D_inv_diag) %in% tmp] <- 10^8
  rm(tmp)
  return(list(Z = Z, D_diag = D_diag, D_inv_diag = D_inv_diag, F_bar = F_bar))
}
library(compiler)
get.D.and.Z <- cmpfun(get.D.and.Z)

