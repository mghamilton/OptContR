#' group_contbn
#'
#' @param group_contbn
#' @return n_fams
#' @return Lbar_male
#' @return Lbar_female
#' @return group_contbn

get.r <- function(group_contbn) {
  #group_contbn should be ordered by sex (males first), then age (oldest first)

  n_fams <- sum(group_contbn$fams_n)/2
  group_contbn$s_1_plus_s_2 <- group_contbn$fams_n / (2 * n_fams)
  group_contbn$age.s_1_plus_s_2 <- group_contbn$age * group_contbn$s_1_plus_s_2

  #generation interval
  Lbar_male <- 2 * sum(group_contbn[group_contbn$sex == "male", "age.s_1_plus_s_2"])
  Lbar_female <- 2 * sum(group_contbn[group_contbn$sex == "female", "age.s_1_plus_s_2"])

  #r
  group_contbn$r <- NA
  group_contbn$age_t_plus_1 <- group_contbn$age - 1
  for(age in max(group_contbn$age):1) {
    group_contbn[group_contbn$sex == "male" & group_contbn$age == age,"r"] <- sum(group_contbn[group_contbn$sex == "male" & group_contbn$age %in% (max(group_contbn$age):age),"s_1_plus_s_2"]) / Lbar_male
  }
  for(age in max(group_contbn$age):1) {
    group_contbn[group_contbn$sex == "female" & group_contbn$age == age,"r"] <- sum(group_contbn[group_contbn$sex == "female" & group_contbn$age %in% (max(group_contbn$age):age),"s_1_plus_s_2"]) / Lbar_female
  }

  group_contbn <- group_contbn

  return(list(n_fams = n_fams,
              Lbar_male = Lbar_male,
              Lbar_female = Lbar_female,
              group_contbn = group_contbn))
}
library(compiler)
get.r <- cmpfun(get.r)


