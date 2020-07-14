#' data_checks
#'
#' @param C_max,
#' @param group_contbn
#' @param ped
#' @param fams
#' @param candidates
#' @param parents_max
# @param optimise_r
#' @param method
#' @return Error message if error identified

data_checks <- function(C_max,
                        group_contbn,
                        ped,
                        fams,
                        candidates,
                        parents_max,
                      #  optimise_r,
                        method
) {

  #Data checks###########################################################################################

  #C_max
  if(!is.numeric(C_max)) {
    stop("C_max must be a number")
  }

  if(C_max > 2 | C_max < 0) {
    stop("C_max must be between 0 and 2")
  }

  #group_contbn
  if(!is.data.frame(group_contbn)) {
    stop("group_contbn must be a data frame")
  }

  if(sum(colnames(group_contbn) %in% c("sex", "age", "fams_n")) != 3) {
    stop("group_contbn must be a data frame including column names: \'sex\', \'age\' and \'fams_n\'")
  }
  group_contbn <- group_contbn[,c("sex", "age", "fams_n")]

  if(sum(group_contbn$sex %in% c("male", "female")) != nrow(group_contbn)) {
    stop("The \'sex\' column of group_contbn must be either \'male\' or \'female\'")
  }

  if(sum(!is.wholenumber(group_contbn$age) | group_contbn$age <= 0) > 0) {
    stop("The \'age\' column of group_contbn must be an integer greater than 0")
  }

  if(sum(!is.wholenumber(group_contbn$fams_n)) > 0) {
    stop("The \'fams_n\' column of group_contbn must be an integer greater than 0")
  }

  if(sum(group_contbn$fams_n >= 0) != nrow(group_contbn)) {
    stop("The \'fams_n\' column of group_contbn must be an integer greater than 0")
  }

  if(sum(group_contbn[group_contbn$sex == "male", "fams_n"]) != sum(group_contbn[group_contbn$sex == "female", "fams_n"])) {
    stop("The \'fams_n\' column of group_contbn must sum to the same number of both males and females")
  }

  #ped
  if(method == "Meuwissen") {
    if(sum(colnames(ped) %in% c("id", "sire", "dam", "ebv", "selection_round")) != 5) {
      stop("When the Meuwissen method is adopted \'ped\' must be a data frame including column names: \'id\', \'sire\', \'dam\', \'ebv\' and \'selection_round\'.  It may also include a \'sex\' column.")
    }

    if(sum(colnames(ped) %in% c("id", "sire", "dam", "ebv", "selection_round")) == 5) {
      ped <- ped[,c("id", "sire", "dam", "ebv", "selection_round")]
    }

    if(sum(colnames(ped) %in% c("id", "sire", "dam", "ebv", "selection_round", "sex")) == 6) {
      ped <- ped[,c("id", "sire", "dam", "ebv", "selection_round", "sex")]





      if(sum(!ped$sex %in% c("male", "female")) > 0) {
        stop("The \'sex\' of a in individual in \'ped\' must be \'male\' or \'female\'.  The \'sex\' column must be removed if not all sexes are known")
      }
      #Maybe should apply this constraint to the age classes in the 'candidates' file only!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    }






    if(sum(!is.wholenumber(ped$selection_round)) > 0) {
      stop("\'selection_round\' column of \'ped\' must be an integer")
    }

  }

  if(method == "Hamilton") {
    if(sum(colnames(ped) %in% c("id", "sire", "dam", "ebv")) != 4) {
      stop("When the Hamilton method is adopted \'ped\' must be a data frame including column names: \'id\', \'sire\', \'dam\' and \'ebv\'")
    }
    ped <- ped[,c("id", "sire", "dam", "ebv")]
  }

  ped$id   <- as.character(ped$id)
  ped$sire <- as.character(ped$sire)
  ped$dam  <- as.character(ped$dam)

  if(!is.numeric(ped$ebv)) {
    stop("The \'ebv\' column of \'ped\' must be numeric")
  }

  if(
    sum((ped$sire == 0 | is.na(ped$sire)) & !(ped$dam == 0 | is.na(ped$dam))) > 0 |
    sum(!(ped$sire == 0 | is.na(ped$sire)) & (ped$dam == 0 | is.na(ped$dam))) > 0) {
    stop("All founders must be defined (i.e. can't define an individual with one known and one unknown parent)")
  }

  if(sum(ped[ped$sire != 0 & !is.na(ped$sire), "sire"] %in% ped[ped$sire != 0 & !is.na(ped$sire), "dam"]) > 0) {
    stop("A \'sire\' in \'ped\' cannot be included as a \'dam\'.")
  }

  #fams               # dataframe. Fields: family_id sire dam selection_round
  if(method == "Meuwissen" & !is.null(fams)) {
    stop("When the Meuwissen method is specified \'fams\' must be NULL")
  }

  if(method == "Hamilton") {
    if(sum(colnames(fams) %in% c("family_id", "sire", "dam", "selection_round")) != 4) {
      stop("\'fams\' must be a data frame including column names: \'family_id\', \'sire\', \'dam\' and \'selection_round\'")
    }
    fams <- fams[,c("family_id", "sire", "dam", "selection_round")]

    fams[is.na(fams[,"sire"]), "sire"] <- 0
    fams[is.na(fams[,"dam"]), "dam"] <- 0

    fams$family_id   <- as.character(fams$family_id)
    fams$sire <- as.character(fams$sire)
    fams$dam  <- as.character(fams$dam)

    if(sum(!is.wholenumber(fams$selection_round)) > 0) {
      stop("\'selection_round\' column of \'fams\' must be an integer")
    }







    if(sum(duplicated(fams[,c("family_id", "sire", "dam")])) > 0) {
      stop("\'sire\' and \'dam\' combinations must be unique to one \'family_id\' in \'fams\'")
    }







    if(sum(duplicated(fams)) > 0) {
      stop("rows must be unique in \'fams\'")
    }

    #check all fams$sires in ped$id column, check all fams$dams in ped$id column
    if(sum(!fams$sires %in% ped$id) > 0 | sum(!fams$dams %in% ped$id) > 0 ) {
      stop("All \'sires\' and \'dams\' in \'fams\' must be present in the \'id\' column of \'ped\'")
    }

    tmp <- left_join(ped, fams[,c("family_id", "sire", "dam")], by = c("sire", "dam"))

    if(
      sum(!((tmp$sire == 0 | is.na(tmp$sire)) & (tmp$dam == 0 | is.na(tmp$dam))) &
          is.na(tmp$family_id)) > 0
    ) {
      stop("\'fams\' must include families of all non-founder individuals in \'ped\'")
    }

    tmp           <- tmp[,c("id", "ebv", "family_id")]
    colnames(tmp) <- c("sire", "sire_ebv", "sire_fam")
    fams          <- left_join(fams, tmp, by = c("sire"))
    colnames(tmp) <- c("dam", "dam_ebv", "dam_fam")
    fams          <- left_join(fams, tmp, by = c("dam"))
    fams$fam_ebv  <- (fams$sire_ebv + fams$dam_ebv) / 2

  }

  #candidates

  if(sum(colnames(candidates) %in% c("id", "cprev_fam", "cmax_fam", "cmin_fam", "sex", "age")) != 6) {
    stop("\'candidates\' must be a data frame including column names: \'id\', \'cprev_fam\', \'cmax_fam\', \'cmin_fam\', \'sex\' and \'age\'")
  }
  candidates <- candidates[,c("id", "cprev_fam", "cmax_fam", "cmin_fam", "sex", "age")]
  candidates$id <- as.character(candidates$id)

  if(sum(!candidates$id %in% ped$id) > 0) {
    stop("Individuals in the \'candidates\' file must be present in the \'ped\' file")
  }

  if(sum(!is.wholenumber(candidates$cprev_fam)) > 0) {
    stop("\'cprev_fam\' column of \'candidates\' must be an integer and greater than or equal to zero")
  }

  if(sum(candidates$cprev_fam < 0) > 0) {
    stop("\'cprev_fam\' column of \'candidates\' must be an integer and greater than or equal to zero")
  }

  if(sum(!is.wholenumber(candidates$cmax_fam)) > 0) {
    stop("\'cmax_fam\' column of \'candidates\' must be an integer and greater than or equal to zero")
  }

  if(sum(candidates$cmax_fam < 0) > 0) {
    stop("\'cmax_fam\' column of \'candidates\' must be an integer and greater than or equal to zero")
  }

  if(sum(!is.wholenumber(candidates$cmin_fam)) > 0) {
    stop("\'cmin_fam\' column of \'candidates\' must be an integer and greater than or equal to zero")
  }

  if(sum(candidates$cmin_fam < 0) > 0) {
    stop("\'cmin_fam\' column of \'candidates\' must be an integer and greater than or equal to zero")
  }

  if(sum(candidates$cmin_fam > candidates$cmax_fam) > 0) {
    stop("\'cmin_fam\' column of \'candidates\' must be less than or equal to \'cmax_fam\'")
  }

  if(sum(candidates$cprev_fam > candidates$cmax_fam) > 0) {
    stop("\'cprev_fam\' column of \'candidates\' must be less than or equal to \'cmax_fam\'")
  }

  if(sum(candidates$cprev_fam > candidates$cmin_fam) > 0) {
    stop("\'cprev_fam\' column of \'candidates\' must be less than or equal to \'cmin_fam\'")
  }

  if(sum(!candidates$sex %in% c("male", "female")) > 0) {
    stop("The \'sex\' of a in individual in \'candidates\' must be \'male\' or \'female\'")
  }

  if(sum(!is.wholenumber(candidates$age) | candidates$age <= 0) > 0) {
    stop("The \'age\' column of the \'candidates\' file must be an integer greater than 0")
  }

  if(sum(!paste0(candidates$sex, "_", candidates$age) %in% paste0(group_contbn$sex, "_", group_contbn$age)) > 0) {
    stop("Combination of \'sex\' and \'age\' in the \'candidates\' file must be present in the \'group_contbn\' file.")
  }

  if(length(candidates$id) != length(unique(candidates$id))) {
    stop("\'id\' in the \'candidates\' file must be represent a unique identifiers (i.e. each individual must be present only once).")
  }

  if(sum(!is.wholenumber(group_contbn$age) | group_contbn$age <= 0) > 0) {
    stop("The \'age\' column of group_contbn must be an integer greater than 0")
  }

  if("sex" %in% colnames(ped)) {
    tmp <- left_join(candidates[,c("id", "sex")], ped[,c("id", "sex")], by = "id")
    if(sum(tmp[,2] != tmp[,3]) > 0) {
      stop("The \'sex\' of each individual must be the same in both \'ped\' and \'candidates\')")
    }
  }

  #parents_max
  if(!is.na(parents_max) & (!is.wholenumber(parents_max) | parents_max <= 2)) {
    stop("parents_max must be an integer greater than 2")
  }

  #optimise_r
#  if(!is.logical(optimise_r)) {
#    stop("optimise_r must be either \'TRUE\' or \'FALSE\'")
#  }

  #method
  if(!method %in% c("Hamilton", "Meuwissen")) {
    stop("method must be either \'Hamilton\' or \'Meuwissen\'")
  }

  return("No errors detected")
}
library(compiler)
data_checks <- cmpfun(data_checks)

