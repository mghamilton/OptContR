#' get.c
#'
#' Implements optimal contributions method detailed in Hamilton MG (2020) Optimal  contributions in highly fecund species with overlapping generations.
#'
#' @title Implements optimal contributions method detailed in Hamilton MG (2020) Optimal contributions in highly fecund species with overlapping generations.
#' @param C_max constraint on the average of relationships between members of the population (numeric between 0 and 2).
#' @param group_contbn a data frame defining sex-age groups with the following columns:
#' \itemize{
#'  \item{\strong{sex}} {- 'male' or 'female'.}
#'  \item{\strong{age}} {age group (integer greater than 0).}
#'  \item{\strong{fams_n}} {number of families in sex-age group (integer greater than 0).}
#' }
#' @param ped a data frame with the following columns:
#' \itemize{
#'  \item{\strong{id}} {- individual identifier (character).}
#'  \item{\strong{sire}} {- sire identifier (character). '0' or NA if unknown.}
#'  \item{\strong{dam}} {- dam identifier (character). '0' or NA if unknown.}
#'  \item{\strong{ebv}} {- estimated breeding value of individual (numeric)}
#'  \item{\strong{selection_round}} {- selection round in which individual was generated (sequential integer).  Not required if method = 'Hamilton'.}
#'  \item{\strong{sex}} {- 'male' or 'female'. Not required if method = 'Hamilton' and may be excluded if method = 'Meuwissen'.}
#' }
#' @param fams Must be NULL if method = 'Meuwissen'.  If method = 'Hamilton', a data frame with the following columns:
#' \itemize{
#'  \item{\strong{family_id}} {- full-sib family identifier (character).}
#'  \item{\strong{sire}} {- sire identifier (character). '0' or NA if unknown.}
#'  \item{\strong{dam}} {- dam identifier (character). '0' or NA if unknown.}
#'  \item{\strong{selection_round}} {- selection round in which individual was generated (sequential integer).  Not required if method = 'Hamilton'.}
#' }
#' @param candidates
#' \itemize{
#'  \item{\strong{id}} {- individual identifier (character).}
#'  \item{\strong{cprev_fam}} {number of families that the individual has previously contributed to (integer greater than or equal to 0).}
#'  \item{\strong{cmax_fam}} {maximum number of families that the individual can contribute to (integer greater than or equal to 0).}
#'  \item{\strong{cmin_fam}} {maximum number of families that the individual can contribute to (integer greater than or equal to 0).}
#'  \item{\strong{ebv}} {- estimated breeding value of individual (numeric)}
#'  \item{\strong{sex}} {- 'male' or 'female'. Not required if method = 'Hamilton' and may be excluded if method = 'Meuwissen'.}
#'  \item{\strong{age}} {age of individual (integer greater than 0).}
#'  }
#' @param parents_max maximum number of parents of each sex that can be selected as a parent from any given family.  cprev_fam and cmin_fam in the candidates data frame override this constraint (integer greater than 2 or NA).
# @param optimise_r If TRUE, fams_n are used as starting values for the r vector (Logical). See Meuwissen THE, Sonesson AK (1998) Maximizing the response of selection with a predefined rate of inbreeding: Overlapping generations. J Anim Sci 76:2575-2583
#' @param method optimal contribution method to implement ('Hamilton' or 'Meuwissen') - refer to Hamilton (2020) and Meuwissen and Sonesson (1998)
#' @return \strong{c_1} Optimal contributions of Class 1 individuals (vector)
#' @return \strong{c_2} Contributions of Class 2 individuals (vector)
#' @return \strong{contbn_realised} Contributions of all individuals by sex-age group
# @return \strong{A_cand_in_A_bar_bb} Relationships used to compute A_bar_bb
#' @return \strong{A_bar_bb} Average relationships among existing sex-age groups
#' @return \strong{C_t_plus_1} Average relationship at time t+1
#' @return \strong{EBV_avg} Weighted mean estimated breeding value given c_1 and c_2
# @return \strong{Hc} Equivalent to H in the Appendix of Meuwissen (1997)
#' @return \strong{Lbar_male} Realised average generation interval for males
#' @return \strong{Lbar_female} Realised average generation interval for females
#' @return \strong{group_contbn} Data frame containing inputted group_contbn, sex_age group, s vector and r vector
#' @return \strong{ped} Pedigree file (with EBVs) for candidates and their ancestors
#' @return \strong{J} a matrix with rows representing all individuals in age groups 2 to q at time t+1 and columns representing the corresponding sex-age groups
#' @return \strong{A_1b} between-individual additive genetic relationship matrix of dimensions equal to the number of Class 1 candidates by the number of existing individuals to be retained
#' @return \strong{A_2b} between-individual additive genetic relationship matrix of dimensions equal to the number of Class 2 candidates by the number of existing individuals to be retained
#' @return \strong{M_1} M matrix of dimensions equal to the number of Class 1 candidates by columns representing sex-age groups
#' @return \strong{M_2} M matrix of dimensions equal to the number of Class 2 candidates by columns representing sex-age groups
#' @examples
#' #Retrieve data for example from Hamilton 2020 (supplementary materials)
#' data(group_contribution_example)
#' data(ped_example)
#' data(fams_example)
#' data(candidates_example)
#'
#' #Run example from Hamilton 2020 (supplementary materials), implementing method = "Hamilton".
#' get.c(C_max = 0.45,
#'       group_contbn = group_contribution_example,
#'       ped = ped_example[,c('id', 'sire', 'dam', 'ebv')],
#'       fams = fams_example,
#'       candidates = candidates_example,
#'       parents_max = NA,
#'       method = "Hamilton")
#'
#' #Run example from Hamilton 2020 (supplementary materials), implementing method = "Meuwissen".
#' get.c(C_max = 0.45,
#'       group_contbn = group_contribution_example,
#'       ped = ped_example,
#'       fams = NULL,
#'       candidates = candidates_example,
#'       parents_max = NA,
#'       method = "Meuwissen")

#' @references Hamilton MG (2020) Optimal  contributions in highly fecund species with overlapping generations
#' @references Meuwissen THE (1997) Maximizing the response of selection with a predefined rate of inbreeding. J Anim Sci 75:934-940
#' @references Meuwissen THE, Sonesson AK (1998) Maximizing the response of selection with a predefined rate of inbreeding: Overlapping generations. J Anim Sci 76:2575-2583

#' @export

get.c <- function(C_max, group_contbn, ped, fams = NULL, candidates, parents_max = NA,  method) { #optimise_r = FALSE,

  library(compiler)
  library(tidyverse)
  library(nadiv)

  data_checks(
    C_max = C_max,
    group_contbn = group_contbn,
    ped = ped,
    fams = fams,
    candidates = candidates,
    parents_max = parents_max,
 #   optimise_r = optimise_r,
    method = method
  )
  rownames(candidates) <- as.character(candidates$id)
  candidates <- candidates[ped[ped[, "id"] %in% candidates$id, "id"], ]

  print("Generating r and generation intervals")
  #group_contbn should be ordered by sex (males first), then age (oldest first)
  group_contbn <- group_contbn[order(group_contbn$age, decreasing = TRUE), ]
  group_contbn <- group_contbn[order(group_contbn$sex, decreasing = TRUE), ]

  #define sex_age age columns
  group_contbn$sex_age <-
    paste0(group_contbn$sex, "_", group_contbn$age)
  candidates$sex_age <- paste0(candidates$sex, "_", candidates$age)

  tmp <- get.r(group_contbn = group_contbn)
  n_fams <- tmp$n_fams
  Lbar_male <- tmp$Lbar_male
  Lbar_female <- tmp$Lbar_female
  group_contbn <- tmp$group_contbn
  rm(tmp)

  print("Generating A matrix")

  #reduced the size of the pedigree file
  if (method == "Hamilton") {
    ped <-
      reduce.ped(ped = ped, indivs = unique(c(candidates$id, fams$sire, fams$dam)))
    parents <- unique(c(fams$sire, fams$dam))
  }

  if (method == "Meuwissen") {
    ped$age <- max(ped$selection_round) - ped$selection_round + 1

    parents <- unique(c(ped[ped$age %in% unique(group_contbn$age),"sire"],
                        ped[ped$age %in% unique(group_contbn$age),"dam"]))
    #  parents <- unique(c(ped$sire, ped$dam))
    parents <- parents[parents != 0]

    ped <- reduce.ped(ped = ped, indivs = ped[ped$age %in% unique(group_contbn$age),"id"])
  }

  #c1 and c2 candidates
  c1_candidates <-
    ped[ped[, "id"] %in% candidates[candidates[, "cmax_fam"] > candidates[, "cprev_fam"], "id"], c("id", "sire", "dam")]
  c1_candidates <-
    left_join(c1_candidates, candidates[, c("id", "sex_age")], by = "id")
  if (method == "Hamilton") {
    c1_candidates <-
      left_join(c1_candidates, unique(fams[, c("family_id", "sire", "dam")]), by = c("sire", "dam"))
  }
  rownames(c1_candidates) <- as.character(c1_candidates$id)

  c2_candidates <-
    ped[ped[, "id"] %in% candidates[candidates[, "cprev_fam"] > 0, "id"], c("id", "sire", "dam")]
  c2_candidates <-
    left_join(c2_candidates, candidates[, c("id", "sex_age", "cprev_fam")], by = "id")
  if (method == "Hamilton") {
    c2_candidates <-
      left_join(c2_candidates, unique(fams[, c("family_id", "sire", "dam")]), by = c("sire", "dam"))
  }
  rownames(c2_candidates) <- as.character(c2_candidates$id)

  #A matrix

  q <- max(group_contbn$age)

  if (method == "Hamilton") {
    tmp <- rbind(fams[, 1:3],fams[, 1:3])
    tmp[,"family_id"] <- c(fams[,"family_id"], paste0(fams[,"family_id"],"__2"))
    colnames(tmp) <- colnames(ped)[1:3]

    indivs <- unique(c(parents, c1_candidates[,1], c2_candidates[,1], fams[,1], paste0(fams[,1],"__2")))
    ped_cand_fams <- reduce.ped(ped = rbind(ped[, 1:3],tmp), indivs = indivs)
    rm(tmp, indivs)
  }

  if (method == "Meuwissen") {
    ped_cand_fams <- reduce.ped(ped = ped[, 1:3], indivs = unique(c(parents, ped[ped$age <= q,"id"], fams[,1])))
  }

  ped_cand_fams[ped_cand_fams[, "sire"] == "0" , "sire"] <- NA
  ped_cand_fams[ped_cand_fams[, "dam"]  == "0" , "dam"] <- NA

  A_cand_fams <- makeA(ped_cand_fams)

  print("Generating A_11")

  A_11 <- A_cand_fams[c1_candidates[,"id"],c1_candidates[,"id"]]

  if(nrow(c2_candidates) != 0) {
    print("Generating A_12")

    A_12 <- A_cand_fams[c1_candidates[,"id"],c2_candidates[,"id"]]

    print("Generating A_22")

    A_22 <- A_cand_fams[c2_candidates[,"id"], c2_candidates[,"id"]]
  } else {
    print("Generating A_12")
    A_12 <- Matrix(NA, nrow = nrow(c1_candidates), ncol = 0)
    rownames(A_12) <- rownames(A_11)

    print("Generating A_22")
    A_22 <- Matrix(NA, nrow = 0, ncol = 0)
  }

  print("Generating A_bar_bb and M")

  if (method == "Hamilton") {
    tmp <-
      get.A_bar_bb.hamilton(group_contbn = group_contbn,
                            fams = fams,
                            A_cand_fams = A_cand_fams)
    L <- tmp$L
    A_bar_bb <- tmp$A_bar_bb
    A_fams   <- tmp$A_fams
    A_fams_in_A_bar_bb <- tmp$A_fams_in_A_bar_bb
    rm(tmp)
  }

  if (method == "Hamilton") {
    tmp <-
      get.M.hamilton(
        c1_candidates = c1_candidates,
        c2_candidates = c2_candidates,
        A_cand_fams = A_cand_fams,
        L = L
      )
    M_1 <- tmp$M_1
    M_2 <- tmp$M_2
    A_1bf <- tmp$A_1bf
    A_2bf <- tmp$A_2bf
    rm(tmp)
  }

  if (method == "Meuwissen") {

    ped$age <- max(ped$selection_round) - ped$selection_round + 1
    ped$sex_age <- paste0(ped$sex, "_", ped$age)

    ped_b <- ped[ped$age < q,]

    tmp <- get.M.meuwissen(
      ped_b = ped_b,
      A_cand_fams = A_cand_fams,
      c1_candidates = c1_candidates,
      c2_candidates = c2_candidates,
      group_contbn = group_contbn
    )
    J <- tmp$J
    A_1b <- tmp$A_1b
    A_2b <- tmp$A_2b
    M_1 <- tmp$M_1
    M_2 <- tmp$M_2
    rm(tmp)
  }

  if (method == "Meuwissen") {
    tmp <-
      get.A_bar_bb.meuwissen(
        groups = group_contbn[group_contbn[, "age"] != max(group_contbn[, "age"]), "sex_age"],
        ped = ped,
        A_cand_fams = A_cand_fams,
        J = J
      )
    A_bar_bb <- tmp$A_bar_bb
    A_cand_in_A_bar_bb <- tmp$A_cand_in_A_bar_bb
    rm(tmp)

    rownames(A_bar_bb) <-
      group_contbn[group_contbn[, "age"] != max(group_contbn[, "age"]), "sex_age"]
    colnames(A_bar_bb) <-
      group_contbn[group_contbn[, "age"] != max(group_contbn[, "age"]), "sex_age"]
  }

  print("Generating Q")

  Q_1 <-
    incidence.matrix(
      row_values = candidates[candidates[, "id"] %in% c1_candidates[, "id"], "sex_age"],
      # col_values = group_contbn$sex_age,
      row_names = candidates[candidates[, "id"] %in% c1_candidates[, "id"], "id"],
      col_names = group_contbn$sex_age
    )

  print("Generating A_11_inv")
  A_11_inv <- solve(A_11)

  print("Generating R")
  Q_1.A_11_inv           <- crossprod(Q_1, A_11_inv)
  colnames(Q_1.A_11_inv) <- colnames(A_11_inv)

  Q_1.A_11_inv.Q_1       <- Q_1.A_11_inv %*% Q_1
  tmp_row                <-
    rowSums(Q_1.A_11_inv.Q_1) == 0 #avoid inversion if rows and cols = 0
  tmp_col                <- rowSums(Q_1.A_11_inv.Q_1) == 0

  Q_1.A_11_inv.Q_1_inv <-
    Q_1.A_11_inv.Q_1 #create matrix of correct dimensions and names with zeros
  Q_1.A_11_inv.Q_1_inv[!tmp_row, !tmp_col] <-
    solve(Q_1.A_11_inv.Q_1[!tmp_row, !tmp_col])

  r1_plus_r3  <-
    group_contbn[group_contbn$sex == "male" &
                   group_contbn$age == 1, "r"] + group_contbn[group_contbn$sex == "female" &
                                                                group_contbn$age == 1, "r"]

  R <-
    (A_11_inv - crossprod(Q_1.A_11_inv, crossprod(Q_1.A_11_inv.Q_1_inv, Q_1.A_11_inv))) /
    (r1_plus_r3 ^ 2)

  print("Generating P")

  c_2 <- matrix(c2_candidates$cprev_fam / n_fams / 2, ncol = 1)
  rownames(c_2) <- rownames(c2_candidates)
  colnames(c_2) <- "c_2"

  r_b <-
    matrix(group_contbn[group_contbn[, "age"] > 1, "r"], ncol = 1)
  rownames(r_b) <-
    paste0(group_contbn[group_contbn[, "age"] > 1, "sex"], "_", group_contbn[group_contbn[, "age"] > 1, "age"])
  colnames(r_b) <- "r_b"

  M_1.r_b <- M_1 %*% r_b

  A_12.c_2 <- A_12 %*% c_2

  P <- r1_plus_r3 ^ 2 * A_12.c_2 + r1_plus_r3 * M_1.r_b

  colnames(P) <- "P"

  print("Generating K")
#  K <- C_max -
#    r1_plus_r3 ^ 2 * crossprod(c_2, A_22) %*% c_2 -
#    2 * r1_plus_r3 * crossprod(c_2, M_2) %*% r_b -
#    crossprod(r_b, A_bar_bb) %*% r_b

  K <- C_max -
    r1_plus_r3 ^ 2 * t(c_2) %*% A_22 %*% c_2 -
    2 * r1_plus_r3 * t(c_2) %*% M_2 %*% r_b -
    t(r_b) %*% A_bar_bb %*% r_b

  print("Generating lambda_0")

  #P.R.P <- crossprod(P, R) %*% P
  P <- as.matrix(P)
  P.R.P <- t(P) %*% R %*% P

  EBV_1 <-
    matrix(candidates[candidates[, "id"] %in% c1_candidates[, "id"], "ebv"], ncol = 1)
  rownames(EBV_1) <-
    candidates[candidates[, "id"] %in% c1_candidates[, "id"], "id"]
  colnames(EBV_1) <- "ebv"

  EBV_2 <-
    matrix(candidates[candidates[, "id"] %in% c2_candidates[, "id"], "ebv"], ncol = 1)
  rownames(EBV_2) <-
    candidates[candidates[, "id"] %in% c2_candidates[, "id"], "id"]
  colnames(EBV_2) <- "ebv"

  group_contbn$sex_age <-
    paste0(group_contbn$sex, "_", group_contbn$age)
  s_2 <-
    candidates %>% group_by(sex_age) %>% summarize(s_2 = sum(cprev_fam, na.rm = TRUE) / (2 * n_fams))
  group_contbn <- left_join(group_contbn, s_2, by = "sex_age")
  group_contbn$s_2[is.na(group_contbn$s_2)] <- 0

  tmp <- (group_contbn$s_1_plus_s_2 - group_contbn$s_2)
  tmp[is.na(tmp)] <- 0
  tmp <- tmp < 0
  if (sum(tmp) > 0) {
    stop(
      paste0(
        "Total contributions within each sex and age category in the \'cprev_fam\' column of \'candidates\' must be less than contributions within corresponding sex and age categories in \'group_contbn\'. Check sex_age:",
        group_contbn$sex_age[tmp]
      )
    )
  }
  rm(tmp)

  s_1 <-
    matrix(group_contbn$s_1_plus_s_2 - group_contbn$s_2, nrow = 1)
  s_1[is.na(s_1)] <- 0
  colnames(s_1) <- group_contbn$sex_age
  rownames(s_1) <- NULL

  s_1.Q_1.A_11_inv.Q_1_inv <- tcrossprod(s_1, Q_1.A_11_inv.Q_1_inv)

  lambda_0_squared_denom <-  (
    K + P.R.P -
      r1_plus_r3 ^ 2 * tcrossprod(s_1.Q_1.A_11_inv.Q_1_inv, s_1) - #%*% tcrossprod(Q_1.A_11_inv.Q_1, s_1.Q_1.A_11_inv.Q_1_inv) -
      2 * s_1.Q_1.A_11_inv.Q_1_inv %*% (Q_1.A_11_inv %*% P)
  )

  lambda_0_squared <-
    0.25 * crossprod(EBV_1, crossprod(R, EBV_1)) / lambda_0_squared_denom

  lambda_0_squared <- as.vector(lambda_0_squared)
  if (lambda_0_squared < 0) {
    stop(
      paste0(
        "The constraint on inbreeding cannot be met.  You may need to increase C_max by: ",
        -lambda_0_squared_denom
      )
    )
  }

  lambda_0 <- sqrt(lambda_0_squared)

  part_1 <-
    (2 * r1_plus_r3 ^ 2 * A_12.c_2) + (2 * r1_plus_r3 * M_1.r_b)

  part_2 <- EBV_1 - lambda_0 * part_1

  print("Generating lambda")
  lambda <-
    Q_1.A_11_inv.Q_1_inv %*% (Q_1.A_11_inv %*% part_2 - t(2 * lambda_0 * r1_plus_r3 ^
                                                            2 * s_1))

  print("Generating c_1")

  c_1 <- A_11_inv %*% (part_2 -
                         (Q_1 %*% lambda)) /
    (2 * lambda_0 * r1_plus_r3 ^ 2)

  #part_3 <- r1_plus_r3 ^ 2 * crossprod(c_1 , A_11) %*% c_1
  c_1 <- as.matrix(c_1)
  colnames(c_1) <- "c_1"
  part_3 <- r1_plus_r3 ^ 2 * t(c_1) %*% A_11 %*% c_1

  print("Generating Hc")
 # Hc <- crossprod(c_1, EBV_1) -
 #   lambda_0 * (part_3 + crossprod(c_1, part_1) - K) -
 #   (crossprod(c_1, Q_1) - s_1) %*% lambda
  Hc <- t(c_1) %*% EBV_1 -
    lambda_0 * (part_3 + t(c_1) %*% part_1 - K) -
    (t(c_1) %*% Q_1 - s_1) %*% lambda

  print("Computing EBV_avg")
  c_2 <- as.matrix(c_2)
  #EBV_avg <- crossprod(c_2, EBV_2) + crossprod(c_1, EBV_1)
  EBV_avg <- t(c_2) %*% EBV_2 + crossprod(c_1, EBV_1)
  rownames(EBV_avg) <- "ebv"

  print("Computing C_t_plus_1")
  Ct_a_a <-
  #  r1_plus_r3 ^ 2 * crossprod(c_2 , A_22) %*% c_2 + 2 * (r1_plus_r3 ^ 2 * crossprod(c_1 , A_12) %*% c_2) + part_3
    r1_plus_r3 ^ 2 * t(c_2) %*% A_22 %*% c_2 + 2 * (r1_plus_r3 ^ 2 * t(c_1) %*% A_12 %*% c_2) + part_3
  Ct_a_b <-
  #  2 * r1_plus_r3 * (crossprod(c_1, M_1) %*% r_b + crossprod(c_2, M_2) %*% r_b)
  2 * r1_plus_r3 * (t(c_1) %*% M_1 %*% r_b + t(c_2) %*% M_2 %*% r_b)
  # Ct_b_b <- crossprod(r_b, A_bar_bb) %*% r_b
  r_b <- as.matrix(r_b)
  Ct_b_b <- t(r_b) %*% A_bar_bb %*% r_b
  C_t_plus_1 <- Ct_a_a + Ct_a_b  + Ct_b_b
  rownames(C_t_plus_1) <- "C_t_plus_1"
  colnames(C_t_plus_1) <- "C_t_plus_1"

  print("Generating realised group contributions")
  Q_2 <-
    incidence.matrix(
      row_values = candidates[candidates[, "id"] %in% c2_candidates[, "id"], "sex_age"],
      # col_values = group_contbn$sex_age,
      row_names = candidates[candidates[, "id"] %in% c2_candidates[, "id"], "id"],
      col_names = group_contbn$sex_age
    )

#  contbn_realised <- crossprod(Q_1, c_1) + crossprod(Q_2, c_2)
  Q_1 <- as.matrix(Q_1)
  Q_2 <- as.matrix(Q_2)
  contbn_realised <- t(Q_1) %*% c_1 + t(Q_2) %*% c_2
  colnames(contbn_realised) <- "contbn_realised"

  if(exists("A_1bf")) {A_1b <- A_1bf}
  if(exists("A_2bf")) {A_2b <- A_2bf}
  if(exists("L")) {J <- cbind(L,L)}
  if(exists("A_fams_in_A_bar_bb")) {A_cand_in_A_bar_bb <- A_fams_in_A_bar_bb}

  return(list(c_1 = c_1,
              c_2 = c_2,
           #   A_cand_in_A_bar_bb = A_cand_in_A_bar_bb,
              A_bar_bb = A_bar_bb,
              C_t_plus_1 = C_t_plus_1,
              contbn_realised = contbn_realised,
              EBV_avg = EBV_avg,
           #   Hc = Hc,
              Lbar_male = Lbar_male,
              Lbar_female = Lbar_female,
              group_contbn = group_contbn,
              ped = ped,
              J = J,
              A_1b = A_1b,
              A_2b = A_2b,
              M_1 = M_1,
              M_2 = M_2))
} # End get.c

