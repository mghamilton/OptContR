#matthewhamilton75@gmail.com
#Nov 2019

##########################################################################
#Generate group_contbn_input
##########################################################################
group_contbn_input <- data.frame(sex = c("male", "female","male", "female","male", "female"),
                                 age = c(1,1,2,2,3,3),
                                 fams_n = c(0, 0, Families_per_sr/5*4, Families_per_sr/5, Families_per_sr/5, Families_per_sr/5*4),
                                 stringsAsFactors = FALSE)
group_contbn_input$s <- group_contbn_input$fams_n/sum(group_contbn_input$fams_n)

##########################################################################
#Generate pedigree
##########################################################################

#generate founder ids
sires <- c(1:(3*Families_per_sr))
dams  <- c((length(sires) + 1):(length(sires) + 3*Families_per_sr))
tmp <- length(c(sires, dams))
founders <- data.frame(id   = paste0("F",c(sires, dams)),
                       sire = rep(NA,tmp),
                       dam  = rep(NA,tmp),
                       selection_round   = rep(0,tmp),
                       #    sex = c(rep("male", length(sires)),rep("female", length(dams))),
                       stringsAsFactors = FALSE)

for(sr in 1:sr_to_generate) {

  if(sr == 1) {

    sires <- sample(paste0("F", 1:Families_per_sr))
    dams  <- sample(paste0("F", (Families_per_sr + 1):(2*Families_per_sr)))

    if(Even_n_per_fam) {
      tmp <- data.frame(id   = ((sr-1)*indiv_per_sr + 1):(sr*indiv_per_sr),
                        sire = rep(sires, each = indiv_per_sr/Families_per_sr),
                        dam  = rep(dams,  each = indiv_per_sr/Families_per_sr),
                        selection_round = sr,
                        sex = rep(c("male", "female"), indiv_per_sr/2),
                        stringsAsFactors = FALSE)
    }

    if(!Even_n_per_fam) {
      tmp <- data.frame(id   = ((sr-1)*indiv_per_sr + 1):((sr-0.75)*indiv_per_sr),
                        sire = rep(sires[1:(Families_per_sr/2)], each = indiv_per_sr/Families_per_sr/2),
                        dam  = rep( dams[1:(Families_per_sr/2)], each = indiv_per_sr/Families_per_sr/2),
                        selection_round = sr,
                        sex = rep(c("male", "female"), indiv_per_sr*0.25/2),
                        stringsAsFactors = FALSE)

      tmp <- rbind(tmp,
                   data.frame(id   = ((sr-0.75)*indiv_per_sr + 1):(sr*indiv_per_sr),
                              sire = rep(sires[(Families_per_sr/2 + 1):Families_per_sr], each = indiv_per_sr/Families_per_sr/2*3),
                              dam  = rep( dams[(Families_per_sr/2 + 1):Families_per_sr], each = indiv_per_sr/Families_per_sr/2*3),
                              selection_round = sr,
                              sex = rep(c("male", "female"), indiv_per_sr*0.75/2),
                              stringsAsFactors = FALSE)
      )
    }

    ped_input <- tmp
  }

  if(sr == 2) {

    sires <- sample(paste0("F", (2*Families_per_sr + 1):(3*Families_per_sr)))
    dams  <- sample(paste0("F", (3*Families_per_sr + 1):(4*Families_per_sr)))

    if(Even_n_per_fam) {

      tmp <- data.frame(id   = ((sr-1)*indiv_per_sr + 1):(sr*indiv_per_sr),
                        sire = rep(sires, each = indiv_per_sr/Families_per_sr),
                        dam  = rep(dams,  each = indiv_per_sr/Families_per_sr),
                        selection_round = sr,
                        sex = rep(c("male", "female"), indiv_per_sr/2),
                        stringsAsFactors = FALSE)
    }

    if(!Even_n_per_fam) {

      tmp <- data.frame(id   = ((sr-1)*indiv_per_sr + 1):((sr-0.75)*indiv_per_sr),
                        sire = rep(sires[1:(Families_per_sr/2)], each = indiv_per_sr/Families_per_sr/2),
                        dam  = rep( dams[1:(Families_per_sr/2)], each = indiv_per_sr/Families_per_sr/2),
                        selection_round = sr,
                        sex = rep(c("male", "female"), indiv_per_sr*0.25/2),
                        stringsAsFactors = FALSE)

      tmp <- rbind(tmp,
                   data.frame(id   = ((sr-0.75)*indiv_per_sr + 1):(sr*indiv_per_sr),
                              sire = rep(sires[(Families_per_sr/2 + 1):Families_per_sr], each = indiv_per_sr/Families_per_sr/2*3),
                              dam  = rep( dams[(Families_per_sr/2 + 1):Families_per_sr], each = indiv_per_sr/Families_per_sr/2*3),
                              selection_round = sr,
                              sex = rep(c("male", "female"), indiv_per_sr*0.75/2),
                              stringsAsFactors = FALSE)
      )
    }
    ped_input <- rbind(ped_input, tmp)
  }

  if(sr == 3) {
    #cycle until all families are unique
    unique_fams <- FALSE
    while(!unique_fams) {
      male_2_avail   <-  sample(ped_input[ped_input[,"selection_round"] == sr-2 & ped_input[,"sex"] == "male","id"])
      female_2_avail <-  sample(ped_input[ped_input[,"selection_round"] == sr-2 & ped_input[,"sex"] == "female","id"])

      male_3_n   <- group_contbn_input[group_contbn_input$sex == "male"   & group_contbn_input$age == 3,"fams_n"]
      female_3_n <- group_contbn_input[group_contbn_input$sex == "female" & group_contbn_input$age == 3,"fams_n"]

      sires <- sample(c(male_2_avail[1:group_contbn_input[group_contbn_input$sex == "male" & group_contbn_input$age == 2,"fams_n"]],
                        paste0("F", (4*Families_per_sr + 1):(4*Families_per_sr + male_3_n))))

      dams  <- sample(c(female_2_avail[1:group_contbn_input[group_contbn_input$sex == "female" & group_contbn_input$age == 2,"fams_n"]],
                        paste0("F", ((4*Families_per_sr + male_3_n) + 1):((4*Families_per_sr + male_3_n) + female_3_n))))


      if(Even_n_per_fam) {

        tmp <- data.frame(id   = ((sr-1)*indiv_per_sr + 1):(sr*indiv_per_sr),
                          sire = rep(sires, each = indiv_per_sr/Families_per_sr),
                          dam  = rep(dams,  each = indiv_per_sr/Families_per_sr),
                          selection_round = sr,
                          sex = rep(c("male", "female"), indiv_per_sr/2),
                          stringsAsFactors = FALSE)
      }

      if(!Even_n_per_fam) {

        tmp <- data.frame(id   = ((sr-1)*indiv_per_sr + 1):((sr-0.75)*indiv_per_sr),
                          sire = rep(sires[1:(Families_per_sr/2)], each = indiv_per_sr/Families_per_sr/2),
                          dam  = rep( dams[1:(Families_per_sr/2)], each = indiv_per_sr/Families_per_sr/2),
                          selection_round = sr,
                          sex = rep(c("male", "female"), indiv_per_sr*0.25/2),
                          stringsAsFactors = FALSE)

        tmp <- rbind(tmp,
                     data.frame(id   = ((sr-0.75)*indiv_per_sr + 1):(sr*indiv_per_sr),
                                sire = rep(sires[(Families_per_sr/2 + 1):Families_per_sr], each = indiv_per_sr/Families_per_sr/2*3),
                                dam  = rep( dams[(Families_per_sr/2 + 1):Families_per_sr], each = indiv_per_sr/Families_per_sr/2*3),
                                selection_round = sr,
                                sex = rep(c("male", "female"), indiv_per_sr*0.75/2),
                                stringsAsFactors = FALSE)
        )
      }

      tmp_2 <- rbind(ped_input, tmp)
      rm(tmp, male_2_avail, female_2_avail, sires, dams)

      tmp_3 <- unique(tmp_2[,c("sire", "dam")])
      tmp_3 <- tmp_3[!(is.na(tmp_3$sire) & is.na(tmp_3$dam)), ]

      tmp_4 <- unique(tmp_2[,c("sire", "dam", "selection_round")])
      tmp_4 <- tmp_4[!(is.na(tmp_4$sire) & is.na(tmp_4$dam)), ]

      if(nrow(tmp_3) == nrow(tmp_4)) (unique_fams <- TRUE)
    }
    ped_input <- tmp_2
  }

  if(sr > 3) {
    #cycle until all families are unique
    unique_fams <- FALSE
    while(!unique_fams) {
      male_2_avail   <-  sample(ped_input[ped_input[,"selection_round"] == sr-2 & ped_input[,"sex"] == "male","id"])
      female_2_avail <-  sample(ped_input[ped_input[,"selection_round"] == sr-2 & ped_input[,"sex"] == "female","id"])

      male_3_avail   <-  sample(ped_input[ped_input[,"selection_round"] == sr-3 & ped_input[,"sex"] == "male","id"])
      female_3_avail <-  sample(ped_input[ped_input[,"selection_round"] == sr-3 & ped_input[,"sex"] == "female","id"])

      sires <- sample(c(male_2_avail[1:group_contbn_input[group_contbn_input$sex   == "male"   & group_contbn_input$age == 2,"fams_n"]],
                        male_3_avail[1:group_contbn_input[group_contbn_input$sex   == "male"   & group_contbn_input$age == 3,"fams_n"]]))
      dams  <- sample(c(female_2_avail[1:group_contbn_input[group_contbn_input$sex == "female" & group_contbn_input$age == 2,"fams_n"]],
                        female_3_avail[1:group_contbn_input[group_contbn_input$sex == "female" & group_contbn_input$age == 3,"fams_n"]]))

      if(Even_n_per_fam) {

        tmp <- data.frame(id   = ((sr-1)*indiv_per_sr + 1):(sr*indiv_per_sr),
                          sire = rep(sires, each = indiv_per_sr/Families_per_sr),
                          dam  = rep(dams,  each = indiv_per_sr/Families_per_sr),
                          selection_round = sr,
                          sex = rep(c("male", "female"), indiv_per_sr/2),
                          stringsAsFactors = FALSE)
      }

      if(!Even_n_per_fam) {

        tmp <- data.frame(id   = ((sr-1)*indiv_per_sr + 1):((sr-0.75)*indiv_per_sr),
                          sire = rep(sires[1:(Families_per_sr/2)], each = indiv_per_sr/Families_per_sr/2),
                          dam  = rep( dams[1:(Families_per_sr/2)], each = indiv_per_sr/Families_per_sr/2),
                          selection_round = sr,
                          sex = rep(c("male", "female"), indiv_per_sr*0.25/2),
                          stringsAsFactors = FALSE)

        tmp <- rbind(tmp,
                     data.frame(id   = ((sr-0.75)*indiv_per_sr + 1):(sr*indiv_per_sr),
                                sire = rep(sires[(Families_per_sr/2 + 1):Families_per_sr], each = indiv_per_sr/Families_per_sr/2*3),
                                dam  = rep( dams[(Families_per_sr/2 + 1):Families_per_sr], each = indiv_per_sr/Families_per_sr/2*3),
                                selection_round = sr,
                                sex = rep(c("male", "female"), indiv_per_sr*0.75/2),
                                stringsAsFactors = FALSE)
        )
      }

      tmp_2 <- rbind(ped_input, tmp)
      rm(tmp, male_2_avail, female_2_avail, male_3_avail, female_3_avail, sires, dams)

      tmp_3 <- unique(tmp_2[,c("sire", "dam")])
      tmp_3 <- tmp_3[!(is.na(tmp_3$sire) & is.na(tmp_3$dam)), ]

      tmp_4 <- unique(tmp_2[,c("sire", "dam", "selection_round")])
      tmp_4 <- tmp_4[!(is.na(tmp_4$sire) & is.na(tmp_4$dam)), ]

      if(nrow(tmp_3) == nrow(tmp_4)) (unique_fams <- TRUE)
      rm(tmp_3, tmp_4)
    }
    ped_input <- tmp_2
    rm(tmp_2)
  }
}

founders$sex <- NA
founders[founders[,"id"] %in% ped_input$sire,"sex"] <- "male"
founders[founders[,"id"] %in% ped_input$dam,"sex"] <- "female"
founders <- founders[!is.na(founders$sex),]

ped_input <- ped_input[,colnames(founders)]

ped_input <- rbind(founders, ped_input)

#Generate breeding values
#http://finzi.psych.upenn.edu/library/nadiv/html/warcolak.html

ped_input$ebv <-runif(nrow(ped_input))

ped_input$age <- max(ped_input$selection_round) - ped_input$selection_round + 1

ped_input[is.na(ped_input[,"sire"]),"sire"] <- 0
ped_input[is.na(ped_input[,"dam"]), "dam" ] <- 0

##########################################################################
#Generate candidates
##########################################################################

tmp <- group_contbn_input
tmp$fams_n_class_2 <- tmp$fams_n * proportion_class_2
tmp <- tmp[tmp$fams_n != 0,]
candidates_input <- left_join (tmp, ped_input, by = c("sex", "age"))

#cprev_fam, cmax_fam cmin_fam
candidates_input$cprev_fam <- 0
candidates_input$cmax_fam  <- 0
candidates_input$cmin_fam  <- 0
for(sex_age in 1:nrow(tmp)) {
  tmp_2 <- (candidates_input$sex == tmp[sex_age, "sex"] & candidates_input$age == tmp[sex_age, "age"])
  candidates_input[tmp_2,"cmax_fam"] <- tmp[sex_age, "fams_n"]

  tmp_2[tmp_2] <- c(rep(T, tmp[sex_age, "fams_n_class_2"]), rep(F, length(tmp_2[tmp_2]) - tmp[sex_age, "fams_n_class_2"]))
  candidates_input[tmp_2,"cprev_fam"] <- 1
}

#for simualation don't let individuals be both class 1 and class 2
candidates_input[candidates_input[,"cmax_fam"] > candidates_input[,"cprev_fam"] & candidates_input[,"cprev_fam"] != 0,"cmax_fam"] <-
  candidates_input[candidates_input[,"cmax_fam"] > candidates_input[,"cprev_fam"] & candidates_input[,"cprev_fam"] != 0,"cprev_fam"]

candidates_input$cmin_fam <- candidates_input$cprev_fam

candidates_input <- candidates_input[,c("id", "cprev_fam", "cmax_fam", "cmin_fam", "ebv", "sex", "age")]

##########################################################################
#Generate fams_input
##########################################################################

fams_input <- unique(ped_input[ped_input[,"selection_round"] != 0,c("sire", "dam", "selection_round")])
fams_input$family_id <- paste(fams_input$selection_round,fams_input$sire,fams_input$dam, sep = "_")

fams_input <- fams_input[,c("family_id", "sire", "dam", "selection_round")]

##########################################################################
#Get ped_input assuming only 4 individuals per family in age 1 (i.e. juveniles)
##########################################################################
ped_input_age_1 <- ped_input[ped_input$age == 1,]
ped_input_age_1$fam <- as.factor(paste0(ped_input_age_1$sire,"_", ped_input_age_1$dam))

ped_input_age_1 <- ped_input_age_1[order(runif(nrow(ped_input_age_1))), ]
ped_input_age_1 <- ped_input_age_1[order(ped_input_age_1$fam), ]

ped_input_age_1 <- by(ped_input_age_1, ped_input_age_1["fam"], head, n=4)

ped_input_age_1 <-   Reduce(rbind, ped_input_age_1)
ped_input_age_1 <- ped_input_age_1[,colnames(ped_input_age_1) != "fam"]

ped_input_4_per_juv <- ped_input[ped_input$age != 1,]
ped_input_4_per_juv <- rbind(ped_input_4_per_juv, ped_input_age_1)

##########################################################################
#Run optimal contributions
##########################################################################

out_hamilton <- get.c(C_max = C_max,
                      group_contbn = group_contbn_input,
                      ped = ped_input[,c('id', 'sire', 'dam', 'ebv')],
                      fams = fams_input,
                      candidates = candidates_input,
                      parents_max = NA,
                      method = "Hamilton")

out_meuwissen <- get.c(C_max = C_max,
                       group_contbn = group_contbn_input,
                       ped = ped_input,
                       fams = NULL,
                       candidates = candidates_input,
                       parents_max = NA,
                       method = "Meuwissen")

out_meuwissen_4_per_juv <- get.c(C_max = C_max,
                       group_contbn = group_contbn_input,
                       ped = ped_input_4_per_juv,
                       fams = NULL,
                       candidates = candidates_input,
                       parents_max = NA,
                       method = "Meuwissen")

out_meuwissen_orig <- out_meuwissen

##########################################################################
#outputs - Hamilton vs Meuwissen
##########################################################################

c_1 <- cbind(out_meuwissen$c_1,out_hamilton$c_1)
colnames(c_1) <- c("c_1_meuwissen", "c_1_hamilton")
c_1 <- as.data.frame(as.matrix(c_1))

plot(c_1_meuwissen ~ c_1_hamilton, data=c_1)
linearMod <- lm(c_1_meuwissen ~ c_1_hamilton, data=c_1)
print(linearMod)
print(summary(linearMod))


out_meuwissen$A_bar_bb
out_hamilton$A_bar_bb

tmp_out <- data.frame(indiv_per_sr       = indiv_per_sr,
                      Additive_Var       = Additive_Var,
                      C_max              = C_max,
                      sr_to_generate     = sr_to_generate,
                      proportion_class_2 = proportion_class_2,
                      Families_per_sr    = Families_per_sr,
                      Even_n_per_fam     = Even_n_per_fam)

colnames(out_hamilton$A_bar_bb) <- colnames(out_meuwissen$A_bar_bb)
rownames(out_hamilton$A_bar_bb) <- rownames(out_meuwissen$A_bar_bb)
tmp_out["diag_A_bar_bb_male_2_male_2_meuwissen"]     <- out_meuwissen$A_bar_bb["male_2","male_2"]
tmp_out["diag_A_bar_bb_male_1_male_1_meuwissen"]     <- out_meuwissen$A_bar_bb["male_1","male_1"]
tmp_out["diag_A_bar_bb_female_2_female_2_meuwissen"] <- out_meuwissen$A_bar_bb["female_2","female_2"]
tmp_out["diag_A_bar_bb_female_1_female_1_meuwissen"] <- out_meuwissen$A_bar_bb["female_1","female_1"]

tmp_out["diag_A_bar_bb_male_2_male_1_meuwissen"]     <- out_meuwissen$A_bar_bb["male_2","male_1"]
tmp_out["diag_A_bar_bb_male_2_female_2_meuwissen"]   <- out_meuwissen$A_bar_bb["male_2","female_2"]
tmp_out["diag_A_bar_bb_male_2_female_1_meuwissen"]   <- out_meuwissen$A_bar_bb["male_2","female_1"]
tmp_out["diag_A_bar_bb_male_1_female_2_meuwissen"]   <- out_meuwissen$A_bar_bb["male_1","female_2"]
tmp_out["diag_A_bar_bb_male_1_female_1_meuwissen"]   <- out_meuwissen$A_bar_bb["male_1","female_1"]
tmp_out["diag_A_bar_bb_female_2_female_1_meuwissen"] <- out_meuwissen$A_bar_bb["female_2","female_1"]

tmp_out["diag_A_bar_bb_male_2_male_2_hamilton"]     <- out_hamilton$A_bar_bb["male_2","male_2"]
tmp_out["diag_A_bar_bb_male_1_male_1_hamilton"]     <- out_hamilton$A_bar_bb["male_1","male_1"]
tmp_out["diag_A_bar_bb_female_2_female_2_hamilton"] <- out_hamilton$A_bar_bb["female_2","female_2"]
tmp_out["diag_A_bar_bb_female_1_female_1_hamilton"] <- out_hamilton$A_bar_bb["female_1","female_1"]

tmp_out["diag_A_bar_bb_male_2_male_1_hamilton"]     <- out_hamilton$A_bar_bb["male_2","male_1"]
tmp_out["diag_A_bar_bb_male_2_female_2_hamilton"]   <- out_hamilton$A_bar_bb["male_2","female_2"]
tmp_out["diag_A_bar_bb_male_2_female_1_hamilton"]   <- out_hamilton$A_bar_bb["male_2","female_1"]
tmp_out["diag_A_bar_bb_male_1_female_2_hamilton"]   <- out_hamilton$A_bar_bb["male_1","female_2"]
tmp_out["diag_A_bar_bb_male_1_female_1_hamilton"]   <- out_hamilton$A_bar_bb["male_1","female_1"]
tmp_out["diag_A_bar_bb_female_2_female_1_hamilton"] <- out_hamilton$A_bar_bb["female_2","female_1"]

tmp_out["diag_A_bar_bb_male_2_male_2_prop"]     <- (out_meuwissen$A_bar_bb["male_2","male_2"]     - out_hamilton$A_bar_bb["male_2","male_2"]) / out_meuwissen$A_bar_bb["male_2","male_2"]
tmp_out["diag_A_bar_bb_male_1_male_1_prop"]     <- (out_meuwissen$A_bar_bb["male_1","male_1"]     - out_hamilton$A_bar_bb["male_1","male_1"]) / out_meuwissen$A_bar_bb["male_1","male_1"]
tmp_out["diag_A_bar_bb_female_2_female_2_prop"] <- (out_meuwissen$A_bar_bb["female_2","female_2"] - out_hamilton$A_bar_bb["female_2","female_2"]) / out_meuwissen$A_bar_bb["female_2","female_2"]
tmp_out["diag_A_bar_bb_female_1_female_1_prop"] <- (out_meuwissen$A_bar_bb["female_1","female_1"] - out_hamilton$A_bar_bb["female_1","female_1"]) / out_meuwissen$A_bar_bb["female_1","female_1"]

tmp_out["diag_A_bar_bb_male_2_male_1_prop"]     <- (out_meuwissen$A_bar_bb["male_2","male_1"]     - out_hamilton$A_bar_bb["male_2","male_1"]) / out_meuwissen$A_bar_bb["male_2","male_1"]
tmp_out["diag_A_bar_bb_male_2_female_2_prop"]   <- (out_meuwissen$A_bar_bb["male_2","female_2"]   - out_hamilton$A_bar_bb["male_2","female_2"]) / out_meuwissen$A_bar_bb["male_2","female_2"]
tmp_out["diag_A_bar_bb_male_2_female_1_prop"]   <- (out_meuwissen$A_bar_bb["male_2","female_1"]   - out_hamilton$A_bar_bb["male_2","female_1"]) / out_meuwissen$A_bar_bb["male_2","female_1"]
tmp_out["diag_A_bar_bb_male_1_female_2_prop"]   <- (out_meuwissen$A_bar_bb["male_1","female_2"]   - out_hamilton$A_bar_bb["male_1","female_2"]) / out_meuwissen$A_bar_bb["male_1","female_2"]
tmp_out["diag_A_bar_bb_male_1_female_1_prop"]   <- (out_meuwissen$A_bar_bb["male_1","female_1"]   - out_hamilton$A_bar_bb["male_1","female_1"]) / out_meuwissen$A_bar_bb["male_1","female_1"]
tmp_out["diag_A_bar_bb_female_2_female_1_prop"] <- (out_meuwissen$A_bar_bb["female_2","female_1"] - out_hamilton$A_bar_bb["female_2","female_1"]) / out_meuwissen$A_bar_bb["female_2","female_1"]

if(Even_n_per_fam) {
  #one row per family
  tmp_rows <- !duplicated(out_meuwissen$ped[(nrow(out_meuwissen$ped) - nrow(out_meuwissen$M_1) + 1):nrow(out_meuwissen$ped),c("sire", "dam")])

tmp <- lm(out_meuwissen$M_1[tmp_rows,1] ~ out_hamilton$M_1[tmp_rows,1])
tmp_out["M1_male_2_intercept"] <- as.vector(tmp$coefficients[1])
tmp_out["M1_male_2_slope"]     <- as.vector(tmp$coefficients[2])
tmp_out["M1_male_2_R2"]        <- as.vector(summary(tmp)$r.squared)
rm(tmp)

tmp <- lm(out_meuwissen$M_1[tmp_rows,2] ~ out_hamilton$M_1[tmp_rows,2])
tmp_out["M1_male_1_intercept"] <- as.vector(tmp$coefficients[1])
tmp_out["M1_male_1_slope"]     <- as.vector(tmp$coefficients[2])
tmp_out["M1_male_1_R2"]        <- as.vector(summary(tmp)$r.squared)
rm(tmp)

tmp <- lm(out_meuwissen$M_1[tmp_rows,3] ~ out_hamilton$M_1[tmp_rows,3])
tmp_out["M1_female_2_intercept"] <- as.vector(tmp$coefficients[1])
tmp_out["M1_female_2_slope"]     <- as.vector(tmp$coefficients[2])
tmp_out["M1_female_2_R2"]        <- as.vector(summary(tmp)$r.squared)
rm(tmp)

tmp <- lm(out_meuwissen$M_1[tmp_rows,4] ~ out_hamilton$M_1[tmp_rows,4])
tmp_out["M1_female_1_intercept"] <- as.vector(tmp$coefficients[1])
tmp_out["M1_female_1_slope"]     <- as.vector(tmp$coefficients[2])
tmp_out["M1_female_1_R2"]        <- as.vector(summary(tmp)$r.squared)
rm(tmp)
} else {
  #one row per family
  tmp_rows <- !duplicated(out_meuwissen$ped[(nrow(out_meuwissen$ped) - nrow(out_meuwissen$M_1) + 1):nrow(out_meuwissen$ped),c("sire", "dam")]) &
    rep(c(T,F,F,F,T,F,F,F), each = (nrow(out_meuwissen$M_1)/8))

  tmp <- lm(out_meuwissen$M_1[tmp_rows,1] ~ out_hamilton$M_1[tmp_rows,1])
  tmp_out["M1_male_2_small_fam_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["M1_male_2_small_fam_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["M1_male_2_small_fam_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp <- lm(out_meuwissen$M_1[tmp_rows,2] ~ out_hamilton$M_1[tmp_rows,2])
  tmp_out["M1_male_1_small_fam_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["M1_male_1_small_fam_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["M1_male_1_small_fam_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp <- lm(out_meuwissen$M_1[tmp_rows,3] ~ out_hamilton$M_1[tmp_rows,3])
  tmp_out["M1_female_2_small_fam_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["M1_female_2_small_fam_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["M1_female_2_small_fam_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp <- lm(out_meuwissen$M_1[tmp_rows,4] ~ out_hamilton$M_1[tmp_rows,4])
  tmp_out["M1_female_1_small_fam_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["M1_female_1_small_fam_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["M1_female_1_small_fam_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp_rows <- !duplicated(out_meuwissen$ped[(nrow(out_meuwissen$ped) - nrow(out_meuwissen$M_1) + 1):nrow(out_meuwissen$ped),c("sire", "dam")]) &
    rep(c(F,T,T,T,F,T,T,T), each = (nrow(out_meuwissen$M_1)/8))

  tmp <- lm(out_meuwissen$M_1[tmp_rows,1] ~ out_hamilton$M_1[tmp_rows,1])
  tmp_out["M1_male_2_large_fam_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["M1_male_2_large_fam_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["M1_male_2_large_fam_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp <- lm(out_meuwissen$M_1[tmp_rows,2] ~ out_hamilton$M_1[tmp_rows,2])
  tmp_out["M1_male_1_large_fam_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["M1_male_1_large_fam_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["M1_male_1_large_fam_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp <- lm(out_meuwissen$M_1[tmp_rows,3] ~ out_hamilton$M_1[tmp_rows,3])
  tmp_out["M1_female_2_large_fam_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["M1_female_2_large_fam_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["M1_female_2_large_fam_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp <- lm(out_meuwissen$M_1[tmp_rows,4] ~ out_hamilton$M_1[tmp_rows,4])
  tmp_out["M1_female_1_large_fam_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["M1_female_1_large_fam_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["M1_female_1_large_fam_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)
}

#################################################################################
#Construct hybrid matricies
#################################################################################

#meuwissen

hybrid_A_1b <- cbind(out_meuwissen$A_1b[,1:(ncol(out_meuwissen$A_1b)/2)],
                     out_hamilton$A_1b[,(ncol(out_hamilton$A_1b)/2+1):ncol(out_hamilton$A_1b)])

hybrid_J <- rbind(out_meuwissen$J[1:(nrow(out_meuwissen$J)/2),],
                     out_hamilton$J[(nrow(out_hamilton$J)/2+1):nrow(out_hamilton$J),])

hybrid_M_1 <- hybrid_A_1b %*% hybrid_J

hybrid_A_bb_top_left     <- out_meuwissen$A_cand_in_A_bar_bb[1:(ncol(out_meuwissen$A_cand_in_A_bar_bb)/2),
                                                             1:(ncol(out_meuwissen$A_cand_in_A_bar_bb)/2)]
hybrid_A_bb_bottom_right <- out_hamilton$A_cand_in_A_bar_bb[(ncol(out_hamilton$A_cand_in_A_bar_bb)/2+1):ncol(out_hamilton$A_cand_in_A_bar_bb),
                                                          (ncol(out_hamilton$A_cand_in_A_bar_bb)/2+1):ncol(out_hamilton$A_cand_in_A_bar_bb)]

n_small_fam <- indiv_per_sr/Families_per_sr/2
col_to_retain <- c(seq(from = 1, to = n_small_fam*Families_per_sr/2, by = n_small_fam),
seq(from = n_small_fam*Families_per_sr/2 + 1, to = indiv_per_sr, by = n_small_fam*3))

hybrid_A_bb_top_right     <- out_meuwissen$A_cand_in_A_bar_bb[1:(ncol(out_meuwissen$A_cand_in_A_bar_bb)/2),
                                                              indiv_per_sr+col_to_retain]
hybrid_A_bb <- cbind(hybrid_A_bb_top_left,hybrid_A_bb_top_right)
hybrid_A_bb <- rbind(hybrid_A_bb,cbind(t(hybrid_A_bb_top_right),hybrid_A_bb_bottom_right))

hybrid_A_bar_bb <- t(hybrid_J) %*% hybrid_A_bb %*% hybrid_J
rm(n_small_fam, col_to_retain,hybrid_A_bb_top_left, hybrid_A_bb_bottom_right, hybrid_A_bb_top_right)

colnames(hybrid_A_bar_bb) <- colnames(out_meuwissen$A_bar_bb)
rownames(hybrid_A_bar_bb) <- rownames(hybrid_A_bar_bb)
tmp_out["diag_A_bar_bb_male_2_male_2_hybrid"]     <- hybrid_A_bar_bb["male_2","male_2"]
tmp_out["diag_A_bar_bb_male_1_male_1_hybrid"]     <- hybrid_A_bar_bb["male_1","male_1"]
tmp_out["diag_A_bar_bb_female_2_female_2_hybrid"] <- hybrid_A_bar_bb["female_2","female_2"]
tmp_out["diag_A_bar_bb_female_1_female_1_hybrid"] <- hybrid_A_bar_bb["female_1","female_1"]

tmp_out["diag_A_bar_bb_male_2_male_1_hybrid"]     <- hybrid_A_bar_bb["male_2","male_1"]
tmp_out["diag_A_bar_bb_male_2_female_2_hybrid"]   <- hybrid_A_bar_bb["male_2","female_2"]
tmp_out["diag_A_bar_bb_male_2_female_1_hybrid"]   <- hybrid_A_bar_bb["male_2","female_1"]
tmp_out["diag_A_bar_bb_male_1_female_2_hybrid"]   <- hybrid_A_bar_bb["male_1","female_2"]
tmp_out["diag_A_bar_bb_male_1_female_1_hybrid"]   <- hybrid_A_bar_bb["male_1","female_1"]
tmp_out["diag_A_bar_bb_female_2_female_1_hybrid"] <- hybrid_A_bar_bb["female_2","female_1"]

tmp_out["diag_A_bar_bb_male_2_male_2_hybrid_prop"]     <- (out_meuwissen$A_bar_bb["male_2","male_2"]     - hybrid_A_bar_bb["male_2","male_2"]) / out_meuwissen$A_bar_bb["male_2","male_2"]
tmp_out["diag_A_bar_bb_male_1_male_1_hybrid_prop"]     <- (out_meuwissen$A_bar_bb["male_1","male_1"]     - hybrid_A_bar_bb["male_1","male_1"]) / out_meuwissen$A_bar_bb["male_1","male_1"]
tmp_out["diag_A_bar_bb_female_2_female_2_hybrid_prop"] <- (out_meuwissen$A_bar_bb["female_2","female_2"] - hybrid_A_bar_bb["female_2","female_2"]) / out_meuwissen$A_bar_bb["female_2","female_2"]
tmp_out["diag_A_bar_bb_female_1_female_1_hybrid_prop"] <- (out_meuwissen$A_bar_bb["female_1","female_1"] - hybrid_A_bar_bb["female_1","female_1"]) / out_meuwissen$A_bar_bb["female_1","female_1"]

tmp_out["diag_A_bar_bb_male_2_male_1_hybrid_prop"]     <- (out_meuwissen$A_bar_bb["male_2","male_1"]     - hybrid_A_bar_bb["male_2","male_1"]) / out_meuwissen$A_bar_bb["male_2","male_1"]
tmp_out["diag_A_bar_bb_male_2_female_2_hybrid_prop"]   <- (out_meuwissen$A_bar_bb["male_2","female_2"]   - hybrid_A_bar_bb["male_2","female_2"]) / out_meuwissen$A_bar_bb["male_2","female_2"]
tmp_out["diag_A_bar_bb_male_2_female_1_hybrid_prop"]   <- (out_meuwissen$A_bar_bb["male_2","female_1"]   - hybrid_A_bar_bb["male_2","female_1"]) / out_meuwissen$A_bar_bb["male_2","female_1"]
tmp_out["diag_A_bar_bb_male_1_female_2_hybrid_prop"]   <- (out_meuwissen$A_bar_bb["male_1","female_2"]   - hybrid_A_bar_bb["male_1","female_2"]) / out_meuwissen$A_bar_bb["male_1","female_2"]
tmp_out["diag_A_bar_bb_male_1_female_1_hybrid_prop"]   <- (out_meuwissen$A_bar_bb["male_1","female_1"]   - hybrid_A_bar_bb["male_1","female_1"]) / out_meuwissen$A_bar_bb["male_1","female_1"]
tmp_out["diag_A_bar_bb_female_2_female_1_hybrid_prop"] <- (out_meuwissen$A_bar_bb["female_2","female_1"] - hybrid_A_bar_bb["female_2","female_1"]) / out_meuwissen$A_bar_bb["female_2","female_1"]


if(Even_n_per_fam) {
  #one row per family
  tmp_rows <- !duplicated(out_meuwissen$ped[(nrow(out_meuwissen$ped) - nrow(out_meuwissen$M_1) + 1):nrow(out_meuwissen$ped),c("sire", "dam")])

tmp <- lm(out_meuwissen$M_1[tmp_rows,1] ~ hybrid_M_1[tmp_rows,1])
tmp_out["hybrid_M1_male_2_intercept"] <- as.vector(tmp$coefficients[1])
tmp_out["hybrid_M1_male_2_slope"]     <- as.vector(tmp$coefficients[2])
tmp_out["hybrid_M1_male_2_R2"]        <- as.vector(summary(tmp)$r.squared)
rm(tmp)

tmp <- lm(out_meuwissen$M_1[tmp_rows,2] ~ hybrid_M_1[tmp_rows,2])
tmp_out["hybrid_M1_male_1_intercept"] <- as.vector(tmp$coefficients[1])
tmp_out["hybrid_M1_male_1_slope"]     <- as.vector(tmp$coefficients[2])
tmp_out["hybrid_M1_male_1_R2"]        <- as.vector(summary(tmp)$r.squared)
rm(tmp)

tmp <- lm(out_meuwissen$M_1[tmp_rows,3] ~ hybrid_M_1[tmp_rows,3])
tmp_out["hybrid_M1_female_2_intercept"] <- as.vector(tmp$coefficients[1])
tmp_out["hybrid_M1_female_2_slope"]     <- as.vector(tmp$coefficients[2])
tmp_out["hybrid_M1_female_2_R2"]        <- as.vector(summary(tmp)$r.squared)
rm(tmp)

tmp <- lm(out_meuwissen$M_1[tmp_rows,4] ~ hybrid_M_1[tmp_rows,4])
tmp_out["hybrid_M1_female_1_intercept"] <- as.vector(tmp$coefficients[1])
tmp_out["hybrid_M1_female_1_slope"]     <- as.vector(tmp$coefficients[2])
tmp_out["hybrid_M1_female_1_R2"]        <- as.vector(summary(tmp)$r.squared)
rm(tmp)

} else {
  #one row per family
  tmp_rows <- !duplicated(out_meuwissen$ped[(nrow(out_meuwissen$ped) - nrow(out_meuwissen$M_1) + 1):nrow(out_meuwissen$ped),c("sire", "dam")]) &
    rep(c(T,F,F,F,T,F,F,F), each = (nrow(out_meuwissen$M_1)/8))

  tmp <- lm(out_meuwissen$M_1[tmp_rows,1] ~ hybrid_M_1[tmp_rows,1])
  tmp_out["hybrid_M1_small_fam_male_2_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["hybrid_M1_small_fam_male_2_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["hybrid_M1_small_fam_male_2_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp <- lm(out_meuwissen$M_1[tmp_rows,2] ~ hybrid_M_1[tmp_rows,2])
  tmp_out["hybrid_M1_small_fam_male_1_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["hybrid_M1_small_fam_male_1_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["hybrid_M1_small_fam_male_1_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp <- lm(out_meuwissen$M_1[tmp_rows,3] ~ hybrid_M_1[tmp_rows,3])
  tmp_out["hybrid_M1_small_fam_female_2_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["hybrid_M1_small_fam_female_2_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["hybrid_M1_small_fam_female_2_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp <- lm(out_meuwissen$M_1[tmp_rows,4] ~ hybrid_M_1[tmp_rows,4])
  tmp_out["hybrid_M1_small_fam_female_1_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["hybrid_M1_small_fam_female_1_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["hybrid_M1_small_fam_female_1_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp_rows <- !duplicated(out_meuwissen$ped[(nrow(out_meuwissen$ped) - nrow(out_meuwissen$M_1) + 1):nrow(out_meuwissen$ped),c("sire", "dam")]) &
    rep(c(F,T,T,T,F,T,T,T), each = (nrow(out_meuwissen$M_1)/8))

  tmp <- lm(out_meuwissen$M_1[tmp_rows,1] ~ hybrid_M_1[tmp_rows,1])
  tmp_out["hybrid_M1_large_fam_male_2_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["hybrid_M1_large_fam_male_2_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["hybrid_M1_large_fam_male_2_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp <- lm(out_meuwissen$M_1[tmp_rows,2] ~ hybrid_M_1[tmp_rows,2])
  tmp_out["hybrid_M1_large_fam_male_1_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["hybrid_M1_large_fam_male_1_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["hybrid_M1_large_fam_male_1_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp <- lm(out_meuwissen$M_1[tmp_rows,3] ~ hybrid_M_1[tmp_rows,3])
  tmp_out["hybrid_M1_large_fam_female_2_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["hybrid_M1_large_fam_female_2_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["hybrid_M1_large_fam_female_2_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp <- lm(out_meuwissen$M_1[tmp_rows,4] ~ hybrid_M_1[tmp_rows,4])
  tmp_out["hybrid_M1_large_fam_female_1_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["hybrid_M1_large_fam_female_1_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["hybrid_M1_large_fam_female_1_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)
}


     # outputs <- rbind(outputs, tmp_out)
      if(!is.null(outputs)) {
      outputs <- merge(outputs, tmp_out, by = colnames(tmp_out)[colnames(tmp_out) %in% colnames(outputs)], all = T)
      } else {
        outputs <- tmp_out
      }
      rm(tmp_out, tmp_rows)

write.table(outputs, file = "outputs.csv", quote = FALSE, sep = ",", row.names = FALSE)

##########################################################################
#outputs - Meuwissen_4_per_juv vs Meuwissen
##########################################################################
out_meuwissen <- out_meuwissen_orig

c_1 <- cbind(out_meuwissen$c_1,out_meuwissen_4_per_juv$c_1)
colnames(c_1) <- c("c_1_meuwissen", "c_1_meuwissen_4_per_juv")
c_1 <- as.data.frame(as.matrix(c_1))

plot(c_1_meuwissen ~ c_1_meuwissen_4_per_juv, data=c_1)
linearMod <- lm(c_1_meuwissen ~ c_1_meuwissen_4_per_juv, data=c_1)
print(linearMod)
print(summary(linearMod))


out_meuwissen$A_bar_bb
out_meuwissen_4_per_juv$A_bar_bb

tmp_out <- data.frame(indiv_per_sr       = indiv_per_sr,
                      Additive_Var       = Additive_Var,
                      C_max              = C_max,
                      sr_to_generate     = sr_to_generate,
                      proportion_class_2 = proportion_class_2,
                      Families_per_sr    = Families_per_sr,
                      Even_n_per_fam     = Even_n_per_fam)

colnames(out_meuwissen_4_per_juv$A_bar_bb) <- colnames(out_meuwissen$A_bar_bb)
rownames(out_meuwissen_4_per_juv$A_bar_bb) <- rownames(out_meuwissen$A_bar_bb)
tmp_out["diag_A_bar_bb_male_2_male_2_meuwissen"]     <- out_meuwissen$A_bar_bb["male_2","male_2"]
tmp_out["diag_A_bar_bb_male_1_male_1_meuwissen"]     <- out_meuwissen$A_bar_bb["male_1","male_1"]
tmp_out["diag_A_bar_bb_female_2_female_2_meuwissen"] <- out_meuwissen$A_bar_bb["female_2","female_2"]
tmp_out["diag_A_bar_bb_female_1_female_1_meuwissen"] <- out_meuwissen$A_bar_bb["female_1","female_1"]

tmp_out["diag_A_bar_bb_male_2_male_1_meuwissen"]     <- out_meuwissen$A_bar_bb["male_2","male_1"]
tmp_out["diag_A_bar_bb_male_2_female_2_meuwissen"]   <- out_meuwissen$A_bar_bb["male_2","female_2"]
tmp_out["diag_A_bar_bb_male_2_female_1_meuwissen"]   <- out_meuwissen$A_bar_bb["male_2","female_1"]
tmp_out["diag_A_bar_bb_male_1_female_2_meuwissen"]   <- out_meuwissen$A_bar_bb["male_1","female_2"]
tmp_out["diag_A_bar_bb_male_1_female_1_meuwissen"]   <- out_meuwissen$A_bar_bb["male_1","female_1"]
tmp_out["diag_A_bar_bb_female_2_female_1_meuwissen"] <- out_meuwissen$A_bar_bb["female_2","female_1"]

tmp_out["diag_A_bar_bb_male_2_male_2_meuwissen_4_per_juv"]     <- out_meuwissen_4_per_juv$A_bar_bb["male_2","male_2"]
tmp_out["diag_A_bar_bb_male_1_male_1_meuwissen_4_per_juv"]     <- out_meuwissen_4_per_juv$A_bar_bb["male_1","male_1"]
tmp_out["diag_A_bar_bb_female_2_female_2_meuwissen_4_per_juv"] <- out_meuwissen_4_per_juv$A_bar_bb["female_2","female_2"]
tmp_out["diag_A_bar_bb_female_1_female_1_meuwissen_4_per_juv"] <- out_meuwissen_4_per_juv$A_bar_bb["female_1","female_1"]

tmp_out["diag_A_bar_bb_male_2_male_1_meuwissen_4_per_juv"]     <- out_meuwissen_4_per_juv$A_bar_bb["male_2","male_1"]
tmp_out["diag_A_bar_bb_male_2_female_2_meuwissen_4_per_juv"]   <- out_meuwissen_4_per_juv$A_bar_bb["male_2","female_2"]
tmp_out["diag_A_bar_bb_male_2_female_1_meuwissen_4_per_juv"]   <- out_meuwissen_4_per_juv$A_bar_bb["male_2","female_1"]
tmp_out["diag_A_bar_bb_male_1_female_2_meuwissen_4_per_juv"]   <- out_meuwissen_4_per_juv$A_bar_bb["male_1","female_2"]
tmp_out["diag_A_bar_bb_male_1_female_1_meuwissen_4_per_juv"]   <- out_meuwissen_4_per_juv$A_bar_bb["male_1","female_1"]
tmp_out["diag_A_bar_bb_female_2_female_1_meuwissen_4_per_juv"] <- out_meuwissen_4_per_juv$A_bar_bb["female_2","female_1"]

tmp_out["diag_A_bar_bb_male_2_male_2_prop"]     <- (out_meuwissen$A_bar_bb["male_2","male_2"]     - out_meuwissen_4_per_juv$A_bar_bb["male_2","male_2"]) / out_meuwissen$A_bar_bb["male_2","male_2"]
tmp_out["diag_A_bar_bb_male_1_male_1_prop"]     <- (out_meuwissen$A_bar_bb["male_1","male_1"]     - out_meuwissen_4_per_juv$A_bar_bb["male_1","male_1"]) / out_meuwissen$A_bar_bb["male_1","male_1"]
tmp_out["diag_A_bar_bb_female_2_female_2_prop"] <- (out_meuwissen$A_bar_bb["female_2","female_2"] - out_meuwissen_4_per_juv$A_bar_bb["female_2","female_2"]) / out_meuwissen$A_bar_bb["female_2","female_2"]
tmp_out["diag_A_bar_bb_female_1_female_1_prop"] <- (out_meuwissen$A_bar_bb["female_1","female_1"] - out_meuwissen_4_per_juv$A_bar_bb["female_1","female_1"]) / out_meuwissen$A_bar_bb["female_1","female_1"]

tmp_out["diag_A_bar_bb_male_2_male_1_prop"]     <- (out_meuwissen$A_bar_bb["male_2","male_1"]     - out_meuwissen_4_per_juv$A_bar_bb["male_2","male_1"]) / out_meuwissen$A_bar_bb["male_2","male_1"]
tmp_out["diag_A_bar_bb_male_2_female_2_prop"]   <- (out_meuwissen$A_bar_bb["male_2","female_2"]   - out_meuwissen_4_per_juv$A_bar_bb["male_2","female_2"]) / out_meuwissen$A_bar_bb["male_2","female_2"]
tmp_out["diag_A_bar_bb_male_2_female_1_prop"]   <- (out_meuwissen$A_bar_bb["male_2","female_1"]   - out_meuwissen_4_per_juv$A_bar_bb["male_2","female_1"]) / out_meuwissen$A_bar_bb["male_2","female_1"]
tmp_out["diag_A_bar_bb_male_1_female_2_prop"]   <- (out_meuwissen$A_bar_bb["male_1","female_2"]   - out_meuwissen_4_per_juv$A_bar_bb["male_1","female_2"]) / out_meuwissen$A_bar_bb["male_1","female_2"]
tmp_out["diag_A_bar_bb_male_1_female_1_prop"]   <- (out_meuwissen$A_bar_bb["male_1","female_1"]   - out_meuwissen_4_per_juv$A_bar_bb["male_1","female_1"]) / out_meuwissen$A_bar_bb["male_1","female_1"]
tmp_out["diag_A_bar_bb_female_2_female_1_prop"] <- (out_meuwissen$A_bar_bb["female_2","female_1"] - out_meuwissen_4_per_juv$A_bar_bb["female_2","female_1"]) / out_meuwissen$A_bar_bb["female_2","female_1"]


if(Even_n_per_fam) {
  #one row per family
  tmp_rows <- !duplicated(out_meuwissen$ped[(nrow(out_meuwissen$ped) - nrow(out_meuwissen$M_1) + 1):nrow(out_meuwissen$ped),c("sire", "dam")])

  tmp <- lm(out_meuwissen$M_1[tmp_rows,1] ~ out_meuwissen_4_per_juv$M_1[tmp_rows,1])
  tmp_out["M1_male_2_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["M1_male_2_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["M1_male_2_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp <- lm(out_meuwissen$M_1[tmp_rows,2] ~ out_meuwissen_4_per_juv$M_1[tmp_rows,2])
  tmp_out["M1_male_1_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["M1_male_1_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["M1_male_1_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp <- lm(out_meuwissen$M_1[tmp_rows,3] ~ out_meuwissen_4_per_juv$M_1[tmp_rows,3])
  tmp_out["M1_female_2_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["M1_female_2_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["M1_female_2_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp <- lm(out_meuwissen$M_1[tmp_rows,4] ~ out_meuwissen_4_per_juv$M_1[tmp_rows,4])
  tmp_out["M1_female_1_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["M1_female_1_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["M1_female_1_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)
} else {
  #one row per family
  tmp_rows <- !duplicated(out_meuwissen$ped[(nrow(out_meuwissen$ped) - nrow(out_meuwissen$M_1) + 1):nrow(out_meuwissen$ped),c("sire", "dam")]) &
    rep(c(T,F,F,F,T,F,F,F), each = (nrow(out_meuwissen$M_1)/8))

  tmp <- lm(out_meuwissen$M_1[tmp_rows,1] ~ out_meuwissen_4_per_juv$M_1[tmp_rows,1])
  tmp_out["M1_male_2_small_fam_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["M1_male_2_small_fam_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["M1_male_2_small_fam_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp <- lm(out_meuwissen$M_1[tmp_rows,2] ~ out_meuwissen_4_per_juv$M_1[tmp_rows,2])
  tmp_out["M1_male_1_small_fam_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["M1_male_1_small_fam_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["M1_male_1_small_fam_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp <- lm(out_meuwissen$M_1[tmp_rows,3] ~ out_meuwissen_4_per_juv$M_1[tmp_rows,3])
  tmp_out["M1_female_2_small_fam_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["M1_female_2_small_fam_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["M1_female_2_small_fam_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp <- lm(out_meuwissen$M_1[tmp_rows,4] ~ out_meuwissen_4_per_juv$M_1[tmp_rows,4])
  tmp_out["M1_female_1_small_fam_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["M1_female_1_small_fam_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["M1_female_1_small_fam_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp_rows <- !duplicated(out_meuwissen$ped[(nrow(out_meuwissen$ped) - nrow(out_meuwissen$M_1) + 1):nrow(out_meuwissen$ped),c("sire", "dam")]) &
    rep(c(F,T,T,T,F,T,T,T), each = (nrow(out_meuwissen$M_1)/8))

  tmp <- lm(out_meuwissen$M_1[tmp_rows,1] ~ out_meuwissen_4_per_juv$M_1[tmp_rows,1])
  tmp_out["M1_male_2_large_fam_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["M1_male_2_large_fam_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["M1_male_2_large_fam_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp <- lm(out_meuwissen$M_1[tmp_rows,2] ~ out_meuwissen_4_per_juv$M_1[tmp_rows,2])
  tmp_out["M1_male_1_large_fam_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["M1_male_1_large_fam_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["M1_male_1_large_fam_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp <- lm(out_meuwissen$M_1[tmp_rows,3] ~ out_meuwissen_4_per_juv$M_1[tmp_rows,3])
  tmp_out["M1_female_2_large_fam_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["M1_female_2_large_fam_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["M1_female_2_large_fam_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)

  tmp <- lm(out_meuwissen$M_1[tmp_rows,4] ~ out_meuwissen_4_per_juv$M_1[tmp_rows,4])
  tmp_out["M1_female_1_large_fam_intercept"] <- as.vector(tmp$coefficients[1])
  tmp_out["M1_female_1_large_fam_slope"]     <- as.vector(tmp$coefficients[2])
  tmp_out["M1_female_1_large_fam_R2"]        <- as.vector(summary(tmp)$r.squared)
  rm(tmp)
}


if(!is.null(outputs_meuwissen_4_per_juv)) {
  outputs_meuwissen_4_per_juv <- merge(outputs_meuwissen_4_per_juv, tmp_out, by = colnames(tmp_out)[colnames(tmp_out) %in% colnames(outputs_meuwissen_4_per_juv)], all = T)
} else {
  outputs_meuwissen_4_per_juv <- tmp_out
}
rm(tmp_out, tmp_rows)

write.table(outputs_meuwissen_4_per_juv, file = "outputs_meuwissen_4_per_juv.csv", quote = FALSE, sep = ",", row.names = FALSE)


