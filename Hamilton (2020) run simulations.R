##########################################################################################
#Run Scenarios
##########################################################################################

rm(list = ls())         #Delete data from the workspace

work_dir <<- getwd()

indiv_per_sr       <- 1280
Additive_Var       <- 0.2	 #Additive variance in parental population (used to compute EBVs but not actually necessary for this example)
C_max              <- 0.5  #Mean relationship per selection round
sr_to_generate     <- 20
proportion_class_2 <- 0

outputs <- NULL
outputs_meuwissen_4_per_juv <- NULL
#outputs <- read.table("outputs.csv", header=TRUE, sep=",")
#outputs_meuwissen_4_per_juv <- read.table("outputs_meuwissen_4_per_juv.csv", header=TRUE, sep=",")

library(OptContR)
library(compiler)
library(tidyverse)
library(nadiv)

#################################################################################
#Start
#################################################################################

for(rep in 1:100) {

Families_per_sr <- 10 #Families per selection round
Even_n_per_fam  <- F
source(file = "Hamilton (2020) simulation code.R", echo = T)

Families_per_sr <- 20 #Families per selection round
Even_n_per_fam  <- F
source(file = "Hamilton (2020) simulation code.R", echo = T)

Families_per_sr <- 40 #Families per selection round
Even_n_per_fam  <- F
source("Hamilton (2020) simulation code.R", echo = T)

Families_per_sr <- 80 #Families per selection round
Even_n_per_fam  <- F
source("Hamilton (2020) simulation code.R", echo = T)

Families_per_sr <- 160 #Families per selection round
Even_n_per_fam  <- F
source("Hamilton (2020) simulation code.R", echo = T)

Families_per_sr <- 320 #Families per selection round
Even_n_per_fam  <- F
source("Hamilton (2020) simulation code.R", echo = T)

Families_per_sr <- 10 #Families per selection round
Even_n_per_fam  <- T
source(file = "Hamilton (2020) simulation code.R", echo = T)

Families_per_sr <- 20 #Families per selection round
Even_n_per_fam  <- T
source("Hamilton (2020) simulation code.R", echo = T)

Families_per_sr <- 40 #Families per selection round
Even_n_per_fam  <- T
source("Hamilton (2020) simulation code.R", echo = T)

Families_per_sr <- 80 #Families per selection round
Even_n_per_fam  <- T
source("Hamilton (2020) simulation code.R", echo = T)

Families_per_sr <- 160 #Families per selection round
Even_n_per_fam  <- T
source("Hamilton (2020) simulation code.R", echo = T)

Families_per_sr <- 320 #Families per selection round
Even_n_per_fam  <- T
source("Hamilton (2020) simulation code.R", echo = T)

}

#source("Summarise outputs.R")
