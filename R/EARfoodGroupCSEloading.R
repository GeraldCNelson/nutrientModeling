# Intro
# -------------------------------------------------------------------
# This script reads in the Shared Socioeconomic Profiles information
# and the nutrient requirements data It produces data tables that have
# the various nutrient requirements weighted by SSP age and gender
# groups In several places old inefficient code is commented out and
# replaced by cleaner code written by Brendan Power of CSIRO

# Copyright (C) 2015 Gerald C. Nelson, except where noted

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or (at
# your option) any later version.  This program is distributed in the
# hope that it will be useful, but WITHOUT ANY WARRANTY; without even
# the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
# PURPOSE.  See the GNU General Public License for more details at
# http://www.gnu.org/licenses/.  Contributors to the work include
# Brendan Power (for coding assistance), Joanne E. Arsenault (for
# constructing the nutrient requirements worksheet) and Joanne E.
# Arsenault, Malcom Reilly, Jessica Bogard, and Keith Lividini (for
# nutrition expertise)

# Source of requirements is
# http://www.nal.usda.gov/fnic/DRI/DRI_Tables/recommended_intakes_individuals.pdf
# ---

setwd("~/Documents/workspace/nutrientModeling")
source(file = "setup.R")

# source('nutrientDataLoading.R') # only run this when there is a new
# nutrients spreadsheet; otherwise the next line is fine
load(file = "data/nutrients_final.RData")
# file name definitions are in setup.R Read in and clean up files
# ----------------------------------------------

# This is the list of food group codes as of June 28, 2015 beverages <-
# c('beverages') cereals <- c('cereals') dairy <- c('dairy') eggs <-
# c('eggs') fish <- c('fish') fruits <- c('fruits') meats <- c('meats')
# oils <-c('fats and oils') oilSeeds <- c('oil seeds') pulses <-
# c('pulses') roots <- c('roots and tubers') sweeteners <- c('sugar and
# sweeteners') vegetables <-c('vegetables')

allFoodGroups <- unique(foodGroupsInfo$food.group.code)
# read in the CSEs - moved to IMPACTdataLoading.R

# Read in and set up the nutrient requirements data
# ---------------------------------------------

# getSheetNames(EARFileName)
req.metadata <- read.xlsx(EARFileName, sheet = 1, colNames = FALSE)
req.EAR <-
  read.xlsx(
    EARFileName,
    sheet = 2,
    startRow = 1,
    cols = 3:24,
    colNames = TRUE
  )
req.RDA.vits <-
  read.xlsx(
    EARFileName,
    sheet = 3,
    startRow = 1,
    cols = 3:17,
    colNames = TRUE
  )
req.RDA.minrls <-
  read.xlsx(
    EARFileName,
    sheet = 4,
    startRow = 1,
    cols = 3:18,
    colNames = TRUE
  )
req.RDA.macro <-
  read.xlsx(
    EARFileName,
    sheet = 5,
    startRow = 1,
    cols = 3:10,
    colNames = TRUE
  )
req.UL.vits <-
  read.xlsx(
    EARFileName,
    sheet = 7,
    startRow = 1,
    cols = 3:18,
    colNames = TRUE
  )
req.UL.minrls <-
  read.xlsx(
    EARFileName,
    sheet = 8,
    startRow = 1,
    cols = 3:22,
    colNames = TRUE
  )
req.AMDR <-
  read.xlsx(
    EARFileName,
    sheet = 6,
    startRow = 1,
    cols = 3:13,
    colNames = TRUE
  )

# make lists of nutrients common to the food nutrient list and the
# requirements list ---
common.EAR <-
  intersect(sort(colnames(nutrients)), sort(colnames(req.EAR)))
common.RDA.vits <-
  intersect(colnames(nutrients), colnames(req.RDA.vits))
common.RDA.minrls <-
  intersect(colnames(nutrients), colnames(req.RDA.minrls))
common.RDA.macro <-
  intersect(colnames(nutrients), colnames(req.RDA.macro))
common.UL.vits <-
  intersect(colnames(nutrients), colnames(req.UL.vits))
common.UL.minrls <-
  intersect(colnames(nutrients), colnames(req.UL.minrls))
common.AMDR <-
  intersect(colnames(nutrients), colnames(req.AMDR))  # this is empty right now
# keep only the common nutrients
req.EAR <- req.EAR[, c("ageGenderCode", common.EAR)]
req.RDA.vits <- req.RDA.vits[, c("ageGenderCode", common.RDA.vits)]
req.RDA.minrls <-
  req.RDA.minrls[, c("ageGenderCode", common.RDA.minrls)]
req.RDA.macro <-
  req.RDA.macro[, c("ageGenderCode", common.RDA.macro)]
req.UL.vits <- req.UL.vits[, c("ageGenderCode", common.UL.vits)]
req.UL.minrls <-
  req.UL.minrls[, c("ageGenderCode", common.UL.minrls)]
req.AMDR <- req.AMDR[, c("ageGenderCode", common.AMDR)]

# naming conventions ---------- M - Male, F- Female, X - children of
# both genders, Preg - pregnant, Lact - lactating

# assuming the population statistics have the number of children 5-9
# years and the EAR is for 4-8 years… you could make a new population
# estimate for 4-8 years by taking 20% of the population for 0-4 year
# olds (this is the number of 4 year olds) plus 80% of the population
# of 5-9 year olds (this is number of 5-8 year olds) – and then adjust
# all the others in a similar manner.

male.ssp <-
  c(
    "SSPM5_9",
    "SSPM10_14",
    "SSPM15_19",
    "SSPM20_24",
    "SSPM25_29",
    "SSPM30_34",
    "SSPM35_39",
    "SSPM40_44",
    "SSPM45_49",
    "SSPM50_54",
    "SSPM55_59",
    "SSPM60_64",
    "SSPM65_69",
    "SSPM70_74",
    "SSPM75_79",
    "SSPM80_84",
    "SSPM85_89",
    "SSPM90_94",
    "SSPM95_99",
    "SSPM100Plus"
  )
male.dri <-
  c(
    "Chil4_8",
    "M9_13",
    "M14_18",
    "M19_30",
    "M19_30",
    "M31_50",
    "M31_50",
    "M31_50",
    "M31_50",
    "M31_50",
    "M51_70",
    "M51_70",
    "M51_70",
    "M70Plus",
    "M70Plus",
    "M70Plus",
    "M70Plus",
    "M70Plus",
    "M70Plus",
    "M70Plus"
  )
male <- data.frame(male.ssp, male.dri, stringsAsFactors = FALSE)

female.ssp <-
  c(
    "SSPF5_9",
    "SSPF10_14",
    "SSPF15_19",
    "SSPF20_24",
    "SSPF25_29",
    "SSPF30_34",
    "SSPF35_39",
    "SSPF40_44",
    "SSPF45_49",
    "SSPF50_54",
    "SSPF55_59",
    "SSPF60_64",
    "SSPF65_69",
    "SSPF70_74",
    "SSPF75_79",
    "SSPF80_84",
    "SSPF85_89",
    "SSPF90_94",
    "SSPF95_99",
    "SSPF100Plus"
  )
female.dri <-
  c(
    "Chil4_8",
    "F9_13",
    "F14_18",
    "F19_30",
    "F19_30",
    "F31_50",
    "F31_50",
    "F31_50",
    "F31_50",
    "F31_50",
    "F51_70",
    "F51_70",
    "F51_70",
    "F70Plus",
    "F70Plus",
    "F70Plus",
    "F70Plus",
    "F70Plus",
    "F70Plus",
    "F70Plus"
  )
female <-
  data.frame(female.ssp, female.dri, stringsAsFactors = FALSE)

children <- c("Inf0_0.5", "Inf0.5_1", "Chil1_3")

reqs <-
  c(
    "req.EAR",
    "req.RDA.vits",
    "req.RDA.minrls",
    "req.RDA.macro",
    "req.UL.vits",
    "req.UL.minrls"
  )
for (j in reqs) {
  temp2 <- eval(parse(text = j))
  temp2[is.na(temp2)] <- 0
  for (i in 1:nrow(male)) {
    temp <- temp2[temp2$ageGenderCode == male[i, 2],]
    temp[1, 1] <- male[i, 1]
    # print(temp[1,1])
    temp2 <- rbind(temp2, temp)
  }
  for (i in 1:nrow(female)) {
    temp <- temp2[temp2$ageGenderCode == female[i, 2],]
    temp[1, 1] <- female[i, 1]
    # print(temp[1,1])
    temp2 <- rbind(temp2, temp)
  }
  # children calcs for nutrient needs SSPF0_4 <- Inf0_0.5*(1/6) +
  # Inf0.5_1*(1/6 + Chil1_3)*(4/6) SSPM0_4 <- Inf0_0.5*(1/6) +
  # Inf0.5_1*(1/6 + Chil1_3)*(4/6) 0 to 4 is four years. Divide into 8 6
  # month intervals.
  chldrn.male <- temp2[temp2$ageGenderCode %in% children,]
  chldrn.male.SSP <-
    chldrn.male[chldrn.male$ageGenderCode == "Inf0_0.5",
                2:length(temp)] * 1 / 8 + chldrn.male[chldrn.male$ageGenderCode ==
                                                        "Inf0.5_1", 2:length(chldrn.male)] * 1 /
    8 + chldrn.male[chldrn.male$ageGenderCode ==
                      "Chil1_3", 2:length(chldrn.male)] * 6 /
    8
  chldrn.male.SSP$ageGenderCode <- "SSPM0_4"
  
  # female children are the same as male children
  chldrn.female.SSP <- chldrn.male.SSP
  chldrn.female.SSP$ageGenderCode <- "SSPF0_4"
  temp2 <-
    rbind_all(list(temp2, chldrn.male.SSP, chldrn.female.SSP))
  
  # calculations needed for the pregnant and lactating women results ----
  # SSPF15_49 <- (F14_18 + F19_30 + F31_50)/3 SSPLact <- (Lact14_18 +
  # Lact19_30 + Lact31_50)/3 SSPPreg <- (Preg14_18 + Preg19_30 +
  # Preg31_50)/3
  
  preg.potent <-
    temp2[temp2$ageGenderCode %in% c("F14_18", "F19_30",
                                     "F31_50"),]
  preg.potent <-
    preg.potent[preg.potent$ageGenderCode == "F14_18", 2:length(preg.potent)] /
    3 +
    preg.potent[preg.potent$ageGenderCode == "F19_30", 2:length(preg.potent)] /
    3 +
    preg.potent[preg.potent$ageGenderCode == "F31_50", 2:length(preg.potent)] /
    3
  preg.potent$ageGenderCode <- "SSPF15_49"
  
  lact <-
    temp2[temp2$ageGenderCode %in% c("Lact14_18", "Lact19_30",
                                     "Lact31_50"),]
  lact <-
    lact[lact$ageGenderCode == "Lact14_18", 2:length(lact)] / 3 +
    lact[lact$ageGenderCode == "Lact19_30", 2:length(lact)] / 3 + lact[lact$ageGenderCode ==
                                                                         "Lact31_50", 2:length(lact)] /
    3
  lact$ageGenderCode <- "SSPLact"
  
  preg <-
    temp2[temp2$ageGenderCode %in% c("Preg14_18", "Preg19_30",
                                     "Preg31_50"),]
  preg <-
    preg[preg$ageGenderCode == "Preg14_18", 2:length(preg)] / 3 +
    preg[preg$ageGenderCode == "Preg19_30", 2:length(preg)] / 3 + preg[preg$ageGenderCode ==
                                                                         "Preg31_50", 2:length(preg)] /
    3
  preg$ageGenderCode <- "SSPPreg"
  
  temp2 <-
    as.data.frame(rbind_all(list(temp2, preg.potent, lact, preg)))
  
  # delete extraneous rows
  temp2 <-
    temp2[(
      !temp2$ageGenderCode %in% male[, 2] & !temp2$ageGenderCode %in%
        female[, 2] &
        !temp2$ageGenderCode %in% children & !temp2$ageGenderCode %in%
        c("Preg14_18", "Preg19_30", "Preg31_50") &
        !temp2$ageGenderCode %in%
        c("Lact14_18", "Lact19_30", "Lact31_50")
    ),]
  newDFname <- paste(j, ".ssp", sep = "")
  nutlistname <- paste()
  assign(newDFname, temp2)
  nutlistname <- paste(j, ".nutlist", sep = "")
  nutlist <- colnames(temp2[, 2:length(temp2)])
  assign(nutlistname, nutlist)
}

remove(
  "temp",
  "temp2",
  "male",
  "male.dri",
  "male.ssp",
  "children",
  "chldrn.male",
  "chldrn.male.SSP",
  "female",
  "chldrn.female.SSP",
  "preg.potent",
  "preg",
  "female.dri",
  "female.ssp",
  "lact",
  "req.EAR.nutlist",
  "req.RDA.macro.nutlist",
  "req.UL.minrls.nutlist",
  "req.RDA.minrls.nutlist",
  "req.RDA.vits.nutlist",
  "req.UL.vits.nutlist",
  "newDFname",
  "nutlistname"
)

# these have been replaced by their equivalent with SSP age categories
remove(
  req.EAR,
  req.RDA.vits,
  req.RDA.minrls,
  req.RDA.macro,
  req.UL.vits,
  req.UL.minrls,
  req.AMDR
)

# remove(common.EAR, common.RDA.vits, common.RDA.minrls,
# common.RDA.macro, common.UL.vits, common.UL.minrls, common.AMDR )