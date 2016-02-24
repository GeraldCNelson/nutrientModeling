#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords SSP data cleanup
# Intro -------------------------------------------------------------------
#' @description
#' This script renames the variables in the SSP file and deletes all years except those in
#' keepYearList, created in dataPrep.setup.R
#' It keeps population results only from the IIASA-WiC POP model.

#Copyright (C) 2015 Gerald C,Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, See the
#     GNU General Public License for more details at http://www.gnu.org/licenses/.
source(file = "R/dataPrep.setup.R")

SSP <-
  read.csv(unzip(SSPdataZip, file = SSPcsv),
           stringsAsFactors = FALSE)
# drop all years except those in keepYearList, created in dataPrep.setup.R
SSP <-
  SSP[c("MODEL", "SCENARIO", "REGION", "VARIABLE", "UNIT", keepYearList)]

#make all names lower case and change region to region_code
names(SSP) <-
  c("model",
    "scenario",
    "ISO_code",
    "variable",
    "unit",
    keepYearList)
dt.SSP <- as.data.table(SSP)

#There are 21 scenarios; 4 each for SSP scenarios 1, 2, 3, and 5 and
# 5 for SSP scenario 4.
#This is the list of dt.SSP.regions3 scenarios
#"SSPx_v9_130424" is from PIK and just for the US
#"SSPx_v9_130325" is from OECD and is just GDP and population
#"SSPx_v9_130219" is from IIASA and is just population and GDP
#"SSPx_v9_130115" contains info from IIASA on population broken down by age,
# gender, and education and NCAR on population broken down by rural and urban.
# The IIASA data are from 2010 to 2100.
# Keep only the population results from IIASA for the age and gender breakdown and
# for the years 2010 to 2050.

# the lists of pop and GDP models are defined in dataPrep.setup.R

# create cleaned up GDP SSP data ---
#' @param dt.SSP.GDP - data table with the SSP results from the model identified in modelListGDP
dt.SSP.GDP <- dt.SSP[model == modelListGDP, ]
saveRDS(dt.SSP.GDP, file = paste(mData, "/SSPGDPClean.", Sys.Date(), ".rds",
                            sep = ""))

# create cleaned up population SSP data ---
#' @param dt.SSP.pop - data table with the SSP results from the model identified in modelListPop
dt.SSP.pop <- dt.SSP[model == modelListPop, ]
# Create population age and gender data set by removing rows with education breakdown -----
#' @param popList - variable name for population
popList <- "Population"

#' @param ageList - variable name for age group categories
ageList <-
  c(
    "Aged0-4",
    "Aged5-9",
    "Aged10-14",
    "Aged15-19",
    "Aged20-24",
    "Aged25-29",
    "Aged30-34",
    "Aged35-39",
    "Aged40-44",
    "Aged45-49",
    "Aged50-54",
    "Aged55-59",
    "Aged60-64",
    "Aged65-69",
    "Aged70-74",
    "Aged75-79",
    "Aged80-84",
    "Aged85-89",
    "Aged90-94",
    "Aged95-99",
    "Aged100+"
  )

#' @param edList - variable name for education categories
edList <-
  c("No Education",
    "Primary Education",
    "Secondary Education",
    "Tertiary Education")
genderList <- c("Male", "Female")

#keep full population count around for bug checking later
dt.SSP.pop.tot <-
  dt.SSP.pop[variable == "Population", c("scenario","ISO_code","unit" ,keepYearList), with = FALSE]

# Remove the aggregates of
# "Population", "Population|Female" and "Population|Male"
deleteRowList <-
  c("Population", "Population|Female", "Population|Male")
dt.SSP.pop.step1 <- dt.SSP.pop[!variable %in% deleteRowList, ]

# split the variable names apart where there is a |
# (eg. X|Y becomes X and Y and new columns are created)
dt.SSP.pop.step2 <-
  cSplit(
    dt.SSP.pop.step1, 'variable', sep = "|", type.convert = FALSE
  )

#name the new columns created by the spliting process above
oldNames <- c("variable_1", "variable_2", "variable_3", "variable_4")
newNames <- c("population", "gender", "ageGenderCode", "education")
setnames(dt.SSP.pop.step2,oldNames,newNames)

#rename variables to align with the requirements names
dt.SSP.pop.step2$ageGenderCode <-
  gsub("Aged", "", dt.SSP.pop.step2$ageGenderCode)
dt.SSP.pop.step2$ageGenderCode[dt.SSP.pop.step2$gender == "Female"] <-
  paste("F",
        dt.SSP.pop.step2$ageGenderCode[dt.SSP.pop.step2$gender == "Female"],
        sep = "")
dt.SSP.pop.step2$ageGenderCode[dt.SSP.pop.step2$gender == "Male"] <-
  paste("M",
        dt.SSP.pop.step2$ageGenderCode[dt.SSP.pop.step2$gender == "Male"],
        sep = "")
dt.SSP.pop.step2$ageGenderCode <-
  gsub("-", "_", dt.SSP.pop.step2$ageGenderCode)
dt.SSP.pop.step2$ageGenderCode <-
  gsub("\\+", "Plus", dt.SSP.pop.step2$ageGenderCode)
dt.SSP.pop.step2 <-
  dt.SSP.pop.step2[order(dt.SSP.pop.step2$ISO_code), ]

#remove rows that breakdown an age group by education
removeRowList <-
  c("No Education",
    "Primary Education",
    "Secondary Education",
    "Tertiary Education")
dt.SSP.pop.step2 <-
  dt.SSP.pop.step2[!dt.SSP.pop.step2$education %in% removeRowList, ]

#remove extraneous columns and keep only the ones needed
keepList <- c("scenario","ISO_code", "ageGenderCode", keepYearList)
colDeleteList <- c("model", "gender","education","population","unit")
dt.SSP.pop.step2[, (colDeleteList) := NULL]

#save cleaned up pop file as an .rds file
saveRDS(dt.SSP.pop.step2, file = paste(mData, "/SSPPopClean.", Sys.Date(), ".rds",
                           sep = ""))

