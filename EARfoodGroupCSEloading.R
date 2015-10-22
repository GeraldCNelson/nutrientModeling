# Intro -------------------------------------------------------------------
#This script reads in the Shared Socioeconomic Profiles information, does some manipulations of the data,
#and pulls out just the population data
#In several places old inefficient code is commented out and replaced by cleaner code written by Brendan Power of CSIRO

#Copyright (C) 2015 Gerald C. Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details at http://www.gnu.org/licenses/.

require(openxlsx)
require(entropy)
require(reshape2)
require(plyr)
require(dplyr)
require(tidyr)
require(data.table)
require(splitstackshape)
require(plotrix)
setwd("~/Documents/workspace/nutrientModeling")
EARFileName <- "data/DRI_IOM_V1.xlsx"
commodityFoodGroupLookupFileName <- "data/food commodity to food group table V1.xlsx"
# CSE - consumer support equivalent
#Note: the price a consumer pays is Pc * (1-CSE)
CSEFileName <- "data/IMPACTData/CSEs20150824.xlsx" 
# Read in and clean up files ----------------------------------------------

# Food groups and commodities ---------------------------------------------
# 13 food groups 
foodGroupsInfo <- read.xlsx(commodityFoodGroupLookupFileName, 
                            sheet = 1,
                            startRow = 1,
                            cols = 1:4,
                            colNames = TRUE)

# Note: staples is not a food group per se but included here because it is used in one of the diversity metrics
staples <- c("cereals", "roots")

# This is the list of food group codes as of June 28, 2015
# beverages <- c("beverages")
# cereals <- c("cereals")
# dairy <- c("dairy")
# eggs <- c("eggs")
# fish <- c("fish")
# fruits <- c("fruits") 
# meats <- c("meats")
# oils <-c("fats and oils")
# oilSeeds <- c("oil seeds")
# pulses <- c("pulses")
# roots <- c("roots and tubers")
# sweeteners <- c("sugar and sweeteners")
# vegetables <-c("vegetables")

allFoodGroups <- unique(foodGroupsInfo$food.group.code)
#read in the CSEs
CSEs <- read.xlsx(CSEFileName, 
                  sheet = 1,
                  cols = 1:3,
                  colNames = TRUE)
colnames(CSEs) <- c("region","IMPACT_code","CSE")

#Set up the EARs data
#getSheetNames(EARFileName)
req.metadata <- read.xlsx(EARFileName, sheet = 1, colNames = FALSE)
req.EAR <- read.xlsx(EARFileName, sheet = 2, startRow = 1, cols = 3:24, colNames = TRUE)
req.RDA.vits <- read.xlsx(EARFileName, sheet = 3, startRow = 1, cols = 3:17, colNames = TRUE)
req.RDA.minrls <- read.xlsx(EARFileName, sheet = 4, startRow = 1, cols = 3:18, colNames = TRUE)
req.RDA.macro <- read.xlsx(EARFileName, sheet = 5, startRow = 1, cols = 3:10, colNames = TRUE)
req.UL.vits <- read.xlsx(EARFileName, sheet = 7, startRow = 1, cols = 3:18, colNames = TRUE)
req.UL.minrls <- read.xlsx(EARFileName, sheet = 8, startRow = 1, cols = 3:22, colNames = TRUE)
req.AMDR <- read.xlsx(EARFileName, sheet = 6, startRow = 1, cols = 3:13, colNames = TRUE)

# name convention, M - Male, F- Female, X - children of both genders, P - pregnant, L - lactating

#children requirements
SSPF0_4 <- Inf0_0.5*(1/6) + Inf0.5_1*(1/6 + Chil1_3)*(4/6)
SSPM0_4 <- Inf0_0.5*(1/6) + Inf0.5_1*(1/6 + Chil1_3)*(4/6)
SSPF5_9 <- Chil4_8
SSPM5_9 <- Chil4_8
SSPF10_14 <- F9_13
SSPM10_14 <- M9_13

#males
SSPM15_19 <- M14_18
SSPM20_24 <- M19_30
SSPM25_29 <- M19_30
SSPM30_34 <- M31_50
SSPM35_39 <- M31_50
SSPM40_44 <- M31_50
SSPM45_49 <- M31_50
SSPM50_54 <- M31_50
SSPM55_59 <- M51_70
SSPM60_64 <- M51_70
SSPM65_69 <- M51_70
SSPM70_74 <- M70Plus
SSPM75_79 <- M70Plus
SSPM80_84 <- M70Plus
SSPM85_89 <- M70Plus
SSPM90_94 <- M70Plus
SSPM95_99 <- M70Plus
SSPM100Plus <- M70Plus

#females

SSPF15_19 <- F14_18
SSPF20_24 <- F19_30
SSPF25_29 <- F19_30
SSPF30_34 <- F31_50
SSPF35_39 <- F31_50
SSPF40_44 <- F31_50
SSPF45_49 <- F31_50
SSPF50_54 <- F31_50
SSPF55_59 <- F51_70
SSPF60_64 <- F51_70
SSPF65_69 <- F51_70
SSPF70_74 <- F70Plus
SSPF75_79 <- F70Plus
SSPF80_84 <- F70Plus
SSPF85_89 <- F70Plus
SSPF90_94 <- F70Plus
SSPF95_99 <- F70Plus
SSPF100Plus <- F70Plus
SSPF15_49 <-  (F14_18 + F19_30 + F31_50)/3
SSPLact <- (L14_18 + L19_30 + L31_50)/3
SSPPreg <- (P14_18 + P19_30 + P31_50)/3

in.df <- data.frame(x = LETTERS[1:3], y = 1:3) 
out.df <- in.df[rep(seq(nrow(in.df)), in.df$y), ] 

#Then if you want to re-name the row.names 

row.names(out.df) <- seq(nrow(out.df)) 

#delete old rows 
EARs <- EARs[, !names(EARs) %in% 
               c("X0_0.5","X0.5_1","X1_3","X4_8",
                 "M9_13","M14_18","M19_30","M31_50","M51_70","M70Plus",
                 "F9_13","F14_18","F19_30","F31_50","F51_70","F70Plus",
                 "P14_18","P19_30","P31_50","L14_18","L19_30","L31_50")]
#convert NAs to zeros
EARs[is.na(EARs)] <- 0
