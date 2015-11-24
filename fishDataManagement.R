# Intro ___________________________________________________________________
#This script reads in fish data and parameters for IMPACT.

#Copyright (C) 2015 Gerald C. Nelson, except where noted

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details at http://www.gnu.org/licenses/.

source("setup.R")

# fish supply in 1000 metric tons
fishS <- read.xlsx(fishInfoIMPACT, 
 sheet = "QS_FishSys",
 cols = 1:6, startRow = 3,
 colNames = FALSE)
colnames(fishS) <- c("fish_type","region","freshAquac","marinAquac","freshCapt","marine_capt")

#rename fish_type to IMPACT commodity names
fishNameIMPACT <- c("c_shrimp", "c_Crust",	"c_Mllsc",	"c_Salmon",	"c_FrshD", "c_Tuna", 
                    "c_OPelag", "c_ODmrsl", "c_OMarn", "c_FshOil")
fishqNameOld<- c("Shrimp", "Crust", "Mllsc", "Salmon", "Dmrsl", "Tuna", "Cobswf", "Eelstg", 
                 "Tilapia", "Pangas", "Carp", "Mullet", "OFrshD", "OCarp", "OPelag", "OMarn")
IMPACT_code_lookup <- c("c_shrimp","c_Crust","c_Mllsc","c_Salmon","c_FrshD","c_Tuna","c_FrshD","c_FrshD","c_FrshD","c_FrshD","c_FrshD",
                    "c_ODmrsl","c_FrshD","c_FrshD","c_OPelag","c_OMarn")

fishLookup <- data.frame(fishqNameOld,fishNameIMPACT)

# fish demand in 1000 metric tons
fishD <- read.xlsx(fishInfoIMPACT, 
                   sheet = "DemandStkChg",
                   cols = 1:11, startRow = 3,
                   colNames = FALSE)
colnames(fishD) <- c("IMPACT_code","region", "net_trade","exports","imports","tot_demand","food_demand",
                     "feed_demand", "other_demand", "stock_change","crush_demand")

fishD[is.na(fishD)] <- 0
fishD <- fishD[order( fishD$region), ]
fishIncElast <- read.xlsx(fishInfoIMPACT, 
                                   sheet = "IncDmdElas",
                                   cols = 1:11, startRow = 1,
                                   colNames = TRUE)

# need to create fish data for the new regions in the latest version of IMPACT

ctyNames.old <- sort(unique(fishD$region))

regionsLookup <- read.xlsx(fishInfoIMPACT, 
                          sheet = "IMPACT 115 Regions",
                          cols = 4:11, rows = 3:120,
                          colNames = FALSE)

temp <- as.data.frame(cSplit(regionsLookup, 'X1', sep=".", type.convert=FALSE))
temp$X1_2 <- gsub("\\(","",temp$X1_2)
temp$X1_2 <- gsub("\\)","",temp$X1_2)

temp2 <- merge(x = temp,y = fishD, by.x = "X1_1", by.y = "region", all = TRUE)
df.ctyNames <- as.data.frame(ctyNames, stringsAsFactors = FALSE)
temp3 <- merge(x = temp2, y = df.ctyNames, by.x = "X1_2", by.y = "ctyNames", all = TRUE)
