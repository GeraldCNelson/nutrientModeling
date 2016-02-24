# Intro -----------------------
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

source("R/dataPrep.setup.R")

# fish supply in 1000 metric tons
#' @param - fishS - supply of fish
fishS <- read.xlsx(
  IMPACTfish,
  sheet = "QS_FishSys",
  cols = 1:6,
  startRow = 3,
  colNames = FALSE
)
colnames(fishS) <-
  c("fish_type",
    "region",
    "freshAquac",
    "marinAquac",
    "freshCapt",
    "marine_capt")

#' @param - fishLookup - fish look up. production and consumption names and IMPACT names
fishLookup <- read.xlsx(
  IMPACTfish,
  sheet = "IMPACT Commodities",
  cols = 6:7,
  startRow = 2,
  colNames = TRUE
)

#' @param - fishD - fish demand in 1000 metric tons
fishD <- read.xlsx(
  IMPACTfish,
  sheet = "DemandStkChg",
  cols = 1:11,
  startRow = 3,
  colNames = FALSE
)
colnames(fishD) <-
  c(
    "IMPACT_code",
    "region",
    "net_trade",
    "exports",
    "imports",
    "tot_demand",
    "food_demand",
    "feed_demand",
    "other_demand",
    "stock_change",
    "crush_demand"
  )

fishD[is.na(fishD)] <- 0
fishD <- fishD[order(fishD$region),]

#' @param - fishIncElast - fish income elasticity
fishIncElast <- read.xlsx(
  IMPACTfish,
  sheet = "IncDmdElas",
  cols = 1:11,
  startRow = 1,
  colNames = TRUE
)

colnames(fishIncElast) <- c("region","c_shrimp","c_Crust","c_Mllsc","c_Salmon","c_FrshD",
"c_Tuna", "c_OPelag", "c_ODmrsl","c_OMarn", "c_FshOil")

# need to create fish data for the new regions in the latest version of IMPACT
regions.all <- getNewestVersion("regions.all")
# merge regions.all and the IMPACT115fishIncElast to assign identical income elasticities
# to all countries in an IMPACT3 region.
temp <- as.data.table(merge(fishIncElast,regions.all, by.x = "region", by.y = "region_code.IMPACT115"))
deleteColList <- c("region","region_name.IMPACT115")
fishIncElast.IMPACT3 <- temp[,(deleteColList) := NULL]

# get list of all the fish codes
temp <- names(fishIncElast.IMPACT3)
fish_code <- temp[1:10]

dt.FBS <- getNewestVersion("FBS")
# get rid of extraneous columns
deleteColList <- c("item_name","definition","IMPACT_missing_code","fish","alcohol",
                   "FAOSTAT_country_code","item_code")
dt.FBS[,(deleteColList) := NULL]

#get rid of all rows other than those with domestic food supply (kcal/capita/day) and food (1000 tonnes)
deleteRowList <- c(
  "Domestic supply quantity",
  "Feed",
  "Seed",
  "Waste",
  "Processing",
  "Other Util",
  "Production",
  "Import Quantity",
  "Food supply quantity (kg/capita",
  "Protein supply quantity",
  "Fat supply quantity",
  "Stock Variation",
  "Export Quantity"
)

# keep only rows with elements food (5142) and Food supply quantity (kg/capita/yr) (645)
keepElements <- c(5142,645)

dt.FBS <- dt.FBS[element_code %in% keepElements, ]

# select the rows that have one of the fish codes in the IMPACT_code column
#dt.FBS.fish <- dt.FBS[IMPACT_code %in% fish_code,]
setkey(dt.FBS,IMPACT_code)
dt.FBS.fish <- dt.FBS[IMPACT_code %in% fish_code]

