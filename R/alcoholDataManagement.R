# Intro -----------------------
#This script reads in FBS data and writes out just the alcohol consumption data.

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

dt.FBS <- getNewestVersion("FBS")
dt.FBS.alcohol <- dt.FBS[!is.na(alcohol)]
alcohol_code <- unique(dt.FBS$alcohol)
# get rid of extraneous columns
deleteColList <- c("item_name","definition","IMPACT_missing_code","fish","alcohol",
                   "FAOSTAT_country_code","item_code")
dt.FBS[,(deleteColList) := NULL]

# #get rid of all rows other than those with domestic food supply (kcal/capita/day) and food (1000 tonnes)
# deleteRowList <- c(
#   "Domestic supply quantity",
#   "Feed",
#   "Seed",
#   "Waste",
#   "Processing",
#   "Other Util",
#   "Production",
#   "Import Quantity",
#   "Food supply quantity (kg/capita",
#   "Protein supply quantity",
#   "Fat supply quantity",
#   "Stock Variation",
#   "Export Quantity"
# )
#dt.FBS <- dt.FBS[!element %in% deleteRowList, ]

# keep only rows with elements food (5142) and Food supply quantity (kg/capita/yr) (645)
keepElements <- c(5142,645)
dt.FBS <- dt.FBS[element_code %in% keepElements, ]

# select the rows that have one of the alcohol codes in the IMPACT_code column
#dt.FBS.alcohol <- dt.FBS[IMPACT_code %in% alcohol_code,]
setkey(dt.FBS,IMPACT_code)
dt.FBS.alcohol <- dt.FBS[IMPACT_code %in% alcohol_code]

# now to calculate the change in consumption over time using the income elasticity

# load income from the SSPs

dt.SSP.GDP <- getNewestVersion("SSPGDPClean")
dt.IMPACT.GDP <- getNewestVersionIMPACT("GDPX0")

dQ <- (elas.Income * Q/income)*dI
