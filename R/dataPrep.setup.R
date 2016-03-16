#' dataPrep.setup.R A script to set up needed variables for preparation of raw data.
#' It should be run first.
#' It is called with a 'source' command from other
#' scripts to ensure this. And it only works properly when Sourced

#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities
# Intro -------------------------------------------------------------------
#' @description

#Copyright (C) 2015 Gerald C. Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify it
#     under the terms of the GNU General Public License as published by the Free
#     Software Foundation, either version 3 of the License, or (at your option)
#     any later version.
#
#     This program is distributed in the hope that it will be useful, but
#     WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#     or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#     for more details at http://www.gnu.org/licenses/.

source("R/setup.global.R")

# create metadata file -----
metadata <- data.frame(file_name_location = character(1), file_description = character(1), stringsAsFactors = FALSE)
# file locations ---------------------
#' @param FBSData
FBSData <- paste(rData, "FBSData", sep = "/")

#' @param resultsDir
resultsDir <- "results"

#' @param SSPdata
SSPdata <- paste(rData, "SSPData", sep = "/")

#' @param IMPACTData - where the raw IMPACT data are kept
IMPACTData <- paste(rData, "IMPACTData", sep = "/")

#' @param IMPACTDataClean - where the cleaned up IMPACT data are kept
IMPACTDataClean <- paste(mData, "IMPACTData", sep = "/")

#' @param NutrientData
NutrientData <- paste(rData, "NutrientData", sep = "/")

# file names ---------------------
#' File names are assigned to variables.
#' @param EARFileName
EARFileName <- "DRI_IOM_V2.xlsx"

#' @param EARs
EARs <- paste(NutrientData, EARFileName, sep = "/")
metadata[(nrow(metadata) + 1), ] <-  c(EARs,"data on nutrient requirements")

# IMPACT data -----
# CSE - consumer support equivalent
#Note: the price a consumer pays is Pc * (1-CSE)

#' @param CSEFileName
CSEFileName <- "CSEs20150824.xlsx"

#' @param CSEs
CSEs <- paste(IMPACTData, CSEFileName, sep = "/")
metadata[(nrow(metadata) + 1), ] <-  c( CSEs,"Consumer Surplus Equivalents for IMPACT commodities")

#' @param IMPACT3regionsFileName
IMPACT3regionsFileName <- "IMPACTRegionsFeb2016.xlsx" # this file includes Denmark plus (DNP) and Sudan plus (SDP)
#' IMPACT3regionsFileName <- "IMPACTRegionsMay2015.csv" # this file includes Denmark plus (DNP) and Sudan plus (SDP) and removes Greenland and South Sudan
#' #IMPACT3regionsFileName <- "IMPACTRegionsJan15tmp.csv" # this file removes Denmark plus (DNP) and South Sudan (SSD) as well as removes Greenland and South Sudan

#' @param IMPACT3regions
IMPACT3regions <- paste(IMPACTData, IMPACT3regionsFileName, sep = "/")
metadata[(nrow(metadata) + 1), ] <-  c(IMPACT3regions,"List of IMPACT regions; single countries and country aggregates")

#' @param IMPACTcsvfileName
IMPACTcsvfileName <- "Demand Results20150817.csv"

#' @param IMPACTcsv
IMPACTcsv <- paste(IMPACTData, IMPACTcsvfileName, sep = "/")
metadata[(nrow(metadata) + 1), ] <-  c(IMPACTcsv,"IMPACT demand data in csv form")

#' @param IMPACTstdRegionsFileName
IMPACTstdRegionsFileName <- "IMPACT-agg-regionsFeb2016.xlsx"
#' @param IMPACTstdRegions
IMPACTstdRegions <- paste(IMPACTData, IMPACTstdRegionsFileName, sep = "/")

#' @param IMPACTgdxfileName
IMPACTgdxfileName <- "Demand Results20150817.gdx"

#' @param IMPACTgdx
IMPACTgdx <- paste(IMPACTData, IMPACTgdxfileName, sep = "/")
metadata[(nrow(metadata) + 1), ] <-  c(IMPACTgdx,"IMPACT demand data in gdx form")

#' @param gdxLib
gdxLib <- "/Applications/GAMS/gams24.5_osx_x64_64_sfx"
metadata[(nrow(metadata) + 1), ] <-  c(gdxLib,"Location and name of gdx libraries; needed for the gdx data import process")

#' @param IMPACTfishFile
IMPACTfishInfo <- "Fish Elasticities and Quantities IMPACT.xlsx"

#' @param IMPACTfish
IMPACTfish <- paste(IMPACTData, IMPACTfishInfo, sep = "/")
metadata[(nrow(metadata) + 1), ] <-  c(IMPACTfish,"data on fish from the IMPACT fish model")

# nutrient data ------
#' @param nutrientFileName
nutrientFileName <- "USDA GFS IMPACT V15.xlsx"

#' @param nutrientLU
nutrientLU <- paste(NutrientData, nutrientFileName, sep = "/")
metadata[(nrow(metadata) + 1), ] <-  c( nutrientLU,"nutrient lookup data for IMPACT commodities")

#' @param commodityFoodGroupLookupFileName
commodityFoodGroupLookupFileName <-
  "food commodity to food group table V2.xlsx"

#' @param foodGroupLU
foodGroupLU <- paste(NutrientData, commodityFoodGroupLookupFileName, sep = "/")
metadata[(nrow(metadata) + 1), ] <-  c(foodGroupLU,"commodity to food group lookup table")

# SSP information ----

#' @param SSPdataZipFile
SSPdataZipFile <- "SspDb_country_data_2013-06-12.csv.zip"

#' @param SSPdataZip
SSPdataZip <- paste(SSPdata, SSPdataZipFile, sep = "/")
metadata[(nrow(metadata) + 1), ] <-  c(SSPdataZip,"zip file containing the SSP data")

#' @param SSPdataFile
temp <-
  unzip(SSPdataZip, list = TRUE) #get the name of the file inside the zip. Assumes only 1
SSPcsv <- temp$Name[1]

#' @param modelListPop - list of models (currently only one) for the population data
#'  are included in the cleaned up SSP population data file
modelListPop <- "IIASA-WiC POP"
metadata[(nrow(metadata) + 1), ] <-  c(modelListPop,"List of SSP models to extract population info from")

#' @param modelListGDP - list of models (currently only one) for the GDP data
#'  included in the cleaned up SSP GDP data file
modelListGDP <- "OECD Env-Growth"
metadata[(nrow(metadata) + 1), ] <-  c(modelListGDP,"List of SSP models to extract population info from")

#' @param scenarioList - list of scenarios in the SSP data
scenarioList <-
  c("SSP1_v9_130325",
    "SSP2_v9_130325",
    "SSP3_v9_130325",
    "SSP4_v9_130325",
    "SSP5_v9_130325"
  )
# Food Balance Sheet Information information ----
#' @source \url{http://faostat3.fao.org/download/FB/FBS/E}
#'
#' FBS to ISO lookup table
#' @source  \url{http://www.fao.org/countryprofiles/iso3list/en/}
FBSlookupTableLink <- "http://www.fao.org/countryprofiles/iso3list/en/"
#' @param FBSdataZipFile
FBSdataZipFile <- "FoodBalanceSheets_E_All_Data.zip"

#' @param FBSdataZip
FBSdataZip <- paste(FBSData, FBSdataZipFile, sep = "/")
list <- unzip(FBSdataZip,list = TRUE)
createDate <- as.character(list$Date[1])
metadata[(nrow(metadata) + 1), ] <-  c(FBSdataZip,"Zip file containing the FBS data")
metadata[(nrow(metadata) + 1), ] <-  c("FBS data creation date",createDate)
metadata[(nrow(metadata) + 1), ] <-  c("FBS lookup table", FBSlookupTableLink)

#' @param FBScsv
temp <-
  unzip(FBSdataZip, list = TRUE) #get the name of the file inside the zip. Assumes only 1
FBScsv <- temp$Name[1]

#' @param FBSCommodityInfoFileName - worksheet with the list of FBS food items by code, name, definition, and IMPACT commodity code
FBSCommodityInfoFileName <- "FBStoIMPACTlookupV3.xlsx"

#' @param FBSCommodityInfoFileName - worksheet with the list of FBS food items by code, name, definition, and IMPACT commodity code
FBSCommodityInfo <- paste(FBSData,FBSCommodityInfoFileName,sep="/")
metadata[(nrow(metadata) + 1), ] <-  c(FBSCommodityInfo,"File in the FBS zip file containing the FBS data")

#' @param FAOCountryNameCodeLookupFile
FAOCountryNameCodeLookupFile <- "FAOCountryNameCodeLookup.xlsx"

#' @param FAOCountryNameCodeLookup
FAOCountryNameCodeLookup <- paste(FBSData,FAOCountryNameCodeLookupFile,sep="/")
metadata[(nrow(metadata) + 1), ] <-  c(FAOCountryNameCodeLookup,"Lookup table for FAOSTAT and other country identification")

#' @param ISOctyCodes
ISOctyCodes <- "ISOCountrycodes.xlsx"

#' @param ISOCodes
ISOCodes <- paste(rData, ISOctyCodes, sep = "/")
metadata[(nrow(metadata) + 1), ] <-  c(ISOCodes,"List of all ISO 3 codes and the names of the countries they represent")

#' Other info --------
#' Username identifies the author of the current output in excel files
#' @param userName
userName <- "Gerald Nelson"

#These regions are reported as their individual member countries during the relevant
# time period (e.g. after 1999 for Belgium-Luxembourg). Their data entries are all NA.
# Although Ethiopia PDR doesn't have data, Ethiopia does.
#' @param FBSregionsToDrop
FBSregionsToDrop <- c("Belgium-Luxembourg","Czechoslovakia","Ethiopia PDR","Montenegro",
                      "Serbia","Serbia and Montenegro","Yugoslav SFR","Europe",
                      "Eastern Europe","Southern Europe",
                      "Western Europe", "European Union","USSR","World",
                      "Netherlands Antilles (former)","Caribbean")

# test the names
test <- "no"
if (test %in% "yes") {
  temp <- read.csv(IMPACTcsv,nrows=5)
  temp <- read.xlsx(EARs)
  temp <- read.xlsx(IMPACTfish)
  temp <- read.xlsx(IMPACT3regions)
  temp <- read.xlsx(CSEs)
  temp <- read.xlsx(ISOCodes)
  temp <- read.xlsx(nutrientLU)
  temp <- read.xlsx(EARs)
  igdx(gdxLib)
  temp <- gdxInfo(gdxName = IMPACTgdx, dump=FALSE, returnList=FALSE, returnDF=TRUE)
}

removeOldVersions("inputFileList")
removeOldVersions.xlsx("inputFileList")
write.xlsx(metadata,file=paste(mData,"/inputFileList.",Sys.Date(),".xlsx",sep=""),
           colWidths="auto")
saveRDS(metadata,file = paste(mData,"/inputFileList.",Sys.Date(),".rds",sep=""))
