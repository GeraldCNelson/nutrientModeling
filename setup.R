#' @title Setup code for the nutrient modelling exercise
#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities
# Intro -------------------------------------------------------------------
#' @description 
#' This script sets up a bunch of variables for use in all the other scripts. 
#' It should be run first. It is usually called with a 'source' command from other
#' scripts to ensure this.

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

#' Make sure required libraries are loaded
#' 
require(roxygen2)
require(openxlsx)
require(entropy)
require(reshape2)
require(plyr)
require(dplyr)
require(tidyr)
require(data.table)
require(splitstackshape)
require(plotrix)
require(ggplot2)
require(stringi)
library("devtools")
setwd("~/Documents/workspace/nutrientModeling")

# get rid of strings as factors, but reset on exit ----
old <- options(stringsAsFactors = FALSE)
on.exit(options(old), add = TRUE)

# file names ---------------------
#' This section is where the file names are assigned to variables. 
#' @param EARFileName
EARFileName <- "data/DRI_IOM_V2.xlsx"

# CSE - consumer support equivalent
#Note: the price a consumer pays is Pc * (1-CSE)
#' @param CSEFileName
CSEFileName <- "data/IMPACTData/CSEs20150824.xlsx" 
#' @param nutrientFileName
nutrientFileName <- "data/USDA GFS IMPACT V9.xlsx"

#' @param nutrientFileName
commodityFoodGroupLookupFileName <- "data/food commodity to food group table V1.xlsx"

#' @param nutrientFileName
IMPACTregionsFileName <- "data/IMPACTRegionsJan2015.xlsx"

#' @param nutrientFileName
SSPdataZipFileLocation <- c("data/SSPData/SspDb_country_data_2013-06-12.csv.zip")

#' @param nutrientFileName
SSPdataZipFileName <- c("SspDb_country_data_2013-06-12.csv") #the name of the file inside the zip

#' @param nutrientFileName
IMPACTfileName <- "data/IMPACTData/Demand Results20150817.csv"

#' #' @param IMPACTregionsFileName
#' IMPACTregionsFileName <- "data/IMPACTRegionsMay2015.csv" # this file includes Denmark plus (DNP) and Sudan plus (SDP) and removes Greenland and South Sudan
#' #IMPACTregionsFileName <- "data/IMPACTRegionsJan15tmp.csv" # this file removes Denmark plus (DNP) and South Sudan (SSD) as well as removes Greenland and South Sudan

IMPACT3Regions <- read.csv(IMPACTregionsFileName, stringsAsFactors = FALSE)
colnames(IMPACT3Regions) <- c("region_code","region_name")
ctyNames  <- IMPACT3Regions$region_code

#' @param fishInfoIMPACT
fishInfoIMPACT <- "data/Fish Elasticities and Quantities IMPACT.xlsx"

#' @param ISOctyCodes
ISOctyCodes <- "data/ISOCountrycodes.xlsx"

#' Other info --------
#' Username identifies the author of the current output in excel files
#' @param userName
userName <- "Gerald Nelson"

IMPACTfoodCommodList <- sort(c("cbeef","cpork","clamb","cpoul","ceggs","cmilk","cbarl","cmaiz",
                               "cmill","crice","csorg","cwhea","cocer","ccass","cpota","cswpt","cyams","corat","cbean","cchkp",
                               "ccowp","clent","cpigp","copul","cbana","cplnt","csubf","ctemf","cvege","csugr","cgrnd","cgdol",
                               "crpsd","crpol","csoyb","csbol","csnfl","csfol","cplol","cpkol","ctols","ctool","ccoco","ccafe",
                               "cteas","cothr"))

#' list of years to keep
#' @param keepYearList
keepYearList <- c("X2010","X2015","X2020","X2025","X2030","X2035","X2040","X2045","X2050")
#source("workBookFunctions.R")
