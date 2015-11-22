# Intro -------------------------------------------------------------------
#This script sets up a bunch of variables for use in all the other scripts. It should be run first.

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
require(ggplot2)
setwd("~/Documents/workspace/nutrientModeling")

# file names ---------------------
EARFileName <- "data/DRI_IOM_V2.xlsx"
# CSE - consumer support equivalent
#Note: the price a consumer pays is Pc * (1-CSE)
CSEFileName <- "data/IMPACTData/CSEs20150824.xlsx" 
nutrientFileName <- "data/USDA GFS IMPACT V9.xlsx"
commodityFoodGroupLookupFileName <- "data/food commodity to food group table V1.xlsx"
IMPACTregionsFileName <- "data/IMPACTRegionsJan2015.xlsx"
SSPdataZipFileLocation <- c("data/SSPData/SspDb_country_data_2013-06-12.csv.zip")
SSPdataZipFileName <- c("SspDb_country_data_2013-06-12.csv") #the name of the file inside the zip
IMPACTfileName <- "data/IMPACTData/Demand Results20150817.csv"
#IMPACTregionsFileName <- "data/IMPACTRegionsMay2015.csv" # this file includes Denmark plus (DNP) and Sudan plus (SDP) and removes Greenland and South Sudan
IMPACTregionsFileName <- "data/IMPACTRegionsJan15tmp.csv" # this file removes Denmark plus (DNP) and South Sudan (SSD) as well as removes Greenland and South Sudan
IMPACTregions <- read.csv(IMPACTregionsFileName, stringsAsFactors = FALSE)
ctyNames  <- IMPACTregions$CTY
fishInfoIMPACT <- "data/Fish Elasticities and Quantities IMPACT.xlsx"

# Other info --------
userName <- "Gerald Nelson"

IMPACTfoodCommodList <- sort(c("cbeef","cpork","clamb","cpoul","ceggs","cmilk","cbarl","cmaiz",
                               "cmill","crice","csorg","cwhea","cocer","ccass","cpota","cswpt","cyams","corat","cbean","cchkp",
                               "ccowp","clent","cpigp","copul","cbana","cplnt","csubf","ctemf","cvege","csugr","cgrnd","cgdol",
                               "crpsd","crpol","csoyb","csbol","csnfl","csfol","cplol","cpkol","ctols","ctool","ccoco","ccafe",
                               "cteas","cothr"))

#list of years to keep
keepYearList <- c("X2010","X2015","X2020","X2025","X2030","X2035","X2040","X2045","X2050")
source("workBookFunctions.R")
