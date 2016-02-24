#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, IMPACT data, gdx
# Intro -------------------------------------------------------------------

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

#' @description
#' read in the IMPACT data from a gdx file and prepare for analysis
#' the gdxrrw package for R is needed to run this. It is available at this url, not from CRAN.
#' @source \url{https://support.gams.com/gdxrrw:interfacing_gams_and_r}

source("R/dataPrep.setup.R")
igdx(gdxLib)
# get a dataframe that has the meta data for the gdx file
temp <- gdxInfo(gdxName = IMPACTgdx, dump=FALSE, returnList=F, returnDF=TRUE)
#extract just the list of parameters

#' @param df.gdx.param
df.gdx.param <- temp$parameters
keepList <- c("name","text") #this removes index, dim, card, doms, and domnames
df.gdx.param <- df.gdx.param[,keepList]
saveRDS(df.gdx.param, file = paste(IMPACTDataClean,"/IMPACTmetaData.",Sys.Date(),".rds",sep=""))
write.xlsx(df.gdx.param, file = paste(IMPACTDataClean,"/IMPACTmetaData.",Sys.Date(),".xlsx",sep=""))

regions.all <- getNewestVersion("regions.all")

#get all the possible IMPACT regions
#' @param dt.IMPACTregions
dt.IMPACTregions <- as.data.table(regions.all[,c("region_code.IMPACT3","region_name.IMPACT3")])
#setnames(dt.IMPACTregions, old=c("CTY","CTYName"), new=c("region","regionName"))
setkey(dt.IMPACTregions)
dt.IMPACTregions <- unique(dt.IMPACTregions)

# landUse variables have data for model, scenario, region, year, commodity, water source, and value 
#' @param landVars
landVars <-c("AREACTYX0", "YLDCTYX0","ANMLNUMCTYX0")

# Commodity variables have data for model, scenario, region, year, commodity, and value 
#' @param commodVars
commodVars <-
  c("PCX0", "QSX0", "QSUPX0", "QDX0", "QFX0", "QBFX0",
    "QLX0", "QINTX0", "QOTHRX0", "QEX0", "QMX0","PerCapKCAL_com","FoodAvailability")

# Region variables have data for model, scenario, region, year, and value
#' @param regionVars
regionVars <-
  c(
    "GDPX0", "pcGDPX0",  "TotalMalnourished",
    "PerCapKCAL", "PopX0", "ShareAtRisk", "PopulationAtRisk")

#' @param worldVars - have data for model, scenario, year, and value
worldVars <- "PWX0"

#deleteRowList <- c("X2005", "X2006", "X2007", "X2008", "X2009")
#use keepYearList instead

# read in land use variables that distinguish whether irrigated or not ----
catNames <- c("scenario","IMPACT_code","region_code.IMPACT3","irrig","year","value")
for (i in 1: length(landVars)) {
  tempName <- landVars[i]
  dt.temp <- as.data.table(rgdx.param(IMPACTgdx, tempName,ts=T, names = catNames))
  #change SDN (Sudan) to SDP (Sudan plus) in the region data from the gdx
  dt.temp[region_code.IMPACT3 == "SDN", region_code.IMPACT3 := "SDP"]
  #change SSD (South Sudan) to SDP (Sudan plus) in the region data from the gdx
  dt.temp[region_code.IMPACT3 == "SSD", region_code.IMPACT3 := "SDP"]
  dt.temp[, year := paste("X", dt.temp$year, sep = "")]
  dt.temp <- dt.temp[year %in% keepYearList]
  dt.temp <- as.data.table(rapply(dt.temp, as.character, classes="factor", how="replace"))
  setorder(dt.temp, scenario, IMPACT_code, region_code.IMPACT3, irrig, year,value)
  # setnames(dt.temp, old=c("value"), new=tempName)
  setkey(dt.temp,scenario, IMPACT_code,region_code.IMPACT3,irrig, year,value)
  setkey(dt.IMPACTregions)
  dt.temp <- merge(dt.temp,dt.IMPACTregions, all=TRUE)
  setcolorder(dt.temp, c("scenario", "region_code.IMPACT3","region_name.IMPACT3","IMPACT_code","irrig", "year","value"))
  setorder(dt.temp, scenario,region_code.IMPACT3,region_name.IMPACT3,
           IMPACT_code,year, na.last=FALSE)
  saveRDS(dt.temp,paste(IMPACTDataClean,"/",tempName,".",Sys.Date(),".rds",sep=""))
  assign(paste("dt",tempName,sep="."),dt.temp)
}

# read in commodVars ----
catNames <- c("scenario","IMPACT_code","region_code.IMPACT3","year","value")
for (i in 1: length(commodVars)) {
  tempName <- commodVars[i]
  dt.temp <- as.data.table(rgdx.param(IMPACTgdx, tempName,ts=T, names = catNames))
  #change SDN (Sudan) to SDP (Sudan plus) in the region data from the gdx
  dt.temp[region_code.IMPACT3 == "SDN", region_code.IMPACT3 := "SDP"]
  #change SSD (South Sudan) to SDP (Sudan plus) in the region data from the gdx
  dt.temp[region_code.IMPACT3 == "SSD", region_code.IMPACT3 := "SDP"]
  dt.temp[, year := paste("X", dt.temp$year, sep = "")]
  dt.temp <- dt.temp[year %in% keepYearList]
  dt.temp <- as.data.table(rapply(dt.temp, as.character, classes="factor", how="replace"))
  setorder(dt.temp, scenario, IMPACT_code, region_code.IMPACT3, year,value)
  setkey(dt.temp,scenario, IMPACT_code,region_code.IMPACT3,year,value)
  setkey(dt.IMPACTregions)
  dt.temp <- merge(dt.temp,dt.IMPACTregions, all=TRUE)
  setcolorder(dt.temp, c("scenario", "region_code.IMPACT3","region_name.IMPACT3","IMPACT_code", "year","value"))
  setorder(dt.temp, scenario,region_code.IMPACT3,region_name.IMPACT3,
           IMPACT_code,year, na.last=FALSE)
  saveRDS(dt.temp,paste(IMPACTDataClean,"/",tempName,".",Sys.Date(),".rds",sep=""))
  assign(paste("dt",tempName,sep="."),dt.temp)
}

# read in regionVars ----
catNames <- c("scenario","region_code.IMPACT3","year","value")
for (i in 1: length(regionVars)) {
  tempName <- regionVars[i]
  dt.temp <- as.data.table(rgdx.param(IMPACTgdx, tempName,ts=T, names = catNames))
  #change SDN (Sudan) to SDP (Sudan plus) in the region data from the gdx
  dt.temp[region_code.IMPACT3 == "SDN", region_code.IMPACT3 := "SDP"]
  #change SSD (South Sudan) to SDP (Sudan plus) in the region data from the gdx
  dt.temp[region_code.IMPACT3 == "SSD", region_code.IMPACT3 := "SDP"]
  dt.temp[, year := paste("X", dt.temp$year, sep = "")]
  dt.temp <- dt.temp[year %in% keepYearList]
  dt.temp <- as.data.table(rapply(dt.temp, as.character, classes="factor", how="replace"))
  setkey(dt.temp,scenario, region_code.IMPACT3,year,value)
  setkey(dt.IMPACTregions)
  dt.temp <- merge(dt.temp,dt.IMPACTregions, all=TRUE)
  setcolorder(dt.temp, c("scenario", "region_code.IMPACT3","region_name.IMPACT3", "year","value"))
  setorder(dt.temp, scenario,region_code.IMPACT3,region_name.IMPACT3,year, na.last=FALSE)
  saveRDS(dt.temp,paste(IMPACTDataClean,"/",tempName,".",Sys.Date(),".rds",sep=""))
  assign(paste("dt",tempName,sep="."),dt.temp)
}

# read in worldVars ----
catNames <- c("scenario","IMPACT_code","year","value")
for (i in 1: length(worldVars)) {
  tempName <- worldVars[i]
  dt.temp <- as.data.table(rgdx.param(IMPACTgdx, tempName,ts=T, names = catNames))
  dt.temp[, year := paste("X", dt.temp$year, sep = "")]
  dt.temp <- dt.temp[year %in% keepYearList]
  dt.temp <- as.data.table(rapply(dt.temp, as.character, classes="factor", how="replace"))
  setorder(dt.temp, scenario, IMPACT_code, year)
  setkey(dt.temp,scenario,year,value)
  setkey(dt.IMPACTregions)
  setcolorder(dt.temp, c("scenario","IMPACT_code", "year","value"))
  setorder(dt.temp, scenario,IMPACT_code,year, na.last=FALSE)
  saveRDS(dt.temp,paste(IMPACTDataClean,"/",tempName,".",Sys.Date(),".rds",sep=""))
  assign(paste("dt",tempName,sep="."),dt.temp)
}

# read in CSE data ----
#' @param dt.CSEs
dt.CSEs <- as.data.table(
  read.xlsx(CSEs,cols=c(1:3)))
setnames(dt.CSEs, old=c("CTY","C","CSE"), new=c("region_code.IMPACT3","IMPACT_code","value"))
setorder(dt.CSEs, region_code.IMPACT3, IMPACT_code)
saveRDS(dt.CSEs,paste(IMPACTDataClean,"/CSEs.",Sys.Date(),".rds",sep=""))

#create separate data table just for food items
#' @param dt.PCX0.food
dt.PCX0.food <- dt.PCX0[IMPACT_code %in% IMPACTfoodCommodList]
#' @param dt.PWX0.food
dt.PWX0.food <- dt.PWX0[IMPACT_code %in% IMPACTfoodCommodList]
#' @param dt.CSEs.food
dt.CSEs.food <- dt.CSEs[IMPACT_code %in% IMPACTfoodCommodList]

# #combine all relevant data tables for analysis
# 
# dtList <- list(dt.FoodAvailability, dt.pcGDPX0, dt.PCX0.food, dt.PWX0.food, dt.CSEs.food)
# setkey(dt.FoodAvailability, "scenario", "region_code.IMPACT3", "IMPACT_code","year")
# setkey(dt.PWX0.food, "scenario", "IMPACT_code","year")
# setkey(dt.CSEs.food, "region_code.IMPACT3", "IMPACT_code")
# setkey(dt.PCX0.food, "scenario", "region_code.IMPACT3", "IMPACT_code","year")
# setkey(dt.pcGDPX0, "scenario", "region_code.IMPACT3","year")
# saveRDS(dt.IMPACTfood, file = paste(IMPACTDataClean,"/IMPACTfood.",Sys.Date(),".rds",sep=""))
