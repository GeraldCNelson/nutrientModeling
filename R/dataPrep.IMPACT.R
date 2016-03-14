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
#' the gdxrrw package is needed to run this. It is available at this url, not from CRAN.
#' @source \url{https://support.gams.com/gdxrrw:interfacing_gams_and_r}
library(gdxrrw)
source("R/dataPrep.setup.R")
igdx(gdxLib)
# get a dataframe that has the meta data for the gdx file
temp <- gdxInfo(gdxName = IMPACTgdx, dump=FALSE, returnList=FALSE, returnDF=TRUE)
#extract just the list of parameters

#' @param df.gdx.param
df.gdx.param <- temp$parameters
keepList <- c("name","text") #this removes index, dim, card, doms, and domnames
df.gdx.param <- df.gdx.param[,keepList]

removeOldVersionsIMPACT("IMPACTmetaData")
write.xlsx(df.gdx.param[,keepList],file = paste(iData,"/IMPACTMetadata.",Sys.Date(),".xlsx",sep=""))

#get all the possible IMPACT regions
#' @param dt.IMPACTregions
dt.IMPACTregions <- as.data.table(read.xlsx(IMPACT3regions))
setnames(dt.IMPACTregions, old=c("CTY","CTYName"), new=c("region_code.IMPACT3","regionName"))

processIMPACT3Data <- function(varName, catNames) {
  dt.temp <-
    as.data.table(rgdx.param(IMPACTgdx, varName, ts = TRUE, names = catNames))
  dt.temp[, year := paste("X", dt.temp$year, sep = "")]
  dt.temp <- dt.temp[year %in% keepYearList]
  dt.temp <-
    as.data.table(rapply(dt.temp, as.character, classes = "factor", how = "replace"))
  #setorder(dt.temp, scenario, IMPACT_code, region_code.IMPACT3, year)
  setorderv(dt.temp, cols = catNames)
  if (varName != "PWX0" ) {
    setkey(dt.temp, region_code.IMPACT3)
    dt.temp <-
      merge(dt.temp, dt.IMPACTregions, by = "region_code.IMPACT3", all = TRUE)
    dt.temp[region_code.IMPACT3  == "SDN", region_code.IMPACT3 := "SDP"]
  }
  #fix the Sudan problem, at least temporarily
  removeOldVersionsIMPACT(varName)
  saveRDS(dt.temp,
          paste(IMPACTDataClean, "/",varName,".", Sys.Date(),".rds", sep = "")
  )
  return(dt.temp)
}

#' @param landVars - scenario, region_code.IMPACT3, landUse,IMPACT_code,year, value
vars.land <-
  c("AREACTYX0", "YLDCTYX0", "ANMLNUMCTYX0")
catNames.land <- c("scenario","IMPACT_code","region_code.IMPACT3","landUse","year","value")
dtlist.land <- lapply(vars.land,processIMPACT3Data,catNames = catNames.land)

for (i in 1:length(dtlist.land)) {
  temp <- dtlist.land[[i]]
  varName <- vars.land[i]
assign(paste("dt", varName, sep = "."), temp)
}

#' @param commodVars - scenario, region_code.IMPACT3, IMPACT_code,year, value
vars.commods <-
  c("PCX0", "AREACTYX0", "YLDCTYX0",
    "ANMLNUMCTYX0", "QSX0", "QSUPX0", "QDX0", "QFX0", "QBFX0",
    "QLX0", "QINTX0", "QOTHRX0", "QEX0", "QMX0","PerCapKCAL_com","FoodAvailability")
catNames.commod <- c("scenario","IMPACT_code","region_code.IMPACT3","year","value")
dtlist.commods <- lapply(vars.commods,processIMPACT3Data,catNames = catNames.commod)

for (i in 1:length(dtlist.commods)) {
  temp <- dtlist.commods[[i]]
  varName <- vars.commods[i]
  assign(paste("dt", varName, sep = "."), temp)
}

#' @param regionVars
vars.region <-
  c("GDPX0", "pcGDPX0",  "TotalMalnourished",
    "PerCapKCAL", "PopX0", "ShareAtRisk", "PopulationAtRisk")
catNames.region <- c("scenario","region_code.IMPACT3","year","value")
dtlist.region <- lapply(vars.region,processIMPACT3Data,catNames = catNames.region)

for (i in 1:length(dtlist.region)) {
  temp <- dtlist.region[[i]]
  varName <- vars.region[i]
  assign(paste("dt", varName, sep = "."), temp)
}

#' @param worldVars
vars.world <- "PWX0"
catNames.world <- c("scenario","IMPACT_code","year","value")
dtlist.world <- lapply(vars.world,processIMPACT3Data,catNames = catNames.world)

for (i in 1:length(dtlist.world)) {
  temp <- dtlist.world[[i]]
  varName <- vars.world[i]
  assign(paste("dt", varName, sep = "."), temp)
}

#' @param dt.CSEs
dt.CSEs <- as.data.table(
  read.xlsx(CSEs,cols=c(1:3)))
setnames(dt.CSEs, old=c("CTY","C","CSE"), new=c("region_code.IMPACT3","IMPACT_code","CSE"))
set(dt.CSEs, which(is.na(dt.CSEs[["CSE"]])), "CSE", 0)
setorder(dt.CSEs, region_code.IMPACT3, IMPACT_code)
saveRDS(dt.CSEs,paste(IMPACTDataClean,"/CSEs.",Sys.Date(),".rds",sep=""))

#create separate data table just for food items
#' @param dt.PCX0.food
dt.PCX0.food <- dt.PCX0[IMPACT_code %in% IMPACTfoodCommodList]
#' @param dt.PWX0.food
dt.PWX0.food <- dt.PWX0[IMPACT_code %in% IMPACTfoodCommodList]
#' @param dt.CSEs.food
dt.CSEs.food <- dt.CSEs[IMPACT_code %in% IMPACTfoodCommodList]

#combine all relevant data tables for analysis

setkey(dt.FoodAvailability, "scenario", "region_code.IMPACT3", "IMPACT_code")
setkey(dt.PWX0.food, "scenario", "IMPACT_code")
setkey(dt.CSEs.food, "region_code.IMPACT3", "IMPACT_code")
setkey(dt.PCX0.food, "scenario", "region_code.IMPACT3", "IMPACT_code")
setkey(dt.pcGDPX0, "scenario", "region_code.IMPACT3")

#' @param dt.IMPACTfood
# dt.IMPACTfood <-
#
# setorder(dt.IMPACTfood, scenario, region, IMPACT_code, year)
# setkey(dt.IMPACTfood, "scenario", "region_code.IMPACT3", "IMPACT_code")
#
# saveRDS(dt.IMPACTfood, file = paste(IMPACTDataClean,"/IMPACTfood",Sys.Date(),".rds",sep=""))
#devtools::document()
unload(gdxrrw)
