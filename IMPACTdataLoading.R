# Intro -------------------------------------------------------------------
#This script reads in the IMPACT data. Does a bit of cleanup and prepares a data table called dt.t1.
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

source("setup.R")

#sorted list of IMPACT food commodities is in IMPACTfoodCommodList, defined in setup.R

# IMPACTfoodCommodities <- as.data.frame(IMPACTfoodCommodList,stringsAsFactors = FALSE)
# colnames(IMPACTfoodCommodities) <- "IMPACT_code"

daysInYear <- 365 #see http://stackoverflow.com/questions/9465817/count-days-per-year for a way to deal with leap years
## IMPACT commodity file readin and cleanup -------------------------------
t1 <-read.csv(IMPACTfileName, stringsAsFactors = FALSE, 
              col.names = c("IMPACTparameter", "scenario", "IMPACT_code" , "region","productiontype", 
                            "year", "value"),
              colClasses = c(rep("character",6),"numeric"))
dt.t1 <- as.data.table(t1)
dt.t1[,year:= paste("X", dt.t1$year,sep="")]
#get rid of years 2005 to 2009
setkey(dt.t1,year)
removeList <- c("X2005","X2006","X2007","X2008","X2009")
dt.t1 <- dt.t1[!year %in% removeList]
#t1 <- t1[!t1$year %in% 2005:2009,]
#t1$year <-  as.character(t1$year)
#drop productiontype because it is not used in the demand analysis
dt.t1[,productiontype := NULL]
#removeListCols <- c("productiontype")
#t1 <- t1[, !(names(t1) %in% removeListCols)]

# manipulations that only need to be done once

# Parameter,Description,csv IMPACT parameters,my short name
# FoodAvailability,Food availability per capita (kg per person per year),FoodAvailXAgg,pcFoodAvail
# GDPX0,Final GDP (billion 2005 USD ppp), NA
# pcGDPX0,Final per capita GDP (000 USD per person ppp),pcGDPXAgg -- Per Capita Income,pcGDP
# PCX0,Solution consumer prices (2005 USD ppp per mt)        ,PCXAgg -- Consumer Prices,Pc
# PerCapKCal,per capita calories available (KCal per person per day),PerCapKCalXAgg,pcKcal
# PopulationAtRisk,Number of people at risk of hunger (million),PopulationAtRiskXagg - Pop at Risk of Hunger,popAtRisk
# POPX0,Final Population (million),PopXAgg -- Population,pop
# PWX0,Solution world prices (2005 USD ppp per mt),PWXAgg -- World Prices,Pw
# QBFX0,Solution biofuel feedstock demand (000 mt),QBFXAgg -- Biofuel Feedstock Demand,bioFuelD
# QDX0,Solution total demand for commodity  (000 mt),QDXAgg -- Total Demand,QdTot
# QFX0,Solution household demand aggregated to cty (000 mt),QFXAgg -- Household Demand,QfoodD
# QINTX0,Solution intermediate demand for commodity (000 mt),QINTXAgg -- Intermediate Demand,intermedD
# QLX0,Solution livestock feed demand (000 mt),QLXAgg -- Livestock Feed Demand,QfeedD
# QOTHRX0,Solution Other demand (000 mt), NA
# ShareAtRisk,Share of population at risk of hunger (%),ShareAtRiskXagg -- Share at Risk of Hunger,shareAtRisk
# TotalMalnourished,Number of malnourished children (millions) ,TotalMalnourishedXagg -- Malnurished Children,totMalnourished

#Note to self: still need to aggregate to new regions for IMPACT
#store per cap GDP 
dt.t1.pcGDP <- dt.t1[IMPACTparameter == "pcGDPXAgg -- Per Capita Income"]

#keep only food items
dt.t1.food <- dt.t1[IMPACT_code %in% IMPACTfoodCommodList]
# t1.food <- t1[t1$IMPACT_code %in% IMPACTfoodCommodities$IMPACT_code,
#               c("IMPACTparameter","IMPACT_code","scenario","region","year","value")]

removeList <- c("PopXAgg -- Population","pcGDPXAgg -- Per Capita Income",
                "PopulationAtRiskXagg - Pop at Risk of Hunger", "ShareAtRiskXagg -- Share at Risk of Hunger",
                "TotalMalnourishedXagg -- Malnurished Children","totMalnourished",
                "QBFXAgg -- Biofuel Feedstock Demand","QINTXAgg -- Intermediate Demand")
dt.t1.food[!IMPACTparameter %in% removeList]

#give variables better names
orgNames <- c("QDXAgg -- Total Demand", "QLXAgg -- Livestock Feed Demand", "QFXAgg -- Household Demand",
  "PWXAgg -- World Prices", "FoodAvailXAgg","PerCapKCalCXAgg -- PcKcal by Commodity", 
  "QBFXAgg -- Biofuel Feedstock Demand", "QINTXAgg -- Intermediate Demand", 
  "PCXAgg -- Consumer Prices")
newNames <- c("QdTot", "QfeedD", "QfoodD", "Pw", "pcFoodAvail", "pcKcal", "bioFuelD", "intermedD", "Pc")
for (i in 1:length(orgNames)) {
  dt.t1.food[IMPACTparameter == orgNames[i], IMPACTparameter := newNames[i]]
}
  

 # #give variables better names; commented out variables are only in t1. This has been replaced by the code just above.
 # #t1.food$IMPACTparameter <- gsub("PopXAgg -- Population","pop",t1.food$IMPACTparameter)
 # t1.food$IMPACTparameter <- gsub("QDXAgg -- Total Demand","QdTot",t1.food$IMPACTparameter)
 # t1.food$IMPACTparameter <- gsub("QLXAgg -- Livestock Feed Demand","QfeedD",t1.food$IMPACTparameter)
 # t1.food$IMPACTparameter <- gsub("pcGDPXAgg -- Per Capita Income","pcGDP",t1.food$IMPACTparameter)
 # t1.food$IMPACTparameter <- gsub("QFXAgg -- Household Demand","QfoodD",t1.food$IMPACTparameter)
 # t1.food$IMPACTparameter <- gsub("PWXAgg -- World Prices","Pw",t1.food$IMPACTparameter)
 # t1.food$IMPACTparameter <- gsub("FoodAvailXAgg","pcFoodAvail",t1.food$IMPACTparameter)
 # t1.food$IMPACTparameter <- gsub("PerCapKCalCXAgg -- PcKcal by Commodity","pcKcal",t1.food$IMPACTparameter)
 # #t1.food$IMPACTparameter <- gsub("PopulationAtRiskXagg - Pop at Risk of Hunger","popAtRisk",t1.food$IMPACTparameter)
 # #t1.food$IMPACTparameter <- gsub("ShareAtRiskXagg -- Share at Risk of Hunger","shareAtRisk",t1.food$IMPACTparameter)
 # #t1.food$IMPACTparameter <- gsub("TotalMalnourishedXagg -- Malnurished Children","totMalnourished",t1.food$IMPACTparameter)
 # t1.food$IMPACTparameter <- gsub("QBFXAgg -- Biofuel Feedstock Demand","bioFuelD",t1.food$IMPACTparameter)
 # t1.food$IMPACTparameter <- gsub("QINTXAgg -- Intermediate Demand","intermedD",t1.food$IMPACTparameter)
 # t1.food$IMPACTparameter <- gsub("PCXAgg -- Consumer Prices","Pc",t1.food$IMPACTparameter)

# Pw <- ddply(t1.food[t1.food$IMPACTparameter == "Pw",],
#             .(scenario,IMPACT_code,year),
#             summarise,
#             Pw=mean(value))

setkey(dt.t1.food,IMPACTparameter,region)
dt.Pw <- dt.t1.food[IMPACTparameter %in% "Pw"]
setorder(dt.Pw,scenario, IMPACT_code,year)
dt.Pw[ ,`:=`(region = NULL, IMPACTparameter = NULL)]
setnames(dt.Pw,"value","Pw")

dt.Pc <- dt.t1.food[IMPACTparameter  %in% "Pc"]
setorder(dt.Pc,scenario, region, IMPACT_code, year)
dt.Pc[ ,`:=`(IMPACTparameter = NULL)]
setnames(dt.Pc,"value","Pc")

dt.pcFoodAvail <- dt.t1.food[IMPACTparameter  %in% "pcFoodAvail"]
setorder(dt.pcFoodAvail,scenario, region, IMPACT_code, year)
dt.pcFoodAvail[ ,`:=`(IMPACTparameter = NULL)]
setnames(dt.pcFoodAvail,"value","food")

# setkey(dt.pcFoodAvail,"scenario","region","IMPACT_code", "year")
# #this adds a column for food that is identical to value. Now done above
# dt.pcFoodAvail[,food:=mean(value),by=key(dt.pcFoodAvail)]

# #why do we want to do this? It drops IMPACTparameter and value
# dt.df0 <- unique(dt.pcFoodAvail[,c(key(dt.pcFoodAvail),"food"),with=F])
dt.CSEs <- as.data.table(CSEs)
setorder(dt.CSEs,"scenario", "region","IMPACT_code","CSE")
setkey(dt.CSEs,"region","IMPACT_code","CSE")

dtList <- list(dt.pcFoodAvail,dt.Pw,dt.Pc, dt.CSEs)
setkey(dt.pcFoodAvail,"scenario","region","IMPACT_code")
setkey(dt.Pw,"scenario","IMPACT_code")
setkey(dt.Pc,"scenario","region","IMPACT_code")

dt.df1 <- join_all(dtList)
dt.df1 <- dt.df0[dt.CSEs[dt.Pc[dt.Pw]]]

temp <- as.data.frame(Pc)

 # Pc <- ddply(t1.food[t1.food$IMPACTparameter == "Pc",],
 #             .(scenario,region,IMPACT_code,year),
 #             summarise,
 #             Pc=mean(value))

#an alternative to the above commented out code; replaced by f.price
# dt.Pc <- as.data.table(t1.food[t1.food$IMPACTparameter == "Pc",])
# setkey(dt.Pc,"scenario","region","IMPACT_code","year")
# dt.Pc[,Pc:=mean(value),by=key(dt.Pc)]
# Pc <- as.data.frame(unique(dt.Pc[,c("scenario","region","IMPACT_code","year","Pc"),with=F]))
# 
#input to this function is "Pw" or "Pc"
f.price <- function(price) {
  dt.Price <- as.data.table(t1.food[t1.food$IMPACTparameter == price,])
  setkey(dt.Price,"scenario","region","IMPACT_code","year")
  dt.Price[,eval(price):=mean(value),by=key(dt.Price)]
  Price <- as.data.frame(unique(dt.Price[,c("scenario","region","IMPACT_code","year",price),with=F]))
}
Pw <- f.price("Pw") # this leaves a region column in Pw
Pc <- f.price("Pc")

# 
# # old slow code
# # df0 <- ddply(t1.food[t1.food$IMPACTparameter == "pcFoodAvail",],
# #              .(scenario,region,IMPACT_code,year),
# #              summarise,
# #              food=mean(value))
# 
# # new fast code
f.df0 <- function(dfIn) {
  dt.tmp <- data.table(dfIn)
  setkey(dt.tmp,"scenario","region","IMPACT_code", "year")
  dt.tmp[,food:=mean(value),by=key(dt.tmp)]
  as.data.frame(unique(dt.tmp[,c(key(dt.tmp),"food"),with=F]))
}  
df0 <- f.df0(dt.pcFoodAvail)

# df1 <- join(df0,Pw)
# df2 <- join(df1,Pc)

dfList <- list(df0,Pw,Pc, CSEs)
df1 <- join_all(dfList)


# df3 <- join(df2,CSEs)
df1[is.na(df1)] <- 0


