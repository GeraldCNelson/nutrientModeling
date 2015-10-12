# Intro -------------------------------------------------------------------
#This script reads in IMPACT data and the nutrient lookup table, does some manipulations of the data,
#and writes out results to an excel spreadsheet

#Copyright (C) 2015 Gerald C. Nelson, except where noted
#Important code contributions from Brendan Power.

#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details at http://www.gnu.org/licenses/.
library(openxlsx)
library(entropy)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(splitstackshape)
library(plotrix)

setwd("~/Documents/workspace/nutrientModeling")

# Info for the basic worksheet ---------------------------------------
userName <- "Gerald Nelson"
dataSource <- "URL here"
dateDownloaded <- "Date URL downloaded here"
dateCreated <- Sys.Date()

# File names, related info and locations -----------------------------------
# Load functions for the rest of the script
source(file="nutrientFunctions.R")
#Read in data
source(file ="IMPACTdataLoading.R")
source(file = "EARfoodGroupCSELoading.R") #currently (10/8) needs to be run before nutrientDataLoading
source(file = "nutrientDataLoading.R")

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

#keep only food items
t1.food <- t1[t1$IMPACT_code %in% IMPACTfoodCommodities$IMPACT_code,c("IMPACTparameter","IMPACT_code","scenario","region","year","value")]
#store per cap GDP 
t1.pcGDP <- t1[t1$IMPACTparameter == "pcGDPXAgg -- Per Capita Income",c("region","scenario","year","value")]

#give variables better names; commented out variables are only in t1.
#t1.food$IMPACTparameter <- gsub("PopXAgg -- Population","pop",t1.food$IMPACTparameter)
t1.food$IMPACTparameter <- gsub("QDXAgg -- Total Demand","QdTot",t1.food$IMPACTparameter)
t1.food$IMPACTparameter <- gsub("QLXAgg -- Livestock Feed Demand","QfeedD",t1.food$IMPACTparameter)
#t1.food$IMPACTparameter <- gsub("pcGDPXAgg -- Per Capita Income","pcGDP",t1.food$IMPACTparameter)
t1.food$IMPACTparameter <- gsub("QFXAgg -- Household Demand","QfoodD",t1.food$IMPACTparameter)
t1.food$IMPACTparameter <- gsub("PWXAgg -- World Prices","Pw",t1.food$IMPACTparameter)
t1.food$IMPACTparameter <- gsub("FoodAvailXAgg","pcFoodAvail",t1.food$IMPACTparameter)
t1.food$IMPACTparameter <- gsub("PerCapKCalCXAgg -- PcKcal by Commodity","pcKcal",t1.food$IMPACTparameter)
#t1.food$IMPACTparameter <- gsub("PopulationAtRiskXagg - Pop at Risk of Hunger","popAtRisk",t1.food$IMPACTparameter)
#t1.food$IMPACTparameter <- gsub("ShareAtRiskXagg -- Share at Risk of Hunger","shareAtRisk",t1.food$IMPACTparameter)
#t1.food$IMPACTparameter <- gsub("TotalMalnourishedXagg -- Malnurished Children","totMalnourished",t1.food$IMPACTparameter)
t1.food$IMPACTparameter <- gsub("QBFXAgg -- Biofuel Feedstock Demand","bioFuelD",t1.food$IMPACTparameter)
t1.food$IMPACTparameter <- gsub("QINTXAgg -- Intermediate Demand","intermedD",t1.food$IMPACTparameter)
t1.food$IMPACTparameter <- gsub("PCXAgg -- Consumer Prices","Pc",t1.food$IMPACTparameter)

Pw <- ddply(t1.food[t1.food$IMPACTparameter == "Pw",],
            .(scenario,IMPACT_code,year),
            summarise,
            Pw=mean(value))

# Pc <- ddply(t1.food[t1.food$IMPACTparameter == "Pc",],
#             .(scenario,region,IMPACT_code,year),
#             summarise,
#             Pc=mean(value))

#an alternative to the above commented out code; replaced by f.price
# dt.Pc <- as.data.table(t1.food[t1.food$IMPACTparameter == "Pc",])
# setkey(dt.Pc,"scenario","region","IMPACT_code","year")
# dt.Pc[,Pc:=mean(value),by=key(dt.Pc)]
# Pc <- as.data.frame(unique(dt.Pc[,c("scenario","region","IMPACT_code","year","Pc"),with=F]))

#input to this function is "Pw" or "Pc"
f.price <- function(price) {
  dt.Price <- as.data.table(t1.food[t1.food$IMPACTparameter == price,])
  setkey(dt.Price,"scenario","region","IMPACT_code","year")
  dt.Price[,eval(price):=mean(value),by=key(dt.Price)]
  Price <- as.data.frame(unique(dt.Price[,c("scenario","region","IMPACT_code","year",price),with=F]))
}
Pw <- f.price("Pw") # this leaves a region column in Pw
Pc <- f.price("Pc")

# old slow code
# df0 <- ddply(t1.food[t1.food$IMPACTparameter == "pcFoodAvail",],
#              .(scenario,region,IMPACT_code,year),
#              summarise,
#              food=mean(value))

# new fast code
dt.pcFoodAvail <- t1.food[t1.food$IMPACTparameter == "pcFoodAvail",]
f.df0 <- function(dfIn) {
  dttmp <- data.table(dfIn)
  setkey(dttmp,"scenario","region","IMPACT_code", "year")
  dttmp[,food:=mean(value),by=key(dttmp)]
  as.data.frame(unique(dttmp[,c(key(dttmp),"food"),with=F]))
}  
df0 <- f.df0(dt.pcFoodAvail)

# df1 <- join(df0,Pw)
# df2 <- join(df1,Pc)

dfList <- list(df0,Pw,Pc, CSEs)
df1 <- join_all(dfList)


# df3 <- join(df2,CSEs)
df1[is.na(df1)] <- 0

#old, slow
# budget <- ddply(df3,
#                 .(scenario,region,year),
#                 summarise,
#                 budget.Pw=sum(food * Pw)/365/1000,
#                 budget.Pc=sum(food * Pc)/365/1000,
#                 budget.Pcon=sum(food * Pc * (1-CSE))/365/1000)

#new, fast
f.budget <- function(dfIn) {
  dttmp <- data.table(dfIn)
  setkey(dttmp,"scenario","region","year")
  dttmp[,budget.Pw:=sum(food * Pw/365/1000),by=key(dttmp)]
  dttmp[,budget.Pcon:=sum(food * Pc * (1-CSE)/365/1000),by=key(dttmp)]
  dttmp[,budget.Pc:=sum(food * Pc/365/1000),by=key(dttmp)]
  as.data.frame(unique(dttmp[,c(key(dttmp),"budget.Pw","budget.Pc","budget.Pcon"),with=F]))
} 
budget <- f.budget(df1)

incomeShare <-join(t1.pcGDP,budget)
incomeShare$Pw <- incomeShare$budget.Pw / ((incomeShare$value * 1000)/365)
incomeShare$Pc <- incomeShare$budget.Pc / ((incomeShare$value * 1000)/365)
incomeShare$Pcon <- incomeShare$budget.Pcon / ((incomeShare$value * 1000)/365)
incomeShare <- incomeShare[,c("scenario","region","year","Pw","Pc","Pcon")]

#nutrient stuff
#include only columns that are needed; IMPACT code and nutrient names
#choices are 
# - all - all relevant nutrients, 23
# - macro - "energy", "protein", "fat", "carbohydrate", "fiber", "sugar"
# - minerals - "calcium", "iron", "potassium", "sodium", "zinc"
# - vitamins - "vitamin_c", "thiamin",	"riboflavin",	"niacin", "vitamin_b6",	"folate", "vitamin_b12",
#      "vitamin_a_RAE", 	"vitamin_e", "vitamin_d2_3"
# - fattyAcids - "ft_acds_tot_sat", "ft_acds_plyunst"
# next line is where the choice is made
short.name <- c("fattyAcids")
nut.list <- eval(parse(text = short.name))
includes <- c("IMPACT_code", nut.list)
nuts.reduced <- nutrients[, (names(nutrients) %in% includes)]

df2 <- join(df1[,c("scenario","region","IMPACT_code","year","food")],foodGroupsInfo)
df2 <- df2[,c("scenario","region","IMPACT_code","year","food","food.group.code")]

#nutrients.df <- gather(nutrients,nutrient,nut.value,energy:fatty_acids_polyunsat)
tmp <- length(nut.list)
nutrients.df <- gather(nuts.reduced,nutrient,nut.value,
                       eval(parse(text = nut.list[1])):eval(parse(text = nut.list[tmp])))

# 
df3 <- join(df2,nutrients.df)
#convert from annual availability to daily availability
df3$food <- df3$food/365

# #-------  new code from Brendan. This creates the same results as the commented out ddply code below but 
#with a different row order

f.nutShare <- function(dfIn) {
  dttmp <- data.table(dfIn)
  setkey(dttmp,"scenario","region","food.group.code","year","nutrient")
  dttmp[,value:=sum(nut.value*food),by=key(dttmp)]
  as.data.frame(unique(dttmp[,c(key(dttmp),"value"),with=F]))
 }                       
nutShare <- f.nutShare(df3)
# #------stuff for testing
# 
# nutShare.compare <- ddply(df4,
#                                    .(scenario,region,food.group.code,year,nutrient),
#                                    summarise,
#                                    value=sum(nut.value*food))
# #--- end stuff for testing

f.nutShareTot <- function(dfIn) {
  dttmp <- data.table(dfIn)
  setkey(dttmp,"scenario","region","year","nutrient")
  dttmp[,value:=sum(nut.value*food),by=key(dttmp)]
  as.data.frame(unique(dttmp[,c(key(dttmp),"value"),with=F]))
}    

nutShareTot <- f.nutShareTot(df3)

# old slower code
# nutShareTot <- ddply(df4,
#                   .(scenario,region,year,nutrient),
#                   summarise,
#                   value=sum(nut.value*food))

#save.image(file = paste(short.name,"image.RData",sep = "_"))

#create excel output
source("workSheetCreation.R")
