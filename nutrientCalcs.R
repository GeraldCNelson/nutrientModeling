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
source(file = "EARfoodGroupCSELoading.R")

#source(file = "nutrientDataLoading.R") # this is run from EARfoodGroupCSELoading.

#old, slow
# budget <- ddply(df3,
#                 .(scenario,region,year),
#                 summarise,
#                 budget.Pw=sum(food * Pw)/365/1000,
#                 budget.Pc=sum(food * Pc)/365/1000,
#                 budget.Pcon=sum(food * Pc * (1-CSE))/365/1000)

#new, fast
f.budget <- function(dfIn) {
  dt.tmp <- data.table(dfIn)
  setkey(dt.tmp,"scenario","region","year")
  dttmp[,budget.Pw:=sum(food * Pw/365/1000),by=key(dt.tmp)]
  dttmp[,budget.Pcon:=sum(food * Pc * (1-CSE)/365/1000),by=key(dt.tmp)]
  dttmp[,budget.Pc:=sum(food * Pc/365/1000),by=key(dt.tmp)]
  as.data.frame(unique(dt.tmp[,c(key(dt.tmp),"budget.Pw","budget.Pc","budget.Pcon"),with=F]))
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
short.name <- c("vitamins")
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
