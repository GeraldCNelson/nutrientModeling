# Intro -------------------------------------------------------------------
#This script reads in the nutrient lookup table for IMPACT commodities (USDA GFS IMPACT Vx).
# It produces a data frame that has for 100 grams of each IMPACT commodity the amount of several nutrients
# adjusted for bone in to boneless, edible portion, and cooking retention.

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
# Contributors to the work include Brendan Power (for coding assistance), and 
# Joanne E. Arsenault, Malcom Reilly, Jessica Bogard, and Keith Lividini (for nutrition expertise)
require(openxlsx)
require(entropy)
require(reshape2)
require(plyr)
require(dplyr)
require(tidyr)
require(data.table)
require(splitstackshape)
require(plotrix)
# Data loading code for nutrientCalcs -------------------------------------
nutrientFileName <- "data/USDA GFS IMPACT V9.xlsx"

nutrients <- read.xlsx(nutrientFileName, 
                       sheet = 1,
                       rows = 3:50,
                       cols = 1:63,
                       colNames = TRUE)

nutrientNames_Units <- read.xlsx(nutrientFileName, 
                                 sheet = 1,
                                 rows = 1:3,
                                 cols = 10:46,
                                 colNames = FALSE)

#remove columns that are dividers, etc. This leaves only the IMPACT_code, edible share, IMPACT_conversion,
# the nutrient values, and the cooking retention values
colsToRemove <- c("name","USDA_code_desc","AUS_code","comment","water_g","inedible_share","proximates",
                  "minerals","vitamins","lipids","other","RetentionCode","RetentionDescription")
nutrients <- nutrients[,!(names(nutrients) %in% colsToRemove)]

#list of columns with cooking retention values
cookretn.cols <- colnames(nutrients[, grep('_cr', names(nutrients))] )
#get list of nutrients in the food nutrient lookup table
#create list of columns that are not nutrients
temp <-  c("IMPACT_code", "edible_share", "IMPACT_conversion",cookretn.cols)
nutrients.food <- colnames(nutrients[, !(names(nutrients) %in% temp)])

# macro <- c("energy", "protein", "fat", "carbohydrate", "fiber", "sugar")
# minerals <- c("calcium", "iron", "potassium", "sodium", "zinc")
# vitamins <- c("vit_c", "thiamin",	"riboflavin",	"niacin", "vit_b6",	"folate", "vit_b12",
#           "vit_a", 	"vit_e_RAE", "vit_d2_3")
# fattyAcids <-  c("ft_acds_tot_sat", "ft_acds_plyunst")

#convert NAs to 100 (percent) for edible_share, IMPACT_conversion, and cooking retention
colsToConvert <- c("IMPACT_conversion","edible_share", cookretn.cols)
nutrients[colsToConvert][is.na(nutrients[colsToConvert])] <- 100

#convert the NAs to 0  in the nutrients columns 
nutrients[,nutrients.food][is.na(nutrients[,nutrients.food])] <- 0

# # change nutrient denominator unit from 100 gm to 1 kg; 
# commented out to leave units what nutritionists are familiar with
# nutrients[,nutrients.food] <- nutrients[,nutrients.food] * 10

# convert IMPACT consumption values to consumer nutrient intake ---
# reduce nutrient amount by conversion of meat from carcass to boneless (IMPACT_conversion)
# reduce nutrient amount by conversion of all items to edible share

nutrients[,nutrients.food] <- nutrients[,nutrients.food] * nutrients$IMPACT_conversion / 100
nutrients[,nutrients.food] <- nutrients[,nutrients.food] * nutrients$edible_share / 100
#multiply the amount of a nutrient in a food by the cooking retention value
for (i in 1:length(cookretn.cols)) {
  nutrientName <- substr(x = cookretn.cols[i], 1, nchar(cookretn.cols[i])-3)
  nutColName <- paste ("nutrients$",nutrientName,sep="")
  print(nutColName)
  nutRetentColName <- paste ("nutrients$",cookretn.cols[i],sep="")
  print(nutRetentColName)
  temp <- as.data.frame(eval(parse(text = nutRetentColName)) * eval(parse(text = nutColName)) /100)
  colnames(temp) <- nutrientName
  nutrients[, nutrientName] <- temp
}

#remove extraneous columns
colsToRemove <- c("edible_share","IMPACT_conversion",cookretn.cols)
nutrients <- nutrients[,!(names(nutrients) %in% colsToRemove)]

# add food groups and staples codes to the nutrients table ---
commodityFoodGroupLookupFileName <- "data/food commodity to food group table V1.xlsx"

# add food groups and staples codes to the nutrients table
foodGroupsInfo <- read.xlsx(commodityFoodGroupLookupFileName, 
                            sheet = 1,
                            startRow = 1,
                            cols = 1:4,
                            colNames = TRUE)
tmp <- foodGroupsInfo[,c("IMPACT_code","food.group.code")]
nutrients <- merge(nutrients, tmp, by = "IMPACT_code")

# staples are roots and cereals
food.groups <- c("fruits", "cereals", "pulses", "meats", "beverages",
                    "roots", "eggs", "oils", "oilSeeds", "diary", "sweeteners",
                    "vegetables")
staple.category <- c("nonstaple","staple","nonstaple","nonstaple","nonstaple",
                     "staple","nonstaple","nonstaple","nonstaple","nonstaple","nonstaple",
                     "nonstaple")
staples <- data.frame(food.group.code = food.groups,
                      staple.code = staple.category, 
                      stringsAsFactors = FALSE)

nutrients <- merge(nutrients, staples, by = "food.group.code", all = TRUE)
nutrients <- nutrients[,c(2:length(nutrients),1)]

nutrients.out <- iconv(nutrients, from = "UTF-8", to = "Windows-1252") #to deal with mu
write.csv(nutrients.out,file = "results/nutrients_final.csv", fileEncoding = "Windows-1252")
