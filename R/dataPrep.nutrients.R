#' @author Gerald C. Nelson, \email{nelson.gerald.c@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient lookup
# Intro -------------------------------------------------------------------
#' @description
#' This script reads in the nutrient lookup table for IMPACT commodities (USDA GFS IMPACT Vx).
#' It produces a data frame that has for 100 grams of each IMPACT commodity the amount of several nutrients
#' adjusted for bone in to boneless, edible portion, and cooking retention.
#' Contributors to the work include Brendan Power (for coding assistance), and
#' Joanne E. Arsenault, Malcom Reilly, Jessica Bogard, and Keith Lividini (for nutrition expertise)

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

source("R/dataPrep.setup.R")

# Data loading code for nutrientCalcs -------------------------------------
#' @param nutrients.raw
nutrients.raw <- read.xlsx(nutrientLU,
  sheet = 1,
  rows = 3:50,
  cols = 1:63,
  colNames = TRUE
)

#' @param nutrientNames_Units
nutrientNames_Units <- read.xlsx(
  nutrientLU,
  sheet = 1,
  rows = 1:3,
  cols = 10:46,
  colNames = FALSE
)

#remove columns that are dividers, etc. This leaves only the IMPACT_code, edible share, IMPACT_conversion,
# the nutrient values, and the cooking retention values
colsToRemove <-
  c(
    "name",
    "USDA_code_desc",
    "AUS_code",
    "comment",
    "water_g",
    "inedible_share",
    "proximates",
    "minerals",
    "vitamins",
    "lipids",
    "other",
    "RetentionCode",
    "RetentionDescription"
  )
#' @param nutrients
nutrients.clean <-
  nutrients.raw[,!(names(nutrients.raw) %in% colsToRemove)]

#get list of nutrients in the food nutrient lookup table
#create list of columns that are not nutrients
#' @param notNutrients.cols
notNutrients.cols <-
  c(
    "IMPACT_code",
    "edible_share",
    "IMPACT_conversion",
    "usda_code",
    "composite_code",
    "retentioncode_aus"
  )

#' @param nutrients.cols
nutrients.cols <-
  names(nutrients.clean)[!(names(nutrients.clean) %in% notNutrients.cols)]

#' @param nutrients
nutrients <- nutrients.clean
#convert NAs to 100 (percent) for edible_share, IMPACT_conversion, and cooking retention
colsToConvertTo100 <-
  c("IMPACT_conversion", "edible_share")
nutrients[colsToConvertTo100][is.na(nutrients[colsToConvertTo100])] <-
  100

#convert the NAs to 0  in the nutrients columns
nutrients[nutrients.cols][is.na(nutrients[nutrients.cols])] <- 0

# convert IMPACT consumption values to consumer nutrient intake ---
# reduce nutrient amount by conversion of meat from carcass to boneless (IMPACT_conversion)
# reduce nutrient amount by conversion of all items to edible share

nutrients[, nutrients.cols] <-
  nutrients[, nutrients.cols] * nutrients$IMPACT_conversion / 100
nutrients[, nutrients.cols] <-
  nutrients[, nutrients.cols] * nutrients$edible_share / 100

#remove extraneous columns
colsToRemove <-
  c(
    "edible_share",
    "IMPACT_conversion",
    "usda_code",
    "composite_code",
    "retentioncode_aus"
  )

nutrients <- nutrients[,!(names(nutrients) %in% colsToRemove)]

# add food groups and staples codes to the nutrients table ---
foodGroupsInfo <- read.xlsx(
  foodGroupLU,
  sheet = 1,
  startRow = 1,
  cols = 1:5,
  colNames = TRUE
)
temp <-
  foodGroupsInfo[, c("IMPACT_code", "staple.group.code", "food.group.code")]
nutrients <- merge(nutrients, temp, by = "IMPACT_code")

#nutrients.out <- iconv(nutrients, from = "UTF-8", to = "Windows-1252") #to deal with mu
# but not needed with an rds file
saveRDS(nutrients, file = paste(mData,"/nutrients.",Sys.Date(),".rds",sep=""))
