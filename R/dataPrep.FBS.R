#' @author Gerald C. Nelson, \email{nelson.gerald.c@gmail.com}
#' @keywords utilities, FAOSTAT, data management

#' @description
#This script reads in the FAO Food Balance Sheet information and does some manipulations of the data
#' @source \url{http://faostat3.fao.org/download/FB/FBS/E}

source("R/dataPrep.setup.R")
# Intro -------------------------------------------------------------------
#This script reads in the FAO Food Balance Sheet information from a zip file, does
# some manipulations of the data,
#and writes out results to an rds file

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

# Read in the FBS data from a zip file
# FAO changed the structure of the zip file some time in 2015.
# Code to read in the old structure remains below but is commented out

charCols <-  c("Country.Code","Country","Item.Code","Item","Element.Code","Element","Unit")
numCols <- c("Y1961","Y1962","Y1963","Y1964","Y1965",
             "Y1966","Y1967","Y1968","Y1969","Y1970",
             "Y1971","Y1972","Y1973","Y1974","Y1975",
             "Y1976","Y1977","Y1978","Y1979","Y1980",
             "Y1981","Y1982","Y1983","Y1984","Y1985",
             "Y1986","Y1987","Y1988","Y1989","Y1990",
             "Y1991","Y1992","Y1993","Y1994","Y1995",
             "Y1996","Y1997","Y1998","Y1999","Y2000",
             "Y2001","Y2002","Y2003","Y2004","Y2005",
             "Y2006","Y2007","Y2008","Y2009","Y2010",
             "Y2011","Y2012","Y2013","Y2014","Y2015")

# the columns which describe the state of the numeric values
dropCols <- c("Y1961F","Y1962F","Y1963F","Y1964F","Y1965F",
              "Y1966F","Y1967F","Y1968F","Y1969F","Y1970F",
              "Y1971F","Y1972F","Y1973F","Y1974F","Y1975F",
              "Y1976F","Y1977F","Y1978F","Y1979F","Y1980F",
              "Y1981F","Y1982F","Y1983F","Y1984F","Y1985F",
              "Y1986F","Y1987F","Y1988F","Y1989F","Y1990F",
              "Y1991F","Y1992F","Y1993F","Y1994F","Y1995F",
              "Y1996F","Y1997F","Y1998F","Y1999F","Y2000F",
              "Y2001F","Y2002F","Y2003F","Y2004F","Y2005F",
              "Y2006F","Y2007F","Y2008F","Y2009F","Y2010F",
              "Y2011F","Y2012F","Y2013F","Y2014F","Y2015F")


# extract the FBScsv file, write it to the rData directory and report the path in zipout
zipout <- unzip(FBSdataZip, file = FBScsv, exdir = rData)
dt.FBSrawData <- fread(zipout,
              drop = dropCols,
                 stringsAsFactors = FALSE)

for (col in numCols) set(dt.FBSrawData, j=col, value=as.numeric(dt.FBSrawData[[col]]))

# old column names

# colnames(FBSrawData) <- c("FAOSTAT_country_code", "country_name", "item_code", "item",
# "element_group", "element_code", "element","year", "unit", "value", "flag")

#add X to beginning of the year (X2009 instead of 2009), old version of the FBS data
#dt.FBSraw[, year := paste("X", dt.FBSraw$year, sep = "")]

colnames(dt.FBSrawData) <- c("FAOSTAT_country_code", "country_name", "item_code", "item",
                             "element_code", "element","unit",
                             "X1961","X1962","X1963","X1964","X1965",
                             "X1966","X1967","X1968","X1969","X1970",
                             "X1971","X1972","X1973","X1974","X1975",
                             "X1976","X1977","X1978","X1979","X1980",
                             "X1981","X1982","X1983","X1984","X1985",
                             "X1986","X1987","X1988","X1989","X1990",
                             "X1991","X1992","X1993","X1994","X1995",
                             "X1996","X1997","X1998","X1999","X2000",
                             "X2001","X2002","X2003","X2004","X2005",
                             "X2006","X2007","X2008","X2009","X2010",
                             "X2011","X2012","X2013","X2014","X2015")


#how to drop years with the previous version of the FBS data
# remove years before 2010. The latest year is 2011 currently.
# setkey(dt.FBSraw,year)
# dt.FBSraw <- dt.FBSraw[year > 2009,]

# FBSregionsToDrop is defined in dataPrep.setup.R
dt.FBSrawData <- dt.FBSrawData[!country_name %in% FBSregionsToDrop,]

#keep just the years from 2000 to 2011
FBSkeepYearList <- c("X2000","X2001","X2002","X2003","X2004",
                     "X2005","X2006","X2007","X2008","X2009","X2010","X2011")

colKeepListYears <- c("FAOSTAT_country_code", "country_name", "item_code", "item",
                 "element_code", "element","unit",
                 FBSkeepYearList)

dt.FBSraw <- dt.FBSrawData[,colKeepListYears, with = FALSE]

# Read in a worksheet with the list of FBS food items by code, name, definition, and IMPACT commodity code
dt.FBScommodLookup <- as.data.table(read.xlsx(FBSCommodityInfo,
                             sheet = 1,
                             startRow = 1,
                             cols = 1:7,
                             colNames = TRUE))

charConvertList <- c("item_code")
for (col in charConvertList) set(dt.FBScommodLookup, j=col, value=as.character(dt.FBScommodLookup[[col]]))

setkey(dt.FBScommodLookup,item_code)
setkey(dt.FBSraw,item_code)
dt.FBS <- dt.FBSraw[dt.FBScommodLookup]

# Read in the region lookup table, created in dataPrep.regions.R
regions.all <- getNewestVersion("regions.all")

# Read in the worksheet that has the FAO country code-ISO country name lookup
dt.FBSNameLookup <- as.data.table(read.xlsx(FAOCountryNameCodeLookup,
                  sheet = 1,
                  startRow = 1,
                  colNames = TRUE))

#convert to character and leave just ISO code and FAOSTAT code
charConvertList <- c("ISO3","FAOSTAT")
dt.FBSNameLookup <- dt.FBSNameLookup[, lapply(.SD, as.character), .SDcols=charConvertList]
setnames(dt.FBSNameLookup,c("ISO3","FAOSTAT"),c("ISO_code","FAOSTAT_country_code"))

setkey(dt.FBS,FAOSTAT_country_code)
setkey(dt.FBSNameLookup,FAOSTAT_country_code)
dt.FBS <- dt.FBS[dt.FBSNameLookup]

# # Check for aggregations of countries; this should have no content
# FBSDat.countryAggs <- subset(dt.FBS,!(ISO_code %in% regions.ISO$ISO_code))
#
# #get rid of rows that are aggregations of countries
# dt.FBS <- subset(dt.FBS,Country %in% regions.ISO$country_name)

# Create separate data frame for aggregations of commodities
aggregates <- c("Animal fats + (Total)",
                "Cereals - Excluding Beer + (Total)",
                "Meat + (Total)",
                "Milk - Excluding Butter + (Total)",
                "Offals + (Total)",
                "Oilcrops + (Total)",
                "Pulses + (Total)",
                "Stimulants + (Total)",
                "Sugar & Sweeteners + (Total)",
                "Vegetable Oils + (Total)",
                "Spices + (Total)",
                "Starchy Roots + (Total)",
                "Sugar Crops + (Total)",
                "Treenuts + (Total)",
                "Vegetables + (Total)",
                "Vegetal Products + (Total)",
                "Alcoholic Beverages + (Total)",
                "Animal Products + (Total)",
                "Aquatic Products, Other + (Total)",
                "Eggs + (Total)",
                "Fish, Seafood + (Total)",
                "Fruits - Excluding Wine + (Total)",
                "Grand Total + (Total)",
                "Miscellaneous + (Total)")
#dt.FBS.aggs <- subset(dt.FBS,(item %in% aggregates))
dt.FBS.commods <- dt.FBS[!item %in% aggregates]

#remove rows where FBS category is 'Miscellaneous' because we don't have an IMPACT equivalent
dt.FBS.commods <- dt.FBS.commods[!item == "Miscellaneous"]
dt.FBScommodLookup <- dt.FBScommodLookup[!item_name == "Miscellaneous"]
#include IMPACT code and nonIMPACT code assignment in the commodities data frame
setkey(dt.FBS.commods,item_code,item_name,definition, IMPACT_code,IMPACT_missing_code, alcohol,fish)
setkey(dt.FBScommodLookup,item_code,item_name,definition, IMPACT_code, IMPACT_missing_code,alcohol,fish)

temp <- dt.FBS.commods[dt.FBScommodLookup]
saveRDS(temp, file = paste(mData,"/FBS.",Sys.Date(),".rds",sep=""))
