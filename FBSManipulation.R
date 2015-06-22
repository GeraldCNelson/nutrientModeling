
# Intro -------------------------------------------------------------------
#This script reads in the FAO Food Balance Sheet information, does some manipulations of the data,
#and writes out results to an excel spreadsheet

#Copyright (C) 2015 Gerald C. Nelson

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
library(dplyr)
library(reshape2)
library(ggplot2)

setwd("~/Documents/workspace/nutrientModeling")

# Info for the basic info worksheet ---------------------------------------

userName <- "Gerald Nelson"
dataSource <- "http://faostat3.fao.org/download/FB/FBS/E"
dateDownloaded <- "8 June 2015"
#file names and locations
ctyFileName <- "data/FBSData/FAOCountryNameCodeLookup.xlsx"
#FBS data
FBSDataFileName <- "data/FBSData/FoodBalanceSheets_E_All_Data.csv"
FBSCommodityInfoFileName <-"data/FBSData/FAOCommodityCodeDefinitions.xlsx"
#create a worksheet with the list of FBS food items by code, name, definition, and IMPACT commodity code
FBScommods <- read.xlsx(FBSCommodityInfoFileName, 
                        sheet = 1,
                        startRow = 2,
                        colNames = FALSE)
colnames(FBScommods) <- c("Item.Code", "Item.Name", "Definition","IMPACTCode")
#read in file with country names and codes
ctyNames <- read.xlsx(ctyFileName, 
                      sheet = 1,
                      startRow = 2,
                      colNames = FALSE)
colnames(ctyNames) <- c("Country", "Official.name", "ISO3","ISO2","UNI", "UNDP","FAOSTAT","GAUL")
#read in the FBS data
FBSDat <- read.csv(FBSDataFileName, stringsAsFactors=FALSE)
#convert all the NAs to 0
#FBSDat[is.na(FBSDat)] <- 0

#get rid of rows that are aggregations of countries
FBSDat <- subset(FBSDat,Country %in% ctyNames$Country)
#get rid of rows that are aggregations of commodities
commodRowsToRemove <- c("Animal fats + (Total)", 
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
FBSDat <- subset(FBSDat,!(Item %in% commodRowsToRemove))

#remove years before 2005
columnsToKeep <- c("Country.Code","Country","Item.Code" ,"Item" ,"Element.Code","Element",
                   "Y2006","Y2007","Y2008","Y2009","Y2010","Y2011","Y2012","Y2013")
FBSDat <- FBSDat[ , columnsToKeep]

#extract the rows with kcal/capita/day data; fixed=TRUE is needed so the string is interpreted as a pure string
FBSDat.kcals <- FBSDat[grep("Food supply (kcal/capita/day)",FBSDat$Element, fixed=TRUE),]
#include IMPACT code assignment
FBSDat.kcals <- merge (FBSDat.kcals,FBScommods, by.x = "Item.Code", sort = FALSE)
#add column that creates an IMPACT fish code
FBSDat.kcals$IMPACTCodewFish <- FBSDat.kcals$IMPACTCode
FBSDat.kcals$IMPACTCodewAlcy <- FBSDat.kcals$IMPACTCode
FBSDat.kcals$IMPACTCodewFish[grep("Fish", FBSDat.kcals$Item)]<- "cfish"
FBSDat.kcals$IMPACTCodewAlcy[grep("Beer", FBSDat.kcals$Item)]<- "cbeer"
FBSDat.kcals$IMPACTCodewAlcy[grep("Wine", FBSDat.kcals$Item)]<- "cwine"
FBSDat.kcals$IMPACTCodewAlcy[grep("Alco", FBSDat.kcals$Item)]<- "calbv"

#the list of columns to do the sum over  
colList <- c("Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013")

#create a df that has the total of calories for each country for each of the years in colList
FBSDat.kcals.sum.allCommods <- aggregate(FBSDat.kcals[colList], by=list(FBSDat.kcals$Country), FUN = "sum", na.rm = FALSE)

#-------- for existing IMPACT commodities
#delete rows that have NA in the IMPACTCode column
FBSDat.kcals.IMPACT <- FBSDat.kcals[complete.cases(FBSDat.kcals["IMPACTCode"]),]
#sum kcal data by country for IMPACT commodities
FBSDat.kcals.sum.IMPACT <- aggregate(FBSDat.kcals.IMPACT[colList], 
                                     by=list(FBSDat.kcals.IMPACT$Country), 
                                     FUN = "sum", na.rm = FALSE)
# calculate the ratio of IMPACT only to total kcals for 2010
kcalRatio2010 <- FBSDat.kcals.sum.IMPACT$Y2010/FBSDat.kcals.sum.allCommods$Y2010
#create a data frame to make plotting easier
kcaldf <- data.frame(FBSDat.kcals.sum.IMPACT$Group.1,kcalRatio2010)
names(kcaldf)[names(kcaldf)=="FBSDat.kcals.sum.IMPACT.Group.1"] <- "Country"
#order the data from lowest ratio to highest ratio
kcaldf$Country <- reorder(kcaldf$Country,kcaldf$kcalRatio2010)
p2 <- ggplot(data=kcaldf, aes(x=Country, y = kcalRatio2010)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
p2

#------ if all fish items in FBS had an IMPACT commodity code, cfish
#create a new dataframe that includes rows with cfish
FBSDat.kcals.IMPACTwFish <- FBSDat.kcals[complete.cases(FBSDat.kcals["IMPACTCodewFish"]),]
#sum kcal data by country for IMPACT commodities
FBSDat.kcals.sum.IMPACTwFish <- aggregate(FBSDat.kcals.IMPACTwFish[colList], by=list(FBSDat.kcals.IMPACTwFish$Country), FUN = "sum", na.rm = FALSE)
kcalRatio2010wFish <- FBSDat.kcals.sum.IMPACTwFish$Y2010/FBSDat.kcals.sum.allCommods$Y2010
#create a data frame to make plotting easier
kcaldfwFish <- data.frame(FBSDat.kcals.sum.IMPACTwFish$Group.1,kcalRatio2010wFish)
names(kcaldfwFish)[names(kcaldfwFish)=="FBSDat.kcals.sum.IMPACTwFish.Group.1"] <- "Country"
kcaldfwFish$Country <- reorder(kcaldfwFish$Country,kcaldfwFish$kcalRatio2010wFish)
p3 <- ggplot(data=kcaldfwFish, aes(x=Country, y = kcalRatio2010wFish)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
p3

#------ if all alcohol items in FBS had an IMPACT commodity code
#add a column that has the IMPACT codes, including one for alcohol
#there are three types of alcoholic beverages - Beer, Wine, and Beverages, Alcoholic

#create a new dataframe that includes rows with calcy, for alcoholic beverages
FBSDat.kcals.IMPACTwAlcy <- FBSDat.kcals[complete.cases(FBSDat.kcals["IMPACTCodewAlcy"]),]
#sum kcal data by country for IMPACT commodities
FBSDat.kcals.sum.IMPACTwAlcy <- aggregate(FBSDat.kcals.IMPACTwAlcy[colList], by=list(FBSDat.kcals.IMPACTwAlcy$Country), FUN = "sum", na.rm = FALSE)
kcalRatio2010wAlcy <- FBSDat.kcals.sum.IMPACTwAlcy$Y2010/FBSDat.kcals.sum.allCommods$Y2010
#create a data frame to make plotting easier
kcaldfwAlcy <- data.frame(FBSDat.kcals.sum.IMPACTwAlcy$Group.1,kcalRatio2010wAlcy)
names(kcaldfwAlcy)[names(kcaldfwAlcy)=="FBSDat.kcals.sum.IMPACTwAlcy.Group.1"] <- "Country"
kcaldfwAlcy$Country <- reorder(kcaldfwAlcy$Country,kcaldfwAlcy$kcalRatio2010wAlcy)
p4 <- ggplot(data=kcaldfwAlcy, aes(x=Country, y = kcalRatio2010wAlcy)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
p4

itemNameList <- as.data.frame(unique(FBSDat$Item)) #Names of commodities
itemCodeList <- as.data.frame(unique(FBSDat$ItemCode))
countryNameList <- as.data.frame(unique(FBSDat$Country)) #Names of countries
countryCodeList <- as.data.frame(unique(FBSDat$CountryCode)) #A numeric code
unitList <- unique(FBSDat$Unit)
# "1000 tonnes", "Kg", "kcal/capita/day", "g/capita/day", "1000" 
elementList <- as.data.frame(unique(FBSDat$Element)) #Names of sources and uses of commodities))
# 1           Total Population - Both sexes
# 2                              Production
# 3                         Import Quantity
# 4                         Stock Variation
# 5                Domestic supply quantity
# 6                                    Seed
# 7                                   Waste
# 8                                    Food
# 9     Food supply quantity (kg/capita/yr)
# 10          Food supply (kcal/capita/day)
# 11 Protein supply quantity (g/capita/day)
# 12     Fat supply quantity (g/capita/day)
# 13                                   Feed
# 14                        Export Quantity
# 15                             Processing
# 16                             Other uses 

#create the table with columns for region, commodity, and years

fbs <- dcast(FBSDat, Country + Item + Element + Unit ~ Year, mean, value.var = "Value")
#get rid of some of the extraneous columns in ctyNames
temp <- ctyNames[,1:3]
fbs <- merge(temp,fbs, by = "Country")
fbs <- arrange(fbs,Country, Item, Element)

#create styles to format the worksheets
numStyle <- createStyle(numFmt = "0.0")
numStyle3 <- createStyle(numFmt = "0.000")
shareStyle <- createStyle(numFmt = "0.0%")
textStyle <- createStyle(fontName = NULL, fontSize = NULL, fontColour = NULL,
                         numFmt = "GENERAL", border = NULL,
                         borderColour = getOption("openxlsx.borderColour", "black"),
                         borderStyle = getOption("openxlsx.borderStyle", "thin"), bgFill = NULL,
                         fgFill = NULL, halign = NULL, valign = NULL, textDecoration = NULL,
                         wrapText = FALSE, textRotation = NULL)

wb <- createWorkbook()
sheetNameList <- ("Sheet names")
sheetNameDesc <- ("Description of sheet contents")
NutfileName <-"results/FBSOutput.xlsx"

#create a worksheet with info on creator, date, model version, etc.
creationInfo <- ("Information on creator, date, model version, etc.")
creationInfo <- rbind(creationInfo, paste("Creator:", userName))
creationInfo <- rbind(creationInfo, paste("Date of file creation:", Sys.time()))
creationInfo <- rbind(creationInfo, paste("Data source:", dataSource))
creationInfo <- rbind(creationInfo, paste("Data download date:", dateDownloaded))
                                            
addWorksheet(wb, sheetName="creationInfo")
writeData(wb, creationInfo, sheet="creationInfo", startRow=1, startCol=1, rowNames = FALSE, colNames = FALSE)
sheetNameList <- rbind(sheetNameList,"creationInfo")
sheetNameDesc <- rbind(sheetNameDesc,"Information on creator, date, model version, etc.")

#create a worksheet with info on the regions
addWorksheet(wb, sheetName="metadataRegions")
sheetNameList <- rbind(sheetNameList,"metadataRegions")
sheetNameDesc <- rbind(sheetNameDesc,"metadata on regions")
writeData(wb, ctyNames, sheet="metadataRegions", startRow=1, startCol=1, rowNames = FALSE)
addStyle(wb, sheet="metadataRegions", 
         style=textStyle, 
         cols = 1:ncol(ctyNames), rows = 1:nrow(ctyNames),gridExpand = TRUE)

#create a worksheet with the list of FBS elements (production, imports, etc.)
addWorksheet(wb, sheetName="metadataFBSElements")
sheetNameList <- rbind(sheetNameList,"metadataFBSElements")
sheetNameDesc <- rbind(sheetNameDesc,"List of elements in the FAO Food Balance Sheet")
writeData(wb, elementList, sheet="metadataFBSElements", startRow=1, startCol=1, rowNames = FALSE, colNames = FALSE)
# addStyle(wb, sheet="metadataFBSElements", 
#          style=textStyle, 
#          cols = 1:ncol(elementList), rows = 1:nrow(elementList),gridExpand = TRUE)
setColWidths(wb, sheet="metadataFBSElements", cols=2:ncol(elementList), widths = 42) ## set column width for row names column

addWorksheet(wb, sheetName="metadataFBSCommodities")
sheetNameList <- rbind(sheetNameList,"metadataFBSCommodities")
sheetNameDesc <- rbind(sheetNameDesc,"List of items in the FAO Food Balance Sheet")
writeData(wb, FBScommods, sheet="metadataFBSCommodities", startRow=1, startCol=1, rowNames = FALSE, colNames = TRUE)
addStyle(wb, sheet="metadataFBSCommodities", 
         style=textStyle, 
         cols = 2:ncol(FBScommods), rows = 1:nrow(FBScommods), gridExpand = TRUE)
setColWidths(wb, sheet="metadataFBSCommodities", cols=2:ncol(FBScommods), widths = 42) ## set column width for row names column

ISO3Names <- unique(fbs$ISO3)
#for (i in 1:length(ISO3Names)) {
  for (i in 1:10) {
    #subset by region i
  regionISO3 <- ISO3Names[i]
  temp <- subset(fbs, (ISO3 %in% regionISO3))
  sheetNameList <- rbind(sheetNameList, regionISO3)
  sheetNameDesc <- rbind(sheetNameDesc, paste("FBS for", regionISO3))
  
  addWorksheet(wb, sheetName=regionISO3)
  writeData(wb, temp, sheet=regionISO3, startRow=1, startCol=1, rowNames = TRUE)
  #addStyle(wb, sheet=tempSheetName, style=numStyle, rows = 2:(nrow(mat)+1), cols=2:(ncol(mat)+1), gridExpand = TRUE)
}

#add sheet with info about each of the worksheets
sheetNameMeta <-as.data.frame(cbind(sheetNameList,sheetNameDesc))
addWorksheet(wb, sheetName="sheetInfo")
writeData(wb, sheetNameMeta, sheet="sheetInfo", startRow=1, startCol=1, rowNames = FALSE, colNames = FALSE)
addStyle(wb, sheet="sheetInfo", style=textStyle, rows = 1:nrow(sheetNameMeta), cols=1:(ncol(sheetNameMeta)), gridExpand = TRUE)
setColWidths(wb, sheet="sheetInfo", cols = 1:2, widths=20)

#move sheetInfo worksheet from the last to the first
temp<- 2:length(names(wb))-1
temp <- c(length(names(wb)),temp)
worksheetOrder(wb) <- temp

saveWorkbook(wb, NutfileName, overwrite = TRUE)
