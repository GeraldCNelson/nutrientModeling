library(openxlsx) #written in C so doesn't need java; currently the library of choice
#library(entropy)
library(dplyr)
library(reshape2)
setwd("~/Documents/workspace/FBS")
#Info for the basic info worksheet
userName <- "Gerald Nelson"
dataSource <- "http://faostat3.fao.org/download/FB/FBS/E"
dateDownloaded <- "30 July 2014"
#file names and locations
ctyFileName <- "FBSData/FAOCountryNameCodeLookup.xlsx"
#FBS data
FBSDataFileName <- "FBSData/FoodBalanceSheets_E_All_Data.csv"
FBSCommodityInfoFileName <-"FBSData/FAOCommodityCodeDefinitions.xlsx"

#read in file with country names and codes
ctyNames <- read.xlsx(ctyFileName, 
                      sheet = 1,
                      startRow = 2,
                      colNames = FALSE)
colnames(ctyNames) <- c("Country", "Official.name", "ISO3","ISO2","UNI", "UNDP","FAOSTAT","GAUL")
#read in the FBS data
FBSDat <- read.csv(FBSDataFileName, stringsAsFactors=FALSE)
#convert all the NAs to 0.
FBSDat[is.na(FBSDat)] <- 0
#get rid of rows that are aggregations of countries
ctyRowsToRemove <- c("Land Locked Developing Countries + (Total)", "Least Developed Countries + (Total)", "Low Income Food Deficit Countries + (Total)", "Melanesia + (Total)", "Micronesia + (Total)", "Middle Africa + (Total)", "Net Food Importing Developing Countries + (Total)", "Northern Africa + (Total)", "Northern America + (Total)", "Northern Europe + (Total)", "Oceania + (Total)", "Polynesia + (Total)", "Small Island Developing States + (Total)", "South America + (Total)", "South-Eastern Asia + (Total)", "Southern Africa + (Total)", "Southern Asia + (Total)", "Southern Europe + (Total)", "Caribbean + (Total)", "Africa + (Total)", "Americas + (Total)", "Asia + (Total)", "Australia & New Zealand + (Total)", "Central America + (Total)", "Central Asia + (Total)", "Eastern Africa + (Total)", "Eastern Asia + (Total)", "Eastern Europe + (Total)", "Europe + (Total)", "European Union + (Total)", "Western Africa + (Total)", "Western Asia + (Total)", "Western Europe + (Total)", "World + (Total)")
FBSDat <- subset(FBSDat,!Country %in% ctyRowsToRemove)
#get rid of rows that are aggregations of countries
commodRowsToRemove <- c("Animal fats + (Total)", "Cereals - Excluding Beer + (Total)", "Meat + (Total)", "Milk - Excluding Butter + (Total)", "Offals + (Total)", "Oilcrops + (Total)", "Pulses + (Total)", "Stimulants + (Total)", "Sugar & Sweeteners + (Total)", "Vegetable Oils + (Total)", "Spices + (Total)", "Starchy Roots + (Total)", "Sugar Crops + (Total)", "Treenuts + (Total)", "Vegetables + (Total)", "Vegetal Products + (Total)", "Alcoholic Beverages + (Total)", "Animal Products + (Total)", "Aquatic Products, Other + (Total)", "Eggs + (Total)", "Fish, Seafood + (Total)", "Fruits - Excluding Wine + (Total)", "Grand Total + (Total)", "Miscellaneous + (Total)")
FBSDat <- subset(FBSDat,!(Item %in% commodRowsToRemove))

itemNameList <- as.data.frame(unique(FBSDat$Item)) #Names of commodities
itemCodeList <- as.data.frame(unique(FBSDat$ItemCode))
countryNameList <- as.data.frame(unique(FBSDat$Country)) #Names of countries
countryCodeList <- as.data.frame(unique(FBSDat$CountryCode)) #A numeric code
yearList <- unique(FBSDat$Year) #Annual 1961 - 2011
unitList <- unique(FBSDat$Unit)
# "1000 tonnes"     "Kg"              "kcal/capita/day" "g/capita/day"    "1000" 
elementList <- as.data.frame(unique(FBSDat$Element)) #Names of sources and uses of commodities))
# [1] "Domestic supply quantity"        "Feed"                           
# [3] "Seed"                            "Waste"                          
# [5] "Processing"                      "Food"                           
# [7] "Other Util"                      "Production"                     
# [9] "Import Quantity"                 "Food supply quantity (kg/capita"
# [11] "Food supply"                     "Protein supply quantity"        
# [13] "Fat supply quantity"             "Stock Variation"                
# [15] "Export Quantity"                 "Total Population - Both sexes" 

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

#create a worksheet with the list of FBS food items by code, name, definition, and IMPACT commodity code
FBScommods <- read.xlsx(FBSCommodityInfoFileName, 
                      sheet = 1,
                      startRow = 2,
                      colNames = FALSE)
colnames(FBScommods) <- c("ItemCode", "ItemName", "Definition","IMPACTCode")
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
