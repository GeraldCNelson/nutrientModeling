# Intro -------------------------------------------------------------------
#This script reads in IMPACT data and the nutrient lookup table, does some manipulations of the data,
#and writes out results to an excel spreadsheet

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
library(openxlsx) 
library(entropy)
library(dplyr)
library(reshape2)
setwd("~/Documents/workspace/nutrientModeling")

# Info for the basic worksheet ---------------------------------------
userName <- "Gerald Nelson"
dataSource <- "URL here"
dateDownloaded <- "Date here"
dateCreated <- Sys.time()

# File names, related info and locations -----------------------------------
NFileName <- "data/GFS nutrient matrix production & supply IMPACT codedV2.xlsx"
IMPACTfileName <- "data/DemandPerCapJan2015Results.xlsx"
climData <- ".NoCC.MidGDPMidPop." #used to construct output file name
IMPACTregionsFileName <- "data/IMPACTRegionsMay2015.xlsx"
IMPACTpricesFileName <- "data/IMPACTPricesJan2015.xlsx"
EARFile <- "data/DRI EAR values.xlsx"
commodityFoodGroupLookupFileName <- "data/food commodity to food group table V1.xlsx"

IMPACTfoodCommodities <- c("cbeef","cpork","clamb","cpoul","ceggs","cmilk","cbarl","cmaiz",
"cmill","crice","csorg","cwhea","cocer","ccass","cpota","cswpt","cyams","corat","cbean","cchkp",
"ccowp","clent","cpigp","copul","cbana","cplnt","csubf","ctemf","cvege","csugr","cgrnd","cgdol",
"crpsd","crpol","csoyb","csbol","csnfl","csfol","cplol","cpkol","ctols","ctool","ccoco","ccafe",
"cteas","cothr")
daysInYear <- 365 #see http://stackoverflow.com/questions/9465817/count-days-per-year for a way to deal with leap years

# Food groups and commodities ---------------------------------------------
# This set of 13 food groups will be reviewed at a workshop in late June 2015 for possible revision.
# Note: staples is not a food group per se but included here because it is used in one of the diversity metrics
staples <- c("cereals", "roots and tubers")
beverages <- c("beverages")
cereals <- c("cereals")
dairy <- c("dairy")
eggs <- c("eggs")
fish <- c("fish")
fruits <- c("fruits") 
meats <- c("meats")
oils <-c("fats and oils")
oilSeeds <- c("oil seeds")
pulses <- c("pulses")
roots <- c("roots and tubers")
sweeteners <- c("sugar and sweeteners")
vegetables <-c("vegetables")

allFoodGroups <- c("beverages", "cereals", "dairy", "eggs", "fish", "fruits", "meats", "fats and oils", 
               "oil seeds", "pulses", "roots and tubers", "sugar and sweeteners", "vegetables")


# Read in and clean up files ----------------------------------------------

## nutrient lookup table readin and cleanup -------------------------------
nutNames <- c("ENERGY","PROT","FAT","CARBO","FIBR","CALC","PHOS","MGNS","POTS","SODM","IRON",
              "ZINC","COPR","MNGN","VITA","VITD","VITE","VITC","THIAM","RIBF","NIAC","VITB6","FOLC",
              "VITB12","PANTO")
nutrients <- read.xlsx(NFileName, 
                       sheet = 1,
                       startRow = 3,
                       colNames = FALSE)

colnames(nutrients) <- c("name","commodity","PlantPart","source", "conversion",
                         nutNames)
#convert the NAs to 0 except in the first 5 columns which are text fields
temp <- nutrients[,5:ncol(nutrients)]
temp[is.na(temp)] <- 0
#nutrients[is.na(nutrients)] <- 0 [is this necessary?]
nutrients[,5:ncol(nutrients)] <- temp
# change nutrient unit from 100 gm to 1 kg
nutrients[,nutNames] <- nutrients[,nutNames] * 10
# convert to IMPACT unit equivalents (nutrients per carcass weight for meat)
nutrients[,nutNames] <- 
  nutrients[,nutNames] * nutrients[,"conversion"]

## IMPACT commodity assignment to food group file readin and cleanup ------
fgLookup <- read.xlsx(commodityFoodGroupLookupFileName, 
                sheet = 1,
                startRow = 2,
                colNames = FALSE)
colnames(fgLookup) <- c("IMPACT_code","name","foodGroup","foodGroups")
## IMPACT commodity file readin and cleanup -------------------------------
t1 <- read.xlsx(IMPACTfileName, 
                sheet = 2,
                startRow = 1,
                colNames = TRUE)
t1.income <- t1[,c("impactparameter","scenario","region","year","val")]
PcIncome.IMPACT <- subset(t1.income,(t1$impactparameter == "pcGDPXAgg -- Per Capita Income" ))
PcIncome.IMPACT.wide <- dcast(PcIncome.IMPACT,scenario + region  ~ year, mean, value.var = "val")
#keep only food items
temp <- t1[t1$commodity %in% IMPACTfoodCommodities, ]
#remove the scenario column for now.
PcIncome.IMPACT.wide[c("scenario")] <- list(NULL)

colnames(PcIncome.IMPACT.wide) <- make.names(colnames(PcIncome.IMPACT.wide))

t1 <- t1[,c("impactparameter","scenario","commodity","region","year","val")] #get rid of extra columns
t2 <- t1[-grep("-",t1$commodity),] #include only the rows with commodity codes; remove those that contain a '-'.
t3 <- subset(t2,impactparameter == "FoodAvailXAgg") #include only the rows with commodity codes; remove those that contain a '-'.
#create the table with columns for region, commodity, and years
commods <- dcast(t3,region + commodity ~ year, mean, value.var = "val")
#add X in front of the year to make the column names syntactically correct
colnames(commods) <- make.names(colnames(commods))
i <- sapply(commods, is.factor)
commods[i] <- lapply(commods[i], as.character)
#get the list of all the commodity names used in this IMPACT output
cropNames <- as.data.frame(sort(unique(commods$commodity)))
colnames(cropNames) <- "commodity"

#cleanup
rm(t1,t2,t3,PcIncome.IMPACT,t1.income)

#Note to self: still need to aggregate to new regions for IMPACT
regions <- sort(unique(commods$region))

#operations on IMPACT prices file
worldPrices <- read.xlsx(IMPACTpricesFileName, 
                       sheet = 1,
                       startRow = 1,
                       colNames = TRUE)
cropNames <- as.data.frame(sort(unique(worldPrices$commodity)))
colnames(cropNames) <- "commodity"

worldPrices.wide <- dcast(worldPrices,commodity ~ year, mean, value.var = "val")
colnames(worldPrices.wide) <- make.names(colnames(worldPrices.wide))


# metric, income share to food --------------------------------------------

## calculate how much is spent per day on IMPACT commodities and its share of pc income, 
## currently using world prices
budget <- data.frame(matrix(vector(), 0, ncol(worldPrices.wide), 
                            dimnames=NULL), stringsAsFactors=F)
colnames(budget) <- c("region",colnames(worldPrices.wide[2:ncol(worldPrices.wide)]))

incomeShare <- data.frame(matrix(vector(), 0, ncol(worldPrices.wide), 
                            dimnames=NULL), stringsAsFactors=F)
colnames(incomeShare) <- c("region",colnames(worldPrices.wide[2:ncol(worldPrices.wide)]))

for (j in 1:length(regions)) { #j is regions
  regionTmp <- subset(commods,(commods$region == regions[j] ))
  tmp <- merge(regionTmp,cropNames, by = "commodity", all=TRUE )
  #remove the region column
  tmp[c("region")] <- list(NULL)
  
  tmp[is.na(tmp)] <- 0
  budget[j,1]<- c(regions[j])
  incomeShare[j,1]<- c(regions[j])
  
  for (i in 2:ncol(worldPrices.wide)) { # i is years
    #commodities in tmp are in kg/person per year
    budget[j,i] <- sum(tmp[,i] * worldPrices.wide[,i])/365/1000
    #PcIncome is in $1000/mt; convert to $1 and get average daily income
    incomeShare[j,i] <- budget[j,i] / ((PcIncome.IMPACT.wide[j,i] * 1000)/365)
    
  }
}
write.csv(budget, file = "results/budget.csv")
write.csv(incomeShare, file = "results/incomeShare.csv")

IMPACTregions <- read.xlsx(IMPACTregionsFileName, colNames = TRUE, sheet = 1)
ctyNames  <- IMPACTregions$CTY
yrs <- colnames(commods)[-(1:2)] #list of the column names for the years of data

# #Read in the RDAs data
RDAFile <- "RDAs.xlsx"
RDAs <- read.xlsx(RDAFile, colNames = TRUE,sheet = 1, startRow = 1)
RDAs <- RDAs[,2:4]

#Read in the EARs data
EARs <- read.xlsx(EARFile, sheet = 1, startRow = 3, colNames = FALSE)
EARs <- EARs[,-1]
#make sure everything that should be numeric, is
for (j in 2:length(EARs)) set(EARs,j=j,value=as.numeric(EARs[[j]]))
#give the columns some names
colnames(EARs) <- c("nutrient","X0_0.5","X0.5_1","X1_3","X4_8",
                    "M9_13","M14_18","M19_30","M_31_50","M51_70","M70Plus",
                    "F9_13","F14_18","F19_30","F_31_50","F51_70","F70Plus",
                    "P14_18","P19_30","P31_50","L14_18","L19_30","L31_50")
#add columns to line up with the SSP pop data distribution from IIASA
#children
EARs$SSPX0_4 <- (EARs$X0_0.5 + EARs$X0.5_1 + EARs$X1_3)/3
EARs$SSPX5_9 <- (EARs$X4_8)
EARs$SSPX10_14 <- (EARs$X9_13)
#males
EARs$SSPM15_19 <- (EARs$X14_18)
EARs$SSPM20_24 <- (EARs$M19_30)
EARs$SSPM25_29 <- (EARs$M19_30)
EARs$SSPM30_34 <- (EARs$M31_50)
EARs$SSPM35_39 <- (EARs$M31_50)
EARs$SSPM40_45 <- (EARs$M31_50)
EARs$SSPM45_49 <- (EARs$M31_50)
EARs$SSPM50_54 <- (EARs$M31_50)
EARs$SSPM55_59 <- (EARs$M51_70)
EARs$SSPM60_64 <- (EARs$M51_70)
EARs$SSPM65_69 <- (EARs$M51_70)
EARs$SSPM70_74 <- (EARs$M70Plus)
EARs$SSPM75_79 <- (EARs$M70Plus)
EARs$SSPM80_84 <- (EARs$M70Plus)
EARs$SSPM85_89 <- (EARs$M70Plus)
EARs$SSPM90_94 <- (EARs$M70Plus)
EARs$SSPM95_99 <- (EARs$M70Plus)
EARs$SSPM100Plus <- (EARs$M70Plus)
#females
EARs$SSPF20_24 <- (EARs$F19_30)
EARs$SSPF25_29 <- (EARs$F19_30)
EARs$SSPF30_34 <- (EARs$F31_50)
EARs$SSPF35_39 <- (EARs$F31_50)
EARs$SSPF40_45 <- (EARs$F31_50)
EARs$SSPF45_49 <- (EARs$F31_50)
EARs$SSPF50_54 <- (EARs$F31_50)
EARs$SSPF55_59 <- (EARs$F51_70)
EARs$SSPF60_64 <- (EARs$F51_70)
EARs$SSPF65_69 <- (EARs$F51_70)
EARs$SSPF70_74 <- (EARs$F70Plus)
EARs$SSPF75_79 <- (EARs$F70Plus)
EARs$SSPF80_84 <- (EARs$F70Plus)
EARs$SSPF85_89 <- (EARs$F70Plus)
EARs$SSPF90_94 <- (EARs$F70Plus)
EARs$SSPF95_99 <- (EARs$F70Plus)
EARs$SSPF100Plus <- (EARs$F70Plus)
#pregnant and lactating are already included.
#delete old columns
EARs <- EARs[, !names(EARs) %in% 
               c("X0_0.5","X0.5_1","X1_3","X4_8",
                 "M9_13","M14_18","M19_30","M_31_50","M51_70","M70Plus",
                 "F9_13","F14_18","F19_30","F_31_50","F51_70","F70Plus")]




#test function inputs
# rgn <- ctyNames[2]
# yr <- yrs[1]
# cat <- "allFoodGroups"

f.regionCmdty <- function(rgn)
{
  #The argument to this function is a region from the commods data frame
  #Currently, it just reads in a 3 letter country code from the cty column
  #Its output is a data frame with nutrition data for the given region
  if (!(rgn %in% ctyNames)) {
    #error checking. At the moment it just checks for typos.
    stop(paste(rgn, " is not in the list of countries in the IMPACT output file"))
  }
  regionTemp <- subset(commods, region == rgn)
}

f.nutSum <-function(rgn,cat,yr) {
  #function to generate data frame that for a given region and yr has the sum of each nutrient from a category of food stuffs
  #rgn is a 3 letter country name
  #cat is a list of food categories
  #yr is a string that is the name of a column in rgn
  
  #create an empty vector to which the nutrient sum data are appended
  vecTemp<-vector()
  
  #make sure that nutTemp has all IMPACT commodities even if consumption is zero. It will also have all nutrients
  nutTemp <- merge(f.regionCmdty(rgn),nutrients, by = "commodity", all.x=TRUE )
  nutTempRgn <- subset(nutTemp,nutTemp$category %in% eval(parse(text = cat)))
  nutTempRgn[c("commodity","region","name","category")]
  nutTempRgn[is.na(nutTempRgn)] <- 0
  
  # The if statement checks to see whether the rgn (AGR, USA, etc.) has values for the cat (staples, etc.) of commodities
  if ("TRUE" %in% (nutTempRgn$category %in% eval(parse(text = cat)))){
    #loop through all nutrients
    for (nutCntr in 1:length(nutNames)) {
      # sum of nutrient per capita per day (assumes 365 days in year) from all commodities
      nutSumTemp <- sum(nutTempRgn[,yr] * nutTempRgn[nutNames[nutCntr]]) / daysInYear
      vecTemp <-append(vecTemp,nutSumTemp)
    }
  } else 
  {
    vecTemp <-append(vecTemp,"0.0")
    print(paste("No", cat, "in", rgn))
  }   
  return(vecTemp)
}

#steps to create the metadata info
nutrientNamesFile <- "NutrientNames.xlsx"
nutrientNames <- read.xlsx(nutrientNamesFile, colNames = TRUE, sheet = 1)
commodityNames <- cbind(nutrients[c("name","commodity","category")])

#create the spreadsheet for a country, add worksheets for metadata and put in the results folder of the working directory
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
#choose the region
rgn <- ctyNames[65] #151 is USA; 43 is EGY; 65 is India. If you add an additional 'for' loop below, all the countries can be done at once.
NutfileName <- paste("results/",rgn,climData,"nutOutput.xlsx",sep="")
mat = matrix(ncol = length(yrs), nrow = length(nutNames))
# 
#Set up the lists to be used document all the worksheets
sheetNameList <- ("Sheet names")
sheetNameDesc <- ("Description of sheet contents")

#create a worksheet with info on creator, date, model version, etc.
creationInfo <- ("Information on creator, date, model version, etc.")
creationInfo <- rbind(creationInfo, paste("Creator:", userName))
creationInfo <- rbind(creationInfo, paste("Date of file creation:", dateCreated))

addWorksheet(wb, sheetName="creationInfo")
writeData(wb, creationInfo, sheet="creationInfo", startRow=1, startCol=1, rowNames = FALSE, colNames = FALSE)
sheetNameList <- rbind(sheetNameList,"creationInfo")
sheetNameDesc <- rbind(sheetNameDesc,"Information on creator, date, model version, etc.")

#create a worksheet with info on the nutrients
addWorksheet(wb, sheetName="metadataNutrients")
writeData(wb, nutrientNames, sheet="metadataNutrients", startRow=1, startCol=1, rowNames = FALSE)
sheetNameList <- rbind(sheetNameList,"metadataNutrients")
sheetNameDesc <- rbind(sheetNameDesc,"metadata on nutrient composition of each commodity.")
#setColWidths(wb, sheet="metadataNutrients", cols = 1:3, widths="auto")

#create a worksheet with info on the commodities, food groups and RDAs
addWorksheet(wb, sheetName="metadataCommodities")
writeData(wb, commodityNames, sheet="metadataCommodities", startRow=1, startCol=1)
#setColWidths(wb, sheet="metadataCommodities", cols = 1:3, widths="auto")
sheetNameList <- rbind(sheetNameList,"metadataCommodities")
sheetNameDesc <- rbind(sheetNameDesc,
                   "Metadata on commodities, RDAs and what commodities are in each food group. Note that the second row indicates whether a commodity is included in the IMPACT model.")

#create a worksheet with info on the regions
addWorksheet(wb, sheetName="metadataRegions")
sheetNameList <- rbind(sheetNameList,"metadataRegions")
sheetNameDesc <- rbind(sheetNameDesc,"metadata on regions")
writeData(wb, IMPACTregions, sheet="metadataRegions", startRow=1, startCol=1, rowNames = FALSE)
addStyle(wb, sheet="metadataRegions", 
         style=textStyle, 
         rows = 1:nrow(IMPACTregions), 
         cols = 1:ncol(IMPACTregions), gridExpand = TRUE)
#setColWidths(wb, sheet="metadataRegions", cols = 1:ncol(IMPACTregions), widths="auto")

addWorksheet(wb, sheetName="metadataRDAs")
sheetNameList <- rbind(sheetNameList,"metadataRDAs")
sheetNameDesc <- rbind(sheetNameDesc,"Metadata on RDA for men and women, 31-50; units are the same as the nutrients metadata")
writeData(wb, RDAs, sheet="metadataRDAs", startRow=1, startCol=1, rowNames = FALSE)
addStyle(wb, sheet="metadataRDAs", style=numStyle, rows = 2:nrow(RDAs)+1, cols=2:ncol(RDAs), gridExpand = TRUE)
#setColWidths(wb, sheet="metadataRDAs", cols = 1:ncol(RDAs), widths="auto")

#create worksheet with daily consumption of all the commodities
pcCons <- paste(rgn,"PcCons",sep=".")
addWorksheet(wb, sheetName=pcCons)

temp <- commods[commods$region == rgn,]
temp[,3:length(temp)] <- temp[,3:length(temp)]/daysInYear
writeData(wb, temp, sheet=pcCons, startRow=1, startCol=1, rowNames = FALSE, colNames = TRUE)
addStyle(wb, sheet=pcCons, style=numStyle3, rows = 1:nrow(temp)+1, cols=3:ncol(temp), gridExpand = TRUE)

sheetNameList <- rbind(sheetNameList,paste(rgn,"pcCons"))
sheetNameDesc <- rbind(sheetNameDesc,"daily per capita consumption of IMPACT commodities (kgs)")

#loop over all the categories; creating a matrix for each and then writing to the worksheet
catList <- c("allFoodGroups","staples", "cereals", "roots", "fruits", "meats","beverages","eggs","oilSeeds","vegetables","vegeOil")
for (cat in catList ) {
  #first calulate the total for each nutrient for each category 
  for (yrCntr in 1:length(yrs)) {
    tempYrs <- (yrs[yrCntr])
    mat[,yrCntr]<- f.nutSum(rgn,cat,tempYrs)
  }
  row.names(mat)<-nutNames
  colnames(mat)<-yrs
  if(cat=="allFoodGroups"){
    mat.all <- mat
  }
  tempSheetName<- paste(rgn,cat,sep=".")
  sheetNameList <- rbind(sheetNameList, tempSheetName)
  sheetNameDesc <- rbind(sheetNameDesc, "daily per capita consumption of nutrients; units in the nutrients metadata worksheet")
  
  addWorksheet(wb, sheetName=tempSheetName)
  writeData(wb, mat, sheet=tempSheetName, startRow=1, startCol=1, rowNames = TRUE)
  addStyle(wb, sheet=tempSheetName, style=numStyle, rows = 2:(nrow(mat)+1), cols=2:(ncol(mat)+1), gridExpand = TRUE)
  #now calculate the share of total coming from the commodities in each category and put into a matrix
  #eval(parse) makes it possible to convert a text string to a variable name. Paste creates the variable name; 
  #as.numeric makes sure the matrix contains numeric values
  #mat.all^(-1) inverts each element of the mat.all matrix
  mat <- mat * mat.all^(-1)
  #matrixName <- paste("mat.",cat,".share",sep="")
  tempSheetName  <- paste(rgn,cat,"share",sep=".")
  addWorksheet(wb, sheetName=tempSheetName)
  sheetNameList <- rbind(sheetNameList, tempSheetName)
  sheetNameDesc <- rbind(sheetNameDesc, "share of daily per capita consumption of nutrients from food group")
  writeData(wb, mat, sheet=tempSheetName, startRow=1, startCol=1, rowNames = TRUE)
  addStyle(wb, sheet=tempSheetName, style=shareStyle, rows = 2:(nrow(mat)+1), cols=2:(ncol(mat)+1), gridExpand = TRUE)
  assign(tempSheetName,mat)
}
#calculate the share of RDAs being met by the nutrient intakes
#nutrientNames <- read.xlsx(nutrientNamesFile, colNames = TRUE, sheet = 1)

tempMat.all<- as.data.frame(mat.all)
tempMat.all$code <- row.names(tempMat.all)
tempMat.all<- merge (tempMat.all,RDAs, by = "code")
RDAMen <- data.frame(matrix(0, ncol = ncol(tempMat.all)-2, nrow = nrow(tempMat.all)))
colnames(RDAMen)<- colnames(tempMat.all[1:47])
RDAMen[1] <- tempMat.all[1]
RDAWomen <- RDAMen
for (i in 1:length(colnames(tempMat.all[1:46]))) {
  tempColName <- colnames(tempMat.all[i+1])
  RDAMen[tempColName] <- tempMat.all[tempColName]/tempMat.all$male_31_50
  RDAWomen[tempColName] <- tempMat.all[tempColName]/tempMat.all$female_31_50
}
sheetNameList <- rbind(sheetNameList, paste(rgn,"RDAMen",sep = "."))
sheetNameDesc <- rbind(sheetNameDesc, "share of recommended daily allowance for men, 31-50")
sheetNameList <- rbind(sheetNameList, paste(rgn,"RDAWomen",sep = "."))
sheetNameDesc <- rbind(sheetNameDesc, "share of recommended daily allowance for women, 31-50")
tempSheetName<- paste(rgn,"RDAMen",sep=".")
addWorksheet(wb, sheetName=tempSheetName)
writeData(wb, RDAMen, sheet=tempSheetName, startRow=1, startCol=1, rowNames = FALSE)
addStyle(wb, sheet=tempSheetName, style=shareStyle, rows = 2:(nrow(RDAMen)+1), cols=2:(ncol(RDAMen)+1), gridExpand = TRUE)
tempSheetName <- paste(rgn,"RDAWomen",sep=".")
addWorksheet(wb, sheetName=tempSheetName)
writeData(wb, RDAWomen, sheet=tempSheetName, startRow=1, startCol=1, rowNames = FALSE)
addStyle(wb, sheet=tempSheetName, style=shareStyle, rows = 2:(nrow(RDAWomen)+1), cols=2:(ncol(RDAWomen)+1), gridExpand = TRUE)

#-----------------------
# calculate the Shannon Wiener diversity index (H)
# H = -SUM[(pi) × ln(pi)], where pi is share of nutrient from ith food group
sumOfShares <- matrix(0,ncol = length(yrs), nrow = length(nutNames))
row.names(sumOfShares)<-nutNames
colnames(sumOfShares)<-yrs
for (cat in c("cereals", "roots", "fruits", "meats","beverages","eggs","oilSeeds","vegetables")) {
  #for (cat in c("cereals", "roots", "fruits", "meats","beverages","eggs","oilSeeds","vegetables","vegeOil")) {
  temp.share <- paste(rgn,cat,"share", sep = ".")
  temp.ln <- paste(rgn,cat,"share.ln", sep = ".")
  temp <- eval(parse(text = paste(rgn,cat,"share", sep = "."))) * 
    log(eval(parse(text = paste(rgn,cat,"share", sep = "."))))
  sumOfShares <- sumOfShares + temp
}
shannonDiversity <- -1*sumOfShares
addWorksheet(wb, sheetName="Shannon Diversity Score")
writeData(wb, shannonDiversity, sheet="Shannon Diversity Score", startRow=1, startCol=1, rowNames = TRUE)
addStyle(wb, sheet="Shannon Diversity Score", style=numStyle, rows = 2:(nrow(shannonDiversity)+1), cols=2:(ncol(shannonDiversity)+1), gridExpand = TRUE)
sheetNameList <- rbind(sheetNameList, "Shannon Diversity Score")
sheetNameDesc <- rbind(sheetNameDesc, "A measure of the diversity of the diet. Can only be calculated if consumption of all commodities is greater than zero.")

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

#---------------------------
#calculate MFAD = (SUM(i to n) *SUM(j to n) * dij)/N
#where dij is the nutritional dissimilarity between commodity i and j 
#Euclidian distance is sqrt(sum((xi - xj) ^ 2))
#N is number of commodities consumed
#clean up the nutrients data frame for this purpose
nutClean <- nutrients[,c(2,7:31)]
nutClean2 <- (nutClean[-grep("0",nutClean$commodity),]) #include only the rows with commodity codes
n<- nutClean2$commodity
nutCleant <- as.data.frame(t(nutClean2[,-1]))
colnames(nutCleant) <- n
nutCleant <- nutCleant[-1, ]
euc.dist <- data.frame()
for (j in 1:ncol(nutCleant)) {
  for (i in 1:ncol(nutCleant)) {
    #  dist(rbind(nutCleant[,1],nutCleant[,i]), method = "euclidean")
    euc.dist[j,i] <- sqrt(sum((nutCleant[,i] - nutCleant[,j]) ^ 2))
  }
}
colnames(euc.dist) <- n
rownames(euc.dist) <- n
wDist <- createWorkbook()
addWorksheet(wDist, sheetName="distance")
writeData(wDist, euc.dist, sheet="distance", startRow=1, startCol=1, rowNames = TRUE, colNames = TRUE)
addStyle(wb, sheet="distance", style=numStyle, rows = 2:(nrow(euc.dist)+1), cols=2:(ncol(euc.dist)+1), gridExpand = TRUE)
saveWorkbook(wDist, "results/euclidDistance.xlsx", overwrite = TRUE)

a