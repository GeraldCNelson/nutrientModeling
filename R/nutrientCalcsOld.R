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
dateDownloaded <- "Date URL downloaded here"
dateCreated <- Sys.Date()

# File names, related info and locations -----------------------------------
source(file="nutrientFunctions.R")
# Load functions for the rest of the script

# Parameter,Description,csv IMPACT parameters,my short name
# FoodAvailability,Food availability per capita (kg per person),FoodAvailXAgg,pcFoodAvail
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

#drop productiontype because it is not used in the demand analysis
drops <- c("productiontype")
t1 <- t1[, !(names(t1) %in% drops)]
#give variables better names
t1$IMPACTparameter <- gsub("PopXAgg -- Population","pop",t1$IMPACTparameter)
t1$IMPACTparameter <- gsub("QDXAgg -- Total Demand","QdTot",t1$IMPACTparameter)
t1$IMPACTparameter <- gsub("QLXAgg -- Livestock Feed Demand","QfeedD",t1$IMPACTparameter)
t1$IMPACTparameter <- gsub("pcGDPXAgg -- Per Capita Income","pcGDP",t1$IMPACTparameter)
t1$IMPACTparameter <- gsub("QFXAgg -- Household Demand","QfoodD",t1$IMPACTparameter)
t1$IMPACTparameter <- gsub("PWXAgg -- World Prices","Pw",t1$IMPACTparameter)
t1$IMPACTparameter <- gsub("FoodAvailXAgg","pcFoodAvail",t1$IMPACTparameter)
t1$IMPACTparameter <- gsub("PerCapKCalXAgg","pcKcal",t1$IMPACTparameter)
t1$IMPACTparameter <- gsub("PopulationAtRiskXagg - Pop at Risk of Hunger","popAtRisk",t1$IMPACTparameter)
t1$IMPACTparameter <- gsub("ShareAtRiskXagg -- Share at Risk of Hunger","shareAtRisk",t1$IMPACTparameter)
t1$IMPACTparameter <- gsub("TotalMalnourishedXagg -- Malnurished Children","totMalnourished",t1$IMPACTparameter)
t1$IMPACTparameter <- gsub("QBFXAgg -- Biofuel Feedstock Demand","bioFuelD",t1$IMPACTparameter)
t1$IMPACTparameter <- gsub("QINTXAgg -- Intermediate Demand","intermedD",t1$IMPACTparameter)
t1$IMPACTparameter <- gsub("PCXAgg -- Consumer Prices","Pc",t1$IMPACTparameter)

#Note to self: still need to aggregate to new regions for IMPACT

#keep only food items
t1.food <- t1[t1$IMPACT_code %in% IMPACTfoodCommodities$IMPACT_code,c("IMPACTparameter","IMPACT_code","scenario","region","year","value")]

#store per cap GDP 
t1.pcGDP <- t1[t1$IMPACTparameter == "pcGDP",c("region","scenario","year","value")]


#set up excel output
source(worksheetCreation.R)

#create a worksheet with info on creator, date, model version, etc.
creationInfo <- ("Information on creator, date, model version, etc.")
creationInfo <- rbind(creationInfo, paste("Creator:", userName))
creationInfo <- rbind(creationInfo, paste("Date of file creation:", dateCreated))
creationInfo <- rbind(creationInfo, paste("IMPACT data:", IMPACTfileName))
creationInfo <- rbind(creationInfo, paste("Nutrient data:", nutrientFileName))
creationInfo <- rbind(creationInfo, paste("EAR data:", EARFileName))

addWorksheet(wb, sheetName="creation_Info")
writeData(wb, creationInfo, sheet="creation_Info", startRow=1, startCol=1, rowNames = FALSE, colNames = FALSE)
wbInfo[(nrow(wbInfo)+1),] <- c("creation_Info", "Information on creator, date, model version, etc.")

#create a worksheet with info on the regions
addWorksheet(wb, sheetName="metadataRegions")
wbInfo[(nrow(wbInfo)+1),] <- c("=HYPERLINK(#metadataRegions!A1,\"metadataRegions\")", "metadata on regions.")
writeData(wb, IMPACTregions, sheet="metadataRegions", startRow=1, startCol=1, rowNames = FALSE)
addStyle(wb, sheet="metadataRegions", 
         style=textStyle, 
         rows = 1:nrow(IMPACTregions), 
         cols = 1:ncol(IMPACTregions), gridExpand = TRUE)
#setColWidths(wb, sheet="metadataRegions", cols = 1:ncol(IMPACTregions), widths="auto")

#create a worksheet with info on the commodities and nutrients
addWorksheet(wb, sheetName="metadataFoodCommods")
#commodityNames <- cbind(nutrients[c("Name","IMPACT_code")])
writeData(wb, nutrients, sheet="metadataFoodCommods", startRow=1, startCol=1)
#setColWidths(wb, sheet="metadataCommodities", cols = 1:3, widths="auto")
wbInfo[(nrow(wbInfo)+1),] <- c("=HYPERLINK(#metadataFoodCommods!A1,\"metadataFoodCommods\")", "Metadata on commodities and their nutrient makeup.")

#create a worksheet with info on all the nutrients

addWorksheet(wb, sheetName="metadataNutrients")
writeData(wb, nutrientNames_Units, sheet="metadataNutrients", startRow=1, startCol=1, rowNames = FALSE,colNames = FALSE)
wbInfo[(nrow(wbInfo)+1),] <- c("=HYPERLINK(#metadataNutrients!A1,\"metadataNutrients\")", 
                               paste("Metadata on nutrient names and units, from the file ", nutrientFileName,sep=""))

addWorksheet(wb, sheetName="metadataEARs")
wbInfo[(nrow(wbInfo)+1),] <- c("=HYPERLINK(#metadataEARs!A1,\"metadataEARs\")", 
                               paste("Metadata on EARs; units are the same as the nutrients metadata."))

writeData(wb, EARs, sheet="metadataEARs", startRow=1, startCol=1, rowNames = FALSE)
addStyle(wb, sheet="metadataEARs", style=numStyle, rows = 2:nrow(EARs)+1, cols=2:ncol(EARs), gridExpand = TRUE)
#setColWidths(wb, sheet="metadataEARs", cols = 1:ncol(EARs), widths="auto")

# Loop over scenario and region -------------------------------------------

for (i in c( "SSP2-NoCC", "SSP2-GFDL", "SSP2-MIROC")) {
  scen.t1.food <- t1.food[t1.food$scenario == i,c("IMPACTparameter","IMPACT_code","region","year","value")]
  scen.pcGDP.IMPACT <- t1.pcGDP[t1.pcGDP$scenario == i,c("region","year","value")]
  scen.pcGDP.IMPACT.wide <- dcast(scen.pcGDP.IMPACT, region ~ year, value.var = "value")
  colnames(scen.pcGDP.IMPACT.wide) <- make.names(colnames(scen.pcGDP.IMPACT.wide))
  
  #world prices
  Pw <- scen.t1.food[scen.t1.food$IMPACTparameter == "Pw", c("year","IMPACT_code","value")]
  Pw.wide <- dcast(Pw,IMPACT_code ~ year, mean, value.var = "value"); colnames(Pw.wide) <- make.names(colnames(Pw.wide))
  shtName <- paste("Pw_",i,sep="")
  addWorksheet(wb, sheetName=shtName)
  writeData(wb, Pw.wide, sheet=shtName, startRow=1, startCol=1, rowNames = FALSE, colNames = TRUE)
  hyperLinkVar <- paste('=HYPERLINK(',shtName,'!A1, \"',shtName,'\")', sep="")
  descVar <- paste("World prices, (2005 USD ppp per mt), scenario", i)
  wbInfo[(nrow(wbInfo)+1),] <- c(hyperLinkVar, descVar)

  # metric: income share of food expenditures -------------------------------
  
  ## calculate how much is spent per day on IMPACT commodities (budget) and its share of per capita income (incomeShare), 
  ## using domestic and world prices
  
  budget.Pw <- budget.Pc <- incomeShare.Pw <- incomeShare.Pc <- 
    data.frame(matrix(vector(), 0, ncol(Pw.wide), dimnames=NULL), stringsAsFactors=F)

colnames(budget.Pw) <- colnames(budget.Pc) <- colnames(incomeShare.Pw) <- colnames(incomeShare.Pc) <- c("region",colnames(Pw.wide[2:ncol(Pw.wide)]))

  for (j in 1:length(ctyNames)) { #j is the row number of regions 
    
    #create per cap consumption df with no extraneous columns and make sure all food commodities included
    tmp.food.percap <- scen.t1.food[scen.t1.food$IMPACTparameter == "pcFoodAvail" & 
                                      scen.t1.food$region == ctyNames[j], c("IMPACT_code","year","value")]
    tmp.food.percap.wide <- dcast(tmp.food.percap, IMPACT_code ~ year, mean, value.var = "value"); colnames(tmp.food.percap.wide) <- make.names(colnames(tmp.food.percap.wide))
    tmp.food.percap.wide <- merge(tmp.food.percap.wide,IMPACTfoodCommodities, by = "IMPACT_code", all.y = TRUE); tmp.food.percap.wide[is.na(tmp.food.percap.wide)] <- 0
    
    #domestic consumer prices with no extraneous columns and make sure all food commodities included
    tmp.Pc <- scen.t1.food[scen.t1.food$IMPACTparameter == "Pc" & 
                             scen.t1.food$region == ctyNames[j], c("IMPACT_code","year","value")]
    tmp.Pc.wide <- dcast(tmp.Pc,IMPACT_code ~ year, mean, value.var = "value")
    tmp.Pc.wide <- merge(tmp.Pc.wide,IMPACTfoodCommodities, by = "IMPACT_code", all.y = TRUE); 
    colnames(tmp.Pc.wide) <- make.names(colnames(tmp.Pc.wide))
    tmp.Pc.wide[is.na(tmp.Pc.wide)] <- 0
 
    budget.Pc[j,1] <- budget.Pw[j,1] <- incomeShare.Pc[j,1] <- incomeShare.Pw[j,1] <- as.character(ctyNames[j])
    
    for (k in 2:ncol(Pw.wide)) { # k is years; starts at 2, col1 is IMPACT_code
      #commodities in tmp are in kg/person per year
      budget.Pw[j,k] <- sum(tmp.food.percap.wide[k] * Pw.wide[k])/365/1000
      budget.Pc[j,k] <- sum(tmp.food.percap.wide[k] * tmp.Pc.wide[k])/365/1000
      #PcGDP is in $1000/mt; convert to $1 and get average daily income
      incomeShare.Pw[j,k] <- budget.Pw[j,k] / ((scen.pcGDP.IMPACT.wide[j,k] * 1000)/365)
      incomeShare.Pc[j,k] <- budget.Pc[j,k] / ((scen.pcGDP.IMPACT.wide[j,k] * 1000)/365)
    } # end of loop to calculate budget cost and income share for a region
  } #end of loop over regions
  
  #write results to the spreadsheet
  shtName <- paste("Budget Pw",i,sep="")
  addWorksheet(wb, sheetName=shtName)
  writeData(wb, budget.Pw, sheet=shtName, startRow=1, startCol=1, rowNames = FALSE, colNames = TRUE)
  hyperLinkVar <- paste('=HYPERLINK(#',shtName,'!A1, \"',shtName,'\")', sep="")
  descVar <- paste("World prices, (2005 USD ppp per mt), scenario", i)
  wbInfo[(nrow(wbInfo)+1),] <- c(hyperLinkVar, descVar)
  
  shtName <- paste("Budget Pc",i,sep="")
  addWorksheet(wb, sheetName=shtName)
  writeData(wb, budget.Pc, sheet=shtName, startRow=1, startCol=1, rowNames = FALSE, colNames = TRUE)
  hyperLinkVar <- paste('=HYPERLINK(#',shtName,'!A1, \"',shtName,'\")', sep="")
  descVar <- paste("Expenditures on IMPACT commodities at domestic prices, (2005 USD ppp per day), scenario", i)
  wbInfo[(nrow(wbInfo)+1),] <- c(hyperLinkVar, descVar)
  
  shtName <- paste("IncomeShare Pw",i,sep="")
  addWorksheet(wb, sheetName=shtName)
  writeData(wb, incomeShare.Pw, sheet=shtName, startRow=1, startCol=1, rowNames = FALSE, colNames = TRUE)
  hyperLinkVar <- paste('=HYPERLINK(#',shtName,'!A1, \"',shtName,'\")', sep="")
  descVar <- paste("Income share of expenditures on IMPACT commodities at world prices, (2005 USD ppp per day), scenario", i)
  wbInfo[(nrow(wbInfo)+1),] <- c(hyperLinkVar, descVar)
  
  shtName <- paste("IncomeShare Pc",i,sep="")
  addWorksheet(wb, sheetName=shtName)
  writeData(wb, incomeShare.Pc, sheet=shtName, startRow=1, startCol=1, rowNames = FALSE, colNames = TRUE)
  hyperLinkVar <- paste('=HYPERLINK(#',shtName,'!A1, \"',shtName,'\")', sep="")
  descVar <- paste("Income share of expenditures on IMPACT commodities at domestic prices, (2005 USD ppp per day), scenario", i)
  wbInfo[(nrow(wbInfo)+1),] <- c(hyperLinkVar, descVar)
  
  } #end of loop over scenarios

#convert wbInfo sheet_Name column to class hyperlink
class(wbInfo$sheet_Name) <- 'hyperlink'
#add sheet with info about each of the worksheets
addWorksheet(wb, sheetName="sheetInfo")
writeData(wb, wbInfo, sheet="sheetInfo", startRow=1, startCol=1, rowNames = FALSE, colNames = FALSE)
addStyle(wb, sheet="sheetInfo", style=textStyle, rows = 1:nrow(wbInfo), cols=1:(ncol(wbInfo)), gridExpand = TRUE)
setColWidths(wb, sheet="sheetInfo", cols = 1:2, widths=20)

#move sheetInfo worksheet from the last to the first
temp<- 2:length(names(wb))-1
temp <- c(length(names(wb)),temp)
worksheetOrder(wb) <- temp
saveWorkbook(wb, xcelOutFileName, overwrite = TRUE)

# metric: Share of EAR consumed for the nutrients in nutCodes -------------

# # #Read in the RDA data
# RDAFile <- "RDAs.xlsx"
# RDAs <- read.xlsx(RDAFile, colNames = TRUE,sheet = 1, startRow = 1)
# RDAs <- RDAs[,2:4]

#test function inputs
# rgn <- ctyNames[2]
# yr <- "X2010"
# cat <- "allFoodGroups"

f.regionCmdty <- function(rgn, scen)
{
  #The arguments to this function are a region from the t1.food data frame
  #and a scenario from the list of scenario choices
  #Currently, it just reads in a 3 letter country code from the cty column
  #Its output is a data frame with per capita nutrition data for the given region and scenario
  if (!(rgn %in% ctyNames)) {
    #error checking. At the moment it just checks for typos.
    stop(paste(rgn, " is not in the list of countries in the IMPACT output file"))
  }
  t1.food[(t1.food$scenario == scen) & 
            (t1.food$region == rgn) &
            (t1.food$IMPACTparameter == "pcFoodAvail"),
          c("IMPACT_code","year","value")]
}

f.nutSum <-function(rgn, scen,cat,yr) {
  #function to generate data frame that for a given scenario, region, food group and yr has the sum of each nutrient from a category of food stuffs
  #rgn is a 3 letter country name
  #cat is a list of food categories
  #yr is a string that is the name of a column in rgn
  
  #create an empty vector to which the nutrient sum data are appended
  vecTemp<-vector()
  
  #make sure that nutTemp has all IMPACT commodities even if consumption is zero in rgn. 
  #It will also have all nutrients.
  nutTemp <- merge(f.regionCmdty(rgn, scen),nutrients, by = "IMPACT_code", all.x=TRUE )
  nutTempRgn <- subset(nutTemp,nutTemp$food.group.code %in% eval(parse(text = cat)))
  nutTempRgn <- nutTemp[nutTemp$food.group.code %in% eval(parse(text = cat)),
                        c("food.group.code","name","category")]
  nutTempRgn[is.na(nutTempRgn)] <- 0
  
  # The if statement checks to see whether the rgn (AGR, USA, etc.) has values for the cat (staples, etc.) of commodities
  if ("TRUE" %in% (nutTempRgn$category %in% eval(parse(text = cat)))){
    #loop through all nutrients
    for (nutCntr in 1:length(nutCodes)) {
      # sum of nutrient per capita per day (assumes 365 days in year) from all commodities
      nutSumTemp <- sum(nutTempRgn[,yr] * nutTempRgn[nutCodes[nutCntr]]) / daysInYear
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


#create the spreadsheet for a country, add worksheets for metadata and put in the results folder of the working directory


wb <- createWorkbook()
#choose the region
rgn <- ctyNames[65] #151 is USA; 43 is EGY; 65 is India. If you add an additional 'for' loop below, all the countries can be done at once.
NutfileName <- paste("results/",rgn,climData,"nutOutput.xlsx",sep="")
mat = matrix(ncol = length(yrs), nrow = length(nutCodes))


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
catList <- c("allFoodGroups","staples", "beverages", "cereals", "dairy", "eggs","oils","fish",
              "fruits", "meats", "oilSeeds","roots","vegetables")
for (cat in catList ) {
  #first calulate the total for each nutrient for each category 
  for (yrCntr in 1:length(yrs)) {
    tempYrs <- (yrs[yrCntr])
    mat[,yrCntr]<- f.nutSum(rgn,cat,tempYrs)
  }
  row.names(mat)<-nutCodes
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
#calculate the share of EARs being met by the nutrient intakes

tempMat.all<- as.data.frame(mat.all)
tempMat.all$code <- row.names(tempMat.all)
tempMat.all<- merge (tempMat.all,EARs, by = "code")
EARMen <- data.frame(matrix(0, ncol = ncol(tempMat.all)-2, nrow = nrow(tempMat.all)))
colnames(EARMen)<- colnames(tempMat.all[1:47])
EARMen[1] <- tempMat.all[1]
EARWomen <- EARMen
for (i in 1:length(colnames(tempMat.all[1:46]))) {
  tempColName <- colnames(tempMat.all[i+1])
  EARMen[tempColName] <- tempMat.all[tempColName]/tempMat.all$male_31_50
  EARWomen[tempColName] <- tempMat.all[tempColName]/tempMat.all$female_31_50
}
sheetNameList <- rbind(sheetNameList, paste(rgn,"EARMen",sep = "."))
sheetNameDesc <- rbind(sheetNameDesc, "share of recommended daily allowance for men, 31-50")
sheetNameList <- rbind(sheetNameList, paste(rgn,"EARWomen",sep = "."))
sheetNameDesc <- rbind(sheetNameDesc, "share of recommended daily allowance for women, 31-50")
tempSheetName<- paste(rgn,"EARMen",sep=".")
addWorksheet(wb, sheetName=tempSheetName)
writeData(wb, EARMen, sheet=tempSheetName, startRow=1, startCol=1, rowNames = FALSE)
addStyle(wb, sheet=tempSheetName, style=shareStyle, rows = 2:(nrow(EARMen)+1), cols=2:(ncol(EARMen)+1), gridExpand = TRUE)
tempSheetName <- paste(rgn,"EARWomen",sep=".")
addWorksheet(wb, sheetName=tempSheetName)
writeData(wb, EARWomen, sheet=tempSheetName, startRow=1, startCol=1, rowNames = FALSE)
addStyle(wb, sheet=tempSheetName, style=shareStyle, rows = 2:(nrow(EARWomen)+1), cols=2:(ncol(EARWomen)+1), gridExpand = TRUE)

#-----------------------
# calculate the Shannon Wiener diversity index (H)
# H = -SUM[(pi) × ln(pi)], where pi is share of nutrient from ith food group
sumOfShares <- matrix(0,ncol = length(yrs), nrow = length(nutCodes))
row.names(sumOfShares)<-nutCodes
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



# metric: MFAD ------------------------------------------------------------

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