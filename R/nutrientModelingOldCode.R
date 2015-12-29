# -- the code below is from an earlier version. 

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