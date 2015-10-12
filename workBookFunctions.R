#create some common styles
numStyle <- createStyle(numFmt = "0.0")
numStyle3 <- createStyle(numFmt = "0.000")
shareStyle <- createStyle(numFmt = "0.0%")
textStyle <- createStyle(fontName = NULL, fontSize = NULL, fontColour = NULL,
                         numFmt = "GENERAL", border = NULL,
                         borderColour = getOption("openxlsx.borderColour", "black"),
                         borderStyle = getOption("openxlsx.borderStyle", "thin"), bgFill = NULL,
                         fgFill = NULL, halign = "left", valign = NULL, textDecoration = NULL,
                         wrapText = FALSE, textRotation = NULL)

#function to create the first, common worksheets in the results workbook
f.createGeneralWorksheet <- function() {
 

  #Set up a dataframe to collect common worksheet names and descriptions. 
  wbInfoGeneral <- data.frame(sheet_Name=character(),sheet_Desc=character(), stringsAsFactors = FALSE)
  #convert wbInfoGeneral sheet_Name column to class hyperlink
  class(wbInfoGeneral$sheet_Name) <- 'hyperlink'
  wbInfoGeneral[(nrow(wbInfoGeneral)+1),] <- c("Sheet names", "Description of sheet contents")
  
  #create workbook with general info
  wbGeneral <- createWorkbook()
  
  #create a worksheet with info on creator, date, model version, etc.
  creationInfo <- ("Information on creator, date, model version, etc.")
  creationInfo <- rbind(creationInfo, paste("Creator:", userName))
  creationInfo <- rbind(creationInfo, paste("Date of file creation:", Sys.Date()))
  creationInfo <- rbind(creationInfo, paste("IMPACT data:", IMPACTfileName))
  creationInfo <- rbind(creationInfo, paste("Nutrient data:", nutrientFileName))
  creationInfo <- rbind(creationInfo, paste("EAR data:", EARFileName))
  
  addWorksheet(wbGeneral, sheetName="creation_Info")
  writeData(wbGeneral, creationInfo, sheet="creation_Info", startRow=1, startCol=1, rowNames = FALSE, colNames = FALSE)
  wbInfoGeneral[(nrow(wbInfoGeneral)+1),] <- c("creation_Info", "Information on creator, date, model version, etc.")
  
  #create a worksheet with info on the regions
  addWorksheet(wbGeneral, sheetName="metadataRegions")
  wbInfoGeneral[(nrow(wbInfoGeneral)+1),] <- c(f.hyperlink("metadataRegions","metadata on regions."), "Region metadata")
  writeData(wbGeneral, IMPACTregions, sheet="metadataRegions", startRow=1, startCol=1, rowNames = FALSE)
  addStyle(wbGeneral, sheet="metadataRegions", 
           style=textStyle, 
           rows = 1:nrow(IMPACTregions), 
           cols = 1:ncol(IMPACTregions), gridExpand = TRUE)
  #setColWidths(wb, sheet="metadataRegions", cols = 1:ncol(IMPACTregions), widths="auto")
  
  #create a worksheet with info on the commodities and nutrients
  addWorksheet(wbGeneral,sheetName="metadataFoodCommods")
  #commodityNames <- cbind(nutrients[c("Name","IMPACT_code")])
  writeData(wbGeneral, nutrients, sheet="metadataFoodCommods", startRow=1, startCol=1)
  #setColWidths(wb, sheet="metadataCommodities", cols = 1:3, widths="auto")
  wbInfoGeneral[(nrow(wbInfoGeneral)+1),] <- c(f.hyperlink("metadataFoodCommods","metadata on commodities and nutrients"), "Metadata on commodities and their nutrient makeup.")
  
  #create a worksheet with info on the nutrients
  addWorksheet(wbGeneral, sheetName="metadataNutrients")
  writeData(wbGeneral, nutrientNames_Units, sheet="metadataNutrients", startRow=1, startCol=1, rowNames = FALSE,colNames = FALSE)
  wbInfoGeneral[(nrow(wbInfoGeneral)+1),] <- c("=HYPERLINK(#metadataNutrients!A1,\"metadataNutrients\")", 
                                               paste("Metadata on nutrient names and units, from the file ", nutrientFileName,sep=""))
  
  addWorksheet(wbGeneral, sheetName="metadataEARs")
  wbInfoGeneral[(nrow(wbInfoGeneral)+1),] <- c("=HYPERLINK(#metadataEARs!A1,\"metadataEARs\")", 
                                               paste("Metadata on EARs; units are the same as the nutrients metadata. column codes: SSP - classification according to SSP groups, M - Male, F- Female, X - children of both genders, P - pregnant, L - lactating"))
  
  writeData(wbGeneral, EARs, sheet="metadataEARs", startRow=1, startCol=1, rowNames = FALSE)
  addStyle(wbGeneral, sheet="metadataEARs", style=numStyle, rows = 2:nrow(EARs)+1, cols=2:ncol(EARs), gridExpand = TRUE)
  setColWidths(wbGeneral, sheet="metadataEARs", cols = 1:ncol(EARs), widths="auto")
  tmp <- list(wbGeneral, wbInfoGeneral)
  return(tmp)
}

#function to generate hyperlinks
f.hyperlink <- function(sheetName,shtDesc) {
  #here's what I need tmp to look like =HYPERLINK("#'metadataRegions'!A1","test")
  part1 <- '=HYPERLINK("#'
  part2 <- paste("'",sheetName,"'",'!A1","',shtDesc,'")', sep = "")
  tmp <- paste(part1, part2, sep="")
}

f.finalizeWB <- function(wb,wbInf,nut.name) {
  #add sheet with info about each of the worksheets
  addWorksheet(wb, sheetName="sheetInfo")
  writeData(wb, wbInf, sheet="sheetInfo", startRow=1, startCol=1, rowNames = FALSE, colNames = FALSE)
  addStyle(wb, sheet="sheetInfo", style=textStyle, rows = 1:nrow(wbInf), cols=1:(ncol(wbInf)), gridExpand = TRUE)
  setColWidths(wb, sheet="sheetInfo", cols = 1:2, widths=20)
  
  #move sheetInfo worksheet from the last to the first
  temp<- 2:length(names(wb))-1
  temp <- c(length(names(wb)),temp)
  worksheetOrder(wb) <- temp
  xcelOutFileName <- paste("results/",nut.name,Sys.Date(),".xlsx",sep="")
  
  saveWorkbook(wb, xcelOutFileName, overwrite = TRUE)
}

f.write.incShare.sheet <- function(incShare,wb) { 
  #incShare contains rows for one scenario, three budget calcs based on Pw, Pc, and Pcon, all regions and all years
  #wb is the spreadsheet file set up in workSheetCreation.R
  #wbInfo is used to create the metadata sheet
  shtName <- paste("IncShare_",unique(incShare$scenario), sep="")
  shtName <- substr(shtName,1,31) #sheetnames in xls must be <= 31
  incShare.wide <- spread(incShare[,c("region","year","Pcon")], year, Pcon)
  addWorksheet(wb, sheetName=shtName)
  writeData(wb, incShare.wide, sheet=shtName, startRow=1, startCol=1, rowNames = FALSE, colNames = TRUE)
  return(c(shtName,paste("Expenditure on IMPACT commodities as a share to per capita GDP in scenario",
                         (unique(incShare$scenario)), sep = " ")))
}  

f.write.nut.sheet <- function(nutdf,wb) { 
  #nutdf contains rows for one scenario, food.group,code, and nutrient, all regions and all years
  #wb is the spreadsheet file set up in workSheetCreation.R
  #wbInfo is used to create the metadata sheet
  shtName <- paste(unique(nutdf$scenario),unique(nutdf$food.group.code),unique(nutdf$nutrient), sep="_")
  shtName <- substr(shtName,1,31) #sheetnames in xls must be <= 31
  print(shtName)
  nutdf.wide <- spread(nutdf[,c("region","year","value")], year,value)
  addWorksheet(wb, sheetName=shtName)
  writeData(wb, nutdf.wide, sheet=shtName, startRow=1, startCol=1, rowNames = FALSE, colNames = TRUE)
  return(c(shtName,paste("Average daily consumption of",
                         (unique(nutdf$nutrient)), "in scenario",
                         (unique(nutdf$scenario)), "and food group",
                         (unique(nutdf$food.group.code)), sep = " ")))
}  

f.write.nut.sum.sheet <- function(nutdf,wb) { 
  #nutdf contains rows for one scenario, food.group,code, and nutrient, all regions and all years
  #wb is the spreadsheet file set up in workSheetCreation.R
  #wbInfo is used to create the metadata sheet
  shtName <- paste(unique(nutdf$scenario),
                   unique(nutdf$nutrient), sep="_")
  shtName <- substr(shtName,1,31) #sheetnames in xls must be <= 31
  nutdf.wide <- spread(nutdf[,c("region","year","value")], year,value)
  addWorksheet(wb, sheetName=shtName)
  writeData(wb, nutdf.wide, sheet=shtName, startRow=1, startCol=1, rowNames = FALSE, colNames = TRUE)
  # the <<- structure below is supposed to allow one to access a global variable such as wbInfo
 return(c(shtName, paste("Average daily consumption of",
                                     (unique(nutdf$nutrient)), "in scenario",
                                     (unique(nutdf$scenario)), sep = " ")))
}  