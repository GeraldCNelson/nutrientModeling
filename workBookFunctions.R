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
  xcelOutFileName <- paste("results/nutVals_",nut.name,Sys.Date(),".xlsx",sep="")
  
  saveWorkbook(wb, xcelOutFileName, overwrite = TRUE)
}

f.write.nut.sheet <- function(nutdf,wb) { 
  #nutdf contains rows for one scenario, food.group,code, and nutrient, all regions and all years
  #wb is the spreadsheet file set up in workSheetCreation.R
  #wbInfo is used to create the metadata sheet
  shtName <- paste(unique(nutdf$scenario),
                   unique(nutdf$food.group.code),
                   unique(nutdf$nutrient), sep="_")
  shtName <- substr(shtName,1,31) #sheetnames in xls must be <= 31
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