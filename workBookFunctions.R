#function to generate hyperlinks
f.hyperlink <- function(sheetName,shtDesc) {
  #here's what I need tmp to look like =HYPERLINK("#'metadataRegions'!A1","test")
  part1 <- '=HYPERLINK("#'
  part2 <- paste("'",sheetName,"'",'!A1","',shtDesc,'")', sep = "")
  tmp <- paste(part1, part2, sep="")
}

f.finalizeWB <- function(wb,wbInf) {
  #add sheet with info about each of the worksheets
  addWorksheet(wb, sheetName="sheetInfo")
  writeData(wb, wbInf, sheet="sheetInfo", startRow=1, startCol=1, rowNames = FALSE, colNames = FALSE)
  addStyle(wb, sheet="sheetInfo", style=textStyle, rows = 1:nrow(wbInf), cols=1:(ncol(wbInf)), gridExpand = TRUE)
  setColWidths(wb, sheet="sheetInfo", cols = 1:2, widths=20)
  
  #move sheetInfo worksheet from the last to the first
  temp<- 2:length(names(wb))-1
  temp <- c(length(names(wb)),temp)
  worksheetOrder(wb) <- temp
  
  saveWorkbook(wb, xcelOutFileName, overwrite = TRUE)
}

f.write.nut.sheet <- function(temp,wb, wbInf) { 
  #temp contains rows for one scenario, food.group,code, and nutrient, all regions and all years
  #wb is the spreadsheet file set up in workSheetCreation.R
  #wbInfo is used to create the metadata sheet
  shtName <- paste(unique(temp$scenario),
                   unique(temp$food.group.code),
                   unique(temp$nutrient), sep="_")
  shtName <- substr(shtName,1,31) #sheetnames in xls must be <= 31
  temp.wide <- spread(temp[,c("region","year","value")], year,value)
  addWorksheet(wb, sheetName=shtName)
  writeData(wb, temp.wide, sheet=shtName, startRow=1, startCol=1, rowNames = FALSE, colNames = TRUE)
  # the <<- structure below is supposed to allow one to access a global variable such as wbInfo
  wbInf[(nrow(wbInf)+1),] <- c(shtName,
                               paste("Average daily consumption of",
                                     (unique(temp$nutrient)), "in scenario",
                                     (unique(temp$scenario)), "and food group",
                                     (unique(temp$food.group.code)), sep = " "))
  return(wbInf)
}  