# Intro -------------------------------------------------------------------
#This script contains functions to generate spreadsheets of the nutrition requirements results

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

source("setup.R")#create some common styles
numStyle <- createStyle(numFmt = "0.00")
numStyle3 <- createStyle(numFmt = "0.000")
shareStyle <- createStyle(numFmt = "0.0%")
textStyle <-
  createStyle(
    fontName = NULL,
    fontSize = NULL,
    fontColour = NULL,
    numFmt = "GENERAL",
    border = NULL,
    borderColour = getOption("openxlsx.borderColour", "black"),
    borderStyle = getOption("openxlsx.borderStyle", "thin"),
    bgFill = NULL,
    fgFill = NULL,
    halign = "left",
    valign = NULL,
    textDecoration = NULL,
    wrapText = FALSE,
    textRotation = NULL
  )

#function to create the first, common worksheets in the results workbook
f.createGeneralWorksheet <- function() {
  wbGeneral <- createWorkbook()
  
  #create a worksheet with info on creator, date, model version, etc.
  creationInfo <-
    ("Information on creator, date, model version, etc.")
  creationInfo <- rbind(creationInfo, paste("Creator:", userName))
  creationInfo <-
    rbind(creationInfo, paste("Date of file creation:", Sys.Date()))
  creationInfo <-
    rbind(creationInfo, paste("IMPACT data:", IMPACTfileName))
  creationInfo <-
    rbind(creationInfo, paste("Nutrient data:", nutrientFileName))
  creationInfo <-
    rbind(creationInfo,
          paste("Nutrient requirements data:", EARFileName))
  creationInfo <-
    rbind(creationInfo, paste("SSP data:", SSPdataZipFileName))
  addWorksheet(wbGeneral, sheetName = "creation_Info")
  writeData(
    wbGeneral,
    creationInfo,
    sheet = "creation_Info",
    startRow = 1,
    startCol = 1,
    rowNames = FALSE,
    colNames = FALSE
  )
  
  #Set up a dataframe to collect common worksheet names and descriptions.
  wbInfoGeneral <-
    data.frame(
      sheet_Name = character(),
      sheet_Desc = character(),
      stringsAsFactors = FALSE
    )
  
  wbInfoGeneral[1,] <-
    c("Sheet names", "Description of sheet contents")
  wbInfoGeneral[(nrow(wbInfoGeneral) + 1),] <-
    c("creation_Info",
      "Information on creator, date, model version, etc.")
  
  #create a worksheet with info on the regions
  addWorksheet(wbGeneral, sheetName = "metadataRegions")
  wbInfoGeneral[(nrow(wbInfoGeneral) + 1),] <-
    c("metadataRegions", "Region metadata")
  writeData(
    wbGeneral,
    IMPACTregions,
    sheet = "metadataRegions",
    startRow = 1,
    startCol = 1,
    rowNames = FALSE
  )
  addStyle(
    wbGeneral,
    sheet = "metadataRegions",
    style = textStyle,
    rows = 1:nrow(IMPACTregions),
    cols = 1:ncol(IMPACTregions),
    gridExpand = TRUE
  )
  
  #create a worksheet with info on the nutrient sources
  req.metadata <-
    read.xlsx(EARFileName, sheet = 1, colNames = FALSE)
  addWorksheet(wbGeneral, sheetName = "MetaDataNutrnts")
  writeData(
    wbGeneral,
    req.metadata,
    sheet = "MetaDataNutrnts",
    startRow = 1,
    startCol = 1,
    rowNames = FALSE,
    colNames = FALSE
  )
  wbInfoGeneral[(nrow(wbInfoGeneral) + 1),] <-
    c("MetaDataNutrnts",
      "Information about the requirements sources")
  
  
  #create a worksheet with info on the commodities and nutrients
  addWorksheet(wbGeneral, sheetName = "IMPACTCommdlist")
  #commodityNames <- cbind(nutrients[c("Name","IMPACT_code")])
  writeData(
    wbGeneral,
    nutrients,
    sheet = "IMPACTCommdlist",
    startRow = 1,
    startCol = 1
  )
  addStyle(
    wbGeneral,
    sheet = "IMPACTCommdlist",
    style = numStyle,
    rows = 1:nrow(nutrients),
    cols = 2:ncol(eval(parse(text = reqList[i]))),
    gridExpand = TRUE
  )
  
  wbInfoGeneral[(nrow(wbInfoGeneral) + 1),] <-
    c("IMPACTCommdList",
      "IMPACT commodities and their nutrient content")
  
  tmp <- list(wbGeneral, wbInfoGeneral)
  return(tmp)
}

f.hyperlink <- function(sheetName, shtDesc) {
  #here's what I need a link to look like =HYPERLINK("#test!A1","testDescription")
  part1 <- '=HYPERLINK("#'
  part2 <- paste(sheetName, "!A1", '",', sep = "")
  part3 <- paste('"', shtDesc, '")', sep = "")
  hlink <- paste(part1, part2, part3, sep = "")
  return(hlink)
}

f.finalizeWB <- function(wb, wbInf, file.name) {
  #add sheet with info about each of the worksheets
  #the first column is written using writeFormula
  addWorksheet(wb, sheetName = "sheetInfo")
  for (i in 1:nrow(wbInf)) {
    writeFormula(
      wb,
      "sheetInfo",
      x = f.hyperlink(wbInf[i, 1], wbInf[i, 1]),
      startCol = 1,
      startRow = i
    )
  }
  
  writeData(
    wb,
    wbInf[, 2],
    sheet = "sheetInfo",
    startRow = 1,
    startCol = 2,
    rowNames = FALSE,
    colNames = FALSE
  )
  addStyle(
    wb,
    sheet = "sheetInfo",
    style = textStyle,
    rows = 1:nrow(wbInf),
    cols = 1:(ncol(wbInf)),
    gridExpand = TRUE
  )
  setColWidths(wb,
               sheet = "sheetInfo",
               cols = 1:2,
               widths = 20)
  
  #move sheetInfo worksheet from the last to the first
  temp <- 2:length(names(wb)) - 1
  temp <- c(length(names(wb)), temp)
  worksheetOrder(wb) <- temp
  xcelOutFileName <-
    paste("results/", file.name, Sys.Date(), ".xlsx", sep = "")
  
  saveWorkbook(wb, xcelOutFileName, overwrite = TRUE)
}

f.write.incShare.sheet <- function(incShare, wb) {
  #incShare contains rows for one scenario, three budget calcs based on Pw, Pc, and Pcon, all regions and all years
  #wb is the spreadsheet file set up in workSheetCreation.R
  #wbInfo is used to create the metadata sheet
  shtName <- paste("IncShare_", unique(incShare$scenario), sep = "")
  shtName <- substr(shtName, 1, 31) #sheetnames in xls must be <= 31
  incShare.wide <-
    spread(incShare[, c("region", "year", "Pcon")], year, Pcon)
  addWorksheet(wb, sheetName = shtName)
  writeData(
    wb,
    incShare.wide,
    sheet = shtName,
    startRow = 1,
    startCol = 1,
    rowNames = FALSE,
    colNames = TRUE
  )
  return(c(
    shtName,
    paste(
      "Expenditure on IMPACT commodities as a share to per capita GDP in scenario",
      (unique(incShare$scenario)),
      sep = " "
    )
  ))
}

f.write.nut.sheet <- function(nutdf, wb) {
  #nutdf contains rows for one scenario, food.group,code, and nutrient, all regions and all years
  #wb is the spreadsheet file set up in workSheetCreation.R
  #wbInfo is used to create the metadata sheet
  shtName <-
    paste(
      unique(nutdf$scenario),
      unique(nutdf$food.group.code),
      unique(nutdf$nutrient),
      sep = "_"
    )
  shtName <- substr(shtName, 1, 31) #sheetnames in xls must be <= 31
  print(shtName)
  nutdf.wide <-
    spread(nutdf[, c("region", "year", "value")], year, value)
  addWorksheet(wb, sheetName = shtName)
  writeData(
    wb,
    nutdf.wide,
    sheet = shtName,
    startRow = 1,
    startCol = 1,
    rowNames = FALSE,
    colNames = TRUE
  )
  return(c(
    shtName,
    paste(
      "Average daily consumption of",
      (unique(nutdf$nutrient)),
      "in scenario",
      (unique(nutdf$scenario)),
      "and food group",
      (unique(nutdf$food.group.code)),
      sep = " "
    )
  ))
}

f.write.nut.sum.sheet <- function(nutdf, wb) {
  #nutdf contains rows for one scenario, food.group,code, and nutrient, all regions and all years
  #wb is the spreadsheet file set up in workSheetCreation.R
  #wbInfo is used to create the metadata sheet
  shtName <- paste(unique(nutdf$scenario),
                   unique(nutdf$nutrient), sep = "_")
  shtName <- substr(shtName, 1, 31) #sheetnames in xls must be <= 31
  nutdf.wide <-
    spread(nutdf[, c("region", "year", "value")], year, value)
  addWorksheet(wb, sheetName = shtName)
  writeData(
    wb,
    nutdf.wide,
    sheet = shtName,
    startRow = 1,
    startCol = 1,
    rowNames = FALSE,
    colNames = TRUE
  )
  # the <<- structure below is supposed to allow one to access a global variable such as wbInfo
  return(c(
    shtName,
    paste(
      "Average daily consumption of",
      (unique(nutdf$nutrient)),
      "in scenario",
      (unique(nutdf$scenario)),
      sep = " "
    )
  ))
}  