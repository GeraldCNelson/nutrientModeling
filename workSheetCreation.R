# A 'subroutine' of nutrientCalcs.R
# Creates and populates several worksheets of an excel file that holds modeling results
#create styles to format the worksheets
numStyle <- createStyle(numFmt = "0.0")
numStyle3 <- createStyle(numFmt = "0.000")
shareStyle <- createStyle(numFmt = "0.0%")
textStyle <- createStyle(fontName = NULL, fontSize = NULL, fontColour = NULL,
                         numFmt = "GENERAL", border = NULL,
                         borderColour = getOption("openxlsx.borderColour", "black"),
                         borderStyle = getOption("openxlsx.borderStyle", "thin"), bgFill = NULL,
                         fgFill = NULL, halign = "left", valign = NULL, textDecoration = NULL,
                         wrapText = FALSE, textRotation = NULL)

source("workBookFunctions.R")

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
                               paste("Metadata on EARs; units are the same as the nutrients metadata."))

writeData(wbGeneral, EARs, sheet="metadataEARs", startRow=1, startCol=1, rowNames = FALSE)
addStyle(wbGeneral, sheet="metadataEARs", style=numStyle, rows = 2:nrow(EARs)+1, cols=2:ncol(EARs), gridExpand = TRUE)
setColWidths(wbGeneral, sheet="metadataEARs", cols = 1:ncol(EARs), widths="auto")

#spreadsheet to output nutrition quantities
wbNut <- createWorkbook()
wbNut <- wbGeneral
wbInfNut <- wbInfoGeneral
tmp.out <- by(nutShare,nutShare[,c("food.group.code","nutrient","scenario")],f.write.nut.sheet,wbNut)

f.finalizeWB(wbNut,wbInfNut,short.name)

#write out spreadsheet for nutrient consumption summary
wbNutsum <- wbGeneral
wbInfsum <- wbInfoGeneral
tmp.sum.out <- by(nutShareTot,nutShareTot[,c("nutrient","scenario")],f.write.nut.sum.sheet(nutdf = nutShareTot, wb = wbNutsum))
f.finalizeWB(wb = wbNutsum,wbInf = wbInfsum,nut.name = paste(short.name,"Sum",sep="_"))

