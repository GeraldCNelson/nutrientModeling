# A 'subroutine' of nutrientCalcs.R
# Creates a populates several worksheets of an excel file that holds modeling results
wb <- createWorkbook()
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
#function to generate hyperlinks, needs more work
hypersheet.fn <- function(shtName,shtDesc) {
  tmp <- paste('HYPERLINK("#',shtName,'!A1"', '","), "',shtDesc,'"',sep="")
}
#Set up a dataframe to document all the worksheets. It will become the first worksheet in the workbook
wbInfo <- data.frame(sheet_Name=character(),sheet_Desc=character(), stringsAsFactors = FALSE)

wbInfo[(nrow(wbInfo)+1),] <- c("Sheet names", "Description of sheet contents")

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

#create a worksheet with info on the nutrients
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