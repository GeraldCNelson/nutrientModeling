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