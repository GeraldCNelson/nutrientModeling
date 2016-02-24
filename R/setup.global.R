# this script holds code that should be executed before anything else.
setwd("~/Documents/workspace/nutrientModeling")

# Make sure required libraries are loaded

require(roxygen2)
require(openxlsx)
require(entropy)
require(reshape2)
require(plyr)
require(dplyr)
require(tidyr)
require(data.table)
require(splitstackshape)
require(plotrix)
require(ggplot2)
require(stringi)
library(devtools)
library(gdxrrw)

# file locations ----

#' @param rData
rData <- "data-raw"

#' @param mData
mData <- "data"

#' @param iData
iData <- "data/IMPACTData"

#' @param resultsDir
resultsDir <- "results"

# list of the data file types and where they are generated
shortNameList = data.frame(
  name = c("FBS", "regions.all", "SSPPopClean"),
  script = c(
    "dataPrep.FBS.R",
    "dataPrep.regions.R",
    "dataPrep.SSP.R"
  )
)

#' @param getNewestVersion - function to make sure latest version of a data file is loaded
#' and generate it if it is not available
getNewestVersion <- function(fileShortName) {
  # see http://stackoverflow.com/questions/7381641/regex-matching-beginning-and-end-strings
  # for an explanation of this regex expression
  regExp <- paste("(?=^", fileShortName, ")(?=.*rds$)", sep = "")
  filesList <-
    grep(regExp,
         list.files(mData),
         value = TRUE,
         perl = TRUE)
  newestFile <- filesList[length(filesList)]
  return (readRDS(file = paste(mData, newestFile, sep = "/")))
}

getNewestVersionIMPACT <- function(fileShortName) {
  # see http://stackoverflow.com/questions/7381641/regex-matching-beginning-and-end-strings
  # for an explanation of this regex expression
  regExp <- paste("(?=^", fileShortName, ")(?=.*rds$)", sep = "")
  filesList <-
    grep(regExp,
         list.files(iData),
         value = TRUE,
         perl = TRUE)
  newestFile <- filesList[length(filesList)]
  return (readRDS(file = paste(iData, newestFile, sep = "/")))
}

# flag missing files
for (i in length(shortNameList)) {
  filesList <-
    grep(shortNameList$name[i], list.files(mData), value = TRUE)
  if (length(filesList) == 0) {
    rowNumber <- which(grepl(shortNameList$name[i], shortNameList$name))
    print(paste("Missing data file", shortNameList$name[i]))
    print(paste(" run R/", shortNameList$script[rowNumber], sep = ""))
    return()
  }
}
# Sets the session default for stringsAsFactors as FALSE
options(stringsAsFactors = FALSE)
