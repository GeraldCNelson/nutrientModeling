# this script holds code that should be executed before anything else.
setwd("~/Documents/workspace/nutrientModeling")

# Make sure required libraries are loaded

require(roxygen2)
require(openxlsx)
#require(entropy)- commented out to check if needed
#require(reshape2) - commented out to check if needed
require(plyr)
require(dplyr)
require(tidyr)
require(data.table)
require(splitstackshape)
require(plotrix)
require(ggplot2)
require(stringi)
library(devtools)


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
  script = c("dataPrep.FBS.R",
             "dataPrep.regions.R",
             "dataPrep.SSP.R")
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

removeOldVersionsIMPACT <- function(fileShortName) {
  # returns a list of all the [fileShortName] files in the iData directory
  regExp <- paste("(?=^", fileShortName, ")(?=.*rds$)", sep = "")
  oldVersionList <- grep(regExp,
                         list.files(iData),
                         value = TRUE,
                         perl = TRUE)
  if (length(oldVersionList) > 0) {
    file.remove(paste(iData, oldVersionList, sep = "/"))
  }
}

removeOldVersions <- function(fileShortName) {
  # returns a list of all the [fileShortName] files in the mData directory
  regExp <- paste("(?=^", fileShortName, ")(?=.*rds$)", sep = "")
  oldVersionList <- grep(regExp,
                         list.files(mData),
                         value = TRUE,
                         perl = TRUE)
  if (length(oldVersionList) > 0) {
    file.remove(paste(mData, oldVersionList, sep = "/"))
  }
}

removeOldVersions.xlsx <- function(fileShortName) {
  # returns a list of all the [fileShortName] files in the mData directory
  regExp <- paste("(?=^", fileShortName, ")(?=.*xlsx$)", sep = "")
  oldVersionList <- grep(regExp,
                         list.files(mData),
                         value = TRUE,
                         perl = TRUE)
  if (length(oldVersionList) > 0) {
    file.remove(paste(mData, oldVersionList, sep = "/"))
  }
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

cleanup <- function(inName, outName) {
  removeOldVersions(outName)
  removeOldVersions.xlsx(outName)
  saveRDS(eval(parse(text = inName)),
          paste(mData, "/",outName,".", Sys.Date(), ".rds", sep = ""))

  write.xlsx(
    x = eval(parse(text = inName)),
    file = paste(mData, "/",outName,".", Sys.Date(), ".xlsx", sep = ""),
    colWidths = "auto",
    colNames = TRUE
  )
}
# Sets the session default for stringsAsFactors as FALSE
options(stringsAsFactors = FALSE)

#' key variables
#' @param keepYearList - list of scenario years to keep
keepYearList <-
  c("X2005",
    "X2010",
    "X2015",
    "X2020",
    "X2025",
    "X2030",
    "X2035",
    "X2040",
    "X2045",
    "X2050")

#' @param keepYearList.FBS - list of FBS years to keep
keepYearList.FBS <- c("X2000", "X2001" ,"X2002", "X2003", "X2004","X2005",
                      "X2006","X2007", "X2008","X2009", "X2010","X2011")

#' @param FBSyearsToAverage - years to average over for base data set
FBSyearsToAverage <- c("X2004", "X2005", "X2006")

#' @param IMPACTfish_code
#' note shrimp, tuna, and salmon are removed in dataManagement.fish.R
#' if fixShrimp is TRUE.
#' This is to deal with the fact that FBS doesn't include individual species
IMPACTfish_code <- c("c_shrimp","c_Crust","c_Mllsc","c_Salmon","c_FrshD",
 "c_Tuna", "c_OPelag", "c_ODmrsl","c_OMarn", "c_FshOil", "c_aqan", "c_aqpl")

#' @param IMPACTalcohol_code
IMPACTalcohol_code <- c("c_wine","c_beer","c_spirits")

#' @param IMPACTfoodCommodList
IMPACTfoodCommodList <-
  sort(
    c(
      "cbeef",
      "cpork",
      "clamb",
      "cpoul",
      "ceggs",
      "cmilk",
      "cbarl",
      "cmaiz",
      "cmill",
      "crice",
      "csorg",
      "cwhea",
      "cocer",
      "ccass",
      "cpota",
      "cswpt",
      "cyams",
      "corat",
      "cbean",
      "cchkp",
      "ccowp",
      "clent",
      "cpigp",
      "copul",
      "cbana",
      "cplnt",
      "csubf",
      "ctemf",
      "cvege",
      "csugr",
      "cgrnd",
      "cgdol",
      "crpsd",
      "crpol",
      "csoyb",
      "csbol",
      "csnfl",
      "csfol",
      "cplol",
      "cpkol",
      "ctols",
      "ctool",
      "ccoco",
      "ccafe",
      "cteas",
      "cothr"
    )
  )
