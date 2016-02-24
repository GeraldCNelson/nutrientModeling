#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, nutrient data, IMPACT food commodities nutrient lookup
# Intro -------------------------------------------------------------------
#' @description
#' # Intro -------------------------------------------------------------------
#' This script contains functions to generate regional aggregations from country data and writes them out to
#' @param regions.rds - Created by dataPrep.regions.R It contains
#' @param regions.all - a data frame that has region info for SSP, IMPACT3, and IMPACT 115
#' #' - all
#' This uses 3 digit country codes are based on the ISO 3166 standard
#' The output is three sets of country codes
#' @param regions.ISO - all the 3 digit codes that the ISO as defined.
#' @param regions.SSP - all the 3 digit codes used in the SSP scenarios.
#' @param regions.IMPACT3 - all the ISO codes used by IMPACT3, including those in regional aggregates
#' @param regions.IMPACT3.plus - all the ISO codes used by IMPACT3 that are in regional aggregates, along with the name of the regional aggregate
#' @param regions.IMPACT115 - all the ISO codes used in the 115 region version of IMPACT, including those in regional aggregates
#' @param regions.IMPACT115.plus - all the ISO codes used in the 115 region version of IMPACT that are in regional aggregates, along with the name of the regional aggregate
#naming conventions
#ISO_code - a 3 letter ISO code
#country_name - a descriptive name for the country
#region - a 3 letter code that describes a group of countries that is made up of one or more country codes
#region_members - one or more country codes that make up the region
#region_name - a descriptive name for the region. Identical to the country name for regions that have only one country
# IMPACT 3 includes all the countries in the ISO list, either individually or in a plus region
# SSP doesn't include the 56 countries in missingList.SSP. These are bunch of really small things.
#Copyright (C) 2015 Gerald C,Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, See the
#     GNU General Public License for more details at http://www.gnu.org/licenses/.
source(file = "R/setup.R")

# get the SSP data and regions lookup info
#' @param dt.SSP
dt.SSP <- getNewestVersion("SSP")
setkey(dt.SSP, ISO_code)
#' @param dt.regions.all - regions lookup table as data.table
dt.regions.all <- as.data.table(getNewestVersion("regions.all"))
setkey(dt.regions.all, ISO_code)

#' @param dt.SSP.new - SSP data with regions from the regions.all lookup table
dt.SSP.new <- dt.SSP[dt.regions.all]

#regions not included in SSP have missing values for scenarios, etc.
temp <- dt.SSP.new[is.na(scenario),]
missingList.SSP <- sort(unique(temp$scenario))

#Remove those rows
dt.SSP.new <- dt.SSP.new[!is.na(scenario),]

keyVariables <- c("scenario",
                  "region_code.IMPACT3")

setkeyv(dt.SSP.new,keyVariables)
byList <- c("scenario",
            "region_code.IMPACT3")

dt.SSP.IMPACT3 <-
  dt.SSP.new[, lapply(.SD, sum), by = key(dt.SSP.new),
             .SDcols = keepYearList]

# get the differences in country codes
# with the complete set of ISO codes to IMPACT3
setdiff(regions.IMPACT3.region_name.IMPACT3.all,
        regions.ISO$ISO_code)
setdiff(regions.ISO$ISO_code,
        regions.IMPACT3.region_name.IMPACT3.all)

# with the complete set of ISO codes to SSP
setdiff(regions.SSP$region, regions.ISO$ISO_code)
setdiff(regions.ISO$ISO_code, regions.SSP$region)

# with the complete set of ISO codes to IMPACT115
setdiff(IMPACT115region_name.IMPACT3codes, regions.ISO$ISO_code)
setdiff(regions.ISO$ISO_code, IMPACT115region_name.IMPACT3codes)

#set row numbers to be sequential
rownames(regions.SSP) <-
  rownames(regions.IMPACT115) <- rownames(regions.ISO) <-
  rownames(regions.IMPACT3) <-  NULL

