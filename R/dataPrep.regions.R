#' @author Gerald C. Nelson, \email{nelson.gerald.c@gmail.com}
#' @keywords utilities, region alignment
# Intro -------------------------------------------------------------------
#' @description
#' # Intro -------------------------------------------------------------------
#' This script contains functions to align regional aggregations from country data and writes them out to
#' @param regions.all.rds It contains
#' @param ISO_code - official ISO 3 digit code for all countries - 249 countries
#' @param region_code.SSP - the SSP region for each ISO code (either an ISO 3 code or NA) - 194 countries
#' @param region_code.IMPACT115 - the 3 digit IMPACT115 region code for each ISO code
#' @param region_name.IMPACT115 - the name of the IMPACT115 region for each ISO code
#' @param region_code.IMPACT3 - the 3 digit IMPACT3 region code for each ISO code
#' @param region_name.IMPACT3 - the name of the IMPACT3 region for each ISO code
#' @param regions.IMPACT115 - all 115 regions in the 115 region version of IMPACT
#' @param regions.IMPACT115.plus - the 20 regions in the 115 region version of IMPACT that are aggregates of individual 'countries'
#' @param regions.IMPACT3 - all 157 regions in the IMPACT3 version of IMPACT (2015)
#' @param regions.IMPACT3.plus - the 20 regions in the IMPACT3 version of IMPACT that are aggregates of individual 'countries'
#' @param regions.all - a data frame that has region info for SSP, IMPACT3, and IMPACT 115
#' The 3 digit country codes are based on the ISO 3166 standard, accessed Nov 2015.
#' @source \url{http://en.wikipedia.org/wiki/ISO_3166-1_alpha-3}

# naming conventions
#ISO_code - a 3 letter ISO code
#country_name - a descriptive name for the country
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
source(file = "R/dataPrep.setup.R")

# create regions.ISO by reading in all ISO country codes ----
#' @param regions.ISO - ISO codes
regions.ISO <- read.xlsx(ISOCodes)
colnames(regions.ISO) <- c("ISO_code", "country_name.ISO")

# Create regions.IMPACT3.plus ------
#For small countries and other political units, IMPACT has created regions that are essentially
# the largest political unit and one or more smaller political units
#regions.IMPACT3.plus is all the regions larger than a single political unit and what political units are included
regions.IMPACT3.plus <- data.frame(
  region_code.IMPACT3 = character(0),
  region_members = character(0),
  region_name.IMPACT3 = character(0),
  stringsAsFactors = FALSE
)

plusBuilding <- function(region_code.IMPACT, lst, region_title) {
  data.frame(region_code.IMPACT, lst, regionname, stringsAsFactors = FALSE)
}

#' @param region_code.IMPACT3 - temporary variable to hold countries that make up a region
region_code.IMPACT3 <- "BLT"
lst <- c("EST", "LTU", "LVA")
regionname <- "Baltic States"
regions.IMPACT3.plus <-
  rbind(regions.IMPACT3.plus,
        plusBuilding(region_code.IMPACT3, lst, regionname))
#txt <- c("BLT Baltic States is Estonia EST, Lithuania LTU, Latvia LVA")

region_code.IMPACT3 <- "BLX"
lst <- c("BEL", "LUX")
regionname <- "Belgium-Luxembourg"
regions.IMPACT3.plus <-
  rbind(regions.IMPACT3.plus,
        plusBuilding(region_code.IMPACT3, lst, regionname))
#txt <- c("BLX Belgium-Luxembourg is Belgium BEL, Luxembourg LUX")

region_code.IMPACT3 <- "CHM"
lst <- c("CHN", "HKG", "MAC", "TWN")
regionname <- "China plus"
regions.IMPACT3.plus <-
  rbind(regions.IMPACT3.plus,
        plusBuilding(region_code.IMPACT3, lst, regionname))
#txt <- c("CHM China plus is China CHN, Hong Kong HKG, Macao MAC, Taiwan TWN")

region_code.IMPACT3 <- "CHP"
lst <- c("CHE", "LIE")
regionname <- "Switzerland plus"
regions.IMPACT3.plus <-
  rbind(regions.IMPACT3.plus,
        plusBuilding(region_code.IMPACT3, lst, regionname))
#txt <- c("CHP Switzerland plus is Switzerland CHE Liechtenstein LIE")

region_code.IMPACT3 <- "CRB"
lst <-
  c(
    "ABW",
    "AIA",
    "ATG",
    "BES",
    "BHS",
    "BLM",
    "BRB",
    "CUW",
    "CYM",
    "DMA",
    "GLP",
    "GRD",
    "KNA",
    "LCA",
    "MAF",
    "MSR",
    "MTQ",
    "PRI",
    "SXM",
    "TCA",
    "TTO",
    "VCT",
    "VGB",
    "VIR"
  )
regionname <- "Other Caribbean"
regions.IMPACT3.plus <-
  rbind(regions.IMPACT3.plus,
        plusBuilding(region_code.IMPACT3, lst, regionname))
#txt <- c("CRB Other Caribbean is Aruba ABW, Anguilla AIA, Netherlands Antilles (obsolete) ANT, Antigua ATG
# Bonaire, Sint Eustatius, and Saba BES, Bahamas BHS, St,Barthélemy BLM, Barbados BRB, Curacao CUW, Cayman Islands CYM
# Dominica DMA, Guadeloupe GLP, Grenada GRD, St,Kitts and Nevis KNA, St,Lucia LCA, Saint Martin MAF
# Montserrat MSR, Martinique MTQ, Puerto Rico PRI, Sint Maarten SXM, Turks and Caicos Islands TCA
#Trinidad and Tobago TTO, St,Vincent and Grenadines VCT, British Virgin Islands VGB, U.S,Virgin Islands VIR")
#ANT dropped from this list

# region_code.IMPACT3 <-  "DNP"
# lst <- c("DNK", "GRL")
# regionname <- "Denmark plus"
# regions.IMPACT3.plus <-
#   rbind(regions.IMPACT3.plus,
#         plusBuilding(region_code.IMPACT3, lst, regionname))
# #txt <- c("DNP Denmark plus is DNK Denmark GRL Greenland")

region_code.IMPACT3 <- "FNP"
lst <- c("ALA", "FIN")
regionname <- "Finland plus"
regions.IMPACT3.plus <-
  rbind(regions.IMPACT3.plus,
        plusBuilding(region_code.IMPACT3, lst, regionname))
#txt <- c("FNP Finland plus is Aland Islands ALA Finland FIN")

region_code.IMPACT3 <- "FRP"
lst <- c("FRA", "MCO")
regionname <- "France plus"
regions.IMPACT3.plus <-
  rbind(regions.IMPACT3.plus,
        plusBuilding(region_code.IMPACT3, lst, regionname))
#txt <- c("FRP France plus is France FRA Monaco MCO")

region_code.IMPACT3 <- "GSA"
lst <- c("GUF", "GUY", "SUR")
regionname <- "Guyanas"
regions.IMPACT3.plus <-
  rbind(regions.IMPACT3.plus,
        plusBuilding(region_code.IMPACT3, lst, regionname))
#txt <- c("GSA Guyanas is South America French Guiana GUF Guyana GUY Suriname SUR")

region_code.IMPACT3 <- "ITP"
lst <- c("ITA", "MLT", "SMR", "VAT")
regionname <- "Italy plus"
regions.IMPACT3.plus <-
  rbind(regions.IMPACT3.plus,
        plusBuilding(region_code.IMPACT3, lst, regionname))
#txt <- c("ITP Italy plus is Italy ITA Malta MLT San Marino SMR Vatican City VAT")

region_code.IMPACT3 <- "MOR"
lst <- c("MAR", "ESH")
regionname <- "Morocco plus"
regions.IMPACT3.plus <-
  rbind(regions.IMPACT3.plus,
        plusBuilding(region_code.IMPACT3, lst, regionname))
#txt <- c("MOR Morocco plus is Morocco MAR Western Sahara ESH")

region_code.IMPACT3 <- "OAO"
# Antartic (ATA) added to this list
lst <-
  c("ATA",
    "BMU",
    "BVT",
    "CPV",
    "FLK",
    "FRO",
    "SGS",
    "SHN",
    "SJM",
    "SPM",
    "STP")
regionname <-  "Other Atlantic Ocean"
regions.IMPACT3.plus <-
  rbind(regions.IMPACT3.plus,
        plusBuilding(region_code.IMPACT3, lst, regionname))
# txt <- c("OAO Other Atlantic Ocean is Bermuda BMU Bouvet Island BVT Cape Verde CPV Falkland Islands FLK Faroe Islands FRO
# South Georgia and South Sandwich Islands SGS Saint Helena, Ascension, and Tristan de Cunha SHN
# Svalbard and Jan Mayen SJM Saint Pierre and Miquelon SPM Sao Tome and Principe STP")

region_code.IMPACT3 <- "OBN"
lst <- c("BIH", "MKD", "MNE", "SRB")
regionname <- "Other Balkans"
regions.IMPACT3.plus <-
  rbind(regions.IMPACT3.plus,
        plusBuilding(region_code.IMPACT3, lst, regionname))
#txt <- c("OBN Other Balkans is Bosnia-Herzegovina BIH Macedonia (FYR) MKD Montenegro MNE Serbia SRB")

region_code.IMPACT3 <- "OIO"
lst <-
  c("ATF",
    "CCK",
    "COM",
    "CXR",
    "HMD",
    "IOT",
    "MDV",
    "MUS",
    "MYT",
    "REU",
    "SYC")
regionname <- "Other Indian Ocean"
regions.IMPACT3.plus <-
  rbind(regions.IMPACT3.plus,
        plusBuilding(region_code.IMPACT3, lst, regionname))
#txt <- c("OIO Other Indian Ocean is Southern Territories ATF Keeling Islands CCK Comoros COM Christmas Island CXR
# Heard and McDonald Islands HMD British Indian Ocean Territory IOT Maldives MDV Mauritius MUS
# Mayotte MYT Réunion REU Seychelles SYC")
#CXR deleted from this list

region_code.IMPACT3 <- "OPO"
lst <-
  c(
    "ASM",
    "COK",
    "FSM",
    "GUM",
    "KIR",
    "MHL",
    "MNP",
    "NCL",
    "NFK",
    "NIU",
    "NRU",
    "PCN",
    "PLW",
    "PYF",
    "TKL",
    "TON",
    "TUV",
    "UMI",
    "WLF",
    "WSM"
  )
regionname <- "Other Pacific Ocean"
regions.IMPACT3.plus <-
  rbind(regions.IMPACT3.plus,
        plusBuilding(region_code.IMPACT3, lst, regionname))
# txt <- c("OPO Other Pacific Ocean is American Samoa ASM Cook Islands COK Micronesia FSM Guam GUM
#  Kiribati KIR Marshall Islands MHL Northern Mariana Islands MNP New Caledonia NCL Norfolk Island NFK
#  Niue NIU Nauru NRU Pitcairn PCN Palau PLW French Polynesia PYF Tokelau TKL Tonga TON Tuvalu TUV
#  Minor Outlying Islands UMI Wallis and Futuna WLF Samoa WSM")

region_code.IMPACT3 <- "OSA"
lst <- c("BRN", "SGP")
regionname <- "Other Southeast Asia"
regions.IMPACT3.plus <-
  rbind(regions.IMPACT3.plus,
        plusBuilding(region_code.IMPACT3, lst, regionname))
#txt <- c("OSA OtherSoutheast Asia is Brunei BRN Singapore SGP")

region_code.IMPACT3 <- "RAP"
lst <- c("ARE", "BHR", "KWT", "OMN", "QAT")
regionname <- "Rest of Arab Peninsula"
regions.IMPACT3.plus <-
  rbind(regions.IMPACT3.plus,
        plusBuilding(region_code.IMPACT3, lst, regionname))
#txt <- c("RAP  Rest of Arab Peninsula is United Arab Emirates ARE
# Bahrain BHR Kuwait KWT Oman OMN Qatar QAT")

region_code.IMPACT3 <- "SDP"
lst <- c("SSD", "SDN")
regionname <- "Sudan plus"
regions.IMPACT3.plus <-
  rbind(regions.IMPACT3.plus,
        plusBuilding(region_code.IMPACT3, lst, regionname))
#txt <- c("SDP Sudan plus is SSD Sudan SDN South Sudan")

region_code.IMPACT3 <- "SPP"
lst <- c("AND", "ESP", "GIB")
regionname <- "Spain plus"
regions.IMPACT3.plus <-
  rbind(regions.IMPACT3.plus,
        plusBuilding(region_code.IMPACT3, lst, regionname))
#txt <- c("SPP  Spain plus is Andorra AND Spain ESP Gibraltar GIB")

region_code.IMPACT3 <- "UKP"
lst <- c("GBR", "GGY", "IMN")
regionname <- "Great Britain plus"
regions.IMPACT3.plus <-
  rbind(regions.IMPACT3.plus,
        plusBuilding(region_code.IMPACT3, lst, regionname))
#txt <- c("UKP  Great Britain plus is Great Britain GBR Guernsey GGY Isle of Man IMN Jersey JEY")

colnames(regions.IMPACT3.plus) <-
  c("region_code.IMPACT3", "ISO_code", "region_name.IMPACT3")

# Create regions.IMPACT3 ----
# IMPACT3regions variable is created in dataPrep.setup.R. All regions in IMPACT version 3.
# The next lines of code get a list of IMPACT 3 regions that are not in IMPACT3.plus
regions.IMPACT3 <- read.xlsx(IMPACT3regions)
colnames(regions.IMPACT3) <-
  c("region_code.IMPACT3", "region_name.IMPACT3")
#' @param regions.IMPACT3.region_name.IMPACT3 regions in IMPACT3 that are only one country
regions.IMPACT3.cty <-
  regions.IMPACT3[!regions.IMPACT3$region_code.IMPACT3 %in% regions.IMPACT3.plus$region_code.IMPACT3, ]
regions.IMPACT3.cty$ISO_code <-
  regions.IMPACT3.cty$region_code.IMPACT3
regions.IMPACT3 <- rbind(regions.IMPACT3.cty, regions.IMPACT3.plus)
regions.IMPACT3 <-
  regions.IMPACT3[order(regions.IMPACT3$ISO_code),]


# Create regions.IMPACT115 and regions.IMPACT115.plus ----

regions115Lookup <- read.xlsx(
  IMPACTfish,
  sheet = "IMPACT 115 Regions",
  cols = 1:4,
  rows = 3:118,
  colNames = TRUE
)

# Break apart the mappingCode variable into its individual countries ---
regions.IMPACT115 <-
  as.data.frame(cSplit(
    regions115Lookup,
    'mappingCode',
    sep = ".",
    type.convert = FALSE
  ))

regions.IMPACT115$mappingCode_2 <-
  substring(regions.IMPACT115$mappingCode_2,
            2,
            nchar(regions.IMPACT115$mappingCode_2) - 1)
colnames(regions.IMPACT115) <-
  c("region_code.IMPACT115",
    "description",
    "mappingCode_1",
    "region_members")
regions.IMPACT115 <-
  regions.IMPACT115[, c("region_code.IMPACT115", "description", "region_members")]
temp1 <-
  stri_split_fixed(regions.IMPACT115$description, ";", simplify = TRUE)
colnames(temp1) <- c("region_name.IMPACT115", "region_members")
#get rid of extra "s (double quotes)
temp1 <- gsub('"', "", temp1[, 1:2])
#get rid of extra spaces
temp1 <- gsub(" ", "", temp1[, 1:2])
regions.IMPACT115 <-
  as.data.frame(cbind(temp1[, "region_name.IMPACT115"],
                      regions.IMPACT115[, c("region_code.IMPACT115", "region_members")],
                      stringsAsFactors = FALSE))
colnames(regions.IMPACT115) <-
  c("region_name.IMPACT115",
    "region_code.IMPACT115",
    "region_members")

temp <- data.frame(
  region_code.IMPACT115 = character(0),
  lst1 = character(0),
  region_name.IMPACT115 = character(0),
  stringsAsFactors = FALSE
)

for (i in 1:nrow(regions.IMPACT115)) {
  lst1 <-
    unlist(strsplit(regions.IMPACT115[i, "region_members"], ","))
  regionname <- regions.IMPACT115[i, "region_name.IMPACT115"]
  regioncode <- regions.IMPACT115[i, "region_code.IMPACT115"]
  temp <- rbind(temp, plusBuilding(regioncode, lst1, regionname))
}
regions.IMPACT115 <- temp
colnames(regions.IMPACT115) <-
  c("region_code.IMPACT115", "ISO_code", "region_name.IMPACT115")
regions.IMPACT <-
  merge(regions.IMPACT115, regions.IMPACT3, by = "ISO_code")

dt.SSP <- getNewestVersion("SSPPopClean")

#' @param regions.SSP SSP regions
regions.SSP <-
  as.data.frame(sort(unique(dt.SSP$ISO_code)), stringsAsFactors = FALSE) #there are 194 regions
regions.SSP[, 2] <- regions.SSP[, 1]
colnames(regions.SSP) <- c("ISO_code", "region_code.SSP")
#' @param regions.all - lookup table for all regions (IMPACT3, IMPACT115, SSP)
regions.all.ISO <- merge(regions.ISO,regions.IMPACT, by = "ISO_code", all = TRUE)
regions.all.SSP <-
  merge(regions.SSP, regions.all.ISO, by = "ISO_code", all = TRUE)

# add IMPACT3 standard world regions ----

IMPACTstdRegions <- read.xlsx(IMPACTstdRegions)
colnames(IMPACTstdRegions) <- c("region_code.IMPACT3","region_code.IMPACTstandard","region_name.IMPACTstandard","region_name.IMPACT3")
IMPACTstdRegions <- IMPACTstdRegions[,c("region_code.IMPACT3","region_code.IMPACTstandard")]

IMPACT.world.regions.lookup <- data.frame(region_code.IMPACTstandard =
                                       c("EAP","EUR","FSU","LAC","MEN","NAM","SAS","SSA"),
                                     region_name.IMPACTstandard =
                                       c("East Asia and Pacific","Europe","Former Soviet Union","Latin America and Caribbean",
                                         "Middle East and North Africa","North America","South Asia","Africa south of the Sahara"))
regions.IMPACTworld <- merge(IMPACTstdRegions,IMPACT.world.regions.lookup, by = "region_code.IMPACTstandard", all = TRUE)

# create regions.all ----
regions.all <-
  merge(regions.all.SSP, regions.IMPACTworld, by = "region_code.IMPACT3", all = TRUE)

# Read in the worksheet that has the FAO country code-ISO country name lookup
FBSNameLookup <- read.xlsx(FAOCountryNameCodeLookup,
                                            sheet = 1,
                                            startRow = 1,
                                            colNames = TRUE)

#convert to character and leave just ISO code and FAOSTAT code
colKeeplist <- c("ISO3","FAOSTAT")
FBSNameLookup <- FBSNameLookup[,colKeeplist]
FBSNameLookup$FAOSTAT <- as.character(FBSNameLookup$FAOSTAT)

#remove a peskey 'country' JEY (Jersey)
regions.all <- regions.all[!regions.all$ISO_code %in% "JEY",]
regions.all <- regions.all[order(regions.all$ISO_code),]

#rearrange the column order
regions.all <- regions.all[c(
  "ISO_code",
  "region_code.SSP",
  "region_code.IMPACT115",
  "region_code.IMPACT3",
  "region_code.IMPACTstandard",
  "country_name.ISO",
  "region_name.IMPACT115",
  "region_name.IMPACT3",
  "region_name.IMPACTstandard")]

saveRDS(regions.all,
        paste(mData, "/regions.all.", Sys.Date(), ".rds", sep = ""))

write.xlsx(
  regions.all,
  paste(mData, "/regions.all.", Sys.Date(), ".xlsx", sep = ""),
  colWidths = "auto",colNames = TRUE)
 
