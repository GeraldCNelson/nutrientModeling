# Intro -------------------------------------------------------------------
#This script contains functions to generate regional aggregations from country data and writes them out to
#data/regions.RData. It contains
# regions.IMPACT115 - all 115 regions in the 115 region version of IMPACT
# regions.IMPACT115.plus - the 20 regions in the 115 region version of IMPACT that are aggregates of individual 'countries'
# regions.IMPACT3 - all 157 regions in the IMPACT3 version of IMPACT (2015)
# regions.IMPACT3.plus -  - the 20 regions in the IMPACT3 version of IMPACT that are aggregates of individual 'countries'
# regions.ISO - the 249 'countries' in the ISO 3 digit codes at http://en.wikipedia.org/wiki/ISO_3166-1_alpha-3
# regions.SSP - the 194 'countries' that have SSP data
#All of the region files, except .ISO contain the variables region_code (a three letter code)
#This uses 3 digit country codes are based on the ISO 3166 standard,See http://en.wikipedia.org/wiki/ISO_3166-1_alpha-3
#The output is three sets of country codes
#regions.ISO - all the 3 digit codes that the ISO as defined, as of Nov 2015. The file name variable is created in the setup.R script
#regions.SSP - all the 3 digit codes used in the SSP scenarios as of Nov 2015. The file name variable is created in the setup.R script

#regions.IMPACT3 - all the ISO codes used by IMPACT3, including those in regional aggregates
#regions.IMPACT3.plus - all the ISO codes used by IMPACT3 that are in regional aggregates, along with the name of the regional aggregate
#regions.IMPACT115 - all the ISO codes used in the 115 region version of IMPACT, including those in regional aggregates
#regions.IMPACT115.plus - all the ISO codes used in the 115 region version of IMPACT that are in regional aggregates, along with the name of the regional aggregate
#naming conventions
#country_code - a 3 letter ISO code
#country_name - a descriptive name for the country
#region_code - a 3 letter code that describes a group of countries that is made up of one or more country codes
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
setwd("~/Documents/workspace/nutrientModeling")
source(file = "setup.R")

# create regions.ISO read in all ISO country codes ----
regions.ISO <- read.xlsx(ISOctyCodes)
colnames(regions.ISO) <- c("country_code", "country_name")

# Create regions.IMPACT3.plus ------
#For small countries and other political units, IMPACT has created regions that are essentially
# the largest political unit and one or more smaller political units
#regions.IMPACT3.plus is all the regions larger than a single political unit and what political units are included
regions.IMPACT3.plus <- data.frame(
  region_code = character(),
  region_name = character(),
  region_members = character(),
  stringsAsFactors = FALSE
)

CTY <- "BLT"
lst <- c("EST,LTU,LVA")
regionname <- "Baltic States"
regions.IMPACT3.plus <- rbind(
  regions.IMPACT3.plus,
  data.frame(
    region_code = CTY,
    region_name = regionname,
    region_members = lst,
    stringsAsFactors = FALSE
  )
)
# filled <- rbind(empty, data.frame(a = 1, b = factor("abc"), c = "def"))
# regionnamelist(length(regionnamelist) +1) <- regionname
# regionmemberslist[[CTY]] <- lst
#txt <- c("BLT Baltic States is Estonia EST, Lithuania LTU, Latvia LVA")

CTY <- "BLX"
lst <- c("BEL,LUX")
regionname <- "Belgium-Luxembourg"
regions.IMPACT3.plus <- rbind(
  regions.IMPACT3.plus,
  data.frame(
    region_code = CTY,
    region_name = regionname,
    region_members = lst,
    stringsAsFactors = FALSE
  )
)
#txt <- c("BLX Belgium-Luxembourg is Belgium BEL, Luxembourg LUX")

CTY <- "CHM"
lst <- c("CHN,HKG,MAC,TWN")
regionname <- "China plus"
regions.IMPACT3.plus <- rbind(
  regions.IMPACT3.plus,
  data.frame(
    region_code = CTY,
    region_name = regionname,
    region_members = lst,
    stringsAsFactors = FALSE
  )
)
#txt <- c("CHM China plus is China CHN, Hong Kong HKG, Macao MAC, Taiwan TWN")

CTY <- "CHP"
lst <- c("CHE,LIE")
regionname <- "Switzerland plus"
regions.IMPACT3.plus <- rbind(
  regions.IMPACT3.plus,
  data.frame(
    region_code = CTY,
    region_name = regionname,
    region_members = lst,
    stringsAsFactors = FALSE
  )
)
#txt <- c("CHP Switzerland plus is Switzerland CHE Liechtenstein LIE")

CTY <- "CRB"
lst <-
  c(
    "ABW,AIA,ATG,BES,BHS,BLM,BRB,CUW,CYM,DMA,GLP,GRD,KNA,LCA,MAF,MSR,MTQ,PRI,SXM,TCA,TTO,VCT,VGB,VIR"
  )
regionname <- "Other Caribbean"
regions.IMPACT3.plus <- rbind(
  regions.IMPACT3.plus,
  data.frame(
    region_code = CTY,
    region_name = regionname,
    region_members = lst,
    stringsAsFactors = FALSE
  )
)
#txt <- c("CRB Other Caribbean is Aruba ABW, Anguilla AIA, Netherlands Antilles (obsolete) ANT, Antigua ATG
# Bonaire, Sint Eustatius, and Saba BES, Bahamas BHS, St,Barthélemy BLM, Barbados BRB, Curacao CUW, Cayman Islands CYM
# Dominica DMA, Guadeloupe GLP, Grenada GRD, St,Kitts and Nevis KNA, St,Lucia LCA, Saint Martin MAF
# Montserrat MSR, Martinique MTQ, Puerto Rico PRI, Sint Maarten SXM, Turks and Caicos Islands TCA
#Trinidad and Tobago TTO, St,Vincent and Grenadines VCT, British Virgin Islands VGB, U.S,Virgin Islands VIR")
#ANT dropped from this list

CTY <-  "DNP"
lst <- c("DNK,GRL")
regionname <- "Denmark plus"
regions.IMPACT3.plus <- rbind(
  regions.IMPACT3.plus,
  data.frame(
    region_code = CTY,
    region_name = regionname,
    region_members = lst,
    stringsAsFactors = FALSE
  )
)
#txt <- c("DNP Denmark plus is DNK Denmark GRL Greenland")

CTY <- "FNP"
lst <- c("ALA,FIN")
regionname <- "Finland plus"
regions.IMPACT3.plus <- rbind(
  regions.IMPACT3.plus,
  data.frame(
    region_code = CTY,
    region_name = regionname,
    region_members = lst,
    stringsAsFactors = FALSE
  )
)
#txt <- c("FNP Finland plus is Aland Islands ALA Finland FIN")

CTY <- "FRP"
lst <- c("FRA,MCO")
regionname <- "France plus"
regions.IMPACT3.plus <- rbind(
  regions.IMPACT3.plus,
  data.frame(
    region_code = CTY,
    region_name = regionname,
    region_members = lst,
    stringsAsFactors = FALSE
  )
)
#txt <- c("FRP France plus is France FRA Monaco MCO")

CTY <- "GSA"
lst <- c("GUF,GUY,SUR")
regionname <- "Guyanas"
regions.IMPACT3.plus <- rbind(
  regions.IMPACT3.plus,
  data.frame(
    region_code = CTY,
    region_name = regionname,
    region_members = lst,
    stringsAsFactors = FALSE
  )
)
#txt <- c("GSA Guyanas is South America French Guiana GUF Guyana GUY Suriname SUR")

CTY <- "ITP"
lst <- c("ITA,MLT,SMR,VAT")
regionname <- "Italy plus"
regions.IMPACT3.plus <- rbind(
  regions.IMPACT3.plus,
  data.frame(
    region_code = CTY,
    region_name = regionname,
    region_members = lst,
    stringsAsFactors = FALSE
  )
)
#txt <- c("ITP Italy plus is Italy ITA Malta MLT San Marino SMR Vatican City VAT")

CTY <- "MOR"
lst <- c("MAR,ESH")
regionname <- "Morocco plus"
regions.IMPACT3.plus <- rbind(
  regions.IMPACT3.plus,
  data.frame(
    region_code = CTY,
    region_name = regionname,
    region_members = lst,
    stringsAsFactors = FALSE
  )
)
#txt <- c("MOR Morocco plus is Morocco MAR Western Sahara ESH")

CTY <- "OAO"
# Antartic (ATA) added to this list
lst <- c("ATA,BMU,BVT,CPV,FLK,FRO,SGS,SHN,SJM,SPM,STP")
regionname <-  "Other Atlantic Ocean"
regions.IMPACT3.plus <- rbind(
  regions.IMPACT3.plus,
  data.frame(
    region_code = CTY,
    region_name = regionname,
    region_members = lst,
    stringsAsFactors = FALSE
  )
)
# txt <- c("OAO Other Atlantic Ocean is Bermuda BMU Bouvet Island BVT Cape Verde CPV Falkland Islands FLK Faroe Islands FRO
# South Georgia and South Sandwich Islands SGS Saint Helena, Ascension, and Tristan de Cunha SHN
# Svalbard and Jan Mayen SJM Saint Pierre and Miquelon SPM Sao Tome and Principe STP")

CTY <- "OBN"
lst <- c("BIH,MKD,MNE,SRB")
regionname <- "Other Balkans"
regions.IMPACT3.plus <- rbind(
  regions.IMPACT3.plus,
  data.frame(
    region_code = CTY,
    region_name = regionname,
    region_members = lst,
    stringsAsFactors = FALSE
  )
)
#txt <- c("OBN Other Balkans is Bosnia-Herzegovina BIH Macedonia (FYR) MKD Montenegro MNE Serbia SRB")

CTY <- "OIO"
lst <- c("ATF,CCK,COM,CXR,HMD,IOT,MDV,MUS,MYT,REU,SYC")
regionname <- "Other Indian Ocean"
regions.IMPACT3.plus <- rbind(
  regions.IMPACT3.plus,
  data.frame(
    region_code = CTY,
    region_name = regionname,
    region_members = lst,
    stringsAsFactors = FALSE
  )
)
#txt <- c("OIO Other Indian Ocean is Southern Territories ATF Keeling Islands CCK Comoros COM Christmas Island CXR
# Heard and McDonald Islands HMD British Indian Ocean Territory IOT Maldives MDV Mauritius MUS
# Mayotte MYT Réunion REU Seychelles SYC")
#CXR deleted from this list

CTY <- "OPO"
lst <-
  c(
    "ASM,COK,FSM,GUM,KIR,MHL,MNP,NCL,NFK,NIU,NRU,PCN,PLW,PYF,TKL,TON,TUV,UMI,WLF,WSM"
  )
regionname <- "Other Pacific Ocean"
regions.IMPACT3.plus <- rbind(
  regions.IMPACT3.plus,
  data.frame(
    region_code = CTY,
    region_name = regionname,
    region_members = lst,
    stringsAsFactors = FALSE
  )
)
# txt <- c("OPO Other Pacific Ocean is American Samoa ASM Cook Islands COK Micronesia FSM Guam GUM
#  Kiribati KIR Marshall Islands MHL Northern Mariana Islands MNP New Caledonia NCL Norfolk Island NFK
#  Niue NIU Nauru NRU Pitcairn PCN Palau PLW French Polynesia PYF Tokelau TKL Tonga TON Tuvalu TUV
#  Minor Outlying Islands UMI Wallis and Futuna WLF Samoa WSM")

CTY <- "OSA"
lst <- c("BRN,SGP")
regionname <- "Other Southeast Asia"
regions.IMPACT3.plus <- rbind(
  regions.IMPACT3.plus,
  data.frame(
    region_code = CTY,
    region_name = regionname,
    region_members = lst,
    stringsAsFactors = FALSE
  )
)
#txt <- c("OSA OtherSoutheast Asia is Brunei BRN Singapore SGP")

CTY <- "RAP"
lst <- c("ARE,BHR,KWT,OMN,QAT")
regionname <- "Rest of Arab Peninsula"
regions.IMPACT3.plus <- rbind(
  regions.IMPACT3.plus,
  data.frame(
    region_code = CTY,
    region_name = regionname,
    region_members = lst,
    stringsAsFactors = FALSE
  )
)
#txt <- c("RAP  Rest of Arab Peninsula is United Arab Emirates ARE
# Bahrain BHR Kuwait KWT Oman OMN Qatar QAT")

CTY <- "SDP"
lst <- c("SSD,SDN")
regionname <- "Sudan plus"
regions.IMPACT3.plus <- rbind(
  regions.IMPACT3.plus,
  data.frame(
    region_code = CTY,
    region_name = regionname,
    region_members = lst,
    stringsAsFactors = FALSE
  )
)
#txt <- c("SDP Sudan plus is SSD Sudan SDN South Sudan")

CTY <- "SPP"
lst <- c("AND,ESP,GIB")
regionname <- "Spain plus"
regions.IMPACT3.plus <- rbind(
  regions.IMPACT3.plus,
  data.frame(
    region_code = CTY,
    region_name = regionname,
    region_members = lst,
    stringsAsFactors = FALSE
  )
)
#txt <- c("SPP  Spain plus is Andorra AND Spain ESP Gibraltar GIB")

CTY <- "UKP"
lst <- c("GBR,GGY,IMN")
regionname <- "Great Britain plus"
regions.IMPACT3.plus <- rbind(
  regions.IMPACT3.plus,
  data.frame(
    region_code = CTY,
    region_name = regionname,
    region_members = lst,
    stringsAsFactors = FALSE
  )
)
#txt <- c("UKP  Great Britain plus is Great Britain GBR Guernsey GGY Isle of Man IMN Jersey JEY")

# list of countries in IMPACT3.plus
ctyNames.IMPACT3.plus <-
  unlist(regions.IMPACT3.plus$region_members,
         recursive = TRUE,
         use.name = FALSE)

# Create regions.IMPACT3 ----
# IMPACT3Regions is created in setup.R
#get list of IMPACT 3 regions that are not in IMPACT3.plus
temp <-
  IMPACT3Regions[!IMPACT3Regions$region_code %in% regions.IMPACT3.plus$region_code, ]
temp$region_members <- temp$region_code
regions.IMPACT3 <- rbind(temp, regions.IMPACT3.plus)
ctyNames.IMPACT3 <-
  sort(unlist(
    regions.IMPACT3$region_members,
    recursive = TRUE,
    use.name = FALSE
  ))

# Create regions.IMPACT115 and regions.IMPACT115.plus ----

regions115Lookup <- read.xlsx(
  fishInfoIMPACT,
  sheet = "IMPACT 115 Regions",
  cols = 1:4,
  rows = 3:118,
  colNames = TRUE
)

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
# regions.IMPACT115$mappingCode_2 <- gsub("\\(,",regions.IMPACT115$mappingCode_2)
# regions.IMPACT115$mappingCode_2 <- gsub("\\),",regions.IMPACT115$mappingCode_2)
temp1 <- regions.IMPACT115[, c("mappingCode_1", "mappingCode_2")]
colnames(temp1) <- c("region_code", "region_members")
temp2 <- regions.IMPACT115[, c("cty", "description")]
temp3 <- stri_split_fixed(temp2$description, ";", simplify = TRUE)
#get rid of extra "s
temp3 <- gsub('"', "", temp3[, 1:2])
#get rid of extra spaces
temp3 <- gsub(" ", "", temp3[, 1:2])
temp4 <-
  as.data.frame(cbind(temp3[, 1], temp2$cty), stringsAsFactors = FALSE)
colnames(temp4) <- c("region_name", "region_code")
temp5 <- merge(temp4, temp1, all = TRUE)
regions.IMPACT115 <- temp5
regions.IMPACT115$region_code_old <- regions.IMPACT115$region_code
#colnames(regions.IMPACT115) <- c("region_code,description,region_members,region_code_old")
regions.IMPACT115.plus <-
  regions.IMPACT115[grep(",", regions.IMPACT115$region_members), ]
# regionNames <- as.data.frame(cSplit(regions115Lookup, 'description', sep=";", type.convert=FALSE))
# regionNames$description_1 <- gsub('"',"",regionNames$description_1)
# regionNames <- regionNames[,c("cty,description_1")]
# colnames(regionNames) <- c("region_code_old,region_name")

# for (i in 1:nrow(regions.IMPACT115.plus)) {
#   regions.IMPACT115.plus$temp[i] <- as.list(strsplit(regions.IMPACT115.plus$region_members[i],,))
# }
# regions.IMPACT115.plus$region_members <- NULL
# names(regions.IMPACT115.plus)[names(regions.IMPACT115.plus)=="temp"] <- "region_members"

#update regions.IMPACT115 to replace IMPACT cty names with their ISO equivalent
# get regions that are single countries
temp <-
  regions.IMPACT115[grep(",", regions.IMPACT115$region_members, invert =
                           TRUE), ]
#regions.IMPACT115.plus <- regions.IMPACT115[grep(,, regions.IMPACT115$region_members),]
temp$region_code <- temp$region_members
regions.IMPACT115 <- rbind(temp, regions.IMPACT115.plus)

#get all the country codes used in IMPACT115
IMPACT115ctycodes <-
  paste0(regions.IMPACT115$region_members,
         sep = ",",
         collapse = "")
IMPACT115ctycodes <-
  substr(IMPACT115ctycodes, 1, nchar(IMPACT115ctycodes) - 1)
#convert to a real list
IMPACT115ctycodes <-
  sort(colnames(read.csv(text = IMPACT115ctycodes)))


# Create regions.SSP ------
# the data frame regions.SSP, created in SSPPopExtract.R, has a single column. Here we add the second one, which is a duplicate
# of the first but with members as its column name. The RDS file is created in SSPPopExtract.R.
# It only needs to be run when new data are made available.

SSP <- readRDS(file = "data/SSPclean.rds")
# SSP regions
regions.SSP <-
  as.data.frame(sort(unique(SSP$country_code)), stringsAsFactors = FALSE) #there are 194 regions
regions.SSP[, 2] <- regions.SSP[, 1]
colnames(regions.SSP) <- c("region_code", "region_members")

# aggregate SSP data to IMPACT 3 regions -----

dt.SSP <- as.data.table(SSP)
#add new columns for region code and region name
dt.SSP$region_code <- dt.SSP$region_name <- rep("NA", nrow(dt.SSP))
setkey(dt.SSP, country_code)

# would be nice to speed up this for loop
rnumFromGrep <- function(ctycode, lookUpcolumn) {
  return(grep(ctycode, lookUpcolumn))
}

#This is still slow; would be nice to speed up more
#ptm <- proc.time()
for (i in 1:nrow(dt.SSP)) {
  rnum <-
    rnumFromGrep(dt.SSP$country_code[i], regions.IMPACT3$region_members)
  dt.SSP[i, c("region_code", "region_name") := as.list(regions.IMPACT3[rnum, c("region_code", "region_name")])]
}
#proc.time() - ptm
setkey(dt.SSP, "model", "scenario", "variable", "region_code")

dt.SSP.IMPACT3 <-
  dt.SSP[, lapply(.SD, sum), by = c("model", "scenario", "variable", "region_code", "region_name"),
         .SDcols = keepYearList]

# get the differences in country codes
# with the complete set of ISO codes to IMPACT3
setdiff(ctyNames.IMPACT3, regions.ISO$country_code)
setdiff(regions.ISO$country_code, ctyNames.IMPACT3)

# with the complete set of ISO codes to SSP
setdiff(regions.SSP$region_code, regions.ISO$country_code)
setdiff(regions.ISO$country_code, regions.SSP$region_code)

# with the complete set of ISO codes to IMPACT115
setdiff(IMPACT115ctycodes, regions.ISO$country_code)
setdiff(regions.ISO$country_code, IMPACT115ctycodes)

#set row numbers to be sequential
rownames(regions.SSP) <-
  rownames(regions.IMPACT115) <- rownames(regions.ISO) <-
  rownames(regions.IMPACT3) <-  NULL

# Create missingList.SSP, list of missing countries in SSP ----
# merge regions.ISO and .SSP to get to missing countries
temp <-
  merge(
    regions.ISO,
    regions.SSP,
    by.x = "country_code",
    by.y = "region_code",
    all = TRUE
  )
# get list of countries in ISO list but not in SSP
missingList.SSP <- temp[is.na(temp$region_members), ]
missingList.SSP <- missingList.SSP[, c("country_code")]

remove(temp, temp1, temp2, temp3, temp4, temp5)
save(
  "regions.IMPACT115",
  "regions.IMPACT115.plus",
  "regions.IMPACT3",
  "regions.IMPACT3.plus",
  "regions.ISO",
  "regions.SSP",
  "dt.SSP.IMPACT3" ,
  file = "data/regions.RData"
)
