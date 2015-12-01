# Intro -------------------------------------------------------------------
#This script contains functions to generate regional aggregations from country data,
#This uses 3 digit country codes are based on the ISO 3166 standard,See http://en.wikipedia.org/wiki/ISO_3166-1_alpha-3
#The output is three sets of country codes
#regions.ISO - all the 3 digit codes that the ISO as defined, as of Nov 2015. The file name variable is created in the setup.R script
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
source("setup.R")

# read in all ISO country codes
regions.ISO <- read.xlsx(ISOctyCodes) 
colnames(regions.ISO) <- c("country_code","country_name")
#remove CXR (Christmas Island) and ATA (Antartica)
removeList <- c("CXR", "ATA", "JEY")
regions.ISO <- regions.ISO[!regions.ISO$country_code %in% removeList,]

# Create regions.IMPACT3.plus------
#For small countries and other political units, IMPACT has created regions that are essentially 
# the largest political unit and one or more smaller political units
#regions.IMPACT3.plus is all the regions larger than a single political unit and what political units are included
regions.IMPACT3.plus <- data.frame(region_code = character(),
                          region_members = character(),
                          stringsAsFactors=FALSE)
CTY <- "BLT"
lst <- c("EST,LTU,LVA") 
#txt <- c("BLT Baltic States is Estonia EST, Lithuania LTU, Latvia LVA")
regions.IMPACT3.plus[nrow(regions.IMPACT3.plus)+1,] <- c(CTY, lst )

CTY <- "BLX"
lst <- c("BEL,LUX")
#txt <- c("BLX Belgium-Luxembourg is Belgium BEL, Luxembourg LUX")
regions.IMPACT3.plus[nrow(regions.IMPACT3.plus)+1,] <- c(CTY, lst )

CTY <- "CHM" 
lst <- c("CHN,HKG,MAC,TWN") 
#txt <- c("CHM China plus is China CHN, Hong Kong HKG, Macao MAC, Taiwan TWN")
regions.IMPACT3.plus[nrow(regions.IMPACT3.plus)+1,] <- c(CTY, lst )

CTY <- "CHP" 
lst <- c("CHE,LIE") 
#txt <- c("CHP Switzerland plus is Switzerland CHE Liechtenstein LIE")
regions.IMPACT3.plus[nrow(regions.IMPACT3.plus)+1,] <- c(CTY, lst )

CTY <- "CRB" 
lst <- c("ABW,AIA,ATG,BES,BHS,BLM,BRB,CUW,CYM,DMA,GLP,GRD,KNA,LCA,MAF,MSR,MTQ,PRI,SXM,TCA,TTO,VCT,VGB,VIR") 
#txt <- c("CRB Other Caribbean is Aruba ABW, Anguilla AIA, Netherlands Antilles (obsolete) ANT, Antigua ATG
# Bonaire, Sint Eustatius, and Saba BES, Bahamas BHS, St,Barthélemy BLM, Barbados BRB, Curacao CUW, Cayman Islands CYM
# Dominica DMA, Guadeloupe GLP, Grenada GRD, St,Kitts and Nevis KNA, St,Lucia LCA, Saint Martin MAF
# Montserrat MSR, Martinique MTQ, Puerto Rico PRI, Sint Maarten SXM, Turks and Caicos Islands TCA
#Trinidad and Tobago TTO, St,Vincent and Grenadines VCT, British Virgin Islands VGB, U.S,Virgin Islands VIR")
#ANT dropped from this list
regions.IMPACT3.plus[nrow(regions.IMPACT3.plus)+1,] <- c(CTY, lst )

CTY <-  "DNP" 
lst <- c("DNK,GRL") 
#txt <- c("DNP Denmark plus is DNK Denmark GRL Greenland")
regions.IMPACT3.plus[nrow(regions.IMPACT3.plus)+1,] <- c(CTY, lst )

CTY <- "FNP" 
lst <- c("ALA,FIN") 
#txt <- c("FNP Finland plus is Aland Islands ALA Finland FIN")
regions.IMPACT3.plus[nrow(regions.IMPACT3.plus)+1,] <- c(CTY, lst )

CTY <- "FRP" 
lst <- c("FRA,MCO") 
#txt <- c("FRP France plus is France FRA Monaco MCO")
regions.IMPACT3.plus[nrow(regions.IMPACT3.plus)+1,] <- c(CTY, lst )

CTY <- "GSA" 
lst <- c("GUF,GUY,SUR") 
#txt <- c("GSA Guyanas is South America French Guiana GUF Guyana GUY Suriname SUR")
regions.IMPACT3.plus[nrow(regions.IMPACT3.plus)+1,] <- c(CTY, lst )

CTY <- "ITP" 
lst <- c("ITA,MLT,SMR,VAT") 
#txt <- c("ITP Italy plus is Italy ITA Malta MLT San Marino SMR Vatican City VAT")
regions.IMPACT3.plus[nrow(regions.IMPACT3.plus)+1,] <- c(CTY, lst )

CTY <- "MOR" 
lst <- c("MAR,ESH") 
#txt <- c("MOR Morocco plus is Morocco MAR Western Sahara ESH")
regions.IMPACT3.plus[nrow(regions.IMPACT3.plus)+1,] <- c(CTY, lst )

CTY <- "OAO" 
lst <- c("BMU,BVT,CPV,FLK,FRO,SGS,SHN,SJM,SPM,STP")
# txt <- c("OAO Other is Atlantic Ocean Bermuda BMU Bouvet Island BVT Cape Verde CPV Falkland Islands FLK Faroe Islands FRO 
# South Georgia and South Sandwich Islands SGS Saint Helena, Ascension, and Tristan de Cunha SHN
# Svalbard and Jan Mayen SJM Saint Pierre and Miquelon SPM Sao Tome and Principe STP")
regions.IMPACT3.plus[nrow(regions.IMPACT3.plus)+1,] <- c(CTY, lst )

CTY <- "OBN" 
lst <- c("BIH,MKD,MNE,SRB") 
#txt <- c("OBN Other is Balkans Bosnia-Herzegovina BIH Macedonia (FYR) MKD Montenegro MNE Serbia SRB")
regions.IMPACT3.plus[nrow(regions.IMPACT3.plus)+1,] <- c(CTY, lst )

CTY <- "OIO" 
lst <- c("ATF,CCK,COM,HMD,IOT,MDV,MUS,MYT,REU,SYC")
#txt <- c("OIO Other is Indian Ocean Southern Territories ATF Keeling Islands CCK Comoros COM Christmas Island CXR
# Heard and McDonald Islands HMD British Indian Ocean Territory IOT Maldives MDV Mauritius MUS
# Mayotte MYT Réunion REU Seychelles SYC")
#CXR deleted from this list
regions.IMPACT3.plus[nrow(regions.IMPACT3.plus)+1,] <- c(CTY, lst )

CTY <- "OPO" 
lst <- c("ASM,COK,FSM,GUM,KIR,MHL,MNP,NCL,NFK,NIU,NRU,PCN,PLW,PYF,TKL,TON,TUV,UMI,WLF,WSM")
# txt <- c("OPO Other is Pacific Ocean American Samoa ASM Cook Islands COK Micronesia FSM Guam GUM
#  Kiribati KIR Marshall Islands MHL Northern Mariana Islands MNP New Caledonia NCL Norfolk Island NFK
#  Niue NIU Nauru NRU Pitcairn PCN Palau PLW French Polynesia PYF Tokelau TKL Tonga TON Tuvalu TUV
#  Minor Outlying Islands UMI Wallis and Futuna WLF Samoa WSM")
regions.IMPACT3.plus[nrow(regions.IMPACT3.plus)+1,] <- c(CTY, lst )

CTY <- "OSA" 
lst <- c("BRN,SGP") 
#txt <- c("OSA Other is Southeast Asia Brunei BRN Singapore SGP")
regions.IMPACT3.plus[nrow(regions.IMPACT3.plus)+1,] <- c(CTY, lst )

CTY <- "RAP" 
lst <- c("ARE,BHR,KWT,OMN,QAT") 
#txt <- c("RAP  Rest of Arab is Peninsula United Arab Emirates ARE
# Bahrain BHR Kuwait KWT Oman OMN Qatar QAT")
regions.IMPACT3.plus[nrow(regions.IMPACT3.plus)+1,] <- c(CTY, lst )

CTY <- "SDP" 
lst <- c("SSD,SDN") 
#txt <- c("SDP Sudan plus is SSD Sudan SDN South Sudan")
regions.IMPACT3.plus[nrow(regions.IMPACT3.plus)+1,] <- c(CTY, lst )

CTY <- "SPP" 
lst <- c("AND,ESP,GIB") 
#txt <- c("SPP  Spain plus is Andorra AND Spain ESP Gibraltar GIB")
regions.IMPACT3.plus[nrow(regions.IMPACT3.plus)+1,] <- c(CTY, lst )

CTY <- "UKP" 
lst <- c("GBR,GGY,IMN") 
#txt <- c("UKP  Great Britain plus Great Britain GBR Guernsey GGY Isle of Man IMN")
regions.IMPACT3.plus[nrow(regions.IMPACT3.plus)+1,] <- c(CTY, lst )

#create regions.IMPACT3. 
regions.IMPACT3 <- merge(regions.IMPACT3,regions.IMPACT3.plus, by = "region_code", all = TRUE)
regions.IMPACT3$region_members[is.na(regions.IMPACT3$region_members)] <- as.character(regions.IMPACT3$region_code[is.na(regions.IMPACT3$region_members)])
colnames(regions.IMPACT3) <- c("region_code","region_name","region_members")

removeList <- c("CXR", "ATA", "JEY")
regions.IMPACT3 <- regions.IMPACT3[!regions.IMPACT3$region_code %in% removeList,]

# Create regions.IMPACT115 and regions.IMPACT115.plus

regions115Lookup <- read.xlsx(fishInfoIMPACT, 
                           sheet = "IMPACT 115 Regions",
                           cols = 4, rows = 3:118,
                           colNames = TRUE)

regions.IMPACT115 <- as.data.frame(cSplit(regions115Lookup, 'mappingCode', sep=".", type.convert=FALSE))
regions.IMPACT115$mappingCode_2 <- gsub("\\(","",regions.IMPACT115$mappingCode_2)
regions.IMPACT115$mappingCode_2 <- gsub("\\)","",regions.IMPACT115$mappingCode_2)
regions.IMPACT115[,3] <- regions.IMPACT115[,1]
colnames(regions.IMPACT115) <- c("region_code","region_members","region_code_old")
#update regions.IMPACT115 to replace IMPACT cty names with their ISO equivalent
temp <- regions.IMPACT115[grep(",", regions.IMPACT115$region_members, invert=TRUE),]
regions.IMPACT115.plus <- regions.IMPACT115[grep(",", regions.IMPACT115$region_members),]
temp$region_code <- temp$region_members
regions.IMPACT115 <- rbind(temp,regions.IMPACT115.plus)

# this is necessary because region_members are just strings of country codes
removeList <- c("CXR", "ATA", "JEY")
for (i in removeList) {
  regions.IMPACT115$region_members <- gsub(paste(i,",",sep=""),"",regions.IMPACT115$region_members)
}
# Create regions.SSP
# the data frame regions.SSP, created in SSPPopExtract.R, has a single column. Here we add the second one, which is a duplicate
# of the first but with members as its column name. The RDS file is created in SSPPopExtract.R.
# It only needs to be run when new data are made available.

SSP <- readRDS(file="data/SSPclean.rds")
# SSP regions
regions.SSP <- as.data.frame(sort(unique(SSP$region)),stringsAsFactors = FALSE) #there are 194 regions

regions.SSP[,2] <- regions.SSP[,1]
colnames(regions.SSP) <-c("region_code","region_members")
setdiff(regions.SSP$region_code,regions.ISO$country_code) #check to see that SSP has all the countries
setdiff(regions.ISO$country_code,regions.SSP$region_code) #check to see that SSP has all the countries
setdiff(regions.SSP$region_code,regions.IMPACT3$region_code)

#get all the country codes used in IMPACT3
IMPACT3ctycodes <- paste0(regions.IMPACT3$region_members, sep=",", collapse="")
IMPACT3ctycodes<- substr(IMPACT3ctycodes, 1, nchar(IMPACT3ctycodes)-1)
#convert to a real list
IMPACT3ctycodes <- sort(colnames(read.csv(text = IMPACT3ctycodes)))

#get all the country codes used in IMPACT115
IMPACT115ctycodes <- paste0(regions.IMPACT115$region_members, sep=",", collapse="")
IMPACT115ctycodes<- substr(IMPACT115ctycodes, 1, nchar(IMPACT115ctycodes)-1)
#convert to a real list
IMPACT115ctycodes <- sort(colnames(read.csv(text = IMPACT115ctycodes)))

# get the differences in country codes
# with the complete set of ISO codes to IMPACT3
setdiff(IMPACT3ctycodes, regions.ISO$country_code)  
setdiff(regions.ISO$country_code,IMPACT3ctycodes)  

# with the complete set of ISO codes to SSP
setdiff(regions.SSP$region_code, regions.ISO$country_code)  
setdiff(regions.ISO$country_code,regions.SSP$region_code)  

# with the complete set of ISO codes to IMPACT115
setdiff(IMPACT115ctycodes, regions.ISO$country_code)  
setdiff(regions.ISO$country_code,IMPACT115ctycodes)  


