# Intro -------------------------------------------------------------------
#This script contains functions to generate regional aggregations from country data,
#This uses 3 digit country codes are based on the ISO 3166 standard,See http://en.wikipedia.org/wiki/ISO_3166-1_alpha-3

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

# read in all ISO country codes
regions.ISO <- read.xlsx(ISOctyCodes) 

# First do IMPACT3 regions, called plusREgions------
#For small countries and other political units, IMPACT has created regions that are essentially 
# the largest political unit and one or more smaller political units
#plusRegions are all the regions larger than a single political unit and what political units are included
plusRegions <- data.frame(region = character(),
                          members = character(),
                          region_desc = character(), 
                          stringsAsFactors=FALSE)
ctyNme <- "BLT"
lst <- c("EST,LTU,LVA") 
txt <- c("BLT Baltic States is Estonia EST, Lithuania LTU, Latvia LVA")
plusRegions[nrow(plusRegions)+1,] <- c(ctyNme, lst, txt )

ctyNme <- "BLX"
lst <- c("BEL,LUX")
txt <- c("BLX Belgium-Luxembourg is Belgium BEL, Luxembourg LUX")
plusRegions[nrow(plusRegions)+1,] <- c(ctyNme, lst, txt )

ctyNme <- "CHM" 
lst <- c("CHN,HKG,MAC,TWN") 
txt <- c("CHM China plus is China CHN, Hong Kong HKG, Macao MAC, Taiwan TWN")
plusRegions[nrow(plusRegions)+1,] <- c(ctyNme, lst, txt )

ctyNme <- "CHP" 
lst <- c("CHE,LIE") 
txt <- c("CHP Switzerland plus is Switzerland CHE Liechtenstein LIE")
plusRegions[nrow(plusRegions)+1,] <- c(ctyNme, lst, txt )

ctyNme <- "CRB" 
lst <- c("ABW,AIA,ANT,ATG,BES,BHS,BLM,BRB,CUW,CYM,DMA,GLP,GRD,KNA,LCA,MAF,MSR,MTQ,PRI,SXM,TCA,TTO,VCT,VGB,VIR") 
txt <- c("CRB Other Caribbean is Aruba ABW, Anguilla AIA, Netherlands Antilles (obsolete) ANT, Antigua ATG
 Bonaire, Sint Eustatius, and Saba BES, Bahamas BHS, St,Barthélemy BLM, Barbados BRB, Curacao CUW, Cayman Islands CYM
 Dominica DMA, Guadeloupe GLP, Grenada GRD, St,Kitts and Nevis KNA, St,Lucia LCA, Saint Martin MAF
 Montserrat MSR, Martinique MTQ, Puerto Rico PRI, Sint Maarten SXM, Turks and Caicos Islands TCA
Trinidad and Tobago TTO, St,Vincent and Grenadines VCT, British Virgin Islands VGB, U.S,Virgin Islands VIR")
plusRegions[nrow(plusRegions)+1,] <- c(ctyNme, lst, txt )

ctyNme <-  "DNP" 
lst <- c("DNK,GRL") 
txt <- c("DNP Denmark plus is DNK Denmark GRL Greenland")
plusRegions[nrow(plusRegions)+1,] <- c(ctyNme, lst, txt )

ctyNme <- "FNP" 
lst <- c("ALA,FIN") 
txt <- c("FNP Finland plus is Aland Islands ALA Finland FIN")
plusRegions[nrow(plusRegions)+1,] <- c(ctyNme, lst, txt )

ctyNme <- "FRP" 
lst <- c("FRA,MCO") 
txt <- c("FRP France plus is France FRA Monaco MCO")
plusRegions[nrow(plusRegions)+1,] <- c(ctyNme, lst, txt )

ctyNme <- "GSA" 
lst <- c("GUF,GUY,SUR") 
txt <- c("GSA Guyanas is South America French Guiana GUF Guyana GUY Suriname SUR")
plusRegions[nrow(plusRegions)+1,] <- c(ctyNme, lst, txt )

ctyNme <- "ITP" 
lst <- c("ITA,MLT,SMR,VAT") 
txt <- c("ITP Italy plus is Italy ITA Malta MLT San Marino SMR Vatican City VAT")
plusRegions[nrow(plusRegions)+1,] <- c(ctyNme, lst, txt )

ctyNme <- "MOR" 
lst <- c("MAR,ESH") 
txt <- c("MOR Morocco plus is Morocco MAR Western Sahara ESH")
plusRegions[nrow(plusRegions)+1,] <- c(ctyNme, lst, txt )

ctyNme <- "OAO" 
lst <- c("BMU,BVT,CPV,FLK,FRO,SGS,SHN,SJM,SPM,STP,BIH")
txt <- c("OAO Other is Atlantic Ocean Bermuda BMU Bouvet Island BVT Cape Verde CPV Falkland Islands FLK Faroe Islands FRO 
South Georgia and South Sandwich Islands SGS Saint Helena, Ascension, and Tristan de Cunha SHN
Svalbard and Jan Mayen SJM Saint Pierre and Miquelon SPM Sao Tome and Principe STP")
plusRegions[nrow(plusRegions)+1,] <- c(ctyNme, lst, txt )

ctyNme <- "OBN" 
lst <- c("BIH,MKD,MNE,SRB") 
txt <- c("OBN Other is Balkans Bosnia-Herzegovina BIH Macedonia (FYR) MKD Montenegro MNE Serbia SRB")
plusRegions[nrow(plusRegions)+1,] <- c(ctyNme, lst, txt )

ctyNme <- "OIO" 
lst <- c("ATF,CCK,COM,CXR,HMD,IOT,MDV,MUS,MYT,REU,SYC")
txt <- c("OIO Other is Indian Ocean Southern Territories ATF Keeling Islands CCK Comoros COM Christmas Island CXR
 Heard and McDonald Islands HMD British Indian Ocean Territory IOT Maldives MDV Mauritius MUS
 Mayotte MYT Réunion REU Seychelles SYC")
plusRegions[nrow(plusRegions)+1,] <- c(ctyNme, lst, txt )

ctyNme <- "OPO" 
lst <- c("ASM,COK,FSM,GUM,KIR,MHL,MNP,NCL,NFK,NIU,NRU,PCN,PLW,PYF,TKL,TON,TUV,UMI,WLF,WSM")
txt <- c("OPO Other is Pacific Ocean American Samoa ASM Cook Islands COK Micronesia FSM Guam GUM
 Kiribati KIR Marshall Islands MHL Northern Mariana Islands MNP New Caledonia NCL Norfolk Island NFK
 Niue NIU Nauru NRU Pitcairn PCN Palau PLW French Polynesia PYF Tokelau TKL Tonga TON Tuvalu TUV
 Minor Outlying Islands UMI Wallis and Futuna WLF Samoa WSM")
plusRegions[nrow(plusRegions)+1,] <- c(ctyNme, lst, txt )

ctyNme <- "OSA" 
lst <- c("BRN,SGP") 
txt <- c("OSA Other is Southeast Asia Brunei BRN Singapore SGP")
plusRegions[nrow(plusRegions)+1,] <- c(ctyNme, lst, txt )

ctyNme <- "RAP" 
lst <- c("ARE,BHR,KWT,OMN,QAT") 
txt <- c("RAP  Rest of Arab is Peninsula	United Arab Emirates	ARE
 Bahrain	BHR Kuwait	KWT Oman	OMN Qatar	QAT")
plusRegions[nrow(plusRegions)+1,] <- c(ctyNme, lst, txt )

ctyNme <- "SDP" 
lst <- c("SSD,SDN") 
txt <- c("SDP Sudan plus is SSD Sudan SDN South Sudan")
plusRegions[nrow(plusRegions)+1,] <- c(ctyNme, lst, txt )

ctyNme <- "SPP" 
lst <- c("AND,ESP,GIB") 
txt <- c("SPP  Spain plus	is Andorra	AND Spain	ESP Gibraltar	GIB")
plusRegions[nrow(plusRegions)+1,] <- c(ctyNme, lst, txt )

ctyNme <- "UKP" 
lst <- c("GBR,GGY,IMN") 
txt <- c("UKP  Great Britain plus	Great Britain	GBR Guernsey	GGY Isle of Man	IMN")
plusRegions[nrow(plusRegions)+1,] <- c(ctyNme, lst, txt )

# Second do IMPACT old regions (115 regions)

regions115Lookup <- read.xlsx(fishInfoIMPACT, 
                           sheet = "IMPACT 115 Regions",
                           cols = 4, rows = 3:117,
                           colNames = TRUE)

regions.IMPACT115 <- as.data.frame(cSplit(regions115Lookup, 'mappingCode', sep=".", type.convert=FALSE))
regions.IMPACT115$mappingCode_2 <- gsub("\\(","",regions.IMPACT115$mappingCode_2)
regions.IMPACT115$mappingCode_2 <- gsub("\\)","",regions.IMPACT115$mappingCode_2)
colnames(IMPACT115Regions) <- c("region","members")
regions.IMPACT115 <- paste0(regions.IMPACT115$members, sep=",", collapse="") 
#convert to a real list
temp <- sort(colnames(read.csv(text = regions.IMPACT115)))

regions.IMPACT3 <- ctyNames

