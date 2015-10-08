# Intro -------------------------------------------------------------------
#This script reads in the Shared Socioeconomic Profiles information, does some manipulations of the data,
#and pulls out just the population data
#In several places old inefficient code is commented out and replaced by cleaner code written by Brendan Power of CSIRO

#Copyright (C) 2015 Gerald C. Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details at http://www.gnu.org/licenses/.

setwd("~/Documents/workspace/SSPDataManipulation")
library(openxlsx) 
library(reshape2)
library(splitstackshape)
library(plotrix)
library(tidyr)
library(plyr)

#userName is reported in the basic info worksheet
userName <- "Gerald Nelson"

# file names
IMPACTregionsFileName <- "../nutrientModeling/data/IMPACTRegionsJan2015.xlsx"
SSPdataZipFileLocation <- c("data/SspDb_country_data_2013-06-12.csv.zip")
SSPdataZipFileName <- c("SspDb_country_data_2013-06-12.csv")

# Read in and manipulate the SSP data -------------------------------------

SSP <- read.csv(unz(description = SSPdataZipFileLocation, file=SSPdataZipFileName), stringsAsFactors=FALSE)
#remove years 1950 to 1995 and 2105 to 2150 because they are all NAs. In addition, NCAR has NA for 2000 and 2005.
#Remove years X2000 and X2005 later because they are NA for pop values
# keepCols <- c("MODEL", "SCENARIO", "REGION", "VARIABLE","UNIT","X2000","X2005","X2010","X2015","X2020","X2025","X2030","X2035","X2040","X2045","X2050","X2055","X2060","X2065","X2070","X2075","X2080","X2085","X2090","X2095","X2100")
# SSPDat.1 <- SSPDat[,keepCols]
#This code does the same thing, is fast, and clean
SSP  <- SSP[,!sapply(SSP,function(x) all(is.na(x)))]

# Extract lists of the scenarios, regions, and data variables
scenarios <- unique(SSP$SCENARIO) #There are 21 scenarios; 4 each for SSP1, 2, 3, and 5 and 5 for SSP 4.
regions <- unique(SSP$REGION) #there are 194 regions
vNames <- unique(SSP$VARIABLE)

#"SSP3_v9_130424" is from PIK and just for the US
#"SSP3_v9_130325" is from OECD and is just GDP and population
#"SSP3_v9_130219" is from IIASA and is just population and GDP
#"SSP3_v9_130115" combine info from IIASA on population broken down by age, gender, and education and
#   NCAR on population broken down by rural and urban. The IIASA data are from 2010 to 2100

#work with only one scenario for now, from IIASA for the age and gender breakdown and for the years 2010 to 2050
scenario3 <- "SSP3_v9_130115"
model <- "IIASA-WiC POP"
yearList <- c("X2010","X2015","X2020","X2025","X2030","X2035","X2040","X2045","X2050")
SSP3Dat <- SSP[SSP$SCENARIO == scenario3 & SSP$MODEL == model,c("REGION","VARIABLE",yearList)]

# Create population-only data set by removing rows with education breakdown and GDP
popList <- "Population"
ageList <- c("Aged0-4", "Aged5-9","Aged10-14", "Aged15-19", "Aged20-24", "Aged25-29", "Aged30-34", "Aged35-39", "Aged40-44", "Aged45-49", "Aged50-54", "Aged55-59", "Aged60-64", "Aged65-69", "Aged70-74", "Aged75-79", "Aged80-84", "Aged85-89", "Aged90-94", "Aged95-99", "Aged100+")
edList <- c("No Education", "Primary Education", "Secondary Education", "Tertiary Education")
genderList <-c("Male","Female")
# NCAR data are for 10 year periods, starting in 2010 and ending in 2100 and are broken down by Urban and Rural
# IIASA population data are for 5 year periods, starting in 2010 and ending in 2100 and 
# are broken down by age, education, and gender

#fill in every fifth year for NCAR with linear interpolation
# SSPDat.NCAR$X2015 <- (SSPDat.NCAR$X2010 + SSPDat.NCAR$X2020)/2
# SSPDat.NCAR$X2025 <- (SSPDat.NCAR$X2020 + SSPDat.NCAR$X2030)/2
# SSPDat.NCAR$X2035 <- (SSPDat.NCAR$X2030 + SSPDat.NCAR$X2040)/2
# SSPDat.NCAR$X2045 <- (SSPDat.NCAR$X2040 + SSPDat.NCAR$X2050)/2
# SSPDat.NCAR$X2055 <- (SSPDat.NCAR$X2050 + SSPDat.NCAR$X2060)/2
# SSPDat.NCAR$X2065 <- (SSPDat.NCAR$X2060 + SSPDat.NCAR$X2070)/2
# SSPDat.NCAR$X2075 <- (SSPDat.NCAR$X2070 + SSPDat.NCAR$X2080)/2
# SSPDat.NCAR$X2085 <- (SSPDat.NCAR$X2080 + SSPDat.NCAR$X2090)/2
# SSPDat.NCAR$X2095 <- (SSPDat.NCAR$X2090 + SSPDat.NCAR$X2100)/2

#This code is much cleaner than the above, if a bit less transparent.
#for (i in seq(2015,2095,10)) SSPDat.NCAR[[paste0("X",i)]] <- rowMeans(SSPDat.NCAR[,paste0("X",c(i-5,i+5))])

# Remove the aggregates of 
# "Population", "Population|Female" and "Population|Male"
removeList <- c("Population", "Population|Female", "Population|Male")
pop3.IIASA <-SSP3Dat[!SSP3Dat$VARIABLE %in% removeList,]
pop3.IIASA <- as.data.frame(cSplit(pop3.IIASA, 'VARIABLE', sep="|", type.convert=FALSE))

names(pop3.IIASA)[12:14] <- c("gender","age","education")
pop3.IIASA$age<- gsub("Aged","",pop3.IIASA$age)
pop3.IIASA$age[pop3.IIASA$gender == "Female"] <- paste("SSPF", pop3.IIASA$age[pop3.IIASA$gender == "Female"], sep="")
pop3.IIASA$age[pop3.IIASA$gender == "Male"] <- paste("SSPM", pop3.IIASA$age[pop3.IIASA$gender == "Male"], sep="")
pop3.IIASA$age<- gsub("-","_",pop3.IIASA$age)


#remove rows that breakdown an age group by education
removeList <- c("No Education","Primary Education", "Secondary Education", "Tertiary Education")
pop3.IIASA <-pop3.IIASA[!pop3.IIASA$education %in% removeList,]

#remove extraneous columns
keepList <- c("REGION", "age", yearList)
pop3.IIASA <-pop3.IIASA[,keepList]

ageRowsToSum <- c("SSPF15_19", "SSPF20_24",
               "SSPF25_29", "SSPF31_34",
               "SSPF35_39", "SSPF41_44", "SSPF45_49")
temp <- rowsum(pop3.IIASA[,yearList],group = pop3.IIASA$REGION)
temp2 <- cbind(REGION = unique(pop3.IIASA$REGION),temp,age = "SSPF15_49", stringsAsFactors = FALSE) #a bit dangerous; assumes unique keeps region names in same order

#get rid of now extraneous rows
pop3.IIASA <-pop3.IIASA[!pop3.IIASA$age %in% ageRowsToSum,]

#add new row by merging pop3.IIASA and temp2 by REGION
temp3 <- rbind(pop3.IIASA,temp2)

#now estimate the number of pregnant women and lactating women and add them



#I think everything from here on down is extraneous, but I'm keeping it for now, just commented out.

# mpop.IIASA <- melt(pop.IIASA, id.vars=c("MODEL","SCENARIO","REGION","UNIT","gender","age","education"),
#              measure.vars =c("X2010","X2015","X2020","X2025", "X2030","X2035","X2040","X2045",
#                              "X2050","X2055","X2060","X2065","X2070","X2075", "X2080","X2085",
#                              "X2090","X2095","X2100"))

# mpop.IIASA <- gather(pop.IIASA,variable,value,X2010:X2100)
# mpop.IIASA <- mpop.IIASA[,-match(c("X2000","X2005","VARIABLE_1"),names(mpop.IIASA))]
# 
# mpop.IIASA.sort <- mpop.IIASA[with( mpop.IIASA, order(SCENARIO, REGION, variable, age)), ]
# #remove rows with NA in the education column. These are the totals from summing across all the education possibilities.
# mpop.IIASA.sort.complete <- mpop.IIASA.sort[complete.cases(mpop.IIASA.sort[,"education"]),]
# 
# # have women and men be separate columns
# #mpop.IIASA.sort.complete.wide <- dcast.data.table(mpop.IIASA.sort.complete, MODEL + SCENARIO + UNIT + REGION + age + education + variable ~ gender, mean, value.var = "value")
# mpop.sort.complete.wide <- spread(mpop.IIASA.sort.complete,gender,value)
# 
# # use the inverse to get the pop totals, then have men and women in separate columns, or have columns for each year
# #mpop.IIASA.sort.tot <- mpop.IIASA.sort[!complete.cases(mpop.IIASA.sort[,"education"]),]
# mpop.IIASA.sort.tot <- mpop.IIASA.sort[is.na(mpop.IIASA.sort$education),]
# 
# #mpop.IIASA.sort.gender <- dcast.data.table(mpop.IIASA.sort.tot, MODEL + SCENARIO + UNIT + REGION + age + education + variable ~ gender, mean, value.var = "value")
# mpop.IIASA.sort.gender <- spread(mpop.IIASA.sort.tot,gender,value)
# 
# #pop.IIASA.tot.years <- dcast.data.table(mpop.IIASA.sort.tot, MODEL + SCENARIO + UNIT + REGION + age + gender ~ variable, mean, value.var = "value")
# pop.IIASA.tot.years <- spread(mpop.IIASA.sort.tot,variable,value)
# 
# regions.NCAR <- data.frame(CTY=sort(unique(SSPDat.NCAR$REGION)))
# #regions.IIASA <- as.data.frame(regions.IIASA); colnames(regions.IIASA)[1]<-"CTY"; regions.IIASA$CTY <- sort(regions.IIASA$CTY)
# regions.IIASA <- data.frame(CTY=sort(unique(SSPDat.IIASA$REGION)))
# 
# # get the regions used by IMPACT
# regions.IMPACT <- read.xlsx(IMPACTregionsFileName, colNames = TRUE, sheet = 1)
# 
# # make sure that the regions used by both IMPACT and IIASA are included
# regions.combined <- merge(regions.IMPACT,regions.IIASA, by="CTY", all = TRUE)
# 
# # reorder IIASA population data so the country codes are the columns
# 
# pop.IIASA.wide <- spread(mpop.IIASA,REGION,value)