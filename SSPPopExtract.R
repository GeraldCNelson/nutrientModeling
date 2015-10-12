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

require(openxlsx)
require(entropy)
require(reshape2)
require(plyr)
require(dplyr)
require(tidyr)
require(data.table)
require(splitstackshape)
require(plotrix)

# file names
IMPACTregionsFileName <- "data/IMPACTRegionsJan2015.xlsx"
SSPdataZipFileLocation <- c("data/SSPData/SspDb_country_data_2013-06-12.csv.zip")
SSPdataZipFileName <- c("SspDb_country_data_2013-06-12.csv") #the name of the file inside the zip

# Read in and manipulate the SSP data -------------------------------------

SSP <- read.csv(unz(description = SSPdataZipFileLocation, file=SSPdataZipFileName), stringsAsFactors=FALSE)
#remove years 1950 to 1995 and 2105 to 2150 because they are all NAs. In addition, NCAR has NA for 2000 and 2005.
#Remove years X2000 and X2005 later because they are NA for pop values
# keepCols <- c("MODEL", "SCENARIO", "REGION", "VARIABLE","UNIT","X2000","X2005","X2010","X2015","X2020","X2025","X2030","X2035","X2040","X2045","X2050","X2055","X2060","X2065","X2070","X2075","X2080","X2085","X2090","X2095","X2100")
# SSPDat.1 <- SSPDat[,keepCols]
#This code does the same thing, is fast, and clean
SSP  <- SSP[,!sapply(SSP,function(x) all(is.na(x)))]

colnames(SSP)[1:5] <- c("model", "scenario", "region", "variable","unit") # convert to lower case

# Extract lists of the scenarios, regions, and data variables
scenarios <- unique(SSP$scenario) #There are 21 scenarios; 4 each for SSP1, 2, 3, and 5 and 5 for SSP 4.
regions <- unique(SSP$region) #there are 194 regions
vNames <- unique(SSP$variable)

#"SSP3_v9_130424" is from PIK and just for the US
#"SSP3_v9_130325" is from OECD and is just GDP and population
#"SSP3_v9_130219" is from IIASA and is just population and GDP
#"SSP3_v9_130115" combine info from IIASA on population broken down by age, gender, and education and
#   NCAR on population broken down by rural and urban. The IIASA data are from 2010 to 2100

#work with only one scenario for now, from IIASA for the age and gender breakdown and for the years 2010 to 2050
scenario3 <- "SSP3_v9_130115"
model <- "IIASA-WiC POP"
yearList <- c("X2010","X2015","X2020","X2025","X2030","X2035","X2040","X2045","X2050")
SSP3Dat <- SSP[SSP$scenario == scenario3 & SSP$model == model,c("region","variable",yearList)]

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

#This code is much cleaner than the above, if a bit less transparent. Not needed at the moment so commented out.
#for (i in seq(2015,2095,10)) SSPDat.NCAR[[paste0("X",i)]] <- rowMeans(SSPDat.NCAR[,paste0("X",c(i-5,i+5))])

population.IIASA <- SSP3Dat[SSP3Dat$variable == "Population",] #keep this around for bug checking later
population.IIASA$variable <- NULL #get rid of variable because it is just 'Population'
population.IIASA <- population.IIASA[order(population.IIASA$region),] #alphabetize by region
# Remove the aggregates of 
# "Population", "Population|Female" and "Population|Male"
removeList <- c("Population", "Population|Female", "Population|Male")
pop3.IIASA <-SSP3Dat[!SSP3Dat$variable %in% removeList,]

#split the variable names apart where there is a | (eg. X|Y becomes X and Y and new columns are created)
pop3.IIASA <- as.data.frame(cSplit(pop3.IIASA, 'variable', sep="|", type.convert=FALSE))

#name the new columns created by the spliting process above
names(pop3.IIASA)[12:14] <- c("gender","age","education")

#rename variables to align with the EAR names
pop3.IIASA$age<- gsub("Aged","",pop3.IIASA$age)
pop3.IIASA$age[pop3.IIASA$gender == "Female"] <- paste("SSPF", pop3.IIASA$age[pop3.IIASA$gender == "Female"], sep="")
pop3.IIASA$age[pop3.IIASA$gender == "Male"] <- paste("SSPM", pop3.IIASA$age[pop3.IIASA$gender == "Male"], sep="")
pop3.IIASA$age<- gsub("-","_",pop3.IIASA$age)
pop3.IIASA$age<- gsub("\\+","Plus",pop3.IIASA$age)
pop3.IIASA <- pop3.IIASA[order(pop3.IIASA$region),] 

#remove rows that breakdown an age group by education
removeList <- c("No Education","Primary Education", "Secondary Education", "Tertiary Education")
pop3.IIASA <-pop3.IIASA[!pop3.IIASA$education %in% removeList,]

#remove extraneous columns and keep only the ones needed
keepList <- c("region", "age", yearList)
pop3.IIASA <-pop3.IIASA[,keepList]

# start process of creating a separte list for pregnant and lactating (P/L) women
#this list is for females who could be pregnant and lactating and have for the most part 
#identical nutrient needs if they are not P/L
ageRowsToSum <- c("SSPF15_19", "SSPF20_24",
                  "SSPF25_29", "SSPF30_34",
                  "SSPF35_39", "SSPF40_44", "SSPF45_49")

#pull out the relevant rows
temp.F15_49 <- pop3.IIASA[pop3.IIASA$age %in% ageRowsToSum,c("region",yearList)] #also gets rid of age column

#sum the relevant rows (females aged 15-49 as those that could be pregnant or lactating) by region
dttmp <- data.table(temp.F15_49)
setkey(dttmp,"region")
dftmp <- as.data.frame(dttmp[,lapply(.SD,sum),by=region])
#add age column with the single value for the new age group
preg.potential <- cbind(dftmp,age = "SSPF15_49")
preg.potential$age <- NULL # get rid of this column since it only has identical elements
#get rid of now extraneous rows so no double counting
pop3.IIASA <- pop3.IIASA[!pop3.IIASA$age %in% ageRowsToSum,]
#sort by region
pop3.IIASA <- pop3.IIASA[order(pop3.IIASA$region),] 

#check to see if population totals are the same. Uncomment to test
# dttmp <- data.table(temp3)
# dttmp$age <- NULL
# setkey(dttmp,"region")
# pop.temp <- as.data.frame(dttmp[,lapply(.SD,sum),by=region])
# summary(pop.temp$X2010 - population.IIASA$X2010) #the differences should be very small

#now estimate the number of pregnant women and lactating women and add them
#the estimate is based on the number of children aged 0 to 4. 
#sum boys and girls 0 to 4
kidsRows <- c("SSPF0_4","SSPM0_4")
#create a temporary data frame with just those rows
age.kids <- pop3.IIASA[pop3.IIASA$age %in% kidsRows,c("region",yearList)]
dttmp <- data.table(age.kids)
setkey(dttmp,"region")
temp.kids <- dttmp[,lapply(.SD,sum),by=region]

#pregnant and lactating women are a constant share of kids 0 to 4. This is a *kludge*!!!
share.preg <- 0.2
share.lact <- 0.2
share.nonPL <- 1 - share.preg + share.lact
temp.preg <- as.data.frame(temp.kids[,lapply(.SD, function(x) x * share.preg),by=region])
temp.lact <- as.data.frame(temp.kids[,lapply(.SD, function(x) x * share.lact),by=region])

#delete number of potentially pregnant and lactating women so no double counting

removeList <- c("SSPF15_49")
pop3.IIASA <-pop3.IIASA[!pop3.IIASA$age %in% removeList,]

#add back number of non PL women
dttmp <- data.table(preg.potential)
setkey(dttmp,"region")
temp.nonPL <- as.data.frame(dttmp[,lapply(.SD, function(x) x * share.nonPL),by=region])

#add age column for the new P/L variables; also to temp.kids to be consistent
temp.kids <- cbind(age.kids,age = "SSPKids0_4", stringsAsFactors = FALSE)
temp.preg <- cbind(temp.preg,age = "SSPPreg", stringsAsFactors = FALSE)
temp.lact <- cbind(temp.lact,age = "SSPLact", stringsAsFactors = FALSE)
temp.nonPL <- cbind(temp.nonPL,age = "SSPF15_49", stringsAsFactors = FALSE)

#add new rows to pop3.IIASA
pop3.IIASA <- rbind(pop3.IIASA,temp.preg,temp.lact,temp.nonPL)
pop3.IIASA <- pop3.IIASA[with(pop3.IIASA, order(region, age)), ]

f.repConsNut <- function(nutrient,pop) {
  dt.nut <- as.data.table(EARs[EARs$NutCode == nutrient,])
  dt.nut[,nutNames.Units:=NULL]
  dt.nut.melt <- melt.data.table(dt.nut,variable.name = "age", id.vars = "NutCode", 
                                 value.name = "nut.value", stringsAsFactors = FALSE)
  dt.nut.melt$age <- as.character(dt.nut.melt$age)
  dt.nut.melt[,NutCode:=NULL]
  dt.pop <- as.data.table(pop)
  dt.pop.melt <- melt.data.table(dt.pop,variable.name = "year", id.vars = c("region","age"), measure.vars = yearList, value.name = "pop.value")
  dt.pop.melt$year <- as.character(dt.pop.melt$year)
  dt.pop.nut <- join(dt.pop.melt,dt.nut.melt)
  #  dt.pop.nut$nutProd <- dt.pop.nut$pop.value * dt.pop.nut$nut.value
  setkey(dt.pop.nut,"region","year")
  expr <- parse(text = paste0(nutrient, ":=sum(pop.value*nut.value)"))
  dt.pop.nut[,eval(expr), by=key(dt.pop.nut)]
  xx <- as.data.frame(unique(dt.pop.nut[,c(key(dt.pop.nut),"region","year",eval(parse(text = nutrient))),with=F]))
  
  
  dt.pop.nut[,nutProdSum:=sum(nutProd),by=key(dt.pop.nut)]
  
}

xx <- f.repConsNut("protein",pop3.IIASA) 



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