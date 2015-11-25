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

source("setup.R")

# generate the nutrient requirements data frames
source("EARfoodGroupCSEloading.R") # this script also runs nutrientDataLoading.R

#list of years to keep; moved to setup.R
#yearList <- c("X2010","X2015","X2020","X2025","X2030","X2035","X2040","X2045","X2050")

# Read in and manipulate the SSP data -------------------------------------
if (!file.exists("data/SSPclean.rds")){
  # - if a clean .Rdata file doesn't exist, read in csv file, once, do some manipulation and save as an .rdata file
  SSP <- read.csv(unz(description = SSPdataZipFileLocation, file=SSPdataZipFileName), 
                  stringsAsFactors=FALSE)
  #Years 1950 to 1995 and 2050 to 2100 because they are all NAs. In addition, NCAR has NA for 2000 and 2005.
  #Remove years X2000 and X2005  because they are NA for pop values
  SSP  <- SSP[,!sapply(SSP,function(x) all(is.na(x)))]
  #make all names lower case
  names(SSP)[1:5] <- tolower(names(SSP)[1:5])
  #drop the data after 2050
  SSP <- SSP[c("model", "scenario", "region","variable",yearList)]
  #save cleaned up file as an .rdata file
  saveRDS(SSP, file="data/SSPclean.rds")
} else {
  SSP <- readRDS(file="data/SSPclean.rds")
}

#now add the new IFPRI regions
source(file = "RegionsAlignment.R")
# SSP regions
regions.SSP <- as.data.frame(sort(unique(SSP$region)),stringsAsFactors = FALSE) #there are 194 regions
colnames(regions.SSP) <- "region_name"
for (j in 1:length(plusRegions)) { # loop through all the plus regions in IMPACT
  ctyList <- eval(parse(text = plusRegions[j]))
  for (i in 1:length(ctyList)) { #look at all the country names in a plus region to make sure they are a IIASA country
    if(!(ctyList[i] %in% regions.SSP)) { #identify countries not in IIASA list
      print(paste(ctyList[i],"from", plusRegions[j], "is not in IIASA countries"))
    }
    else {
      ctyListNew <- c(ctyListNew,ctyList[i])
    }
  }
  print(paste(ctyListNew, " is in ctyListNew for", plusRegions[j]))
  #construct column names by adding the df name to the front of the cty name.
  ctyList <- gsub("^","pop.IIASA.wide$",ctyListNew)
  
  #convert the list into an expression that sums across the cty columns that are in the IIASA data, 
  # and that make up the IMPACT region
  pop.IIASA.wide$tmp <- eval(parse(text = paste(ctyList, sep = "", collapse=" + ")))
  income.IIASA.wide$tmp <- eval(parse(text = paste(ctyList, sep = "", collapse=" + ")))
  #Give the tmp column its correct name
  names(pop.IIASA.wide)[names(pop.IIASA.wide)=="tmp"] <- plusRegions[j]
  names(income.IIASA.wide)[names(income.IIASA.wide)=="tmp"] <- plusRegions[j]
  #Now remove the countries that make up the IMPACT plus region
  #ctyListNew.as.c <- c(paste(ctyListNew, sep = "\", collapse=" , "
  pop.IIASA.wide <- pop.IIASA.wide[,!(names(pop.IIASA.wide) %in% ctyListNew)]
  income.IIASA.wide <- income.IIASA.wide[,!(names(income.IIASA.wide) %in% ctyListNew)]
  
}
# Extract lists of the scenarios, regions, and data variables ------------------------------------

vNames <- unique(SSP$variable)
scenarios <- unique(SSP$scenario) #There are 21 scenarios; 4 each for SSP1, 2, 3, and 5 and 5 for SSP 4.
#This is the list of SSP3 scenarios
#"SSP3_v9_130424" is from PIK and just for the US
#"SSP3_v9_130325" is from OECD and is just GDP and population
#"SSP3_v9_130219" is from IIASA and is just population and GDP
#"SSP3_v9_130115" combine info from IIASA on population broken down by age, gender, and education and
#   NCAR on population broken down by rural and urban. The IIASA data are from 2010 to 2100

#work with only one SSP3 scenario for now, from IIASA for the age and gender breakdown and for the years 2010 to 2050
scenario3 <- "SSP3_v9_130115"
model <- "IIASA-WiC POP"
ssp3 <- SSP[SSP$scenario == scenario3 & SSP$model == model,]

# Create population-only data set by removing rows with education breakdown and GDP -----
popList <- "Population"
ageList <- c("Aged0-4", "Aged5-9","Aged10-14", "Aged15-19", "Aged20-24", "Aged25-29", 
             "Aged30-34", "Aged35-39", "Aged40-44", "Aged45-49", "Aged50-54", "Aged55-59", 
             "Aged60-64", "Aged65-69", "Aged70-74", "Aged75-79", "Aged80-84", "Aged85-89", 
             "Aged90-94", "Aged95-99", "Aged100+")
edList <- c("No Education", "Primary Education", "Secondary Education", "Tertiary Education")
genderList <-c("Male","Female")

#keep full population count around for bug checking later
ssp3.pop.tot.IIASA <- ssp3[ssp3$variable == "Population",c("region",yearList)]
ssp3.pop.tot.IIASA <- ssp3.pop.tot.IIASA[order(ssp3.pop.tot.IIASA$region),] #alphabetize by region
# Remove the aggregates of 
# "Population", "Population|Female" and "Population|Male"
removeList <- c("Population", "Population|Female", "Population|Male")
ssp3.pop.IIASA <-ssp3[!ssp3$variable %in% removeList,]

#split the variable names apart where there is a | (eg. X|Y becomes X and Y and new columns are created)
ssp3.pop.IIASA <- as.data.frame(cSplit(ssp3.pop.IIASA, 'variable', sep="|", type.convert=FALSE))

#name the new columns created by the spliting process above
colnames(ssp3.pop.IIASA)[colnames(ssp3.pop.IIASA) == c("variable_1","variable_2","variable_3","variable_4")] <- 
  c("population","gender","ageGenderCode","education")

#rename variables to align with the requirements names
ssp3.pop.IIASA$ageGenderCode<- gsub("Aged","",ssp3.pop.IIASA$ageGenderCode)
ssp3.pop.IIASA$ageGenderCode[ssp3.pop.IIASA$gender == "Female"] <- paste("SSPF", ssp3.pop.IIASA$ageGenderCode[ssp3.pop.IIASA$gender == "Female"], sep="")
ssp3.pop.IIASA$ageGenderCode[ssp3.pop.IIASA$gender == "Male"] <- paste("SSPM", ssp3.pop.IIASA$ageGenderCode[ssp3.pop.IIASA$gender == "Male"], sep="")
ssp3.pop.IIASA$ageGenderCode<- gsub("-","_",ssp3.pop.IIASA$ageGenderCode)
ssp3.pop.IIASA$ageGenderCode<- gsub("\\+","Plus",ssp3.pop.IIASA$ageGenderCode)
ssp3.pop.IIASA <- ssp3.pop.IIASA[order(ssp3.pop.IIASA$region),] 

#remove rows that breakdown an age group by education
removeList <- c("No Education","Primary Education", "Secondary Education", "Tertiary Education")
ssp3.pop.IIASA <-ssp3.pop.IIASA[!ssp3.pop.IIASA$education %in% removeList,]

#remove extraneous columns and keep only the ones needed
keepList <- c("region", "ageGenderCode", yearList)
ssp3.pop.IIASA <-ssp3.pop.IIASA[,keepList]

# start process of creating a separate list for pregnant and lactating (P/L) women----
#this list is for females who could be pregnant and lactating and have for the most part 
#identical nutrient needs if they are not P/L
ageRowsToSum <- c("SSPF15_19", "SSPF20_24",
                  "SSPF25_29", "SSPF30_34",
                  "SSPF35_39", "SSPF40_44", "SSPF45_49")

#pull out the relevant rows
ssp3.pop.F15_49.IIASA <- ssp3.pop.IIASA[ssp3.pop.IIASA$ageGenderCode %in% ageRowsToSum,c("region","ageGenderCode",yearList)] 

#sum the relevant rows (females aged 15-49 as those that could be pregnant or lactating) by region
dt.tmp <- data.table(ssp3.pop.F15_49.IIASA)
ssp3.pop.F15_49.sum.IIASA <- as.data.frame(dt.tmp[,lapply(.SD,sum),by=region, .SDcols = yearList])
#add age column with the single value for the new age group
ssp3.pop.F15_49.sum.IIASA$ageGenderCode <- rep("SSPF15_49",nrow(ssp3.pop.F15_49.sum.IIASA))
#get rid of now extraneous rows in ssp3.pop.IIASA so no double counting
ssp3.pop.IIASA <- ssp3.pop.IIASA[!ssp3.pop.IIASA$ageGenderCode %in% ageRowsToSum,]
#add the SSPF15_49 row to ssp3.pop.IIASA
ssp3.pop.IIASA <- rbind(ssp3.pop.IIASA,ssp3.pop.F15_49.sum.IIASA)
#sort by region
ssp3.pop.IIASA <- ssp3.pop.IIASA[order(ssp3.pop.IIASA$region),] 

#check to see if population totals are the same. Uncomment to test
dt.tmp <- data.table(ssp3.pop.IIASA)
setkey(dt.tmp,"region")
pop.temp <- as.data.frame(dt.tmp[,lapply(.SD,sum),by=region, .SDcols = yearList])
temp <- pop.temp$X2010 - ssp3.pop.tot.IIASA$X2010 #this is a dumb thing to do. At least make sure they are sorted by region
summary(temp) #the differences should be very small

#now estimate the number of pregnant women and lactating women and add them
#the estimate is based on the number of children aged 0 to 4. 
#sum boys and girls 0 to 4
kidsRows <- c("SSPF0_4","SSPM0_4")
#create a temporary data frame with just those rows
kids.0_4 <- ssp3.pop.IIASA[ssp3.pop.IIASA$ageGenderCode %in% kidsRows,c("region",yearList)]
dt.kids.0_4 <- data.table(kids.0_4)
setkey(dt.kids.0_4,"region")
dt.kids.sum <- dt.kids.0_4[,lapply(.SD,sum),by = "region"]
#setkey(dt.kids.sum,"region")

#pregnant and lactating women are a constant share of kids 0 to 4. This is a *kludge*!!!
share.preg <- 0.2
share.lact <- 0.2
temp.preg <- as.data.frame(dt.kids.sum[,lapply(.SD, function(x) x * share.preg),by="region"])
temp.lact <- as.data.frame(dt.kids.sum[,lapply(.SD, function(x) x * share.lact),by="region"])

#delete number of potentially pregnant and lactating women so no double counting
removeList <- c("SSPF15_49")
ssp3.pop.IIASA <-ssp3.pop.IIASA[!ssp3.pop.IIASA$ageGenderCode %in% removeList,]

#add back number of non PL women
dt.tmp <- as.data.table(ssp3.pop.F15_49.sum.IIASA)
dt.tmp2 <- as.data.table(temp.preg)
setkey(dt.tmp,"region")

cols <- paste0("X",seq(2010,2050,5))
temp.nonPL <- ssp3.pop.F15_49.sum.IIASA[match(temp.preg$region,ssp3.pop.F15_49.sum.IIASA$region),cols] - 
  temp.preg[,cols] - 
  temp.lact[match(temp.preg$region,temp.lact$region),cols] 
temp.nonPL$region <- ssp3.pop.F15_49.sum.IIASA$region
#add age column for the new P/L variables; also to temp.kids to be consistent
temp.kids <- cbind(dt.kids.sum,ageGenderCode = "SSPKids0_4", stringsAsFactors = FALSE)
temp.preg <- cbind(temp.preg,ageGenderCode = "SSPPreg", stringsAsFactors = FALSE)
temp.lact <- cbind(temp.lact,ageGenderCode = "SSPLact", stringsAsFactors = FALSE)
temp.nonPL <- cbind(temp.nonPL,ageGenderCode = "SSPF15_49", stringsAsFactors = FALSE)

#add new rows to ssp3.pop.IIASA
ssp3.pop.IIASA <- rbind(ssp3.pop.IIASA,temp.preg,temp.lact,temp.nonPL)
ssp3.pop.IIASA <- ssp3.pop.IIASA[with(ssp3.pop.IIASA, order(region, ageGenderCode)), ]

#check to see if population totals are the same. Uncomment to test
ssp3.pop.tot.IIASA <- ssp3.pop.tot.IIASA[order(ssp3.pop.tot.IIASA$region),] 
dt.tmp <- data.table(ssp3.pop.IIASA)
setkey(dt.tmp,"region")
pop.temp <- as.data.frame(dt.tmp[,lapply(.SD,sum),by=region, .SDcols = yearList])
temp <- pop.temp$X2010 - ssp3.pop.tot.IIASA$X2010 #this is a dumb thing to do. At least make sure they are sorted by region
summary(temp) #the differences should be very small

dt.pop <- as.data.table(ssp3.pop.IIASA)
dt.pop.melt <- melt(dt.pop,variable.name = "year",variable.factor = FALSE, 
                    id.vars = c("region","ageGenderCode"), measure.vars = yearList, value.name = "pop.value")

repCons <- function(nutReq,common.nut) {
  #remove the nutrient requirements for the female age groups in ageRowsToSum because they are already in SSPF15_49
  nutReq <- nutReq[!(nutReq$ageGenderCode %in% ageRowsToSum),]
  setkey(dt.pop.melt,"ageGenderCode")
  dt.temp <- dt.pop.melt[data.table(nutReq)]
  dt.temp[, (paste(common.nut,"prod",sep=".")):= lapply(.SD, function(x) x*dt.temp$pop.value), .SDcols=c(common.nut)]
  setkey(dt.temp,"region","year")
  reqlist <- paste(common.nut,"prod",sep=".")
  dt.temp[, (paste(common.nut,"sum",sep=".")):= lapply(.SD, sum), by=c("region","year"), .SDcols=c(reqlist) ]
  #remove the .prod columns
  dt.temp[,c(reqlist):= NULL]
  dt.temp.sum <- unique(dt.temp[,c("region","year",paste(common.nut,"sum",sep=".")),with=F])
  
  dt.pop.IIASA <- as.data.table(ssp3.pop.tot.IIASA)
  dt.pop.IIASA.melt <- melt(dt.pop.IIASA,id.vars = "region", 
                            measure.vars = yearList, variable.name = "year", variable.factor = FALSE,
                            value.name = "pop.tot")
  setkey(dt.pop.IIASA.melt,"region","year")
  dt.temp <- dt.pop.IIASA.melt[dt.temp.sum]
  # paste(common.nut,"fin",sep=".")
  dt.temp[, (paste(common.nut,"fin",sep=".")):= lapply(.SD, function(x) x/dt.temp$pop.tot), .SDcols=c(paste(common.nut,"sum",sep="."))]
  dt.temp[,c("pop.tot",paste(common.nut,"sum",sep=".")):= NULL]
  dt.temp.melt <- melt(dt.temp,id.vars = c("region","year"), 
                       measure.vars = paste(common.nut,"fin",sep="."), variable.name = "nutrient", variable.factor = FALSE,
                       value.name = "value")
  return(dcast.data.table(dt.temp.melt,region + nutrient ~ year,value.var="value"))
  # #convert every 5 years data to every year. this is not working so commented out.
  # setkey(dt.temp.dcast,"region","nutrient")
  # yrs <- seq(2010,2050,5)
  # lin.interp <- function(y,yrs) predict(lm(y~yrs),data.frame(yrs=min(yrs):max(yrs)))
  # dt.temp.dcast[, paste0("X",min(yrs):max(yrs)):= apply(.SD,FUN = lin.interp, yrs, MARGIN = 1), .SDcols = yearList, key=dt.temp.dcast]
  # 
  # p1 <- as.data.frame(apply(temp.preg[,2:4],1,lin.interp,yrs))
  # names(p1) <- paste0("X",min(yrs):max(yrs))
}

#nutrient requirements are calculated in EARfoodGroupCSEloading.R. the next line is a list as of Oct 26
reqs <- c("req.EAR","req.RDA.vits","req.RDA.minrls","req.RDA.macro","req.UL.vits","req.UL.minrls")
#The list of nutrients for each is common.EAR, common.RDA.vits, common.RDA.minrls, common.RDA.macro, common.UL.vits, 
#        common.UL.minrls, common.AMDR
common <- c("common.EAR", "common.RDA.vits", "common.RDA.minrls", "common.RDA.macro", "common.UL.vits", 
            "common.UL.minrls")
# for loop to write out the nutrient requirements files
wbGeneral <- createWorkbook()
numStyle <- createStyle(numFmt = "0.00")
textStyle <- createStyle(fontName = NULL, fontSize = NULL, fontColour = NULL,
                         numFmt = "GENERAL", border = NULL,
                         borderColour = getOption("openxlsx.borderColour", "black"),
                         borderStyle = getOption("openxlsx.borderStyle", "thin"), bgFill = NULL,
                         fgFill = NULL, halign = "left", valign = NULL, textDecoration = NULL,
                         wrapText = FALSE, textRotation = NULL)
#create a worksheet with info on creator, date, model version, etc.
creationInfo <- ("Information on creator, date, model version, etc.")
creationInfo <- rbind(creationInfo, paste("Creator:", userName))
creationInfo <- rbind(creationInfo, paste("Date of file creation:", Sys.Date()))
#creationInfo <- rbind(creationInfo, paste("IMPACT data:", IMPACTfileName))
creationInfo <- rbind(creationInfo, paste("Nutrient data:", nutrientFileName))
creationInfo <- rbind(creationInfo, paste("Nutrient requirements data:", EARFileName))
creationInfo <- rbind(creationInfo, paste("SSP data:", SSPdataZipFileName))
addWorksheet(wbGeneral, sheetName="creation_Info")
writeData(wbGeneral, creationInfo, sheet="creation_Info", startRow=1, startCol=1, rowNames = FALSE, colNames = FALSE)
class(wbInfoGeneral$sheet_Name) <- 'hyperlink'
wbInfoGeneral[(nrow(wbInfoGeneral)+1),] <- c("creation_Info", "Information on creator, date, model version, etc.")

#Set up a dataframe to collect common worksheet names and descriptions. 
wbInfoGeneral <- data.frame(sheet_Name=character(),sheet_Desc=character(), stringsAsFactors = FALSE)
#convert wbInfoGeneral sheet_Name column to class hyperlink
wbInfoGeneral[(nrow(wbInfoGeneral)+1),] <- c("Sheet names", "Description of sheet contents")

#create a worksheet with info on the commodities and nutrients
addWorksheet(wbGeneral,sheetName="IMPACTCommdlist")
#commodityNames <- cbind(nutrients[c("Name","IMPACT_code")])
writeData(wbGeneral, nutrients, sheet="IMPACTCommdlist", startRow=1, startCol=1)
wbInfoGeneral[(nrow(wbInfoGeneral)+1),] <- c("IMPACTCommdList", "IMPACT commodities and their nutrient content")

addWorksheet(wbGeneral, sheetName="MetaDataNutrnts")
writeData(wbGeneral, req.metadata, sheet="MetaDataNutrnts", startRow=1, startCol=1, rowNames = FALSE, colNames = FALSE)
wbInfoGeneral[(nrow(wbInfoGeneral)+1),] <- c("MetaDataNutrnts", "Information about the requirements sources")

for (i in 1:length(reqs)) {
  nutReq <- paste(reqs[i],"ssp",sep=".")
  dt.temp.internal <- repCons(eval(parse(text = nutReq)),eval(parse(text = common[i])))
  setkey(dt.temp.internal,"nutrient","region")
  
  dt.temp.internal$nutrient <- gsub(".fin", "", dt.temp.internal$nutrient)
  
  dt.name <- paste(reqs[i],"percap",sep=".")
  print(dt.name)
  addWorksheet(wbGeneral, sheetName=dt.name)
  addStyle(wbGeneral, sheet=dt.name, style=numStyle, rows = 2:nrow(dt.temp.internal), cols=3:(ncol(dt.temp.internal)), gridExpand = TRUE)
  writeData(wbGeneral, dt.temp.internal, sheet=dt.name, startRow=1, startCol=1, rowNames = FALSE, colNames = TRUE)
  nutrientList <- gsub(" ",", ",paste((eval(parse(text = common[i]))),collapse = " "))
  sheetDesc <- paste("nutrients included: ",nutrientList)
  wbInfoGeneral[(nrow(wbInfoGeneral)+1),] <- c(dt.name, sheetDesc)
  
  assign(dt.name,dt.temp.internal)
  #  setnames(dt.temp.internal,dt.temp.internal,dt.name)
  fname <- paste("results/",dt.name,".rds",sep="")
  print(fname)
  saveRDS(eval(parse(text = dt.name)),file = fname)
  #fileEncoding = "Windows-1252"
}

#add sheet with info about each of the worksheets
addWorksheet(wbGeneral, sheetName="sheetInfo")
writeData(wbGeneral, wbInfoGeneral, sheet="sheetInfo", startRow=1, startCol=1, rowNames = FALSE, colNames = FALSE)
addStyle(wbGeneral, sheet="sheetInfo", style=textStyle, rows = 1:nrow(wbInfoGeneral), cols=1:(ncol(wbInfoGeneral)), gridExpand = TRUE)
setColWidths(wbGeneral, sheet="sheetInfo", cols = 1:2, widths=20)

#move sheetInfo worksheet from the last to the first
temp<- 2:length(names(wbGeneral))-1
temp <- c(length(names(wbGeneral)),temp)
worksheetOrder(wbGeneral) <- temp

xcelOutFileName <- paste("results/nut.requirements",Sys.Date(),".xlsx",sep="")
saveWorkbook(wbGeneral, xcelOutFileName, overwrite = TRUE)


#I think everything from here on down is extraneous, but I'm keeping it for now, just commented out.
# 
# 
# for (req.type in type.list) {
#   for (nutrient in req.type) {
#     dt.nut <- as.data.table(EARs[grep(nutrient,EARs$NutCode),])
#     print(nutrient)
#     dt.nut[,nutNames.Units:=NULL]
#     dt.nut.melt <- melt(dt.nut,variable.name = "ageGenderCode", id.vars = "NutCode", 
#                         value.name = "nut.value", stringsAsFactors = FALSE) 
#     dt.nut.melt[,ageGenderCode:=as.character(ageGenderCode)]
#     dt.nut.melt[,NutCode:=NULL]
#     setkey(dt.nut.melt,"ageGenderCode")
#     #merge dt.pop.melt and dt.nut.melt
#     dt.pop.nut <- dt.pop.melt[dt.nut.melt] 
#     setkey(dt.pop.nut,"region","year")
#     dt.pop.nut[,val:=sum(pop.value*nut.value),by=key(dt.pop.nut)]
#     xx <- unique(dt.pop.nut[,c("region","year","val"),with=F])
#     dt.regionYear <- dt.regionYear[xx] #tot amount of nutrient needed by all (sum of nutrient needed by age group over all age groups)
#     dt.regionYear[,percapval:=val/pop.tot,by=key(dt.regionYear)]
#     #  setkey(dt.regionYear,"region","year")
#     setnames(dt.regionYear,"val",nutrient)
#     setnames(dt.regionYear,"percapval",paste(nutrient,"_percap",sep=""))
#   }
# }
# temp <- melt(dt.regionYear,id.vars=c("region","year"),measure.vars = c("energy_percap", "protein_percap", "fat_percap", 
#                                                                        "carbohydrate_percap", "fiber_percap", 
#                                                                        "calcium_percap", "iron_percap", "magnesium_percap",  "phosphorus_percap",
#                                                                        "potassium_percap",     "sodium_percap","zinc_percap",   
#                                                                        "vit_c_percap",    "thiamin_percap",  "riboflavin_percap","niacin_percap", "vit_b6_percap", "folate_percap", 
#                                                                        "vit_b12_percap", "vit_a_IU_percap", "vit_e_percap", "vit_d_percap"),
#              variable.name = "nutrient", value.name = "percapEAR")
# 
# temp[,nutrient:=as.character(nutrient)]
# temp.wide <- dcast(temp, region +  nutrient ~ year, value.var = "percapEAR")
# # energy_percap + protein_percap + fat_percap + 
# # carbohydrate_percap + fiber_percap + 
# #   calcium_percap + iron_percap + magnesium_percap +  phosphorus_percap +
# #   potassium_percap +     sodium_percap +zinc_percap +   
# #   vit_c_percap +    thiamin_percap +  riboflavin_percap +niacin_percap + vit_b6_percap + folate_percap + 
# #   vit_b12_percap + vit_a_IU_percap + vit_e_percap + vit_d_percap
# write.csv(dt.regionYear, file = "results/regionyear.csv")
# write.csv(temp.wide, file = "results/repConsumerByYear.csv")
# 
# 
# #convert every 5 years data to every year
# for (i in seq(2010,2050,5)) temp.wide[[paste0("X",i)]] <- rowMeans(SSPDat.NCAR[,paste0("X",c(i-5,i+5))])
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