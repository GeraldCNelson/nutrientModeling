
# Data loading file for nutrientCalcs -------------------------------------
nutrientFileName <- "data/USDA GFS IMPACT V5.xlsx"
IMPACTfileName <- "data/IMPACTData/Demand Results20150817.csv"
EARFileName <- "data/DRI EAR valuesV2.xlsx"
commodityFoodGroupLookupFileName <- "data/food commodity to food group table V1.xlsx"
# CSE - consumer support equivalent
#Note: the price a consumer pays is Pc * (1-CSE)
CSEFileName <- "data/IMPACTData/CSEs20150824.xlsx" 
#IMPACTregionsFileName <- "data/IMPACTRegionsMay2015.csv" # this file includes Denmark plus (DNP) and Sudan plus (SDP) and removes Greenland and South Sudan
IMPACTregionsFileName <- "data/IMPACTRegionsJan15tmp.csv" # this file removes Denmark plus (DNP) and South Sudan (SSD) as well as removes Greenland and South Sudan
IMPACTregions <- read.csv(IMPACTregionsFileName, stringsAsFactors = FALSE)
ctyNames  <- IMPACTregions$CTY
xcelOutFileName <- paste("nutrMetrics_",dateCreated,".xlsx",sep="")

#sorted list of IMPACT food commodities
tmp <- sort(c("cbeef","cpork","clamb","cpoul","ceggs","cmilk","cbarl","cmaiz",
              "cmill","crice","csorg","cwhea","cocer","ccass","cpota","cswpt","cyams","corat","cbean","cchkp",
              "ccowp","clent","cpigp","copul","cbana","cplnt","csubf","ctemf","cvege","csugr","cgrnd","cgdol",
              "crpsd","crpol","csoyb","csbol","csnfl","csfol","cplol","cpkol","ctols","ctool","ccoco","ccafe",
              "cteas","cothr"))
IMPACTfoodCommodities <- as.data.frame(tmp,stringsAsFactors = FALSE)
colnames(IMPACTfoodCommodities) <- "IMPACT_code"

daysInYear <- 365 #see http://stackoverflow.com/questions/9465817/count-days-per-year for a way to deal with leap years

# Read in and clean up files ----------------------------------------------

# Food groups and commodities ---------------------------------------------
# 13 food groups 
foodGroupsInfo <- read.xlsx(commodityFoodGroupLookupFileName, 
                            sheet = 1,
                            startRow = 1,
                            cols = 1:4,
                            colNames = TRUE)

# Note: staples is not a food group per se but included here because it is used in one of the diversity metrics
staples <- c("cereals", "roots")

# This is the list of food group codes as of June 28, 2015
# beverages <- c("beverages")
# cereals <- c("cereals")
# dairy <- c("dairy")
# eggs <- c("eggs")
# fish <- c("fish")
# fruits <- c("fruits") 
# meats <- c("meats")
# oils <-c("fats and oils")
# oilSeeds <- c("oil seeds")
# pulses <- c("pulses")
# roots <- c("roots and tubers")
# sweeteners <- c("sugar and sweeteners")
# vegetables <-c("vegetables")

allFoodGroups <- unique(foodGroupsInfo$food.group.code)

## nutrient lookup table readin and cleanup -------------------------------
#nutCodes <- c("ENERGY","PROT","FAT","CARBO","FIBR","CALC","PHOS","MGNS","POTS","SODM","IRON",
#              "ZINC","COPR","MNGN","VITA","VITD","VITE","VITC","THIAM","RIBF","NIAC","VITB6","FOLC",
#              "VITB12","PANTO") 

nutCodes <- c("water", "energy", "protein", "fat", "carbohydrate", "fiber", "sugar", "minerals",	
              "calcium", "iron", "magnesium", "phosphorus", "potassium", "sodium", "zinc","vitamins",	
              "vitamin_c", "thiamin",	"riboflavin",	"niacin", "vitamin_b6",	"folate", "vitamin_b12",	
              "vitamin_a_RAE", 	"vitamin_a_IU",	"vitamin_e", "vitamin_d2_3", "vitamin_d",	"vitamin_k",
              "lipids", "fatty_acids_tot_sat", "fatty_acids_mono_unsat", "fatty_acids_polyunsat",	
              "cholesterol", "other", "caffeine", "fatty_acids_tot_trans")

nutrients <- read.xlsx(nutrientFileName, 
                       sheet = 1,
                       rows = 3:46,
                       cols = 1:46,
                       colNames = TRUE)
nutrientNames_Units <- read.xlsx(nutrientFileName, 
                                 sheet = 1,
                                 rows = 1:3,
                                 cols = 10:46,
                                 colNames = FALSE)

#convert the NAs to 0 except in the first 5 columns which are text fields
temp <- nutrients[,6:ncol(nutrients)]
temp[is.na(temp)] <- 0
nutrients[,6:ncol(nutrients)] <- temp

# change nutrient unit from 100 gm to 1 kg
nutrients[,nutCodes] <- nutrients[,nutCodes] * 10

# convert to IMPACT unit equivalents (nutrients per carcass weight for meat)
nutrients[,nutCodes] <- 
  nutrients[,nutCodes] * nutrients[,"IMPACT_conversion"]

#add food groups to nutrients in a column called category
tmp <- foodGroupsInfo[,c("IMPACT_code","food.group.code")]
nutrients <- merge(nutrients, tmp, by = "IMPACT_code")

## IMPACT commodity file readin and cleanup -------------------------------
t1 <-read.csv(IMPACTfileName, stringsAsFactors = FALSE)
colnames(t1) <- c("IMPACTparameter", "scenario", "IMPACT_code" , "region","productiontype", 
                  "year", "value")
t1$year <-  as.character(t1$year)
#drop productiontype because it is not used in the demand analysis
drops <- c("productiontype")
t1 <- t1[, !(names(t1) %in% drops)]

#read in the CSEs
CSEs <- read.xlsx(CSEFileName, 
                  sheet = 1,
                  cols = 1:3,
                  colNames = TRUE)
colnames(CSEs) <- c("cty","IMPACT_code","CSE")

#Set up the EARs data
EARs <- read.xlsx(EARFileName, sheet = 1, startRow = 3, colNames = FALSE)
#make sure everything that should be numeric, is
for (j in 3:length(EARs)) (EARs[j] <- as.numeric(EARs[,j]))
#give the columns some names
colnames(EARs) <- c("NutCode","nutNames.Units","X0_0.5","X0.5_1","X1_3","X4_8",
                    "M9_13","M14_18","M19_30","M_31_50","M51_70","M70Plus",
                    "F9_13","F14_18","F19_30","F_31_50","F51_70","F70Plus",
                    "P14_18","P19_30","P31_50","L14_18","L19_30","L31_50")
#add columns to line up with the SSP pop data distribution from IIASA

#children
EARs$SSPX0_4 <- (EARs$X0_0.5 + EARs$X0.5_1 + EARs$X1_3)/3
EARs$SSPX5_9 <- (EARs$X4_8)
EARs$SSPX10_14 <- (EARs$X9_13)

#males
EARs$SSPM15_19 <- (EARs$X14_18)
EARs$SSPM20_24 <- (EARs$M19_30)
EARs$SSPM25_29 <- (EARs$M19_30)
EARs$SSPM30_34 <- (EARs$M31_50)
EARs$SSPM35_39 <- (EARs$M31_50)
EARs$SSPM40_45 <- (EARs$M31_50)
EARs$SSPM45_49 <- (EARs$M31_50)
EARs$SSPM50_54 <- (EARs$M31_50)
EARs$SSPM55_59 <- (EARs$M51_70)
EARs$SSPM60_64 <- (EARs$M51_70)
EARs$SSPM65_69 <- (EARs$M51_70)
EARs$SSPM70_74 <- (EARs$M70Plus)
EARs$SSPM75_79 <- (EARs$M70Plus)
EARs$SSPM80_84 <- (EARs$M70Plus)
EARs$SSPM85_89 <- (EARs$M70Plus)
EARs$SSPM90_94 <- (EARs$M70Plus)
EARs$SSPM95_99 <- (EARs$M70Plus)
EARs$SSPM100Plus <- (EARs$M70Plus)

#females
EARs$SSPF20_24 <- (EARs$F19_30)
EARs$SSPF25_29 <- (EARs$F19_30)
EARs$SSPF30_34 <- (EARs$F31_50)
EARs$SSPF35_39 <- (EARs$F31_50)
EARs$SSPF40_45 <- (EARs$F31_50)
EARs$SSPF45_49 <- (EARs$F31_50)
EARs$SSPF50_54 <- (EARs$F31_50)
EARs$SSPF55_59 <- (EARs$F51_70)
EARs$SSPF60_64 <- (EARs$F51_70)
EARs$SSPF65_69 <- (EARs$F51_70)
EARs$SSPF70_74 <- (EARs$F70Plus)
EARs$SSPF75_79 <- (EARs$F70Plus)
EARs$SSPF80_84 <- (EARs$F70Plus)
EARs$SSPF85_89 <- (EARs$F70Plus)
EARs$SSPF90_94 <- (EARs$F70Plus)
EARs$SSPF95_99 <- (EARs$F70Plus)
EARs$SSPF100Plus <- (EARs$F70Plus)
#pregnant and lactating are already included.
#delete old columns
EARs <- EARs[, !names(EARs) %in% 
               c("X0_0.5","X0.5_1","X1_3","X4_8",
                 "M9_13","M14_18","M19_30","M_31_50","M51_70","M70Plus",
                 "F9_13","F14_18","F19_30","F_31_50","F51_70","F70Plus")]
