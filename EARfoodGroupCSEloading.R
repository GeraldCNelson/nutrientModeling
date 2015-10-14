EARFileName <- "data/DRI EAR valuesV2.xlsx"
commodityFoodGroupLookupFileName <- "data/food commodity to food group table V1.xlsx"
# CSE - consumer support equivalent
#Note: the price a consumer pays is Pc * (1-CSE)
CSEFileName <- "data/IMPACTData/CSEs20150824.xlsx" 
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
#read in the CSEs
CSEs <- read.xlsx(CSEFileName, 
                  sheet = 1,
                  cols = 1:3,
                  colNames = TRUE)
colnames(CSEs) <- c("region","IMPACT_code","CSE")

#Set up the EARs data

EARs <- read.xlsx(EARFileName, sheet = 1, startRow = 3, colNames = FALSE)
#make sure everything that should be numeric, is
for (j in 3:length(EARs)) (EARs[j] <- as.numeric(EARs[,j]))
#give the columns some names, M - Male, F- Female, X - children of both genders, P - pregnant, L - lactating
colnames(EARs) <- c("NutCode","nutNames.Units","X0_0.5","X0.5_1","X1_3","X4_8",
                    "M9_13","M14_18","M19_30","M31_50","M51_70","M70Plus",
                    "F9_13","F14_18","F19_30","F31_50","F51_70","F70Plus",
                    "P14_18","P19_30","P31_50","L14_18","L19_30","L31_50")

#add columns to line up with the SSP pop data distribution from IIASA; adjusted for pregnant and lactating women

#children
EARs$SSPF0_4 <- (EARs$X0_0.5 + EARs$X0.5_1 + EARs$X1_3)/3
EARs$SSPM0_4 <- (EARs$X0_0.5 + EARs$X0.5_1 + EARs$X1_3)/3
EARs$SSPF5_9 <- (EARs$X4_8)
EARs$SSPM5_9 <- (EARs$X4_8)
EARs$SSPF10_14 <- (EARs$F9_13)
EARs$SSPM10_14 <- (EARs$M9_13)

#males
EARs$SSPM15_19 <- (EARs$M14_18)
EARs$SSPM20_24 <- (EARs$M19_30)
EARs$SSPM25_29 <- (EARs$M19_30)
EARs$SSPM30_34 <- (EARs$M31_50)
EARs$SSPM35_39 <- (EARs$M31_50)
EARs$SSPM40_44 <- (EARs$M31_50)
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
# EARs$SSPF20_24 <- (EARs$F19_30)
# EARs$SSPF25_29 <- (EARs$F19_30)
# EARs$SSPF30_34 <- (EARs$F31_50)
# EARs$SSPF35_39 <- (EARs$F31_50)
EARs$SSPF15_49 <-  (EARs$F14_18 + EARs$F19_30 + EARs$F31_50)/3
# EARs$SSPF40_45 <- (EARs$F31_50)
# EARs$SSPF45_49 <- (EARs$F31_50)
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
EARs$SSPLact <- (EARs$L14_18 + EARs$L19_30 + EARs$L31_50)/3
EARs$SSPPreg <- (EARs$P14_18 + EARs$P19_30 + EARs$P31_50)/3
#delete old columns
EARs <- EARs[, !names(EARs) %in% 
               c("X0_0.5","X0.5_1","X1_3","X4_8",
                 "M9_13","M14_18","M19_30","M31_50","M51_70","M70Plus",
                 "F9_13","F14_18","F19_30","F31_50","F51_70","F70Plus",
                 "P14_18","P19_30","P31_50","L14_18","L19_30","L31_50")]
#convert NAs to zeros
EARs[is.na(EARs)] <- 0
