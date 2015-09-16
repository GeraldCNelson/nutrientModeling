# A 'subroutine' of nutrientCalcs.R
# Data loading code for nutrientCalcs -------------------------------------
nutrientFileName <- "data/USDA GFS IMPACT V5.xlsx"


## nutrient lookup table readin and cleanup -------------------------------
#nutCodes <- c("ENERGY","PROT","FAT","CARBO","FIBR","CALC","PHOS","MGNS","POTS","SODM","IRON",
#              "ZINC","COPR","MNGN","VITA","VITD","VITE","VITC","THIAM","RIBF","NIAC","VITB6","FOLC",
#              "VITB12","PANTO") 

#the following codes are available but not being used right now. 
nutCodesExcluded <- c( "water", "vitamins",	"lipids", "fatty_acids_tot_sat", "fatty_acids_mono_unsat", 
                       "fatty_acids_polyunsat", "fatty_acids_tot_trans","other","vitamins"
                      "cholesterol")
nutCodes <- c( "energy", "protein", "fat", "carbohydrate", "fiber", "sugar", 	
              "calcium", "iron", "magnesium", "phosphorus", "potassium", "sodium", "zinc",	
              "vitamin_c", "thiamin",	"riboflavin",	"niacin", "vitamin_b6",	"folate", "vitamin_b12",	
              "vitamin_a_RAE", 	"vitamin_a_IU",	"vitamin_e", "vitamin_d2_3", "vitamin_d",	"vitamin_k",
              "lipids", "fatty_acids_tot_sat", "fatty_acids_mono_unsat", "fatty_acids_polyunsat",	
              "cholesterol")
macro <- c("energy", "protein", "fat", "carbohydrate", "fiber", "sugar")
minerals <- c("calcium", "iron", "potassium", "sodium", "zinc")
vitamins <- c("vitamin_c", "thiamin",	"riboflavin",	"niacin", "vitamin_b6",	"folate", "vitamin_b12",
                      "vitamin_a_RAE", 	"vitamin_e", "vitamin_d2_3")
fattyAcids <-  c("fatty_acids_tot_sat", "fatty_acids_polyunsat")

all <- c( "energy", "protein", "fat", "carbohydrate", "fiber", "sugar", 	
                          "calcium", "iron", "potassium", "sodium", "zinc",	
                          "vitamin_c", "thiamin",	"riboflavin",	"niacin", "vitamin_b6",	"folate", "vitamin_b12",	
                          "vitamin_a_RAE", 	"vitamin_e", "vitamin_d2_3", 	
                          "fatty_acids_tot_sat", "fatty_acids_polyunsat")

nutrients <- read.xlsx(nutrientFileName, 
                       sheet = 1,
                       rows = 3:50,
                       cols = 1:46,
                       colNames = TRUE)
nutrientNames_Units <- read.xlsx(nutrientFileName, 
                                 sheet = 1,
                                 rows = 1:3,
                                 cols = 10:46,
                                 colNames = FALSE)

#convert NAs to 1 for edible_share and IMPACT_conversion
nutrients[c("IMPACT_conversion","edible_share")][is.na(nutrients[c("IMPACT_conversion","edible_share")])] <- 1

#convert the NAs to 0  in the nutrients columns 
nutrients[,nutCodes][is.na(nutrients[,nutCodes])] <- 0

# change nutrient unit from 100 gm to 1 kg
nutrients[,nutCodes] <- nutrients[,nutCodes] * 10

# convert to IMPACT unit equivalents (nutrients per carcass weight for meat)
nutrients[,nutCodes] <- nutrients[,nutCodes] * nutrients[,"IMPACT_conversion"]
nutrients[,nutCodes] <- nutrients[,nutCodes] * nutrients[,"edible_share"]

#add food groups to nutrients in a column called category
tmp <- foodGroupsInfo[,c("IMPACT_code","food.group.code")]
nutrients <- merge(nutrients, tmp, by = "IMPACT_code")

