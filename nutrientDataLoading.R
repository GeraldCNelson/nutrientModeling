# A 'subroutine' of nutrientCalcs.R
require(openxlsx)
require(entropy)
require(reshape2)
require(plyr)
require(dplyr)
require(tidyr)
require(data.table)
require(splitstackshape)
require(plotrix)
# Data loading code for nutrientCalcs -------------------------------------
nutrientFileName <- "data/USDA GFS IMPACT V6.xlsx"


## nutrient lookup table readin and cleanup -------------------------------
#nutCodes <- c("ENERGY","PROT","FAT","CARBO","FIBR","CALC","PHOS","MGNS","POTS","SODM","IRON",
#              "ZINC","COPR","MNGN","VITA","VITD","VITE","VITC","THIAM","RIBF","NIAC","VITB6","FOLC",
#              "VITB12","PANTO") 

#the following codes are available but not being used right now. 
nutCodesExcluded <- c( "water", "vits",	"lipids", "ft_acds_tot_sat", "ft_acds_mono_unsat", 
                       "ft_acds_tot_trans","other","vits",
                       "cholesterol")
nutCodes <- c( "energy", "protein", "fat", "carbohydrate", "fiber", "sugar", 	
               "calcium", "iron", "magnesium", "phosphorus", "potassium", "sodium", "zinc",	
               "vit_c", "thiamin",	"riboflavin",	"niacin", "vit_b6",	"folate", "vit_b12",	
               "vit_a_RAE", 	"vit_a_IU",	"vit_e", "vit_d2_3", "vit_d",	"vit_k",
               "lipids", "ft_acds_tot_sat", "ft_acds_mono_unsat", "ft_acds_plyunst",	
               "cholesterol")
macro <- c("energy", "protein", "fat", "carbohydrate", "fiber", "sugar")
minerals <- c("calcium", "iron", "potassium", "sodium", "zinc")
vitamins <- c("vit_c", "thiamin",	"riboflavin",	"niacin", "vit_b6",	"folate", "vit_b12",
          "vit_a_RAE", 	"vit_e", "vit_d2_3")
fattyAcids <-  c("ft_acds_tot_sat", "ft_acds_plyunst")

all <- c( "energy", "protein", "fat", "carbohydrate", "fiber", "sugar", 	
          "calcium", "iron", "potassium", "sodium", "zinc",	
          "vit_c", "thiamin",	"riboflavin",	"niacin", "vit_b6",	"folate", "vit_b12",	
          "vit_a_RAE", 	"vit_e", "vit_d2_3", 	
          "ft_acds_tot_sat", "ft_acds_plyunst")

nutrients <- read.xlsx(nutrientFileName, 
                       sheet = 1,
                       rows = 3:50,
                       cols = 1:63,
                       colNames = TRUE)
#shorten variable names
colnames(nutrients) <- sub("fatty_acids","ft_acds",colnames(nutrients))
colnames(nutrients) <- sub("vitamin","vit",colnames(nutrients))
colnames(nutrients) <- sub("polyunsat","plyunst",colnames(nutrients))

nutrientNames_Units <- read.xlsx(nutrientFileName, 
                                 sheet = 1,
                                 rows = 1:3,
                                 cols = 10:46,
                                 colNames = FALSE)


#convert NAs to 100 (percent) for edible_share, IMPACT_conversion, and cooking retention
#note these names have been shortened (e.g., vitamin to vit)
colsToConvert <- c("IMPACT_conversion","edible_share", 
                   #minerals
                   "calcium_cr", "iron_cr", "magnesium_cr", "phosphorus_cr", "potassium_cr",
                   "sodium_cr", "zinc_cr",
                   #selected vitamins
                   "vit_c_cr", "thiamin_cr", "riboflavin_cr","niacin_cr", "vit_b6_cr", 
                   "folate_cr", "vit_b12_cr", "vit_A_cr")
nutrients[colsToConvert][is.na(nutrients[colsToConvert])] <- 100


#convert the NAs to 0  in the nutrients columns 
nutrients[,nutCodes][is.na(nutrients[,nutCodes])] <- 0

# change nutrient unit from 100 gm to 1 kg
nutrients[,nutCodes] <- nutrients[,nutCodes] * 10

# convert to IMPACT unit equivalents (nutrients per edible share, e.g. carcass weight for meat to boneless)
nutrients[,nutCodes] <- nutrients[,nutCodes] * nutrients[,"IMPACT_conversion"]
nutrients[,nutCodes] <- nutrients[,nutCodes] * nutrients[,"edible_share"]

#convert for cooking loss. There's probably a better way to do this but this kludge will work
nutrients[,nutCodes] <- nutrients[,nutCodes] * nutrients[,"calcium_cr"]
nutrients[,nutCodes] <- nutrients[,nutCodes] * nutrients[,"iron_cr"]        
nutrients[,nutCodes] <- nutrients[,nutCodes] * nutrients[,"magnesium_cr"]   
nutrients[,nutCodes] <- nutrients[,nutCodes] * nutrients[,"phosphorus_cr"]
nutrients[,nutCodes] <- nutrients[,nutCodes] * nutrients[,"potassium_cr"]
nutrients[,nutCodes] <- nutrients[,nutCodes] * nutrients[,"sodium_cr"]
nutrients[,nutCodes] <- nutrients[,nutCodes] * nutrients[,"zinc_cr"]           
nutrients[,nutCodes] <- nutrients[,nutCodes] * nutrients[,"vit_c_cr"]
nutrients[,nutCodes] <- nutrients[,nutCodes] * nutrients[,"thiamin_cr"]
nutrients[,nutCodes] <- nutrients[,nutCodes] * nutrients[,"riboflavin_cr"]
nutrients[,nutCodes] <- nutrients[,nutCodes] * nutrients[,"niacin_cr"]
nutrients[,nutCodes] <- nutrients[,nutCodes] * nutrients[,"vit_b6_cr"]         
nutrients[,nutCodes] <- nutrients[,nutCodes] * nutrients[,"folate_cr"]
nutrients[,nutCodes] <- nutrients[,nutCodes] * nutrients[,"vit_b12_cr"]
nutrients[,nutCodes] <- nutrients[,nutCodes] * nutrients[,"vit_A_cr"]

#add food groups to nutrients in a column called category
tmp <- foodGroupsInfo[,c("IMPACT_code","food.group.code")]
nutrients <- merge(nutrients, tmp, by = "IMPACT_code")

