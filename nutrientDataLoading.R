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
nutrientFileName <- "data/USDA GFS IMPACT V8.xlsx"

nutrients <- read.xlsx(nutrientFileName, 
                       sheet = 1,
                       rows = 3:50,
                       cols = 1:63,
                       colNames = TRUE)

nutrientNames_Units <- read.xlsx(nutrientFileName, 
                                 sheet = 1,
                                 rows = 1:3,
                                 cols = 10:46,
                                 colNames = FALSE)

#remove columns that are dividers, etc. This leaves only the IMPACT_code, edible share, IMPACT_conversion,
# the nutrient values, and the cooking retention values
colsToRemove <- c("name","USDA_code_desc","AUS_code","comment","water_g","inedible_share","proximates",
                  "minerals","vitamins","lipids","other","RetentionCode","RetentionDescription")
nutrients <- nutrients[,!(names(nutrients) %in% colsToRemove)]

# Source of requirements is http://www.nal.usda.gov/fnic/DRI/DRI_Tables/recommended_intakes_individuals.pdf ---

#list of columns with cooking retention values
cookretn.cols <- colnames(nutrients[, grep('_cr', names(nutrients))] )
#get list of nutrients in the food nutrient lookup table
#create list of columns that are not nutrients
temp <-  c("IMPACT_code", "edible_share", "IMPACT_conversion",cookretn.cols)
nutrients.food <- colnames(nutrients[, !(names(nutrients) %in% temp)])

# make lists of nutrients common to the food nutrient list and the requirements list ---
common.EAR <- intersect(colnames(nutrients),colnames(req.EAR.ssp))
common.RDA.macro <- intersect(colnames(nutrients),colnames(req.RDA.macro.ssp))
common.RDA.vits <- intersect(colnames(nutrients),colnames(req.RDA.vits.ssp))
common.UL.minrls <- intersect(colnames(nutrients),colnames(req.UL.minrls.ssp))
common.UL.vits <- intersect(colnames(nutrients),colnames(req.UL.vits.ssp))


# macro <- c("energy", "protein", "fat", "carbohydrate", "fiber", "sugar")
# minerals <- c("calcium", "iron", "potassium", "sodium", "zinc")
# vitamins <- c("vit_c", "thiamin",	"riboflavin",	"niacin", "vit_b6",	"folate", "vit_b12",
#           "vit_a", 	"vit_e_RAE", "vit_d2_3")
# fattyAcids <-  c("ft_acds_tot_sat", "ft_acds_plyunst")

#convert NAs to 100 (percent) for edible_share, IMPACT_conversion, and cooking retention
colsToConvert <- c("IMPACT_conversion","edible_share", cookretn.cols)
nutrients[colsToConvert][is.na(nutrients[colsToConvert])] <- 100

#convert the NAs to 0  in the nutrients columns 
nutrients[,nutrients.food][is.na(nutrients[,nutrients.food])] <- 0

# # change nutrient denominator unit from 100 gm to 1 kg; 
# commented out to leave units what nutritionists are familiar with
# nutrients[,nutrients.food] <- nutrients[,nutrients.food] * 10

# convert IMPACT consumption values to consumer nutrient intake ---
# reduce nutrient amount by conversion of meat from carcass to boneless (IMPACT_conversion)
# reduce nutrient amount by conversion of all items to edible share

nutrients[,nutrients.food] <- nutrients[,nutrients.food] * nutrients$IMPACT_conversion / 100
nutrients[,nutrients.food] <- nutrients[,nutrients.food] * nutrients$edible_share / 100
#multiply the amount of a nutrient in a food by the cooking retention value
for (i in 1:length(cookretn.cols)) {
  nutrientName <- substr(x = cookretn.cols[i], 1, nchar(cookretn.cols[i])-3)
  nutColName <- paste ("nutrients$",nutrientName,sep="")
  print(nutColName)
  nutRetentColName <- paste ("nutrients$",cookretn.cols[i],sep="")
  print(nutRetentColName)
  temp <- as.data.frame(eval(parse(text = nutRetentColName)) * eval(parse(text = nutColName)) /100)
  colnames(temp) <- nutrientName
  nutrients[, nutrientName] <- temp
}

#remove extraneous columns
colsToRemove <- c("edible_share","IMPACT_conversion",cookretn.cols)
nutrients <- nutrients[,!(names(nutrients) %in% colsToRemove)]
#add food groups to nutrients in a column called category
tmp <- foodGroupsInfo[,c("IMPACT_code","food.group.code")]
nutrients <- merge(nutrients, tmp, by = "IMPACT_code")

write.csv(nutrients,file = "results/nutrients_final.csv", fileEncoding = "Windows-1252")
