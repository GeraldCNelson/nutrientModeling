IMPACTfileName <- "data/IMPACTData/Demand Results20150817.csv"
#IMPACTregionsFileName <- "data/IMPACTRegionsMay2015.csv" # this file includes Denmark plus (DNP) and Sudan plus (SDP) and removes Greenland and South Sudan
IMPACTregionsFileName <- "data/IMPACTRegionsJan15tmp.csv" # this file removes Denmark plus (DNP) and South Sudan (SSD) as well as removes Greenland and South Sudan
IMPACTregions <- read.csv(IMPACTregionsFileName, stringsAsFactors = FALSE)
ctyNames  <- IMPACTregions$CTY

#sorted list of IMPACT food commodities
tmp <- sort(c("cbeef","cpork","clamb","cpoul","ceggs","cmilk","cbarl","cmaiz",
"cmill","crice","csorg","cwhea","cocer","ccass","cpota","cswpt","cyams","corat","cbean","cchkp",
"ccowp","clent","cpigp","copul","cbana","cplnt","csubf","ctemf","cvege","csugr","cgrnd","cgdol",
"crpsd","crpol","csoyb","csbol","csnfl","csfol","cplol","cpkol","ctols","ctool","ccoco","ccafe",
"cteas","cothr"))
IMPACTfoodCommodities <- as.data.frame(tmp,stringsAsFactors = FALSE)
colnames(IMPACTfoodCommodities) <- "IMPACT_code"

daysInYear <- 365 #see http://stackoverflow.com/questions/9465817/count-days-per-year for a way to deal with leap years
## IMPACT commodity file readin and cleanup -------------------------------
t1 <-read.csv(IMPACTfileName, stringsAsFactors = FALSE)
colnames(t1) <- c("IMPACTparameter", "scenario", "IMPACT_code" , "region","productiontype", 
"year", "value")
t1$year <-  as.character(t1$year)
#drop productiontype because it is not used in the demand analysis
drops <- c("productiontype")
t1 <- t1[, !(names(t1) %in% drops)]