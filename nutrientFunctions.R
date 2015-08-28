f.perCapFood <- function(rgn, scen) {
  tmp <- t1.food[(t1.food$IMPACTparameter == "pcFoodAvail") & 
                   (t1.food$region == rgn) &
                   (t1.food$scenario == scen), 
                 c("IMPACT_code","year","value")]
  f.cleanUp(tmp)
}

f.cleanUp <- function(tmp) {
  tmp.wide <- dcast(tmp, IMPACT_code ~ year, mean, value.var = "value")
  colnames(tmp.wide) <- make.names(colnames(tmp.food.percap.wide))
  tmp.wide <- merge(tmp.wide,IMPACTfoodCommodities, by = "IMPACT_code", all.y = TRUE)
  tmp.wide[is.na(tmp.wide)] <- 0
  return(tmp.wide)
}

f.perCapGDP <- function(rgn, scen) {
  tmp <- t1.food[(t1$IMPACTparameter == "pcGDP") & 
                   (t1.food$region == rgn) &
                   (t1.food$scenario == scen), 
                 c("IMPACT_code","year","value")]
  f.cleanUp(tmp)  
}

f.Pw <- function(scen) {
  tmp <- t1.food[(t1.food$IMPACTparameter == "Pw") & 
                   (t1.food$scenario == scen), 
                 c("IMPACT_code","year","value")]
  f.cleanUp(tmp)  
}

f.Pc <- function(rgn, scen)
{
  tmp <- t1.food[(t1.food$IMPACTparameter == "Pc") & 
                   (t1.food$region == rgn) &
                   (t1.food$scenario == scen), 
                 c("IMPACT_code","year","value")]
  f.cleanUp(tmp)  
}

f.Pcon <- function(rgn, scen)
{
  tmp <- t1.food[(t1.food$IMPACTparameter == "Pc") & 
                   (t1.food$region == rgn) &
                   (t1.food$scenario == scen), 
                 c("IMPACT_code","year","value")]
  tmp.wide <-  f.cleanUp(tmp)
  tmp.cse <- CSEs[CSEs$cty == rgn, c("IMPACT_code","CSE")]
  tmp.cse <- merge(tmp.cse,IMPACTfoodCommodities, by = "IMPACT_code", all.y = TRUE)
  tmp.cse[is.na(tmp.cse)] <- 0
  test <- tmp.wide[,2:47] * (1-tmp.cse$CSE)
  return(cbind(tmp.cse$IMPACT_code,test,stringsAsFactors=FALSE))
}

