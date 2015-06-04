fbs <- read.csv("~/Downloads/FoodBalanceSheets_E_All_Data.csv")

test<-FoodBalanceSheets_E_All_Data[!duplicated(FoodBalanceSheets_E_All_Data$Country),]
FAO.Countries <- test[,c("Country","CountryCode")]
row.names(FAO.Countries) <- seq(nrow(FAO.Countries))

test<-FoodBalanceSheets_E_All_Data[!duplicated(FoodBalanceSheets_E_All_Data$Item),]
FAO.Items<- test[,c("Item","ItemCode","Unit")]
row.names(FAO.Items) <- seq(nrow(FAO.Items)) 


test<-FoodBalanceSheets_E_All_Data[!duplicated(FoodBalanceSheets_E_All_Data$Element),]
FAO.Elements <- test[,c("Element","ElementGroup","ElementCode")]
row.names(FAO.Elements) <- seq(nrow(FAO.Elements)) 


FAO.Unit <- unique(FoodBalanceSheets_E_All_Data$Unit) 
df.Item<-data.frame(FAO.Item,FAO.ItemCode)
df.Element<-data.frame(FAO.Element,FAO.ElementGroup,FAO.ElementCode)
df.Countries<-data.frame(FAO.Country,FAO.CountryCode)
