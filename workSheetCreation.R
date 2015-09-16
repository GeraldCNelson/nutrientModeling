# A 'subroutine' of nutrientCalcs.R
# Creates and populates several worksheets of an excel file that holds modeling results
#create styles to format the worksheets

#spreadsheet to output nutrition quantities
wbNut <- f.createGeneralWorksheet()
wbInfNut <- wbInfoGeneral
tmp.out <- by(nutShare,nutShare[,c("food.group.code","nutrient","scenario")],f.write.nut.sheet,wbNut)
tmp <- as.data.frame(matrix(unlist(tmp.out), nrow=length(tmp.out), byrow=T))
colnames(tmp) <- colnames(wbInfNut)
wbInfNut <- rbind(wbInfNut,tmp)
f.finalizeWB(wbNut,wbInfNut,short.name)

#write out spreadsheet for nutrient consumption summary
wbNutsum <- f.createGeneralWorksheet()
wbInfsum <- wbInfoGeneral
tmp.sum.out <- by(nutShareTot,nutShareTot[,c("nutrient","scenario")],f.write.nut.sum.sheet,wbNutsum)
f.finalizeWB(wb = wbNutsum,wbInf = wbInfsum,nut.name = paste(short.name,"Sum",sep="_"))

