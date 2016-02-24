#' @author Gerald C. Nelson, \email{nelson.gerald.c@@gmail.com}
#' @keywords utilities, IMPACT data, gdx, alcohol elasticities
# Intro -------------------------------------------------------------------

#Copyright (C) 2015 Gerald C. Nelson, except where noted

#     This program is free software: you can redistribute it and/or modify it
#     under the terms of the GNU General Public License as published by the Free
#     Software Foundation, either version 3 of the License, or (at your option)
#     any later version.
#
#     This program is distributed in the hope that it will be useful, but
#     WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#     or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#     for more details at http://www.gnu.org/licenses/.

#' @description
#' Create a table of elasticities for alcoholic beverages. Initially
#' both price and income elasticities are identical for all countries for all years
#' 
#' # Uses income and price elasticity parameters for the US from
# Nelson, Jon P. 2013. “Meta-Analysis of Alcohol Price and Income Elasticities –
# with Corrections for Publication Bias.” Health Economics Review 3 (1): 17.
# doi:10.1186/2191-1991-3-17. http://www.healtheconomicsreview.com/content/3/1/17.

# beverage,price_elast,income_elast
# beer, -0.30, 0.50
# wine, -0.45, 1.00
# spirits, -0.55, 1.00

source("R/dataPrep.setup.R")
regions.all <- getNewestVersion("regions.all")

dt.alcIncElast <-
  data.table(
    ISO_code = regions.all$ISO_code,
    priceElas.beer = -0.30,
    incElas.beer = 0.50,
    priceElas.wine = -0.45,
    incElas.wine = 1.00,
    priceElas.spirits = -0.55,
    incElas.spirits = 1.00
  )

saveRDS(dt.alcIncElast, file = paste(mData,"/alcElast.",Sys.Date(),".rds",sep=""))
