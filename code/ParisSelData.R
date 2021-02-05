# Script to Select Essential Data for Paris
# Start from Input Data

#  load data 
dbparis <- read.csv("../data/DataParisInput.csv")

#  make list of column names to be selected
dbparisc <- dbparis[,c("EntryID", "PropIDMod", "TransID", "Duplicate", "StreetSerieMatch", "Arr1", "Arr2", "Arr3", "Arr4", "QuartierVasserotEstim", 
         "House_Number_mod", "House_No_Mod2", "Sale", "DayTax", "Mtax",
         "Ytax", "Dfinal", "Mfinal", "Yfinal", "PriceMod2")]

# remove duplicate observations
dbparisc <- dbparisc[dbparisc$Duplicate == 0 & dbparisc$Yfinal > 0,]
dbparisc$Duplicate <- NULL

# update column names
colnames(dbparisc) <- c("EntryID", "PropID", "TransID" ,"Street", "Arr1", "Arr2", "Arr3", "Arr4", "Quartier", "HouseNumber", 
                        "HouseNumberNumeric", "SaleType", "DayReg", "MonthReg", 
                        "YearReg", "Day", "Month", "Year", "Price")

dbparisc$SaleType <- tolower(dbparisc$SaleType)
dbparisc$SaleType[dbparisc$SaleType == "vente "] <- c("vente")
dbparisc$SaleType[dbparisc$SaleType == "liquidiation"] <- c("liquidation")

# write file used for operations
write.csv(dbparisc, "../data/DataParisAnalysis.csv")