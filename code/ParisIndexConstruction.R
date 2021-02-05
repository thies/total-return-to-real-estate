# Code to estimate house price and rent price index for Paris
# This codes estimates the house price index (Section 3) and modified rent price index for Paris (Appendix B)

# change working directory to the directory of the script
try( setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))

# load data
data <- read.csv("../data/DataParisAnalysis.csv")
data$X <- NULL

# add Eichholtz, Korevaar & Lindenthal Paris RPI for each observation to the data to remove outliers
tseries1 <- read.csv("../data/TimeSeriesParisAnalysis.csv")
tseries <- tseries1[,c("Year", "EichholtzRPI")] # when new rent index is finished, add new rent index instead
tseries$RPI <- tseries1$EichholtzRPI
data <- merge(data, tseries, by.x="Year", by.y="Year", all.x="TRUE", all.y="FALSE", sort="FALSE")
# data <- data[order(data$PropID, data$Year, data$Month, data$Day),]
# data <- data[,c(2:18,1,19,20, 21)]

data <- subset(data, data$Price > 0 & data$Year > 1805 & data$Year < 1944)

# STEP 2: Estimate indices for prices (Section 3) and rents (Appendix B)

# subset in sales and rent datasets
rents <- subset(data, SaleType == "bail" | SaleType == "succession" | SaleType == "donation")
sales <- subset(data, SaleType == "vente" | SaleType == "adjudication" | SaleType == "liquidation" | SaleType == "licitation")

# set rents or sales
a <- sales

# decapitalize post-1918 observations, activate this code for rents only
# a$Price[a$SaleType == "Succession" & a$Year > 1917] <- a$Price[a$SaleType == "Succession" & a$Year > 1917]/10

a$date <- as.Date(paste (a$Day, a$Month, a$Year, sep = "." ), format = "%d.%m.%Y")
startDate <- as.Date("1806-01-01")
endDate <- as.Date("1943-12-31")
indexFreq <- 12

# delete observations outside range
a <- subset(a, date >= startDate & date <= endDate & Price > 0)
a <- a[order(a$PropID, a$date),]

# set intervals for all sales
months <- seq(from=startDate, to=endDate+1, by="month")
intervals <- months[seq((1+indexFreq), length(months), indexFreq)]-1

a$interval <- 1
for(i in length(intervals):1){
  a$interval[a$date <= intervals[i]]<-i
}

# log price differences
a$dlnprice[2:nrow(a)] <- diff(log(a$Price))
a$dlnrpi[2:nrow(a)] <- diff(log(a$RPI))
a$priceminrpi[2:nrow(a)] <- a$dlnprice[2:nrow(a)]-a$dlnrpi[2:nrow(a)]

# set buy and sel id
a$buyid[2:nrow(a)] <- a$PropID[1:(nrow(a)-1)] 
a$pricebuy[2:nrow(a)] <- a$price[1:(nrow(a)-1)] 
a$yearbuy[2:nrow(a)] <- a$year[1:(nrow(a)-1)] 
a$selid <- NA
a$selid[1:(nrow(a)-1)] <- a$PropID[2:nrow(a)] 

a$datesel <- as.Date(NA)
a$datesel[1:(nrow(a)-1)] <- a$date[2:nrow(a)]
a$datebuy <- as.Date(NA)
a$datebuy[2:nrow(a)] <- a$date[1:(nrow(a)-1)]
a$typebuy <- NA
a$typebuy[2:nrow(a)] <- a$SaleType[1:nrow(a)-1]

# set time to sale
a$timetosale[2:nrow(a)] <- diff(a$date)

# set time periods of buy months / intervals 
a$buyinterval[2:nrow(a)] <- a$interval[1:(nrow(a)-1)]
a$selinterval <- a$interval
a$buyyear[2:nrow(a)] <- a$Year[1:(nrow(a)-1)] 

table(a$buyyear)

# take out pairs (not same date)
a <- subset(a, PropID == buyid)
a <- subset(a, a$priceminrpi > -1.95 & a$priceminrpi < 1.95) # set to 1.95 for prices, 1.386 for rents
a <- subset(a, a$buyyear != a$Year & a$timetosale > 0)

# The four lines below (86-89) and line 109 can be  activated to estimate the sensitivity of index volatility to sample size
# d <- a
# vol <- rep(NA, 1000)
# for (j in 1:1000) { 
# a <- d[sample(nrow(d), 0.99*17770),]

### time dummies
b <- matrix(0, nrow=nrow(a), ncol=length(intervals))

## distribute -1 and +1 over matrix
for (i in 1:nrow(b)) {
  b[i,a$buyinterval[i]] <- -1
  b[i,a$selinterval[i]] <- 1
}

# set names of independent variables
colnames(b) <- as.character(format(intervals,'%Y.%m'))

# drop first year; normalized to zero (base year = 1808)
b <- b[,-1]

#### ESTIMATION OF INDEX ####
BMNreg <- lm(a$dlnprice ~ b -1)
summary(BMNreg)
#vol[j] <- sd(diff(BMNreg$coefficients[3:137])) }

# summary statistics of index
summary(c(0,diff(BMNreg$coefficients)))
sd(diff(BMNreg$coefficients))

# identify price index
PriceIndex <- data.frame(1806:1943, c(100,exp(BMNreg$coefficients)*100))
colnames(PriceIndex) <- c("Year", "HPI")

# identify updated price index
RentIndex <- data.frame(1806:1943, c(100,exp(BMNreg$coefficients)*100))
colnames(RentIndex) <- c("Year", "RPItot")

# merge indices with other time series and write into file (already done here)
# tseries1 <- merge(tseries1, RentIndex, by="Year", all.x=T, all.y=T)
# tseries1 <- merge(tseries1, PriceIndex, by="Year", all.x=T, all.y=T)
# write.csv(tseries1, file="../data/TimeSeriesParisAnalysis.csv")