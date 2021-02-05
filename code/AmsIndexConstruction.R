# Code to estimate house price and rent price index for Amsterdam

# load data
data <- read.csv("../data/Amsterdam/DataAmsRepeatSales.csv")
data2 <- read.csv("../data/Amsterdam/AmsRPI.csv")

a <- merge(data, data2, by.x="year", by.y="X", all=T)

# set dates 
a$day <- 15
a$month <- 7
a$date <- as.Date(paste (a$day, a$month, a$year, sep = "." ), format = "%d.%m.%Y")
startDate <- as.Date("1840-01-01")
endDate <- as.Date("1979-12-31")
indexFreq <- 12

# delete observations outside range
# a <- subset(a, date >= startDate & date <= endDate & duplicate == 0 & price > 0 & (appraisal == 0 | year == 1945) ) # without appraisals
a <- subset(a, date >= startDate & date <= endDate & duplicate == 0 & price > 0 & Stad == "Amsterdam") # with appraisals 
# a <- subset(a, date >= startDate & date <= endDate & appraisal == 1 & duplicate == 0 & price > 0 ) # appraisals only
a <- a[order(a$propid, a$date),]

# set intervals for all sales
months <- seq(from=startDate, to=endDate+1, by="month")
intervals <- months[seq((1+indexFreq), length(months), indexFreq)]-1

a$interval <- 1
for(i in length(intervals):1){
  a$interval[a$date <= intervals[i]]<-i
}

# log price differences
a$dlnprice[2:nrow(a)] <- diff(log(a$price))
a$priceminrpi[2:nrow(a)] <- diff(log(a$price)) - diff(log(a$RPI))

# set buy and sel id
a$buyid[2:nrow(a)] <- a$propid[1:(nrow(a)-1)] 
a$pricebuy[2:nrow(a)] <- a$price[1:(nrow(a)-1)] 
a$yearbuy[2:nrow(a)] <- a$year[1:(nrow(a)-1)] 

# set buy and sel data
a$datesel <- as.Date(NA)
a$datesel[1:(nrow(a)-1)] <- a$date[2:nrow(a)]
a$datebuy <- as.Date(NA)
a$datebuy[2:nrow(a)] <- a$date[1:(nrow(a)-1)]
a$type <- NA
a$type[2:nrow(a)] <- diff(a$appraisal)

# set time to sale
a$timetosale[2:nrow(a)] <- diff(a$date)

# set time periods of buy months / intervals 
a$buyinterval[2:nrow(a)] <- a$interval[1:(nrow(a)-1)]
a$selinterval <- a$interval
a$buyyear[2:nrow(a)] <- a$year[1:(nrow(a)-1)] 

# take out pairs (not same date)
a <- subset(a, propid == buyid & a$timetosale > 1)
a <- subset(a, a$priceminrpi > -1.95 & a$priceminrpi < 1.95) # 1.95 for prices, 1.5 for rents
a <- subset(a, a$buyyear != a$year)

# the four lines below (lines 63 to 66) and lines 85 can be activated to measure sensitivity to sample size: 
# d <- a
# vol <- rep(NA, 1000)
# for (j in 1:1000) { 
# a <- d[sample(nrow(d), 0.46*15125),]

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
# vol[j] <- sd(diff(BMNreg$coefficients[60:139])) }

summary(diff(BMNreg$coefficients[60:139]))
sd(diff(BMNreg$coefficients[60:139]))

PriceIndex <- data.frame(1899:1979, exp(BMNreg$coefficients[59:139])*100)
colnames(PriceIndex) <- c("Year", "HPI")

PriceIndexIncl <- data.frame(1900:1979, exp(BMNreg$coefficients[60:139])*100)
colnames(PriceIndexIncl) <- c("Year", "HPIwithApp")

tseries1 <- read.csv("../data/TimeSeriesAmsAnalysis.csv")
tseries1$HPI <- PriceIndex$HPI
tseries1$HPIwithApp <- PriceIndexIncl$HPIwithApp
# tseries1 <- merge(tseries1, PriceIndex, by="Year", all.x=T, all.y=T)
# tseries1 <- merge(tseries1, PriceIndexIncl, by="Year", all.x=T, all.y=T)
tseries1 <- tseries1[,3:33]

write.csv(tseries1, file="../data/TimeSeriesAmsAnalysis.csv")