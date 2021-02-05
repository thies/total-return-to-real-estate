#####################################################################
############### Code for Idiosyncratic Risk Analyses ################
#####################################################################
# load packages
library(dplyr)
library(stargazer)
library(lmtest)
library(sandwich)
library(ggplot2)
library(tidyr)
library(reshape2)
library(boot)
options(scipen=15)
set.seed(999)

try( setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))


## Colour scheme
colour <- c("#230356","#076391","#2fd1d1","#b51676","#e90d3a","#f98e19","#ffc217")

##################################################
### Set Up Repeat Cap Gain / Yield / Rent Data ###
##################################################

# load initial data
dat <- read.csv("../data/Amsterdam/DataAmsterdamInput.csv")
ts <- read.csv("../data/Amsterdam/TimeSeriesAmsAnalysis.csv")
coords <- read.csv("../data/Amsterdam/geocoded.addresses.amsterdam.csv", as.is=TRUE)
# add neighbourhood ids to data
dat$full.addr <- paste(dat$StraatMod, dat$No)
dat$full.addr <- paste(dat$full.addr, ", Amsterdam, Netherlands", sep="")
dat <- merge(dat, subset(coords, select=c("full.addr", "neigh.name", "neigh.code", "lat","lon", "dist.centre")), by="full.addr", all.x=TRUE)

# modify options.
dat <- dat[,c(colnames(dat)[1:37], "neigh.code", "lat", "lon","dist.centre")]

dat$M[is.na(dat$M)] <- 7
dat$D[is.na(dat$D)] <- 15
dat$date <- as.Date(paste (dat$D, dat$M, dat$Y, sep = "." ), format = "%d.%m.%Y")

# 1 excludes appraisals, 2 includes appraisals
ts1 <- data.frame(ts$Year, ts$MedianYield, ts$HPI, ts$RPI)
colnames(ts1) <- c("Year", "MedYield", "HPI", "RPI")
ts2 <- data.frame(ts$Year, ts$MedianYieldincApp, ts$HPIwithApp, ts$RPI)
colnames(ts2) <- c("Year", "MedYield", "HPI", "RPI")

# Selection option: without or without appraisals, main body of paper uses Option 2, including appraisal

# OPTION 1: without appraisals
d <- subset(dat, Y > 1899 & Plaats == "Amsterdam" & GrossYieldFinal > 0 & Price > 0 & RentAll > 0 & Veilingprijs > 0)
e <- subset(dat, Y > 1899 & Plaats == "Amsterdam" & Veilingprijs > 0)
d <- merge(d, ts1, by.x="Y", by.y="Year", all.x=T, all.y=F)
e <- merge(e, ts1, by.x="Y", by.y="Year", all.x=T, all.y=F)

# OPTION 2: with appraisals
d <- subset(dat, Y > 1899 & Plaats == "Amsterdam" & GrossYieldFinal > 0 & Price > 0 & RentAll > 0)
e <- subset(dat, Y > 1899 & Plaats == "Amsterdam" & Price > 0)
d <- merge(d, ts2, by.x="Y", by.y="Year", all.x=T, all.y=F)
e <- merge(e, ts2, by.x="Y", by.y="Year", all.x=T, all.y=F)

# Remove outliers
medianprice <- aggregate(dat$Price[!is.na(dat$Price)], by=list(dat$Y[!is.na(dat$Price)]), median) # get median price to remove
colnames(medianprice) <- c("Year", "MedianPriceInit")
d <- merge(d, medianprice, by.x="Y", by.y="Year", all.x="TRUE", all.y="FALSE", sort="FALSE")
e <- merge(e, medianprice, by.x="Y", by.y="Year", all.x="TRUE", all.y="FALSE", sort="FALSE")
d <- subset(d, log(d$GrossYieldFinal) > log(d$MedYield) -1.386 & log(d$GrossYieldFinal) < log(d$MedYield) + 1.386 &
              log(d$Price) > log(d$MedianPriceInit) -2.198 & log(d$Price) < log(d$MedianPriceInit) + 2.198)
e <- subset(e, log(e$Price) > log(e$MedianPriceInit) -2.198 & log(e$Price) < log(e$MedianPriceInit) + 2.198)

# Compute summary statistics yields
sd(log(d$GrossYieldFinal+1))
sd(log(d$GrossYieldFinal+1)-log(d$MedYield+1))

# Compute summary statistics by 10 year period
d$Year10 <- floor(d$Y/10)
d$Year5 <- floor(d$Y/5)

# Volatility per 10 year period and 5 year period
aggregate(log(d$GrossYieldFinal+1), by=list(10*d$Year10), sd)
aggregate(log(d$GrossYieldFinal+1)-log(d$MedYield+1), by=list(10*d$Year10), sd)
aggregate(log(d$GrossYieldFinal+1), by=list(5*d$Year5), sd)
aggregate(log(d$GrossYieldFinal+1)-log(d$MedYield+1), by=list(5*d$Year5), sd)

###########################################
### Covariance Yields and Capital gains ###
###########################################

l <- 20 # set number of years to be analyzed: 15 for regular sales, 20 for sales and appraisals
a <- d
a <- a[order(a$PropID, a$date),]

# log price differences
a$dlnprice[2:nrow(a)] <- diff(log(a$Price))
a$dlnrent[2:nrow(a)] <- diff(log(a$RentAll))
a$dlnryield[2:nrow(a)] <- diff(log(a$GrossYieldFinal))
a$dlnmyield[2:nrow(a)] <- diff(log(a$MedYield))
a$dlnmprice[2:nrow(a)] <- diff(log(a$HPI))
a$dlnmrent[2:nrow(a)] <- diff(log(a$RPI))
a$RentDiffBuy[2:nrow(a)] <- a$RentDiff[1:nrow(a)-1]
a$InitYield[2:nrow(a)] <- a$GrossYieldFinal[1:nrow(a)-1]
a$InitMedYield[2:nrow(a)] <- a$MedYield[1:nrow(a)-1]

# set buy id and time difference
a$buyid[2:nrow(a)] <- a$PropID[1:(nrow(a)-1)] 
a$timetosale[2:nrow(a)] <- diff(a$date)
a$yeardiff[2:nrow(a)] <- diff(a$Y)
a$buyyear[2:nrow(a)] <-  a$Y[1:(nrow(a)-1)] 

# compute residuals rent differences and capital gains
a$rentresid <- a$dlnrent-a$dlnmrent
a$priceresid <- a$dlnprice-a$dlnmprice

# take out pairs (not same year)
a <- subset(a, a$PropID == a$buyid & a$yeardiff > 0)

table(a$buyyear)
table(a$Y)

# account for neighbourhood effects in yields
a$neigh.code[is.na(a$neigh.code)] <- 999 # geocoding is noisy
neigh.effects <- lm(GrossYieldFinal~factor(Y)+factor(neigh.code), data=a)
neigh.effects.coeffs <- as.data.frame( neigh.effects$coefficients )
colnames(neigh.effects.coeffs) <- "neigh.effect"
neigh.effects.coeffs$neigh.code <- row.names(neigh.effects.coeffs)
neigh.effects.coeffs <- subset(neigh.effects.coeffs, grepl('neigh.code',neigh.code) )
neigh.effects.coeffs$neigh.code <-as.numeric( gsub("[a-z().]","", neigh.effects.coeffs$neigh.code, perl=TRUE))
a <- merge( a,neigh.effects.coeffs, by="neigh.code", all.x = TRUE)
a$neigh.effect[is.na(a$neigh.effect)] <- 0 # base case

# Define Relevant Variables
df <- data.frame(log(a$InitYield+1))
colnames(df) <- c("LogInitYield")
df$LogInitYieldResid <- log(a$InitYield+1)-log(a$InitMedYield+1)
df$LogInitYieldResidNeigh <- log(a$InitYield+1)-log(a$InitMedYield+1)-log(a$neigh.effect+1)
df$LogInitYieldResidSq <- df$LogInitYieldResid^2
df$LogFinYield <- log(a$GrossYieldFinal+1)
df$LogFinYieldResid <- log(a$GrossYieldFinal+1)-log(a$MedYield+1)
df$LogFinYieldResidNeigh <- log(a$GrossYieldFinal+1)-log(a$MedYield+1)-log(a$neigh.effect+1)
df$LogFinYieldResidSq <- df$LogFinYieldResid^2
df$DiffLogYield <- a$dlnryield 
df$DiffLogYieldResid <- a$dlnryield - a$dlnmyield
df$LogCapGain <- a$dlnprice
df$LogCapGainResid <- a$priceresid
df$LogRentDiff <- a$dlnrent
df$LogRentDiffResid <- a$rentresid
df$YearsToSale <- a$yeardiff
# df$TimeToSale <- a$timetosale/365.25

# volatility of repeated yields
sd(c(df$LogInitYieldResidNeigh, df$LogFinYieldResidNeigh))

# Correlations between Yields and Capital Gains
cor.test(df$LogCapGain, df$LogInitYield)
cor.test(df$LogCapGain, df$LogFinYield)
cor.test(df$LogCapGain, (df$LogFinYield+df$LogInitYield)/2)

cor.test(df$LogCapGainResid, df$LogInitYieldResidNeigh)
cor.test(df$LogCapGainResid, df$LogFinYieldResidNeigh)
cor.test(df$LogCapGainResid, (df$LogFinYieldResidNeigh+df$LogInitYieldResidNeigh)/2)

# Compute Correlation in Yields, Capital Gains and Rent / Prices Over Time
CorrelYields <- df %>%
  group_by(YearsToSale) %>%
  summarize(COR=cor(LogInitYield, LogFinYield))
colnames(CorrelYields) <- c("YearsToSale", "CorrelationYields")

CorrelYieldsResid <- df %>%
  group_by(YearsToSale) %>%  
  summarize(COR=cor(LogInitYieldResid, LogFinYieldResid))
colnames(CorrelYieldsResid) <- c("YearsToSale", "CorrelationYieldsResid")

CorrelYieldsResidNeigh <- df %>%
  group_by(YearsToSale) %>%  
  summarize(COR=cor(LogInitYieldResidNeigh, LogFinYieldResidNeigh))
colnames(CorrelYieldsResidNeigh) <- c("YearsToSale", "CorrelationYieldsResidNeigh")

CorrelInitYieldsCapGain <- df %>%
  group_by(YearsToSale) %>%  
  summarize(COR=cor(LogInitYield, LogCapGain))
colnames(CorrelInitYieldsCapGain) <- c("YearsToSale", "CorrelationInitYieldsCapGain")

CorrelInitYieldsCapGainResid <- df %>%
  group_by(YearsToSale) %>%  
  summarize(COR=cor(LogInitYieldResid, LogCapGainResid))
colnames(CorrelInitYieldsCapGainResid) <- c("YearsToSale", "CorrelationInitYieldsCapGainResid")

CorrelInitYieldsCapGainResidNeigh <- df %>%
  group_by(YearsToSale) %>%  
  summarize(COR=cor(LogInitYieldResidNeigh, LogCapGainResid))
colnames(CorrelInitYieldsCapGainResidNeigh) <- c("YearsToSale", "CorrelationInitYieldsCapGainResidNeigh")

CorrelFinYieldsCapGain <- df %>%
  group_by(YearsToSale) %>%  
  summarize(COR=cor(LogFinYield, LogCapGain))
colnames(CorrelFinYieldsCapGain) <- c("YearsToSale", "CorrelationFinYieldsCapGain")

CorrelFinYieldsCapGainResid <- df %>%
  group_by(YearsToSale) %>%  
  summarize(COR=cor(LogFinYieldResid, LogCapGainResid))
colnames(CorrelFinYieldsCapGainResid) <- c("YearsToSale", "CorrelationFinYieldsCapGainResid")

CorrelFinYieldsCapGainResidNeigh <- df %>%
  group_by(YearsToSale) %>%  
  summarize(COR=cor(LogFinYieldResidNeigh, LogCapGainResid))
colnames(CorrelFinYieldsCapGainResidNeigh) <- c("YearsToSale", "CorrelationFinYieldsCapGainResidNeigh")

Correl <- merge(CorrelYields, CorrelYieldsResid, by="YearsToSale")
Correl <- merge(Correl, CorrelInitYieldsCapGain, by="YearsToSale")
Correl <- merge(Correl, CorrelInitYieldsCapGainResid, by="YearsToSale")
Correl <- merge(Correl, CorrelInitYieldsCapGainResidNeigh, by="YearsToSale")
Correl <- merge(Correl, CorrelFinYieldsCapGain, by="YearsToSale")
Correl <- merge(Correl, CorrelFinYieldsCapGainResid, by="YearsToSale")
Correl <- merge(Correl, CorrelFinYieldsCapGainResidNeigh, by="YearsToSale")
Correl <- merge(Correl, CorrelYieldsResidNeigh, by="YearsToSale")

Correl$Count <- table(df$YearsToSale)
Correl <- subset(Correl, YearsToSale > -1 & YearsToSale < l+1)

# Mean correlations
mean(Correl$CorrelationInitYieldsCapGain)
mean(Correl$CorrelationInitYieldsCapGainResid)
mean(Correl$CorrelationInitYieldsCapGain)
mean(Correl$CorrelationInitYieldsCapGainResid)

# Plots of Yield Correlations
# pdf("../figures/YieldCorrelation.pdf", height=5, width=9)
pdf("../figures/YieldCorrelationAll.pdf", height=5, width=9)
# pdf("../figures/YieldCorrelationAllPre1940.pdf", height=5, width=9)
# pdf("../figures/YieldCorrelationAllPost1940.pdf", height=5, width=9)
par(mar = c(5,5,1,1))
plot(Correl$YearsToSale, Correl$CorrelationYields, pch=19, col=colour[1], ylim=c(-0.25,1), ylab="Correlation", xlab="Holding Period (Years)")
abline(lm(Correl$CorrelationYields ~ Correl$YearsToSale), col=colour[1], lty=1, lwd=2)
abline(lm(Correl$CorrelationYieldsResid ~ Correl$YearsToSale), col=colour[5], lty=2, lwd=2)
abline(lm(Correl$CorrelationYieldsResidNeigh ~ Correl$YearsToSale), col=colour[2], lty=3, lwd=2)
points(Correl$YearsToSale, Correl$CorrelationYieldsResid, pch=4, col=colour[5])
points(Correl$YearsToSale, Correl$CorrelationYieldsResidNeigh, pch=5, col=colour[2])
legend("bottomleft", 
       c("Correlation Repeat Yields", "Trend Correlation","Residual Correlation Repeat Yields, Market","Trend Residual Correlation, Market",  "Residual Correlation Repeat Yields, Neighborhood","Trend Residual Correlation, Neighborhood"),
       pch=c(19,NA,4,NA,5,NA), lty=c(NA,1,NA,2,NA,3), lwd=c(NA, 2, NA, 2,NA,2), col=colour[c(1,1,5,5,2,2)], bty="n")
dev.off()

# pdf("Figures/YieldCapGainCorrelation.pdf", height=5, width=9)
pdf("../figures/YieldCapGainCorrelationAll.pdf", height=5, width=9) # not in paper
par(mar = c(2,5,1,1))
plot(Correl$YearsToSale, Correl$CorrelationFinYieldsCapGain, pch=19, col=colour[1], ylim=c(-1,1), ylab="Correlation", xlab="Holding Period (Years)")
points(Correl$YearsToSale, Correl$CorrelationFinYieldsCapGainResid, pch=4, col=colour[2])
abline(lm(Correl$CorrelationFinYieldsCapGain ~ Correl$YearsToSale), col=colour[1], lty=1, lwd=2)
abline(lm(Correl$CorrelationFinYieldsCapGainResid ~ Correl$YearsToSale), col=colour[2], lty=2, lwd=2)
points(Correl$YearsToSale, Correl$CorrelationInitYieldsCapGain, pch=17, col=colour[5])
points(Correl$YearsToSale, Correl$CorrelationInitYieldsCapGainResid, pch=3, col=colour[6])
abline(lm(Correl$CorrelationInitYieldsCapGainResid ~ Correl$YearsToSale), col=colour[6], lty=2, lwd=2)
abline(lm(Correl$CorrelationInitYieldsCapGain ~ Correl$YearsToSale), col=colour[5], lty=1, lwd=2)
legend("topleft", 
       c("Correlation Yield at Sale - Capital Gain", "Residual Correlation Yield at Sale - Capital Gain",
         "Correlation Yield at Purchase - Capital Gain", "Residual Correlation Yield at Purchase - Capital Gain"),
       pch=c(19,4, 17, 3), col=colour[c(1,2,5,6)], bty="n")
dev.off()

## bootstrap standard errors correlations per holding period, in Appendix.
SDCorYield <- matrix(NA, nrow=20, ncol=1)
SDCorYieldResid <- matrix(NA, nrow=20, ncol=1)
SDCorYieldResidNeigh <- matrix(NA, nrow=20, ncol=1)

for (i in 1:20) {
  c <- df[df$YearsToSale == i,]
  
  x <- replicate(1000, {
    s <- c[sample(nrow(c), nrow(c), replace=T), ]
    x = cor(s$LogInitYield, s$LogFinYield)
  })
  SDCorYield[i] <- sd(x)
}

Correl$SDCorYield <- SDCorYield

for (i in 1:20) {
  c <- df[df$YearsToSale == i,]
  
  x <- replicate(1000, {
    s <- c[sample(nrow(c), nrow(c), replace=T), ]
    x = cor(s$LogInitYieldResid, s$LogFinYieldResid)
  })
  SDCorYieldResid[i] <- sd(x)
}

Correl$SDCorYieldResid <- SDCorYieldResid


for (i in 1:20) {
  c <- df[df$YearsToSale == i,]
  
  x <- replicate(1000, {
    s <- c[sample(nrow(c), nrow(c), replace=T), ]
    x = cor(s$LogInitYieldResidNeigh, s$LogFinYieldResidNeigh)
  })
  SDCorYieldResidNeigh[i] <- sd(x)
}

Correl$SDCorYieldResidNeigh <- SDCorYieldResidNeigh

# pool in data frame
CorPlot <- data.frame(Correl$YearsToSale, Correl$CorrelationYields, Correl$CorrelationYieldsResid, Correl$CorrelationYieldsResidNeigh)
colnames(CorPlot) <- c("YearsToSale", "Yields", "Residual Yields", "Neigborhood-Residual Yields")
CorPlot2 <- melt(CorPlot, id.vars="YearsToSale")
CorPlot2$SE <- c(Correl$SDCorYield, Correl$SDCorYieldResid, Correl$SDCorYieldResidNeigh)
colnames(CorPlot2) <- c("HoldingPeriod", "Variable", "Correlation", "SE")

# make plot
pdf("../figures/YieldCorrAllSE.pdf", height=5, width=9)
par(mar = c(2,5,1,1))
ggplot(CorPlot2, aes(x=HoldingPeriod, y=Correlation, fill=Variable)) +
  xlab("Holding Period (Years)") +
  geom_col(position = "dodge", width=0.6, size=0.6) +
  geom_errorbar(aes(ymin=Correlation - SE, ymax = Correlation + SE), position="dodge", width=0.6, size=0.6) +
  theme_bw()
dev.off()   

############################################
### Compute Components of Total Variance ###
############################################

# constrict subset with less than x years to sale
l <- 20
df2 <- subset(df, YearsToSale < l+1 & YearsToSale > 0)


# estimate variance of the variance of yields at different sample sizes, using original sample
CountHoldingPeriod <- table(df2$YearsToSale)

y <- matrix(NA, nrow=20, ncol=1)

for (i in 1:20) {
  x <- replicate(10000, {
    mm <- sample(log(d$GrossYieldFinal+1), CountHoldingPeriod[i])
    sd(mm)
  })
  y[i] <- sd(x)
}

# estimate variance of capital gain and yield for different holding periods
LogCapGainVar <- aggregate(df2$LogCapGain, by=list(df2$YearsToSale), var)
LogYieldVar <- var(c(df2$LogInitYield, df2$LogFinYield))
LogYieldVarResid <- var(c(df2$LogInitYieldResid, df2$LogFinYieldResid))
LogYieldVarResidNeigh <- var(c(df2$LogInitYieldResidNeigh, df2$LogFinYieldResidNeigh))

CovLogYieldTime <- df2 %>%
  group_by(YearsToSale) %>%
  summarize(Val=cov(LogInitYield, LogFinYield))
CovLogIYCG <- df2 %>%
  group_by(YearsToSale) %>%
  summarize(Val=cov(LogInitYield, LogCapGain))
CovLogFYCG <- df2 %>%
  group_by(YearsToSale) %>%
  summarize(Val=cov(LogFinYield, LogCapGain))
VarYieldPurchase <- df2 %>%
  group_by(YearsToSale) %>%
  summarize(Val=sd(LogInitYield))
VarYieldSale <- df2 %>%
  group_by(YearsToSale) %>%
  summarize(Val=sd(LogFinYield))
MeanYieldPurchase <- df2 %>%
  group_by(YearsToSale) %>%
  summarize(Val=mean(LogInitYield))
MeanYieldSale <- df2 %>%
  group_by(YearsToSale) %>%
  summarize(Val=mean(LogFinYield))

# CovLogInitYieldCapGainReg <- fitted(lm(CovLogIYCG$Val ~ CovLogIYCG$YearsToSale + log(CovLogIYCG$YearsToSale)))
# CovLogFinalYieldCapGainReg <- fitted(lm(CovLogFYCG$Val ~ CovLogFYCG$YearsToSale + log(CovLogFYCG$YearsToSale)))

SumLogYieldCov <- matrix(0, nrow=l, ncol=l)
for (n in 1:l) {
  for (i in 1:n) {
    SumLogYieldCov[i,n] <- 2*(n-i)*CovLogYieldTime$Val[i]
  }
}

LogTotVar <- data.frame(rep(0,l))
colnames(LogTotVar) <- "All"

### Bootstrap standard erros of the different timers
TotVarYieldVar <- matrix(NA, nrow=20, ncol=1)
VarLogCapGainVar <- matrix(NA, nrow=20, ncol=1)
VarLogYieldCov <- matrix(NA, nrow=20, ncol=1)
VarCovLogIYCG <- matrix(NA, nrow=20, ncol=1)
VarCovLogFYCG <- matrix(NA, nrow=20, ncol=1)
VarLogTotVar <- matrix(NA, nrow=20, ncol=1)
VarYieldPurchase <- matrix(NA, nrow=20, ncol=1)
VarYieldSale <- matrix(NA, nrow=20, ncol=1)
SDYieldPurchase <- matrix(NA, nrow=20, ncol=1)
SDYieldSale <- matrix(NA, nrow=20, ncol=1)

# boostrap standard errors of the yield volatility at purchase and at sale
for (i in 1:20) {
  c <- df2[df2$YearsToSale == i,]
  
  x<- replicate(1000, {
    s <- c[sample(nrow(c), nrow(c), replace=T), ]
    x = c(sd(s$LogInitYield), sd(s$LogFinYield))
  })
  x <- t(x)
  SDYieldPurchase[i] <- sd(x[,1])
  SDYieldSale[i] <- sd(x[,2])
}

# bootstrap standard errors for the capital gains variance, yield covariance, and yield-capital gain covariance, yield variance)
for (i in 1:20) {
  c <- df2[df2$YearsToSale == i,]
  
  x<- replicate(1000, {
    s <- c[sample(nrow(c), nrow(c), replace=T), ]
    x = c(cov(s$LogInitYield, s$LogFinYield), cov(s$LogInitYield, s$LogCapGain), cov(s$LogFinYield, s$LogCapGain), var(s$LogCapGain), var(s$LogInitYield), var(s$LogFinYield))
  })
  x <- t(x)
  VarLogYieldCov[i] <- var(x[,1])
  VarCovLogIYCG[i] <- var(x[,2])
  VarCovLogFYCG[i] <- var(x[,3])
  VarLogCapGainVar[i] <- var(x[,4])
  VarYieldPurchase[i] <- var(x[,5])
  VarYieldSale[i] <- var(x[,6])
}  

# VarSumLogYieldCov <- matrix(0, nrow=l, ncol=l)
# for (n in 1:l) {
#  for (i in 1:n) {
#    VarSumLogYieldCov[i,n] <- (2*(n-i))^2*VarLogYieldCov[i]
#  }
# }

# estimate total of each variance component and the 
for (i in 1:l) {
  # VarLogTotVar[i] <- VarLogCapGainVar[i] + sum(VarYieldVar[1:i]) + i*VarCovLogIYCG[i] + i*VarCovLogFYCG[i] + sum(VarSumLogYieldCov[,i])
  LogTotVar$All[i] <- LogCapGainVar$x[i] + i*LogYieldVar + i*CovLogIYCG$Val[i] + i*CovLogFYCG$Val[i] + sum(SumLogYieldCov[,i])
  LogTotVar$CapGain[i] <- LogCapGainVar$x[i]
  LogTotVar$Yield[i] <- i*LogYieldVar
  LogTotVar$YieldCov[i] <- sum(SumLogYieldCov[,i])
  # LogTotVar$CovYieldCapGain[i] <- i*CovLogIYCG$Val[i] +  i*CovLogFYCG$Val[i]
  LogTotVar$VarCapGainVar[i] <- sqrt(VarLogCapGainVar[i])
  # LogTotVar$VarYieldVar[i] <- sqrt(i^2*VarYieldVar)
  # LogTotVar$VarYieldCov[i] <- sqrt(sum(i*VarSumLogYieldCov[,i]))
  # LogTotVar$VarCovYieldCapGain[i] <- sqrt(i^2*VarCovLogIYCG[i] +  i^2*VarCovLogFYCG[i])
}

LogTotVar$Years <- 1:l

# pdf("../figures/CompositionTotalVariance.pdf", height=5, width=9)
pdf("../figures/CompositionTotalVarianceAll.pdf", height=5, width=9)
# pdf("../figures/CompositionTotalVarianceAllPre1940.pdf", height=5, width=9)
# pdf("../figures/CompositionTotalVarianceAllPost1940.pdf", height=5, width=9)
par(mar = c(4.5,5,1,1))
temp <- (LogTotVar[,2:5]/LogTotVar[,1])*100
temp4 <- temp[,4]
temp[temp[,4]<0,4] <- 0
barplot(t(as.matrix(temp[,4:1])), ylim=c(-100, 200), 
        col=c("lightblue","pink","orange","lightblue"), 
        ylab="Relative contribution to total variance (%)",
        xlab="Holding period (Years)", names.arg=1:l,
        border=FALSE, legend=FALSE)
barplot(t(as.matrix(temp4)), add=TRUE, col="red", border=FALSE)
abline(h=0, lwd=2, lty=2)
abline(h=100, lwd=2, lty=2)
box()
legend("bottomleft", 
       c("Cap. gain variance", "Yield covariance", "Yield variance", "Yield-cap. gain covariance"), 
       fill=c("lightblue","pink","orange","red"),
       bty="n", ncol=2, border=FALSE)
dev.off()

# add standard errors to esimates
CovLogYieldTime$SE <- sqrt(VarLogYieldCov)
CovLogIYCG$SE <- sqrt(VarCovLogIYCG)
CovLogFYCG$SE <- sqrt(VarCovLogFYCG)

# plot yield covariance per holdign period with standard errors
pdf("../figures/YieldCovariance.pdf", height=5, width=9)
ggplot(data=CovLogYieldTime, aes(x=YearsToSale, y=Val)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=Val-SE, ymax=Val+SE)) +
  ylab("Yield Covariance") +
  xlab("Holding Period (Years)") +
  theme_bw()
dev.off()

# plot capital gains - yield at purchase covariance per holding period with standard errors
pdf("../figures/YieldPurchaseCapitalGainVariance.pdf", height=5, width=9)
ggplot(data=CovLogIYCG, aes(x=YearsToSale, y=Val)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=Val-SE, ymax=Val+SE)) +
  ylab("Yield at Purchase - Capital Gains Covariance") +
  xlab("Holding Period (Years)") +
  theme_bw()
dev.off()

# plot capital gains - yield at sale covariance per holding period with standard errors
pdf("../figures/YieldSaleCapitalGainCovariance.pdf", height=5, width=9)
ggplot(data=CovLogFYCG, aes(x=YearsToSale, y=Val)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=Val-SE, ymax=Val+SE)) +
  ylab("Yield at Sale - Capital Gains Covariance") +
  xlab("Holding Period (Years)") +
  theme_bw()
dev.off()

# plot capital gains variance per holding period with standard errors
pdf("../figures/CapitalGainsVariance.pdf", height=5, width=9)
ggplot(data=LogTotVar, aes(x=Years, y=CapGain)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=CapGain-VarCapGainVar, ymax = CapGain+VarCapGainVar)) +
  ylab("Capital Gains Variance") +
  xlab("Holding Period (Years)") +
  theme_bw() 
dev.off()

###############################################
### Compute Components of Residual Variance ###
###############################################

l <- 20
 
LogCapGainVarResid <- aggregate(df2$LogCapGainResid, by=list(df2$YearsToSale), var)
LogYieldVarResid <- aggregate(df2$LogFinYieldResidNeigh, by=list(df2$YearsToSale), var)
LogYieldVarResid$x <- c(df2$LogInitYieldResidNeigh, LogYieldVarResidNeigh$x[1:(l-1)])

LogYieldVar <- 
  
  aggregate(df2$LogInitYieldResidNeigh, by=list(df2$YearsToSale), var)
summary(lm(LogYieldVar$x ~ LogYieldVar$Group.1))
LogYieldVar$x <- c(var(df2$LogInitYield), LogYieldVar$x[1:(l-1)])

CovLogYieldTimeResid <- df2 %>%
  group_by(YearsToSale) %>%
  summarize(Val=cov(LogInitYieldResidNeigh, LogFinYieldResidNeigh))
CovLogIYCGResid <- df2 %>%
  group_by(YearsToSale) %>%
  summarize(Val=cov(LogInitYieldResidNeigh, LogCapGainResid))
CovLogFYCGResid <- df2 %>%
  group_by(YearsToSale) %>%
  summarize(Val=cov(LogFinYieldResidNeigh, LogCapGainResid))

# CovLogYieldTimeResidReg <- fitted(lm(CovLogYieldTimeResid$Val ~ CovLogYieldTimeResid$YearsToSale + log(CovLogYieldTimeResid$YearsToSale)))
# CovLogInitYieldCapGainResidReg <- fitted(lm(CovLogIYCGResid$Val ~ CovLogIYCGResid$YearsToSale + log(CovLogIYCGResid$YearsToSale)))
# CovLogFinalYieldCapGainResidReg <- fitted(lm(CovLogFYCGResid$Val ~ CovLogFYCGResid$YearsToSale + log(CovLogFYCGResid$YearsToSale)))

SumLogYieldCovResid <- matrix(0, nrow=l, ncol=l)
for (n in 1:l) {
  for (i in 1:n) {
    SumLogYieldCovResid[i,n] <- 2*(n-i)*CovLogYieldTimeResid$Val[i]
  }
}

LogTotVarResid <- data.frame(rep(0,l))
colnames(LogTotVarResid) <- "All"

for (i in 1:l) {
  LogTotVarResid$All[i] <- LogCapGainVarResid$x[i] + sum(LogYieldVarResid$x[1:i]) + i*CovLogFYCGResid$Val[i] + i*CovLogIYCGResid$Val[i] + sum(SumLogYieldCovResid[,i])
  LogTotVarResid$CapGain[i] <- LogCapGainVarResid$x[i]
  LogTotVarResid$Yield[i] <- sum(LogYieldVarResid$x[1:i])
  LogTotVarResid$YieldCov[i] <- sum(SumLogYieldCovResid[,i])
  LogTotVarResid$CovYieldCapGain[i] <- i*CovLogFYCGResid$Val[i] + i*CovLogIYCGResid$Val[i]
}
LogTotVarResid$Years <- 1:l

# New Figure, Thies
# pdf("Figures/CompositionResidualVariance.pdf", height=4.5, width=9)
pdf("../figures/CompositionResidualVarianceAll.pdf", height=4.5, width=9)
par(mar = c(4.5,5,1,1))
temp <- (LogTotVarResid[,2:5]/LogTotVarResid[,1])*100
temp4 <- temp[,4]
temp[temp[,4]<0,4] <- 0
barplot(t(as.matrix(temp[,4:1])), ylim=c(-100,200), 
        col=c("red","pink","orange","lightblue"), 
        ylab="Relative contribution to residual variance (%)",
        xlab="Holding period (Years)", names.arg=1:l,
        border=FALSE, legend=FALSE)
barplot(t(as.matrix(temp4)), add=TRUE, col="red", border=FALSE)
abline(h=0, lwd=2, lty=2)
abline(h=100, lwd=2, lty=2)
box()
legend("bottomleft", 
       c("Cap. gain var.", "Yield covar.", "Yield var.", "Yield-cap. gain covar."), 
       fill=c("lightblue","pink","orange","red"),
       bty="n", ncol=2, border=FALSE)
dev.off()


#### Compute Composition of Residual Return Variance ###
totVar <- LogTotVar$All
residVarMarket <- LogTotVarResid$All
barplot(cbind(totVar, residVarMarket))
systematicRisk <- totVar - residVarMarket

plotdata <- t( cbind(residVarMarket, systematicRisk))
colnames(plotdata) <- 1:l

pdf("../figures/SystematicVsIdiosyncraticRisk.pdf", height=4, width=9)
par(mar = c(5,5,1,1))
barplot(plotdata, col=c("lightblue","red"), border=FALSE, ylim=c(0, max(totVar) + 0.02), ylab="Variance", xlab="Holding Period (Years)")
box()
legend("topleft", c("Systematic Risk","Idiosyncratic Risk"), fill=c("red","lightblue"), bty="n", border=FALSE)
dev.off()
        
        