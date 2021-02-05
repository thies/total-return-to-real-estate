# Code to estimate yield index for Amsterdam
try( setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))


## Colour scheme
colour <- c("#230356","#076391","#2fd1d1","#b51676","#e90d3a","#f98e19","#ffc217")

library(stargazer)
library(reshape2)
library(ggplot2)

# load data
dat <- read.csv("../data/Amsterdam/DataAmsterdamInput.csv")
dat2 <- subset(dat, dat$RentAll > 0 & Plaats == "Amsterdam" & Y < 1980)

############################
### COMPUTE GROSS YIELDS ###
############################

# remove observations without yields
data <- subset(dat, Y > 1899 & Plaats == "Amsterdam" & GrossYieldFinal > 0 & Price > 0 & RentAll > 0)

# Remove extreme observations: median log yield +/- 1.386 (+300/-75%), and log median price + 3/-3 (+2000%/095%)
median <- aggregate(data$GrossYieldFinal, by=list(data$Y), median)
medianprice <- aggregate(data$Price, by=list(data$Y), median)
colnames(median) <- c("Year", "MedianYieldInit")
colnames(medianprice) <- c("Year", "MedianPriceInit")
data <- merge(data, median, by.x="Y", by.y="Year", all.x="TRUE", all.y="FALSE", sort="FALSE")
data <- merge(data, medianprice, by.x="Y", by.y="Year", all.x="TRUE", all.y="FALSE", sort="FALSE")
dno <- subset(data, log(data$GrossYieldFinal) > log(data$MedianYieldInit) - 1.386 & log(data$GrossYieldFinal) < log(data$MedianYieldInit) + 1.386 &
              log(data$Price) > log(data$MedianPriceInit) - 2.198 & log(data$Price) < log(data$MedianPriceInit) + 2.198)
nrow(dno)/nrow(data) # check number of observations removed; about 1.8

# Compute Median and Aggregate Yield Including Appraisals
median2 <- aggregate(dno$GrossYieldFinal, by=list(dno$Y), median)
rentsum2 <- aggregate(dno$Rent[dno$Rent > 0 & dno$Price > 0], by=list(dno$Y[dno$Rent > 0 & dno$Price > 0]), sum)
pricesum2 <- aggregate(dno$Price[dno$Rent > 0 & dno$Price > 0], by=list(dno$Y[dno$Rent > 0 & dno$Price > 0]), sum)
median2$AggYield <- rentsum2$x/pricesum2$x
median2$CountYieldincApp <- table(dno$Y)
colnames(median2) <- c("Year", "MedianYieldincApp", "AggYieldincApp", "CountYieldincApp")
plot(median2$AggYieldincApp, type="l")

# Compute Median and Aggregate Yield Excluding Appraisals
dnop <- subset(dno, Veilingprijs > 0)
median3 <- aggregate(dnop$GrossYieldFinal[!is.na(dnop$GrossYieldFinal)], by=list(dnop$Y[!is.na(dnop$GrossYieldFinal)]), median)
rentsum3 <- aggregate(dnop$Rent[dnop$Rent > 0 & dnop$Price > 0], by=list(dnop$Y[dnop$Rent > 0 & dnop$Price > 0]), sum)
pricesum3 <- aggregate(dnop$Price[dnop$Rent > 0 & dnop$Price > 0], by=list(dnop$Y[dnop$Rent > 0 & dnop$Price > 0]), sum)
median3$AggYield <- rentsum3$x/pricesum3$x
median3$CountYield <- table(dnop$Y)
colnames(median3) <- c("Year", "MedianYield", "AggYield", "CountYield")

# Plot of all yields
plot(median2$Year, median2$MedianYieldincApp, type="l", lty=2, col="black", lwd=2, ylim=c(0,0.25))
# lines(median3$Year, median3$MedianYield, type="l", lty=2, col="red", lwd=1.5, ylim=c(0,0.25))
# lines(median3$Year, median3$AggYield, type="l", lty=3, lwd=2, col="red", ylim=c(0,0.25))
lines(median2$Year, median2$AggYieldincApp, type="l", col="blue", lwd=2, ylim=c(0,0.25))

# Correlations between headline yield and other yield
cor.test(median2$AggYieldincApp, median2$MedianYieldincApp)
cor.test(median2$AggYieldincApp, median3$MedianYield)
cor.test(median2$AggYieldincApp, median3$AggYield)

# Summary stats log yields
mean(log(median2$MedianYieldincApp+1))
sd(log(median2$MedianYieldincApp+1))
mean(log(median3$MedianYield+1))
sd(log(median3$MedianYield+1))
mean(log(median2$AggYieldincApp+1))
sd(log(median2$AggYieldincApp+1))
mean(log(median3$AggYield+1))
sd(log(median3$AggYield+1))

############################
### COMPUTE TAX FRACTION ###
############################

## COMPUTE TAX COST
data2 <- subset(dno, Y > 1899 & Plaats == "Amsterdam" & TaxFrac > 0 & Y > 1921)

# REMOVE OUTLIERS
mediantax <- aggregate(data2$TaxFrac, by=list(data2$Y), median)
colnames(mediantax) <- c("Year", "MedianFracInit")
data2 <- merge(data2, mediantax, by.x="Y", by.y="Year", all.x="TRUE", all.y="FALSE", sort="FALSE")
# data2 <- merge(data2, medianprice, by.x="Y", by.y="Year", all.x="TRUE", all.y="FALSE", sort="FALSE")
dno2 <- subset(data2, log(data2$TaxFrac) > log(data2$MedianFracInit) - 1.38 & log(data2$TaxFrac) < log(data2$MedianFracInit) + 1.38 &
                log(data2$Price) > log(data2$MedianPriceInit) - 2.198 & log(data2$Price) < log(data2$MedianPriceInit) + 2.198)
nrow(dno2)/nrow(data2)

# Compute median tax fraction
mediantax <- aggregate(dno2$TaxFrac, by=list(dno2$Y), median)
rentsumtax <- aggregate(dno2$Rent[dno2$Rent > 0 & dno2$Taks > 0], by=list(dno2$Y[dno2$Rent > 0 & dno2$Taks > 0]), sum)
taxsum <- aggregate(dno2$Taks[dno2$Rent > 0 & dno2$Taks > 0], by=list(dno2$Y[dno2$Rent > 0 & dno2$Taks > 0]), sum)
mediantax$AggTaxYield <- taxsum$x/rentsumtax$x
mediantax$TaxCount <- table(dno2$Y)
colnames(mediantax) <- c("Year", "MedianTaxFrac", "AggTaxFrac", "TaxCount")

# Plot tax fraction
plot(mediantax$Year, mediantax$MedianTaxFrac, col="black", type="l", lwd=2, ylim=c(0,0.4))
lines(mediantax$Year, mediantax$AggTaxFrac, col="red", lwd=2, lty=2,)

# Summary stats tax fraction
mean(mediantax$MedianTaxFrac)
sd(mediantax$MedianTaxFrac)
mean(mediantax$AggTaxFrac)
sd(mediantax$AggTaxFrac)

# Merge series
tot <- data.frame(median2, median3[,2:4])
tot <- merge(tot, mediantax, by.x="Year", by.y="Year", all.x="TRUE", all.y="FALSE", sort="FALSE")
tot <- tot[order(tot$Year),]

#############################
### COMPUTE COST FRACTION ###
#############################

cost <- read.csv("../data/AmsCostData.csv")

cost <- subset(cost, cost$HuurTotPJ > 0 & cost$MedPriceInYear > 0)
cost$RentDiff <- log(cost$HuurTotPJ) - log(cost$MedPriceInYear)
cost$RentDiff2 <- cost$HuurTotPJ - cost$MedPriceInYear

reg1 <- lm(cost$NonTaxFrac ~ cost$Commercial)
summary(reg1)

reg2 <- lm(cost$NonTaxFrac ~ cost$Commercial + cost$RentDiff)
summary(reg2)

reg3 <- lm(cost$NonTaxFrac ~ cost$Commercial + cost$RentDiff + cost$Wijk)
summary(reg3)


reg4 <- lm(cost$CostFraction ~ cost$Commercial)
summary(reg4)

reg5 <- lm(cost$CostFraction ~ cost$Commercial + cost$RentDiff)
summary(reg5)

reg6 <- lm(cost$CostFraction ~ cost$Commercial + cost$RentDiff + cost$Wijk)
summary(reg6)

# Table in Appendix of Main Paper
stargazer(reg1, reg2, reg3, reg4, reg5, reg6)


# Compute components of Cost Shares
# cost <- subset(cost, Commercial == 0) # excludes commercial properties

tothuur <- aggregate(cost$HuurTotPJ, by=list(cost$Year), sum)
tax <- aggregate(cost$Taxes, by=list(cost$Year), sum)
insurance <- aggregate(cost$Insurance, by=list(cost$Year), sum)
onderhoud <- aggregate(cost$Maintenance...Renovation, by=list(cost$Year), sum)
other <- aggregate(cost$Other, by=list(cost$Year), sum)
loss <- aggregate(cost$Vacancies, by=list(cost$Year), sum)

huurprijs <- tothuur
huurprijs$tax <- tax$x/huurprijs$x
huurprijs$insurance <- insurance$x/huurprijs$x
huurprijs$onderhoud <- onderhoud$x/huurprijs$x
huurprijs$other <- other$x/huurprijs$x
huurprijs$loss <- loss$x/huurprijs$x

d <- data.frame(huurprijs$Group.1, huurprijs$tax, huurprijs$insurance, huurprijs$onderhoud, huurprijs$other, huurprijs$loss)
colnames(d) <- c("Year", "Tax", "Insurance", "Maintenance & Renovation", "Other", "Vacancy costs")

dat6 <- melt(d, id="Year")
colnames(dat6) <- c("Year", "Costs", "Share")

pdf("../figures/costs_share_ams.pdf", height=5, width=9)
ggplot(dat6, aes(x=Year, y=Share, fill=Costs), ylim=c(0,1.5)) + 
  geom_area() + labs(fill="") + scale_fill_manual(values=colour) + theme(
    legend.position="bottom",
    panel.background = element_rect(fill = 'white', colour = 'black'),
    panel.grid.major = element_line(colour = "grey", linetype = "dotted"))
dev.off()

# Note: only for comparison; 30 percent used in main analysis. 

############################
### COMPUTE TOTAL SERIES ###
############################

# Merge with all files

# IMPORTANT: REMOVE OBS IF NECESSARY!
tseries <- read.csv("../data/TimeSeriesAmsAnalysis.csv")
tseries2 <- tseries2[ -c(1,16:32)]
tseries2 <- merge(tseries2, tot, by="Year", all.x=T, all.y=T)
# tseries2 <- tseries2[ -c(2:3) ]

# Construct Tax Fraction per Year
tseries2$MedianTaxFracAll <- c(tseries2$TaxIndex[1:23]/(tseries2$TaxIndex[24]/0.13),tseries2$MedianTaxFrac[24:80])
tseries2$AggTaxFracAll <- c(tseries2$TaxIndex[1:23]/(tseries2$TaxIndex[24]/0.13),tseries2$AggTaxFrac[24:80])

# Compute Net Yield
tseries2$NetMedYield <- tseries2$MedianYield*(1-tseries2$MedianTaxFracAll-tseries2$VacancyRate-0.3)
tseries2$NetMedYieldincApp <- tseries2$MedianYieldincApp*(1-tseries2$MedianTaxFracAll-tseries2$VacancyRate-0.3)
tseries2$NetAggYield <- tseries2$AggYield*(1-tseries2$AggTaxFracAll-tseries2$VacancyRate-0.3)
tseries2$NetAggYieldL <- tseries2$AggYield*(1-tseries2$AggTaxFracAll-tseries2$VacancyRate-0.375)
tseries2$NetAggYieldH <- tseries2$AggYield*(1-tseries2$AggTaxFracAll-tseries2$VacancyRate-0.225)
tseries2$NetAggYieldincApp <- tseries2$AggYieldincApp*(1-tseries2$AggTaxFracAll-tseries2$VacancyRate-0.3)

# Summary statistics
mean(tseries2$NetAggYield)
sd(tseries2$NetAggYield)
mean(tseries2$NetAggYieldL)
sd(tseries2$NetAggYieldL)
mean(tseries2$NetAggYieldH)
sd(tseries2$NetAggYieldH)
mean(tseries2$NetAggYieldincApp)
sd(tseries2$NetAggYieldincApp)

mean(tseries2$NetMedYield)
sd(tseries2$NetMedYield)
mean(tseries2$NetMedYieldincApp)
sd(tseries2$NetMedYieldincApp)

# Correlation with Jorda
cor.test(tseries2$NetAggYield, tseries2$JordaRentRtn)
cor.test(tseries2$NetAggYieldincApp, tseries2$JordaRentRtn)

# Write file
write.csv(tseries2, file="../data/TimeSeriesAmsAnalysis.csv")