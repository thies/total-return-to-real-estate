##################################################
############### Matthijs Korevaar ################
##################################################

# change working directory to the directory of the script
try( setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))

# load packages
library(reshape2)
library(MASS)
library(zoo)
library(plm)
library(RcppRoll)
options(scipen=6)

# Summary Statistics
a <- read.csv("../data/Amsterdam/TimeSeriesAmsAnalysis.csv")
p <- read.csv("../data/Paris/TimeSeriesParisAnalysis.csv")

mean(a$AggTaxFracAll[a$Year > 1900])
mean(p$TaxRate[p$Year > 1808 & p$Year < 1944])
mean(p$TransTax[p$Year > 1808 & p$Year < 1944])

#################
### Amsterdam ###
#################

# Capital Gains, annual
a$JordaCapGain[2:nrow(a)] <- diff(log(a$JordaHPI))
a$JordaCapGainA[2:nrow(a)] <- diff(a$JordaHPI)/a$JordaHPI[1:(nrow(a)-1)]
a$JordaCapGainR[2:nrow(a)] <- diff(log(a$JordaHPI)) - diff(log(a$CPI))
a$EichholtzCapGain[2:nrow(a)] <- diff(log(a$EichholtzHPI))
a$EichholtzCapGainA[2:nrow(a)] <- diff(a$EichholtzHPI)/a$EichholtzHPI[1:(nrow(a)-1)]
a$EichholtzCapGainR[2:nrow(a)] <- diff(log(a$EichholtzHPI)) - diff(log(a$CPI))
a$CapGain[2:nrow(a)] <- diff(log(a$HPI))
a$CapGainA[2:nrow(a)] <- diff(a$HPI)/a$HPI[1:(nrow(a)-1)]
a$CapGainR[2:nrow(a)] <- diff(log(a$HPI)) - diff(log(a$CPI))
a$CapGainwithApp[2:nrow(a)] <- diff(log(a$HPIwithApp))
a$CapGainwithAppA[2:nrow(a)] <- diff(a$HPIwithApp)/a$HPIwithApp[1:(nrow(a)-1)]
a$CapGainwithAppR[2:nrow(a)] <- diff(log(a$HPIwithApp)) - diff(log(a$CPI))
a$Infl[2:nrow(a)] <- diff(log(a$CPI))

# Capital Gains, statistics
mean(a$JordaCapGainA[2:nrow(a)])
sd(a$JordaCapGainA[2:nrow(a)])
mean(a$JordaCapGain[2:nrow(a)])
sd(a$JordaCapGain[2:nrow(a)])
mean(a$JordaCapGainR[2:nrow(a)])
sd(a$JordaCapGainR[2:nrow(a)])

cor.test(a$CapGainwithApp, a$JordaCapGain)

mean(a$EichholtzCapGainA[2:nrow(a)], na.rm=T)
sd(a$EichholtzCapGainA[2:nrow(a)],na.rm=T)
mean(a$EichholtzCapGain[2:nrow(a)],na.rm=T)
sd(a$EichholtzCapGain[2:nrow(a)],na.rm=T)
mean(a$EichholtzCapGainR[2:nrow(a)],na.rm=T)
sd(a$EichholtzCapGainR[2:nrow(a)],na.rm=T)

cor.test(a$CapGainwithApp, a$EichholtzCapGain)

mean(a$CapGainA[2:nrow(a)])
sd(a$CapGainA[2:nrow(a)])
mean(a$CapGain[2:nrow(a)])
sd(a$CapGain[2:nrow(a)])
mean(a$CapGainR[2:nrow(a)])
sd(a$CapGainR[2:nrow(a)])

mean(a$CapGainwithAppA[2:nrow(a)])
sd(a$CapGainwithAppA[2:nrow(a)])
mean(a$CapGainwithApp[2:nrow(a)])
sd(a$CapGainwithApp[2:nrow(a)])
mean(a$CapGainwithAppR[2:nrow(a)])
sd(a$CapGainwithAppR[2:nrow(a)])

cor.test(a$CapGain, a$CapGainwithApp)

a <- subset(a, a$Year > 1899)

# Statistics Gross Yields
mean(a$AggYieldincApp)
sd(a$AggYieldincApp)

mean(log(a$AggYieldincApp+1))
sd(log(a$AggYieldincApp+1))

mean(a$MedianYieldincApp)
sd(a$MedianYieldincApp)

mean(log(a$MedianYieldincApp+1))
sd(log(a$MedianYieldincApp+1))

plot(a$AggYieldincApp, type="l")

cor.test(log(a$AggYieldincApp+1),log(a$MedianYieldincApp+1))
cor.test(log(a$AggYield+1),log(a$AggYieldincApp+1))

mean(a$AggYield)
sd(a$AggYield)

mean(log(a$AggYield+1))
sd(log(a$AggYield+1))

# Statistics Net Yields
mean(a$NetAggYield)
sd(a$NetAggYield)

mean(log(a$NetAggYield+1))
sd(log(a$NetAggYield+1))

mean(a$NetAggYieldincApp)
sd(a$NetAggYieldincApp)

mean(log(a$NetAggYieldincApp+1))
sd(log(a$NetAggYieldincApp+1))

mean(a$JordaRentRtn)
sd(a$JordaRentRtn)

mean(log(a$JordaRentRtn+1))
sd(log(a$JordaRentRtn+1))

cor.test(log(a$NetAggYieldincApp+1),log(a$JordaRentRtn+1))
cor.test(log(a$NetAggYield+1),log(a$NetAggYieldincApp+1))

mean(a$NetAggYieldH)
sd(a$NetAggYieldH)

mean(log(a$NetAggYieldH+1))
sd(log(a$NetAggYieldH+1))

cor.test(log(a$NetAggYieldincApp+1),log(a$NetAggYieldH+1))

mean(a$NetAggYieldL)
sd(a$NetAggYieldL)

mean(log(a$NetAggYieldL+1))
sd(log(a$NetAggYieldL+1))

cor.test(log(a$NetAggYieldincApp+1),log(a$NetAggYieldL+1))

# Total Returns
a$TotRet <- a$CapGainwithApp + log(a$NetAggYieldincApp+1)
a$TotRetA <- a$CapGainwithAppA + a$NetAggYieldincApp
a$TotRetR <- a$CapGainwithAppR + log(a$NetAggYieldincApp+1) 

mean(a$TotRetA)
sd(a$TotRetA)

mean(a$TotRet)
sd(a$TotRet)

mean(a$TotRetR)
sd(a$TotRetR)

a$JordaRet <- a$JordaCapGain + log(a$JordaRentRtn+1)
a$JordaRetA <- a$JordaCapGainA + a$JordaRentRtn
a$JordaRetR <- a$JordaCapGainR + log(a$JordaRentRtn+1)

mean(a$JordaRetA)
sd(a$JordaRetA)

mean(a$JordaRet)
sd(a$JordaRet)

mean(a$JordaRetR)
sd(a$JordaRetR)

cor.test(a$JordaRet, a$TotRet)

a$TotRet2 <- a$CapGain + log(a$NetAggYield+1)
a$TotRet2A <- a$CapGainA + a$NetAggYield
a$TotRet2R <- a$CapGainR + log(a$NetAggYield+1) 

mean(a$TotRet2A)
sd(a$TotRet2A)

mean(a$TotRet2)
sd(a$TotRet2)

mean(a$TotRet2R)
sd(a$TotRet2R)

cor.test(a$TotRet, a$TotRet2)

a$TotRetH <- a$CapGainwithApp + log(a$NetAggYieldH+1)
a$TotRetHA <- a$CapGainwithAppA + a$NetAggYieldH
a$TotRetHR <- a$CapGainwithAppR + log(a$NetAggYieldH+1) 

mean(a$TotRetHA)
sd(a$TotRetHA)

mean(a$TotRetH)
sd(a$TotRetH)

mean(a$TotRetHR)
sd(a$TotRetHR)

a$TotRetL <- a$CapGainwithApp + log(a$NetAggYieldL+1)
a$TotRetLA <- a$CapGainwithAppA + a$NetAggYieldL
a$TotRetLR <- a$CapGainwithAppR + log(a$NetAggYieldL+1) 

mean(a$TotRetLA)
sd(a$TotRetLA)

mean(a$TotRetL)
sd(a$TotRetL)

mean(a$TotRetLR)
sd(a$TotRetLR)

a$JordaEqRet <- log(a$JordaEqTR+1)
a$JordaEqRetR <- log(a$JordaEqTR+1) - a$Infl

# Total Sharpe Ratio
(mean(a$TotRet, na.rm=T)-mean(log(a$JordaBondYield+1), na.rm=T)-0.0031)/sd(a$TotRet, na.rm=T)
(mean(a$TotRet, na.rm=T)-mean(log(a$JordaBillRate+1), na.rm=T))/sd(a$TotRet, na.rm=T)
(mean(a$JordaRet, na.rm=T)-mean(log(a$JordaBondYield+1), na.rm=T))/sd(a$JordaRet, na.rm=T)
(mean(a$JordaRet, na.rm=T)-mean(log(a$JordaBillRate+1), na.rm=T))/sd(a$JordaRet, na.rm=T)
(mean(a$JordaEqRet, na.rm=T)-mean(log(a$JordaBondYield+1), na.rm=T))/sd(a$JordaEqRet, na.rm=T) 
(mean(a$JordaEqRet, na.rm=T)-mean(log(a$JordaBillRate+1), na.rm=T))/sd(a$JordaEqRet, na.rm=T) 

# Compute Rolling Returns, Sharpe Ratios
test <- NULL
horizon <- 5
test <- data.frame(1900:1979, roll_sumr(a$JordaRet,horizon), roll_sumr(a$TotRet,horizon), roll_sumr(a$JordaEqRet,horizon), horizon*log(a$JordaBondYield[as$Year > (1901-horizon) & as$Year < (1981-horizon)]+1))
test2 <- data.frame(1900:1979, roll_sumr(a$JordaRetR,horizon),roll_sumr(a$TotRetR,horizon), roll_sumr(a$JordaEqRetR,horizon))
colnames(test) <- c("Year", "Jorda", "EKL", "Equity", "BondYield")
colnames(test2) <- c("Year", "JordaR", "EKLR", "EquityR")

# Yields
mean(roll_sumr(log(a$NetAggYieldincApp[!is.na(a$NetAggYieldincApp)]+1), horizon), na.rm=T)
sd(roll_sumr(log(a$NetAggYieldincApp[!is.na(a$NetAggYieldincApp)]+1), horizon), na.rm=T)

mean(roll_sumr(log(a$JordaRentRtn[!is.na(a$JordaRentRtn)]+1), horizon), na.rm=T)
sd(roll_sumr(log(a$JordaRentRtn[!is.na(a$JordaRentRtn)]+1), horizon), na.rm=T)

cor.test(roll_sumr(log(a$NetAggYieldincApp[!is.na(a$NetAggYieldincApp)]+1), horizon),roll_sumr(log(a$JordaRentRtn[!is.na(a$JordaRentRtn)]+1), horizon))

# Sharpe Ratios
(mean(test$EKL[!is.na(test$EKL)])-mean(test$BondYield[!is.na(test$EKL)]))/sd(test$EKL[!is.na(test$EKL)])
(mean(test$Jorda[!is.na(test$Jorda)])-mean(test$BondYield[!is.na(test$Jorda)]))/sd(test$Jorda[!is.na(test$Jorda)])
(mean(test$Equity[!is.na(test$Equity)])-mean(test$BondYield[!is.na(test$Equity)]))/sd(test$Equity[!is.na(test$Equity)]) 
(mean(test$Equity[!is.na(test$Equity)])-mean(test$BondYield[!is.na(test$Equity)]))/sd(test$Equity[!is.na(test$Equity)]) 

# Returns
mean(test$EKL[!is.na(test$Jorda)])
sd(test$EKL[!is.na(test$Jorda)])

mean(test$Jorda[!is.na(test$Jorda)])
sd(test$Jorda[!is.na(test$Jorda)])

mean(test$Equity[!is.na(test$Equity)])
sd(test$Equity[!is.na(test$Equity)])

cor.test(test$Jorda, test$EKL)
cor.test(test$Equity, test$EKL)

mean(test2$EKLR[!is.na(test2$EKLR)])
sd(test2$EKLR[!is.na(test2$EKLR)])

mean(test2$JordaR[!is.na(test2$JordaR)])
sd(test2$JordaR[!is.na(test2$JordaR)])

mean(test2$EquityR[!is.na(test2$EquityR)])
sd(test2$EquityR[!is.na(test2$EquityR)])

cor.test(test2$JordaR, test2$EKLR)
cor.test(test2$EquityR, test2$EKLR)

#############
### Paris ###
#############

p <- subset(p, Year > 1808 & Year < 1944)

# Capital Gains, annual
p$JordaCapGain[2:nrow(p)] <- diff(log(p$JordaHPI))
p$JordaCapGainA[2:nrow(p)] <- diff(p$JordaHPI)/p$JordaHPI[1:(nrow(p)-1)]
p$JordaCapGainR[2:nrow(p)] <- diff(log(p$JordaHPI)) - diff(log(p$CPI))
p$DuonCapGain[2:nrow(p)] <- diff(log(p$DuonHPI))
p$DuonCapGainA[2:nrow(p)] <- diff(p$DuonHPI)/p$DuonHPI[1:(nrow(p)-1)]
p$DuonCapGainR[2:nrow(p)] <- diff(log(p$DuonHPI)) - diff(log(p$CPI))
p$CapGain[2:nrow(p)] <- diff(log(p$HPI))
p$CapGainA[2:nrow(p)] <- diff(p$HPI)/p$HPI[1:(nrow(p)-1)]
p$CapGainR[2:nrow(p)] <- diff(log(p$HPI)) - diff(log(p$CPI))

# Capital Gains, statistics
mean(p$CapGainA[2:nrow(p)])
sd(p$CapGainA[2:nrow(p)])
mean(p$CapGain[2:nrow(p)])
sd(p$CapGain[2:nrow(p)])
mean(p$CapGainR[2:nrow(p)])
sd(p$CapGainR[2:nrow(p)])

# Capital Gains, statistics
mean(p$CapGainA[p$Year > 1840])
sd(p$CapGainA[p$Year > 1840])
mean(p$CapGain[p$Year > 1840])
sd(p$CapGain[p$Year > 1840])
mean(p$CapGainR[p$Year > 1840])
sd(p$CapGainR[p$Year > 1840])

mean(p$DuonCapGainA[p$Year > 1840])
sd(p$DuonCapGainA[p$Year > 1840])
mean(p$DuonCapGain[p$Year > 1840])
sd(p$DuonCapGain[p$Year > 1840])
mean(p$DuonCapGainR[p$Year > 1840])
sd(p$DuonCapGainR[p$Year > 1840])

cor.test(p$DuonCapGain, p$CapGain)

# Capital Gains, statistics
mean(p$CapGainA[p$Year > 1870])
sd(p$CapGainA[p$Year > 1870])
mean(p$CapGain[p$Year > 1870])
sd(p$CapGain[p$Year > 1870])
mean(p$CapGainR[p$Year > 1870])
sd(p$CapGainR[p$Year > 1870])

mean(p$JordaCapGainA[p$Year > 1870])
sd(p$JordaCapGainA[p$Year > 1870])
mean(p$JordaCapGain[p$Year > 1870])
sd(p$JordaCapGain[p$Year > 1870])
mean(p$JordaCapGainR[p$Year > 1870])
sd(p$JordaCapGainR[p$Year > 1870])

cor.test(p$JordaCapGain, p$CapGain)

# Statistics Gross Yields
mean(p$PortfolioYield)
sd(p$PortfolioYield)

mean(log(p$PortfolioYield+1))
sd(log(p$PortfolioYield+1))

mean(p$MedianYield)
sd(p$MedianYield)

mean(log(p$MedianYield+1))
sd(log(p$MedianYield+1))

cor.test(log(p$PortfolioYield+1),log(p$MedianYield+1))

mean(p$PortfolioYield10)
sd(p$PortfolioYield10)

mean(log(p$PortfolioYield10+1))
sd(log(p$PortfolioYield10+1))

cor.test(log(p$PortfolioYield+1),log(p$PortfolioYield10+1))

mean(p$PortfolioYield[p$Year > 1808 & p$Year < 1856])
sd(p$PortfolioYield[p$Year > 1808 & p$Year < 1856])

mean(log(p$PortfolioYield[p$Year > 1808 & p$Year < 1856]+1))
sd(log(p$PortfolioYield[p$Year > 1808 & p$Year < 1856]+1))

mean(p$PortfolioYield[p$Year > 1871 & p$Year < 1941])
sd(p$PortfolioYield[p$Year > 1871 & p$Year < 1941])

mean(log(p$PortfolioYield[p$Year > 1871 & p$Year < 1941]+1))
sd(log(p$PortfolioYield[p$Year > 1871 & p$Year < 1941]+1))

mean(p$RegYieldFig[p$Year > 1871 & p$Year < 1941])
sd(p$RegYieldFig[p$Year > 1871 & p$Year < 1941])

mean(log(p$RegYieldFig[p$Year > 1871 & p$Year < 1941]+1))
sd(log(p$RegYieldFig[p$Year > 1871 & p$Year < 1941]+1))

cor.test(log(p$RegYieldFig[p$Year > 1871 & p$Year < 1941]+1),log(p$PortfolioYield[p$Year > 1871 & p$Year < 1941]+1))

# rent contracts only, yield
mean(p$PortfolioBail[p$Year > 1808 & p$Year < 1856])
sd(p$PortfolioBail[p$Year > 1808 & p$Year < 1856])

mean(log(p$PortfolioBail[p$Year > 1808 & p$Year < 1856]+1))
sd(log(p$PortfolioBail[p$Year > 1808 & p$Year < 1856]+1))

cor.test(log(p$PortfolioYield[p$Year > 1808 & p$Year < 1856]+1),log(p$PortfolioBail[p$Year > 1808 & p$Year < 1856]+1))

# Statistics Net Yields
mean(p$MedianNetYield)
sd(p$MedianNetYield)

mean(log(p$MedianNetYield+1))
sd(log(p$MedianNetYield+1))

mean(p$PortfolioNetYield)
sd(p$PortfolioNetYield)

mean(log(p$PortfolioNetYield+1))
sd(log(p$PortfolioNetYield+1))

mean(p$PortfolioNetYieldAlt[p$Year > 1871 & p$Year < 1941])
sd(p$PortfolioNetYieldAlt[p$Year > 1871 & p$Year < 1941])

mean(log(p$PortfolioNetYieldAlt[p$Year > 1871 & p$Year < 1941]+1))
sd(log(p$PortfolioNetYieldAlt[p$Year > 1871 & p$Year < 1941]+1))

cor.test(log(p$PortfolioNetYieldAlt[p$Year > 1871 & p$Year < 1941]+1),log(p$PortfolioNetYield[p$Year > 1871 & p$Year < 1941]+1))

mean(log(p$PortfolioNetYield+1))
sd(log(p$PortfolioNetYield+1))

mean(p$PortfolioNetYield[p$Year > 1870])
sd(p$PortfolioNetYield[p$Year > 1870])

mean(log(p$PortfolioNetYield[p$Year > 1870]+1))
sd(log(p$PortfolioNetYield[p$Year > 1870]+1))

mean(p$JordaRentRtn[p$Year > 1870])
sd(p$JordaRentRtn[p$Year > 1870])

mean(log(p$JordaRentRtn[p$Year > 1870]+1))
sd(log(p$JordaRentRtn[p$Year > 1870]+1))

cor.test(log(p$PortfolioNetYield+1),log(p$JordaRentRtn+1))
cor.test(log(p$PortfolioYield+1),log(p$JordaRentRtn+1))


mean(p$PortfolioNetYield)
sd(p$PortfolioNetYield)

mean(log(p$PortfolioNetYield+1))
sd(log(p$PortfolioNetYield+1))


mean(p$PortfolioNetH)
sd(p$PortfolioNetH)

mean(log(p$PortfolioNetH+1))
sd(log(p$PortfolioNetH+1))


mean(p$PortfolioNetL)
sd(p$PortfolioNetL)

mean(log(p$PortfolioNetL+1))
sd(log(p$PortfolioNetL+1))

# Total Return
p$JordaRet[p$Year > 1870] <- p$JordaCapGain[p$Year > 1870] + log(p$JordaRentRtn[p$Year > 1870]+1)
p$JordaRetA[p$Year > 1870] <- p$JordaCapGainA[p$Year > 1870] + p$JordaRentRtn[p$Year > 1870]
p$JordaRetR[p$Year > 1870] <- p$JordaCapGain[p$Year > 1870] + log(p$JordaRentRtn[p$Year > 1870]+1) - diff(log(p$CPI[p$Year > 1869]))

p$TotRet[2:nrow(p)] <- diff(log(p$HPI)) + log(p$PortfolioNetYield[2:nrow(p)]+1)
p$TotRetA[2:nrow(p)] <- p$CapGainA[2:nrow(p)] + p$PortfolioNetYield[2:nrow(p)]
p$TotRetR[2:nrow(p)] <- diff(log(p$HPI)) + log(p$PortfolioNetYield[2:nrow(p)]+1) - diff(log(p$CPI))

mean(p$TotRetA[2:nrow(p)])
sd(p$TotRetA[2:nrow(p)])

mean(p$TotRet[2:nrow(p)])
sd(p$TotRet[2:nrow(p)])

mean(p$TotRetR[2:nrow(p)])
sd(p$TotRetR[2:nrow(p)])

mean(p$TotRetA[p$Year > 1870])
sd(p$TotRetA[p$Year > 1870])

mean(p$TotRet[p$Year > 1870])
sd(p$TotRet[p$Year > 1870])

mean(p$TotRetR[p$Year > 1870])
sd(p$TotRetR[p$Year > 1870])

mean(p$JordaRetA[p$Year > 1870])
sd(p$JordaRetA[p$Year > 1870])

mean(p$JordaRet[p$Year > 1870])
sd(p$JordaRet[p$Year > 1870])

mean(p$JordaRetR[p$Year > 1870])
sd(p$JordaRetR[p$Year > 1870])

cor.test(p$TotRet[p$Year > 1870], p$JordaRet[p$Year > 1870])

p$TotRetAlt[2:nrow(p)] <- diff(log(p$HPI)) + log(p$PortfolioNetYieldAlt[2:nrow(p)]+1)
p$TotRetAltA[2:nrow(p)] <- p$CapGainA[2:nrow(p)] + p$PortfolioNetYieldAlt[2:nrow(p)]
p$TotRetAltR[2:nrow(p)] <- diff(log(p$HPI)) + log(p$PortfolioNetYieldAlt[2:nrow(p)]+1) - diff(log(p$CPI))

mean(p$TotRetAltA[p$Year > 1871 & p$Year < 1941])
sd(p$TotRetAltA[p$Year > 1871 & p$Year < 1941])

mean(p$TotRetAlt[p$Year > 1871 & p$Year < 1941])
sd(p$TotRetAlt[p$Year > 1871 & p$Year < 1941])

mean(p$TotRetAltR[p$Year > 1871 & p$Year < 1941])
sd(p$TotRetAltR[p$Year > 1871 & p$Year < 1941])

cor.test(p$TotRetAlt, p$TotRet)
cor.test(p$TotRetAlt, log(p$JordaHousingTR+1))

p$TotHRet[2:nrow(p)] <- diff(log(p$HPI)) + log(p$PortfolioNetH[2:nrow(p)]+1)
p$TotHRetA[2:nrow(p)] <- p$CapGainA[2:nrow(p)] + p$PortfolioNetH[2:nrow(p)]
p$TotHRetR[2:nrow(p)] <- diff(log(p$HPI)) + log(p$PortfolioNetH[2:nrow(p)]+1) - diff(log(p$CPI))

mean(p$TotHRetA[2:nrow(p)])
sd(p$TotHRetA[2:nrow(p)])

mean(p$TotHRet[2:nrow(p)])
sd(p$TotHRet[2:nrow(p)])

mean(p$TotHRetR[2:nrow(p)])
sd(p$TotHRetR[2:nrow(p)])

p$TotLRet[2:nrow(p)] <- diff(log(p$HPI)) + log(p$PortfolioNetL[2:nrow(p)]+1)
p$TotLRetA[2:nrow(p)] <- p$CapGainA[2:nrow(p)] + p$PortfolioNetL[2:nrow(p)]
p$TotLRetR[2:nrow(p)] <- diff(log(p$HPI)) + log(p$PortfolioNetL[2:nrow(p)]+1) - diff(log(p$CPI))

mean(p$TotLRetA[2:nrow(p)])
sd(p$TotLRetA[2:nrow(p)])

mean(p$TotLRet[2:nrow(p)])
sd(p$TotLRet[2:nrow(p)])

mean(p$TotLRetR[2:nrow(p)])
sd(p$TotLRetR[2:nrow(p)])

p$EquityRet[2:nrow(p)] <- diff(log(p$EquityTR+1),1)
p$EquityRetA[2:nrow(p)] <- diff(p$EquityTR,1)/p$EquityTR[1:(nrow(p)-1)]
p$JordaEqRet[2:nrow(p)] <- log(p$JordaEquityTR[2:nrow(p)]+1)
p$JordaEqRetA[2:nrow(p)] <- p$JordaEquityTR[2:nrow(p)]
p$JordaEqRetR[2:nrow(p)] <- log(p$JordaEquityTR[2:nrow(p)]+1) - diff(log(p$CPI))

# Total Sharpe Ratio
(mean(p$TotRet[p$Year > 1808], na.rm=T)-mean(log(p$BondYield[p$Year > 1808]+1), na.rm=T)-0.0071)/sd(p$TotRet[p$Year > 1808], na.rm=T)
(mean(p$EquityRet[p$Year > 1808], na.rm=T)-mean(log(p$BondYield[p$Year > 1808]+1), na.rm=T))/sd(p$EquityRet[p$Year > 1808], na.rm=T)

ps <- p[p$Year > 1870 & p$Year < 1944,]

# Compute Rolling Returns, Sharpe Ratios
test <- NULL
horizon <- 1
test <- data.frame(1871:1943, roll_sumr(ps$JordaRet,horizon), roll_sumr(ps$TotRet,horizon), roll_sumr(ps$JordaEqRet,horizon), horizon*log(p$JordaBondYield[p$Year > (1871-horizon) & p$Year < (1945-horizon)]+1), horizon*log(p$JordaBillYield[p$Year > (1871-horizon) & p$Year < (1945-horizon)]+1))
test2 <- data.frame(1871:1943, roll_sumr(ps$JordaRetR,horizon),roll_sumr(ps$TotRetR,horizon), roll_sumr(ps$JordaEqRetR,horizon))
colnames(test) <- c("Year", "Jorda", "EKL", "Equity", "BondYield", "BillYield")
colnames(test2) <- c("Year", "JordaR", "EKLR", "EquityR")

# Yields
mean(roll_sumr(log(ps$PortfolioNetYield[!is.na(ps$PortfolioNetYield)]+1), horizon), na.rm=T)
sd(roll_sumr(log(ps$PortfolioNetYield[!is.na(ps$PortfolioNetYield)]+1), horizon), na.rm=T)

mean(roll_sumr(log(ps$JordaRentRtn[!is.na(ps$JordaRentRtn)]+1), horizon), na.rm=T)
sd(roll_sumr(log(ps$JordaRentRtn[!is.na(ps$JordaRentRtn)]+1), horizon), na.rm=T)

cor.test(roll_sumr(log(ps$PortfolioNetYield[!is.na(ps$PortfolioNetYield)]+1), horizon),roll_sumr(log(ps$JordaRentRtn[!is.na(ps$JordaRentRtn)]+1), horizon))

# Sharpe Ratios
(mean(test$EKL[!is.na(test$EKL)])-mean(test$BondYield[!is.na(test$EKL)]))/sd(test$EKL[!is.na(test$EKL)])
(mean(test$EKL[!is.na(test$EKL)])-mean(test$BillYield[!is.na(test$BillYield)]))/sd(test$EKL[!is.na(test$EKL)])
(mean(test$Jorda[!is.na(test$Jorda)])-mean(test$BondYield[!is.na(test$Jorda)]))/sd(test$Jorda[!is.na(test$Jorda)])
(mean(test$Jorda[!is.na(test$Jorda)])-mean(test$BillYield[!is.na(test$BillYield)]))/sd(test$Jorda[!is.na(test$Jorda)])
(mean(test$Equity[!is.na(test$Equity)])-mean(test$BondYield[!is.na(test$Equity)]))/sd(test$Equity[!is.na(test$Equity)])
(mean(test$Equity[!is.na(test$Equity)])-mean(test$BillYield[!is.na(test$BillYield)]))/sd(test$Equity[!is.na(test$Equity)])

# Returns
mean(test$EKL[!is.na(test$Jorda)])
sd(test$EKL[!is.na(test$Jorda)])

mean(test$Jorda[!is.na(test$Jorda)])
sd(test$Jorda[!is.na(test$Jorda)])

cor.test(test$Jorda, test$EKL)

mean(test2$EKLR[!is.na(test2$EKLR)])
sd(test2$EKLR[!is.na(test2$EKLR)])

mean(test2$JordaR[!is.na(test2$JordaR)])
sd(test2$JordaR[!is.na(test2$JordaR)])

cor.test(test2$JordaR, test2$EKLR)

mean(test$Equity[!is.na(test$Equity)])
sd(test$Equity[!is.na(test$Equity)])

cor.test(test$Equity, test$EKL)

mean(test2$EquityR[!is.na(test2$EquityR)])
sd(test2$EquityR[!is.na(test2$EquityR)])

cor.test(test2$EquityR, test2$EKLR)

# Correlation Le Temps Yields
mean(ps$PortfolioYield[ps$Year == 1883 | ps$Year == 1884 | (ps$Year > 1931 & ps$Year < 1940)], na.rm=T)
sd(ps$PortfolioYield[ps$Year == 1883 | ps$Year == 1884 | (ps$Year > 1931 & ps$Year < 1940)], na.rm=T)

mean(ps$RegYieldFig[ps$Year == 1883 | ps$Year == 1884 | (ps$Year > 1931 & ps$Year < 1940)], na.rm=T)
sd(ps$RegYieldFig[ps$Year == 1883 | ps$Year == 1884 | (ps$Year > 1931 & ps$Year < 1940)], na.rm=T)

mean(ps$PortfolioYieldLeTemps, na.rm=T)
sd(ps$PortfolioYieldLeTemps, na.rm=T)


mean(log(ps$PortfolioYield[ps$Year == 1883 | ps$Year == 1884 | (ps$Year > 1931 & ps$Year < 1940)]+1), na.rm=T)
sd(log(ps$PortfolioYield[ps$Year == 1883 | ps$Year == 1884 | (ps$Year > 1931 & ps$Year < 1940)]+1), na.rm=T)

mean(log(ps$RegYieldFig[ps$Year == 1883 | ps$Year == 1884 | (ps$Year > 1931 & ps$Year < 1940)]+1), na.rm=T)
sd(log(ps$RegYieldFig[ps$Year == 1883 | ps$Year == 1884 | (ps$Year > 1931 & ps$Year < 1940)]+1), na.rm=T)

mean(log(ps$PortfolioYieldLeTemps+1), na.rm=T)
sd(log(ps$PortfolioYieldLeTemps+1), na.rm=T)

cor.test(log(ps$PortfolioYieldLeTemps+1), log(ps$PortfolioYield+1))
cor.test(log(ps$PortfolioYieldLeTemps+1), log(ps$RegYieldFig+1))
cor.test(log(ps$PortfolioYield+1), log(ps$RegYieldFig+1))
