##################################################
############### Matthijs Korevaar ################
##################################################

# load packages
library(reshape2)
library(MASS)
library(zoo)
library(plm)
options(scipen=6)

### File Amsterdam
# Code to estimate house price and rent price index for Paris

# change working directory to the directory of the script
try( setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))

## Colour scheme
colour <- c("#230356","#076391","#2fd1d1","#b51676","#e90d3a","#f98e19","#ffc217")

p <- read.csv("../data/TimeSeriesParisAnalysis.csv")
a <- read.csv("../data/TimeSeriesAmsAnalysis.csv")

e <- merge(p, a, by="Year", all.x=TRUE, all.y = TRUE)
e <- e[e$Year > 1808,]

p <- p[p$Year > 1808 & p$Year < 1944,]





# Total return figures
# Amsterdam
a$hpi_return[2:nrow(a)] <- exp(diff(log(ts( a$HPI))))-1
a$tr <- a$hpi_return + a$NetAggYield

a$indexTR <- 100
a$indexInc <- 100
a$indexTRJorda <- 100
for(i in 2:nrow(a)){
        a$indexTR[i] <- a$indexTR[i-1]*(1+a$tr[i])    
        a$indexTRJorda[i] <- a$indexTRJorda[i-1]*(1+a$JordaHousingTR[i])    
        a$indexInc[i] <- a$indexInc[i-1]*(1+a$NetAggYield[i])    
}

a$indexTRR <- a$indexTR/a$CPI
a$indexTRR <- a$indexTRR/a$indexTRR[1]*100
a$indexTRRJorda <- a$indexTRJorda/a$CPI
a$indexTRRJorda <- a$indexTRRJorda/a$indexTRRJorda[1]*100


a$HPIR <- a$HPI/a$CPI
a$HPIR <-a$HPIR/a$HPIR[1]*100
a$indexIncR <- a$indexInc/a$CPI
a$indexIncR <-a$indexIncR/a$indexIncR[1]*100


pdf("../figures/returns_ams_small.pdf", height=3.2, width=4)
par(mar = c(3,4,0.8,1))
plot( range(a$Year), range(c(25, a$indexTRRJorda)), type="n", log="y", xlab="Year", ylab="1900=100, in logs")
lines(a$Year, a$indexTRR, lwd=2, col=colour[1])
lines(a$Year, a$HPIR, lwd=2, col=colour[5])
lines(a$Year, a$indexTRRJorda, lwd=1, col=colour[1], lty=3)
#lines(a$Year, a$indexIncR, lwd=2, col=colour[6])
legend("topleft",c("Total Return","Capital Gain","TR Jordà et al."), col=colour[c(1,5)], lwd=c(2,2,1),lty=c(1,1,3), bty="n")
dev.off()

pdf("../figures/returns_ams.pdf", height=4.5, width=7)
par(mar = c(3,4,0.8,1))
plot( range(a$Year), range(c(25, a$indexTRR)), type="n", log="y", xlab="Year", ylab="1900=100, in logs")
lines(a$Year, a$indexTRR, lwd=2, col=colour[1])
lines(a$Year, a$HPIR, lwd=2, col=colour[5])
#lines(a$Year, a$indexTRRJorda, lwd=1, col=colour[1], lty=3)
#lines(a$Year, a$indexIncR, lwd=2, col=colour[6])
legend("topleft",c("Total Return","Capital Gain"), col=colour[c(1,5)], lwd=2,lty=1, bty="n")
dev.off()




# ----- Paris -----
p$hpi_return[2:nrow(p)] <- exp(diff(log(ts( p$HPI))))-1
p$tr <- p$hpi_return + p$PortfolioNetYield

p$indexTR <- 100
for(i in 2:nrow(p)){
        p$indexTR[i] <- p$indexTR[i-1]*(1+p$tr[i])    
}


p$indexTRJorda <- NA
p$indexTRJorda[p$Year == 1870] <- 100
for(y in 1871:1943){
        p$indexTRJorda[p$Year == y] <- p$indexTRJorda[p$Year== (y-1)]*(1+p$JordaHousingTR[p$Year==y])    
}        

p$indexTRR <- p$indexTR/p$CPI
p$indexTRR <- p$indexTRR/p$indexTRR[1]*100
p$indexTRRJorda <- p$indexTRJorda/p$CPI
p$indexTRRJorda <- p$indexTRRJorda/p$indexTRRJorda[p$Year==1870]
p$indexTRRJorda <- p$indexTRRJorda*p$indexTRR[p$Year == 1870]

p$HPIR <- p$HPI/p$CPI
p$HPIR <-p$HPIR/p$HPIR[1]*100



pdf(file="../figures/returns_par.pdf", height = 4.5, width=7)
par(mar = c(3,4,0.8,1))
plot( range(p$Year), c(50,50000), type="n", log="y", xlab="Year", ylab="1809=100, in logs")
lines(p$Year, p$indexTRR, lwd=2, col=colour[1])
#lines(p$Year, p$indexTRRJorda, lwd=1, col=colour[1], lty=3)
lines(p$Year, p$HPIR, lwd=2, col=colour[5])
legend("topleft",c("Total Return","Capital Gain"), col=colour[c(1,5)], lwd=2, lty=1, bty="n")
dev.off()


ps <- subset(p, Year >= 1870)
ps$indexTRR <- ps$indexTRR/ps$indexTRR[1]*100
ps$indexTRRJorda <- ps$indexTRRJorda/ps$indexTRRJorda[1]*100
pdf(file="../figures/returns_par_small.pdf", height = 3.2, width=4)
par(mar = c(3,4,0.8,1))
plot( c(1870,1943), range(ps$indexTRRJorda), type="n", log="y", xlab="Year", ylab="1870=100, in logs")
lines(ps$Year, ps$indexTRR, lwd=2, col=colour[1])
lines(ps$Year, ps$indexTRRJorda, lwd=2, col=colour[5])
legend("bottomright",c("Total Return", "TR Jordà et al."), col=colour[c(1,5)], lwd=c(2,2), lty=c(1,1), bty="n")
dev.off()






pdf("../figures/AmsPriceIndex.pdf", height=4.5, width=9)
par(mar = c(2,5,1,1))
plot(a$Year,  a$HPIwithApp/a$HPIwithApp[a$Year == 1900]*100, col=colour[1], log="y", type="l", lwd=3, ylim=c(50,2500), ylab="Index (1900=100)", xlab="Year")
#lines(a$Year,  a$HPI/a$HPI[a$Year == 1900]*100, col=colour[1], lty=3, lwd=2)
lines(a$Year, a$JordaHPI/a$JordaHPI[a$Year == 1900]*100, col=colour[2], lty=1, lwd=3)
lines(a$Year, a$EichholtzHPI/a$EichholtzHPI[a$Year == 1900]*100, col=colour[5], lty=1, lwd=3)
#lines(a$Year, (a$HPIwithApp/a$CPI)/(a$HPIwithApp[a$Year == 1900]/a$CPI[a$Year==1900])*100, col=colour[7], lty=4, lwd=3)
legend("topleft",
       c("Repeat-Sales Index", "Knoll et al. (2017)", "Ambrose et al. 2013"),
       lwd=4, lty=c(1,1,1), col=colour[c(1,2,5)], bty="n")
dev.off()

pdf("../figures/ParisPrices.pdf", height=4.5, width=9)
par(mar = c(2,5,1,1))
plot(p$Year,  p$JordaHPI/p$JordaHPI[p$Year == 1900]*100, col=colour[2], type="l", lwd=3, log="y", ylim=c(10,800), ylab="Index (1900=100)", xlab="Year")
lines(p$Year, p$DuonHPI/p$DuonHPI[p$Year == 1900]*100, col=colour[5], lty=1, lwd=3)
lines(p$Year, p$HPI/p$HPI[p$Year == 1900]*100, col=colour[1], lty=1, lwd=3)
lines(p$Year, (p$HPI/p$CPI)/(p$HPI[p$Year == 1900]/p$CPI[p$Year==1900])*100, col=colour[7], lty=4, lwd=3)
legend("topleft",
       c("Repeat-Sales Index", "Repeat-Sales Index (real)", "Duon (1946)", "Knoll et al. (2017)"),
       lwd=3,
       bty="n",
       col=colour[c(1,7,5,2)], lty=c(1,4,1,1))
dev.off()

pdf("../figures/TotTaxRate.pdf", height=4.5, width=9)
par(mar = c(2,5,1,1))
plot(e$Year, e$TaxRate, col=colour[1], type="l", lwd=3, ylim=c(0,0.375), ylab="Tax / Vacancy Rate (% of Rent)", xlab="Year")
lines(e$Year[e$Year > 1868], e$VacancyRate.x[e$Year > 1868], col=colour[1], lty=3, lwd=3)
lines(e$Year, e$MedianTaxFracAll, col=colour[5], lty=1, lwd=3)
lines(e$Year, e$VacancyRate.y, col=colour[5], lty=3, lwd=3)
legend("topleft", c("Paris - Tax Rate", "Paris - Vacancy Rate", "Amsterdam - Tax Rate", "Amsterdam - Vacancy Rate"), bty="n",lwd=3, lty=c(1,2,1,2), col=colour[c(1,1,5,5)])
dev.off()

pdf("../figures/AllGrossYields.pdf", height=4.5, width=9)
par(mar = c(2,5,1,1))
plot(e$Year,  e$PortfolioYield, col=colour[1], type="l", lwd=3, ylim=c(0,0.25), ylab="Gross Yield", xlab="Year")
lines(e$Year, e$MedianYield.x, col=colour[1], lty=3, lwd=2)
lines(e$Year, e$RegYieldFig, col=colour[7], lty=1, lwd=3)
# lines(e$Year[1:45], e$PortfolioBail[1:45], col=colour[6], lty=4, lwd=2)
lines(e$Year, e$AggYieldincApp, col=colour[5], lty=1, lwd=3)
lines(e$Year, e$MedianYieldincApp, col=colour[5], lty=3, lwd=2)
legend("topleft",
       bty="n",
       c("Portfolio Yield - Paris ", "Median Yield - Paris", "Le Figaro Yield - Paris", "Portfolio Yield - Amsterdam ", "Median Yield - Amsterdam"), 
       lwd=c(3,2,3,3,2), 
       lty=c(1,3,1,1,3), 
       col=colour[c(1,1,7,5,5)])
dev.off()


pdf("../figures/NetYield.pdf", height=4.5, width=9)
par(mar = c(2,5,1,1))
plot(e$Year,  e$PortfolioNetYield, col=colour[1], type="l", lwd=3, ylim=c(0,0.14), ylab="Yield", xlab="Year")
lines(e$Year, e$JordaRentRtn.x, col=colour[1], lty=3, lwd=2)
#lines(e$Year[e$Year > 1870], e$PortfolioNetYieldAlt[e$Year > 1870], col=colour[7], lty=1, lwd=3)
lines(e$Year, e$NetAggYieldincApp, col=colour[5], lty=1, lwd=3)
lines(e$Year, e$JordaRentRtn.y, col=colour[5], lty=3, lwd=2)
legend("topleft",
       bty="n",
       c("Portfolio Net Yield - Paris ", "Jorda Rental Yield - France", "Portfolio Net Yield - Amsterdam ", "Jorda Rental Yield - Netherlands"), 
       lwd=c(3,2,3,2), 
       lty=c(1,3,1,3), 
       col=colour[c(1,1,5,5)])
dev.off()

cor.test(d$ParisNetYield-d$ParisBondYield, d$Paris5YInflation)
cor.test(d$AmsNetYield-d$AmsBondYield, d$Ams5yInflation)

pdf("../Paper/figures/YieldDiffInflation.pdf", height=4.5, width=9)
par(mar = c(2,5,1,1))
plot(d$Year,  d$ParisNetYield-d$ParisBondYield, col=colour[1], type="l", lwd=3, ylim=c(-0.08,0.25), ylab="Yield", xlab="Year")
lines(d$Year, d$AmsNetYield-d$AmsBondYield, col=colour[5], lty=1, lwd=3)
par=new
lines(d$Year, d$Paris5YInflation, col=colour[2], lty=1, lwd=2)
lines(d$Year, d$Ams5yInflation, col=colour[6], lty=1, lwd=2)
legend("topright",
       bty="n",
       c("Housing-Bond Yield Differential - Paris", "Previous 5-year Inflation - Paris", "Housing-Bond Yield Differential - Paris", "Previous 5-year Inflation - Amsterdam"), 
       lwd=c(3,2,3,2), 
       lty=c(1,1,1,1), 
       col=colour[c(1,2,5,6)])
dev.off()






### EXTRA

pdf("../figuresappendix/ParisInitYields.pdf", height=4.5, width=9)
par(mar = c(2,5,1,1))
plot(f$Year,  f$ParisGrossAvgYield, col=colour[1], type="l", lty=1, lwd=3, ylim=c(0,0.15), ylab="Gross Yield", xlab="Year")
lines(f$Year, f$ParisGrossInitYield, col=colour[5], lty=3, lwd=2)
legend("topleft", c("Median Yield - Adjusted", "Median Yield - Initial"),
       bty="n",
       lwd=c(3,2),
       lty=c(1,3),
       col=colour[c(1,5)])
dev.off()

pdf("../figures/AmsNetYields.pdf", height=4.5, width=9)
par(mar = c(2,5,1,1))
plot(d$Year,  d$AmsNetYield, col=colour[1], type="l", lty=1, lwd=3, ylim=c(0,0.14), ylab="Index", xlab="Year")
lines(d$Year, d$AmsJordaYield, col=colour[5], lty=1, lwd=2)
legend("topleft",
       c("Portfolio Net Yield", "Jorda et al. (2019)"),
       bty="n",
       lwd=c(3,3),
       lty=c(1,1),
       col=colour[c(1,5)])
dev.off()


pdf("../figures/ParisNetYields.pdf", height=4.5, width=9)
par(mar = c(2,5,1,1))
plot(f$Year,  f$ParisNetYield, col=colour[1], type="l", lty=1, lwd=3, ylim=c(0,0.14), ylab="Index", xlab="Year")
lines(f$Year, f$ParisJordaYield, col=colour[5], lty=1, lwd=2)
legend("topleft",
       c("Portfolio Net Yield", "Jorda et al. (2019)"),
       bty="n",
       lwd=c(3,2),
       lty=c(1,1),
       col=colour[c(1,5)])
dev.off()

