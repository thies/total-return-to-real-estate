# ================================================
#
# Analysis of yields at neighbourhood level
# How much of the variation can be explained 
# by local factors? Whatever they might be...
#
# ================================================
# Libraries
library(raster)
library(spdep)
library(dplyr)


# set working directory
try( setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))

# load yields
y <- read.csv("../data/DataAmsterdamInput.csv")
# merge coordinates
y$full.addr <- paste(y$StraatMod, y$No)
y$full.addr <- paste(y$full.addr, ", Amsterdam, Netherlands", sep="")
coords <- read.csv("../data/geocoded.addresses.amsterdam.csv", as.is=TRUE)
y <- merge(y, coords, by="full.addr", all.x=TRUE)
# write.csv(y, file="../../Final series/yielddataamscleaned.geocoded.csv", row.names=FALSE)

# only keep observations that are located in at least one Amsterdam neighbourhood
y <- subset(y, !is.na(neigh.code))

#====================== 
run.morans.I <- function(){
  # Investigate spatial dependency in yields: Moran I test
  # First account for changes by year/neighbourhood in yields:
  # OLS regression: GrossYield ~ Year + NB 
  # The estimate Moran's I statistic in the residuals
  # ( this might take a while )
  tmp <- subset(y, !is.na(GrossYieldFinal) & !is.na(lon) & (Y >=1900 ))
  # First, control for year and neighbourhood
  yield.reg.nb <- lm(GrossYieldFinal~factor(Y)+factor(neigh.code), data=tmp)
  # Then contrast this to year controls only
  yield.reg <- lm(GrossYieldFinal~factor(Y), data=tmp)
  # save residuals
  tmp$yield.resid <- resid(yield.reg)
  tmp$yield.resid.nb <- resid(yield.reg.nb)
  # and aggregate per location (simple means) in case there are observations in multiple years
  tmp$lon.lat <- paste(tmp$lon, tmp$lat)
  tmp.aggr <- as.data.frame(tapply(tmp$yield.resid, tmp$lon.lat, mean))
  colnames(tmp.aggr) <- "yield.resid"
  tmp.aggr.nb <- as.data.frame(tapply(tmp$yield.resid.nb, tmp$lon.lat, mean))
  colnames(tmp.aggr.nb) <- "yield.resid.nb"
  tmp.aggr$lon.lat <- row.names(tmp.aggr)
  tmp.aggr.nb$lon.lat <- row.names(tmp.aggr.nb)
  tmp.aggr <- merge(tmp.aggr, tmp.aggr.nb, by=c("lon.lat"))
  tmp.aggr$lon <- as.numeric( gsub(" [0-9\\.]+","",tmp.aggr$lon.lat, perl=TRUE))
  tmp.aggr$lat <- as.numeric( gsub("[0-9\\.]+ ","",tmp.aggr$lon.lat, perl=TRUE))
  tmp.aggr$lon.lat <- NULL
  coords <- cbind(tmp.aggr$lon, tmp.aggr$lat)
  # define nearest neigbours based on distance: 0.2 = 200m 
  dnb <- dnearneigh(coords, 0, 0.2, longlat=TRUE) # distance in km
  lw <- nb2listw(dnb, zero.policy = TRUE)
  mt <- moran.test(tmp.aggr$yield.resid, lw, zero.policy = TRUE)
  mt.nb <- moran.test(tmp.aggr$yield.resid.nb, lw, zero.policy = TRUE)
  return(list(mt, mt.nb))
}
# This will take a while, only run if needed...
# M.I <- run.morans.I() 
# sink("../data/snippets/moransi.ams.txt")
# print(M.I)
# sink()
# ------------------------



col.gen <- colorRampPalette(c("blue", "red"), space = "Lab")
# plot median yields per 
coordinates(y) <- ~lon+lat

shp <- shapefile("../data/gis/gis.ams/bc2010zw_region.shp")
shp$bc <- as.numeric(shp$bc)
proj4string(y) <- CRS("+init=epsg:4326")
y.proj <- spTransform(y, crs(shp))
nbhd.ids <- c(1:10, 15 , 17:31, 38, 40:41,44,45,47,48, 50, 27, 48)
shp <- subset(shp, id %in% nbhd.ids)
y.proj <- subset(y.proj, neigh.code %in% as.numeric(shp$bc))
y.proj$yield <- y.proj$GrossYieldFinal
y.proj$year <- y.proj$Y
y.proj <- subset(y.proj, !is.na(yield))

plot.map <- function(shp, yields, breaks=NA, filename=NA, stepsize=NA){
  annual.median <- as.data.frame( tapply(yields$yield, yields$year, median))
  colnames( annual.median ) <- "annual.median"
  annual.median$year <- as.numeric( row.names(annual.median) )
  yields <- merge(yields, annual.median)
  yields$yield.excess <- yields$yield - yields$annual.median
  m.y <- as.data.frame( tapply(yields$yield.excess, yields$neigh.code, median) )
  colnames(m.y) <- "yield"
  m.y$bc <- row.names(m.y)
  if(is.na(breaks)){
    r <- range(m.y$yield, na.rm=TRUE)
    breaks <- seq(from=r[1], to=r[2], length.out = 6)
  }
  colours <- col.gen(length(breaks)-1)
  m.y$col <- colours[1]
  for(i in 2:length(colours)){
    m.y$col[ m.y$yield > breaks[i]] <- colours[i]
  }
  m.y <- m.y[order(m.y$yield),]
  shp <- merge(shp, m.y, by="bc", all.y=TRUE)
  legend.labels <- paste(sprintf("%.3f", breaks[1:(length(breaks)-1)]), sprintf("%.3f", breaks[2:length(breaks)]), sep="-")
  if(is.na(filename)){
    plot(shp, col=shp$col)
    legend("topright", legend.labels, col=colours, lwd=5, bty="n")
  } else {
    jpeg(filename, width=400, height=340, quality = 100)
    par(mar = c(0,0,0,0))
    plot(shp, col=shp$col)
    legend("topright", legend.labels, col=colours, lwd=5, bty="n")
    dev.off()
  }
}

plot.map.equal.size <- function(shp, yields, breaks=5, filename=NA, stepsize=NA){
  reg <- lm(yield~Y, data=yields)
  yields$yield_resid <- resid(reg)
  m.y <- as.data.frame( tapply(yields$yield_resid, yields$neigh.code, median) )
  colnames(m.y) <- "yield"
  m.y$bc <- row.names(m.y)
  colours <- col.gen(breaks)
  m.y$ntile <- ntile(m.y$yield, breaks)  
  m.y$col <- NA
  for(i in 1:breaks){
    m.y$col[ m.y$ntile  == i] <- colours[i]
  }
  m.y <- m.y[order(m.y$yield),]
  shp <- merge(shp, m.y, by="bc", all.y=TRUE)
  legend.labels <- paste("Quintile", 1:5)
  if(is.na(filename)){
    plot(shp, col=shp$col)
    legend("topright", legend.labels, col=colours, lwd=5, bty="n")
  } else {
    jpeg(filename, width=400, height=340, quality = 100)
    par(mar = c(0,0,0,0))
    plot(shp, col=shp$col)
    legend("topright", legend.labels, col=colours, lwd=5, bty="n")
    dev.off()
  }
}
plot.map.equal.size(shp, subset(y.proj, Y > 1918 & Y < 1940 & !is.na(yield)), filename="../figures/median_yield_amsterdam_1919-1939_equal_size.jpg")
plot.map.equal.size(shp, subset(y.proj, Y > 1950 & Y <= 1970 & !is.na(yield)), filename = "../figures/median_yield_amsterdam_1950-1970_equal_size.jpg")

#breaks <- seq(from=-0.05, to=0.10, length.out = 6) 
breaks <- NA
plot.map(shp, subset(y.proj, year > 1918 & year < 1940 & !is.na(yield)), filename="../figures/median_yield_amsterdam_1919-1939.jpg", breaks=breaks)
plot.map(shp, subset(y.proj, year > 1950 & year <= 1970 & !is.na(yield)), filename = "../figures/median_yield_amsterdam_1950-1970.jpg", breaks=breaks)



y.proj.ny <- y.proj
y.proj.ny$yield <- y.proj.ny$NetYield
y.proj.ny <- subset(y.proj.ny, !is.na(yield))
y.proj.ny <- subset(y.proj.ny, yield < quantile( y.proj.ny$yield , 0.975) & yield > quantile(y.proj.ny$yield, 0.025) )

plot.map(shp, y.proj.ny , filename = "../figures/median_net_yield_amsterdam.jpg", breaks=NA)



