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
y <- read.csv("../data/yields.paris.csv")
colnames(y) <- tolower(colnames(y))
y$house.no <- y$housenumber
y$street <- tolower(y$street)
# merge coordinates
coords <- read.csv("../data/geocoded.addresses.paris.csv", as.is=TRUE)
y <- merge(y, coords, by=c("street", "house.no"), all.x=TRUE)


# only keep observations that are located in at least one Paris Vasserot Arrondissements
y <- subset(y, !is.na(num.arrond))


# Investigate spatial dependency in yields: Moran I test
# First account for changes by year/neighbourhood in yields:
# OLS regression: GrossYield ~ Year + NB 
# The estimate Moran's I statistic in the residuals
# ( this might take a while )
run.morans.I <- function(){
  tmp <- subset(y, !is.na(yield) & !is.na(lon))
  # First, control for year and neighbourhood
  yield.reg.nb <- lm(yield~factor(year)+factor(num.arrond), data=tmp)
  # Then contrast this to year controls only
  yield.reg <- lm(yield~factor(year), data=tmp)
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
M.I <- run.morans.I() 
sink("../data/snippets/moransi.paris.txt")
print(M.I)
sink()
# ------------------------

# ------------------------



col.gen <- colorRampPalette(c("blue", "red"), space = "Lab")
# plot median yields per 
coordinates(y) <- ~lon+lat

shp <- shapefile("../data/gis/gis.paris/Vasserot_Arrondissements.shp")
shp$num.arrond <- as.numeric(shp$NUM_ARROND)
proj4string(y) <- CRS("+init=epsg:4326")
y.proj <- spTransform(y, crs(shp))

plot.map <- function(shp, yields, breaks=NA, filename=NA, stepsize=NA){
  annual.median <- as.data.frame( tapply(yields$yield, yields$year, median))
  colnames( annual.median ) <- "annual.median"
  annual.median$year <- as.numeric( row.names(annual.median) )
  yields <- merge(yields, annual.median)
  yields$yield.excess <- yields$yield - yields$annual.median
  m.y <- as.data.frame( tapply(yields$yield.excess, yields$num.arrond, median) )
  colnames(m.y) <- "yield"
  m.y$num.arrond <- row.names(m.y)
  if(is.na(breaks)){
    r <- range(m.y$yield)
    breaks <- seq(from=r[1], to=r[2], length.out = 6)
  }
  colours <- col.gen(length(breaks)-1)
  m.y$col <- colours[1]
  for(i in 2:length(colours)){
    m.y$col[ m.y$yield > breaks[i]] <- colours[i]
  }
  m.y <- m.y[order(m.y$yield),]
  shp <- merge(shp, m.y, by="num.arrond", all.y=TRUE)
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
  reg <- lm(yield~year, data=yields)
  yields$yield_resid <- resid(reg)
  m.y <- as.data.frame( tapply(yields$yield_resid, yields$num.arrond, median) )
  colnames(m.y) <- "yield"
  m.y$num.arrond <- row.names(m.y)
  colours <- col.gen(breaks)
  m.y$ntile <- ntile(m.y$yield, breaks)  
  m.y$col <- NA
  for(i in 1:breaks){
    m.y$col[ m.y$ntile  == i] <- colours[i]
  }
  m.y <- m.y[order(m.y$yield),]
  shp <- merge(shp, m.y, by="num.arrond", all.y=TRUE)
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
plot.map.equal.size(shp, subset(y.proj, year >= 1810 & year <= 1840), filename="../figures/median_yield_paris_1810-1840_equal_size.jpg")
plot.map.equal.size(shp, subset(y.proj, year >= 1870 & year <= 1900), filename="../figures/median_yield_paris_1870-1900_equal_size.jpg")

breaks <- seq(from=-0.012, to=0.01, length.out = 6) 
plot.map(shp, subset(y.proj, year >= 1810 & year <= 1840), filename="../figures/median_yield_paris_1810-1840.jpg", breaks=breaks)
plot.map(shp, subset(y.proj, year >= 1870 & year <= 1900), filename="../figures/median_yield_paris_1870-1900.jpg", breaks=breaks)


y.sec <- read.csv("../data/geocoded.yields.paris.secondary.csv")
y.sec$year <- y.sec$Year
y.sec <- subset(y.sec, !is.na(lat))
coordinates(y.sec) <- ~lon+lat
proj4string(y.sec) <- CRS("+init=epsg:4326")
y.sec.proj <- spTransform(y.sec, crs(shp))

# just checking that the projections work out...
# plot(shp)
# points(addr.proj)
# now do an overlay
y.sec.proj$num.arrond <- as.numeric(over(y.sec.proj, shp)$NUM_ARROND)
y.sec.proj.vasserot <- subset(y.sec.proj, !is.na(num.arrond) & !is.na(yield))
y.sec.proj.vasserot <- subset(y.sec.proj.vasserot, yield > quantile(y.sec.proj.vasserot$yield, 0.025) & yield < quantile( y.sec.proj.vasserot$yield, 0.975))

plot.map.equal.size(shp, subset(y.sec.proj.vasserot, year >= 1870 & year <= 1900), filename="../figures/median_yield_paris_1810-1840_equal_size_secondary.jpg")
