### Survey simulation package
# Like WiSP, but using spatial objects & SRF
# Names:
#  zone = the area of the survey
#  samp = sample units (transects, blocks, APS?)
#  popn = population

# 2015-04-07 Note that owin objects cannot be created from shapefiles read
#     in by rgdal! Must use maptools.

library(sp)
library(maptools) # Needed for 
library(rgdal)
library(spatstat)
library(rgeos)
library(mapdata)

# Jolly II function
Jolly <- function(srftable) {
  # Computes an estimate and variance from unequal-size samples
  #
  # Args:
  #   srftable - a data frame with the following variables:
  #     transect: sample (transect) number ID;
  #     count: number of observations;
  #     tran.samp: area sampled in transect;
  #     tran.area: total area of transect
  # Returns:
  #   A vector comprising:
  #     obs: total number of observations;
  #     R: 
  #     Y: estimate of population;
  #     n: number of samples
  #     N: estimated total population from which sample was drawn;
  #     varY: Variance of Y
  #     sterr: standard error
  obs <- sum(srftable$count)
  R <- sum(srftable$count)/sum(srftable$tran.samp) # density
  Y <- R * sum(srftable$tran.area) # Pop estimate
  n <- length(srftable$transect) # number of samples
  sy2 <- var(srftable$count) 
  sz2 <- var(srftable$tran.samp)
  szy <- (1/(n-1)) * (sum(srftable$count*srftable$tran.samp) - sum(srftable$tran.area) * sum(srftable$count)/n)
  N <- length(srftable$transect) * sum(srftable$tran.area) / sum(srftable$tran.samp)  # Population of samples (NOT the real one - estimate!)
  varY <- (N*(N-n)/n)*sy2-2*R*n+R^2*sz2  # Variance of population estimate
  sterr <- sqrt(varY)
  answer <- c(obs, R, Y, n, N, varY, sterr)
  return(answer)
}

## Generate survey area
# Use shapefile or generic shape
GenZone <- function (xmin, xmax, ymin, ymax, crs.EPSG = 32731) {
  # Generates a square zone bounded by min/max values.
  # Default CRS is 32736 for Tanzania.
  zone.def <- cbind(c(xmin, xmin, xmax, xmax, xmin), c(ymin, ymax, ymax, ymin, ymin))
  zone.polygon <- Polygon(zone.def)
  zone.polygons <- Polygons(list(zone.polygon), "zone")
  zone.CRS <- CRS(paste("+init=epsg:", crs.EPSG, sep=""))
  zone <- SpatialPolygons(list(zone.polygons), proj4string=zone.CRS)
  return(zone)
}

GenPop <- function (pop.groups = 500, pop.groupsize = 3, zone.owin=zone.owin, zone.CRS=zone.CRS) {
  # Generate population
  # Create locations of individuals or groups
  # Random / clumped, group size > generate locations
  pop.type <- "random"
  pop.points <- runifpoint(pop.groups, win=zone.owin)
  
  pop.points <- as(pop.points, "SpatialPoints") # Convert to spatial
  pop.points <- SpatialPointsDataFrame(pop.points, data=data.frame(count=rpois(pop.groups, pop.groupsize))) 
  pop.total <- sum(pop.points$count)
  proj4string(pop.points) <- zone.CRS
  return(pop.points)
}

GenTran <- function(tran.num = 20, tran.width = 300, zone, crs.EPSG = 32736) {
  ## Generate sample transects / strips / blocks
  # Parameters
  zone.bounds <- bbox(zone)
  tran.list <- list()
  for (tran in 1:tran.num) {
    tn.x <- c(zone.bounds[3], zone.bounds[3], zone.bounds[1], zone.bounds[1], zone.bounds[3])
    tn.ydiv <- (zone.bounds[4] - zone.bounds[2]) / tran.num
    tn.y <- c(tn.ydiv * tran, tn.ydiv * tran + tran.width, tn.ydiv * tran + tran.width, tn.ydiv * tran, tn.ydiv * tran) + (zone.bounds[2] - tn.ydiv/2)
    tn.polygon <- Polygon(cbind(tn.x, tn.y))
    tn.polygons <- Polygons(list(tn.polygon), tran)
    tran.list[tran] <- tn.polygons
  }
  zone.CRS <- CRS(paste("+init=epsg:", crs.EPSG, sep=""))
  return(SpatialPolygons(tran.list, proj4string = zone.CRS))
}

## Sample from population
OverlayPoints <- function (pop.points, transects, tran.num, zone, transect) {
  # Overlay transects
  tran.over <- data.frame(transect=over(pop.points, transects), count=pop.points$count)
  ## Calculate
  # Summarise by transect
  
  tran.table <- data.frame(transect = 1:tran.num, tran.samp = gArea(transects) / tran.num, tran.area = gArea(zone) / tran.num)
  tran.summary <- aggregate(count ~ transect, data=tran.over, sum)
  tran.summary <- merge(tran.table, tran.summary, by='transect', fun=sum, all.x=T)
  tran.summary$count[is.na(tran.summary$count)] <- 0
  return(tran.summary)
}

## Calcs
# zone <- GenZone(540000, 800000, 9038000, 9360000)
# zone.owin <- as.owin(bbox(zone)[c(1,3,2,4)])

setwd("/Users/simbamangu/Dropbox/Projects/GEC-WAP/")
t.surveyshp <- readShapePoly("WAP_surveyboundary_buff5_utm")
EPSG <- 32731   # WAP 
zone.CRS <- CRS(paste("+init=epsg:", EPSG, sep=""))
t.win <- as(t.surveyshp, "owin")
t.pop <- GenPop(500, 3, t.win, zone.CRS)
t.tran <- GenTran(20, 300, t.surveyshp, EPSG)
t.pop.clip <- t.pop[!is.na(over(t.pop,t.tran)),]
plot(t.win)
points(t.pop, pch = 18, cex = 0.4, col = 'gray')
plot(t.tran, add=T)
points(t.pop.clip, col = 'green', pch = 18, cex = 0.5)
proj4string(t.surveyshp) <- proj4string(t.tran)

# t.surveyshp.poly <- as(t.surveyshp, "SpatialPolygons")
# t.tran.clip <- over(t.tran, t.surveyshp)
# t.tran.clip <- gIntersection(t.tran, t.surveyshp.poly)

## Effects of number of transects
sim.start <- Sys.time()

tran.nums <- seq(10, 100, 10)
reps <- 20
numrows <- length(tran.nums) * reps
simulation.trans <- data.frame(id=integer(numrows), transects = integer(numrows), actual = numeric(numrows), estimate = numeric(numrows), stderr = numeric(numrows))
sim <- 1
id <- 0
for(tran.num in tran.nums) {
  print(tran.num)
  for (rep in 1:reps) {
    id <- id + 1
    pop.points <- GenPop(500, 3, zone.owin, zone.CRS)
    transects <- GenTran(tran.num, tran.width = 300, zone, tran, zone.CRS)
    #tran.summary <- overlayPoints(pop.points, transects, tran.num, zone, transect)
    tryCatch(tran.summary <- overlayPoints(pop.points, transects, tran.num, zone, transect), error = function(e) {tran.empty <- data.frame(transect=1:tran.num, tran.samp=1, tran.area=2, count=0)})
    result <- Jolly(tran.summary)
    pop.total <- sum(pop.points$count)
    #print(paste("Transects: ", tran.num, "Actual: ", pop.total, " - ", "Estimate: ", result[3], " +/- ", result[7]))
    simulation.trans[sim,] <- c(id, tran.num, pop.total, result[3], result[7])
    sim <- sim + 1
  }
}

Sys.time() - sim.start

boxplot(simulation.trans$estimate ~ simulation.trans$transects)
simulation.trans$diff <- simulation.trans$actual - simulation.trans$estimate
boxplot(simulation.trans$diff ~ simulation.trans$transects)
simulation.trans$cv <- simulation.sw$stderr / simulation.trans$estimate
boxplot(simulation.trans$cv ~ simulation.trans$transects, ylim = c(0,2))

## Effects of strip width
sim.start <- Sys.time()
tran.widths <- c(100, 200, 300, 400, 500)
reps <- 30
numrows <- length(tran.widths) * reps
simulation.sw <- data.frame(id=integer(numrows), width = integer(numrows), actual = numeric(numrows), estimate = numeric(numrows), stderr = numeric(numrows))
sim <- 1
id <- 0

for(tran.width in tran.widths) {
  print(tran.width)
  for (rep in 1:reps) {
    id <- id + 1
    pop.points <- GenPop(500, 3, zone.owin, zone.CRS)
    transects <- GenTran(tran.num = 40, tran.width, zone, tran, zone.CRS)
    #tran.summary <- overlayPoints(pop.points, transects, tran.num, zone, transect)
    tryCatch(tran.summary <- OverlayPoints(pop.points, transects, tran.num, zone, transect), error = function(e) {tran.empty <- data.frame(transect=1:2, tran.samp=1, tran.area=2, count=0)})
    result <- Jolly(tran.summary)
    pop.total <- sum(pop.points$count)
    # print(paste("Transects: ", tran.num, "Actual: ", pop.total, " - ", "Estimate: ", result[3], " +/- ", result[7]))
    simulation.sw[sim,] <- c(id, tran.width, pop.total, result[3], result[7])
    sim <- sim + 1
  }
}
Sys.time() - sim.start

boxplot(simulation.sw$estimate ~ simulation.sw$width)
simulation.sw$diff <- simulation.sw$actual - simulation.sw$estimate
boxplot(simulation.sw$diff ~ simulation.sw$width)
simulation.sw$cv <- simulation.sw$stderr / simulation.sw$estimate
boxplot(simulation.sw$cv ~ simulation.sw$width)

plot(zone)
plot(transects, add=T, col = 'lightgray')
points(pop.points, cex = pop.points$count / 5, col = 'red')
