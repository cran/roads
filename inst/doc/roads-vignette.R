## ----setup1, include = FALSE--------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, results='hide', warning=FALSE, message=FALSE----------------------
library(roads)
library(terra)
library(dplyr)
library(raster)

## colours for displaying cost raster 
if(requireNamespace("viridis", quietly = TRUE)){
  # Use colour blind friendly palette if available
  rastColours <- c('grey50', viridis::viridis(20))
} else {
  rastColours <- c('grey50', terrain.colors(20))
}

## -----------------------------------------------------------------------------
costRaster <- CLUSexample$cost

## ---- fig.show='hold'---------------------------------------------------------
## existing roads network
roadsLine <- sf::st_sfc(geometry = sf::st_linestring(
  matrix(c(0.5, 4.5, 4.5, 4.5),
         ncol = 2, byrow = T) 
)) %>%
  sf::st_as_sf()

## ---- fig.show='hold', fig.width=5, fig.height=4.85---------------------------
## landings as spatial points
landings <- roads::CLUSexample$landings

## plot example scenario
plot(costRaster, col = rastColours, main = 'Example Scenario')
plot(roadsLine, add = TRUE)
plot(landings, add = TRUE, pch = 19)
points(x=5.6,y=4.5,pch=19,xpd=TRUE)
text(x=5.8,y=4.5,labels='landing',adj=c(0,0.4),xpd=TRUE)
lines(x=c(5.3,5.6),y=c(4.2,4.2),lwd=2,xpd=TRUE)
text(x=5.75,y=4.2,labels='roads',adj=c(0,0.3),xpd=TRUE)


## ---- fig.show='hold', fig.width=5, fig.height=4.85---------------------------
## project new roads using the 'snap' approach
projRoads_snap <- roads::projectRoads(landings, costRaster, roadsLine,
                                      roadMethod = 'snap')

## plot the cost raster, landings, and roads segments to the landings
plot(costRaster, col = rastColours, main = "'Snapped' roads")
points(landings, pch = 19, col = 'red')  
plot(projRoads_snap$roads, add = TRUE) 

## update legend
points(x = 5.5, y = 4.8, pch = 19, xpd = TRUE, col = 'red')
text(x = 5.7, y = 4.8, labels = 'landing', adj = c(0, 0.4), xpd = TRUE)
lines(x = c(5.3, 5.6), y = c(4.2, 4.2), lwd = 2, xpd = TRUE)
text(x = 5.75, y = 4.2, labels = 'roads', adj = c(0, 0.3), xpd = TRUE)

## ---- fig.show='hold', fig.width=5, fig.height=4.85---------------------------
## project new roads using the 'LCP' approach
projRoads_lcp <- roads::projectRoads(landings, 
                                        costRaster, 
                                        roadsLine, 
                                        roadMethod = 'lcp')

## plot the cost raster and overlay it with new roads
plot(costRaster, col = rastColours, main = "'LCP' roads")
plot(projRoads_lcp$roads, add = TRUE)
points(landings, pch = 19, col = 'red')  ## landings points
## legend
points(x = 5.5, y = 4.8, pch = 19, xpd = TRUE, col = 'red')
text(x = 5.7, y = 4.8, labels = 'landing', adj = c(0, 0.4), xpd = TRUE)
lines(x = c(5.3, 5.6), y = c(4.2, 4.2), lwd = 2, xpd = TRUE)
text(x = 5.75, y = 4.2, labels = 'roads', adj = c(0, 0.3), xpd = TRUE)

## ---- fig.show='hold', fig.width=5, fig.height=4.85---------------------------
## project new roads using the 'DLCP' approach
projRoads_dlcp <- roads::projectRoads(landings, 
                                        costRaster, 
                                        roadsLine, 
                                        roadMethod = 'dlcp')

## plot the cost raster and overlay it with new roads
plot(costRaster, col = rastColours, main = "'DLCP' roads")
plot(projRoads_dlcp$roads, add = TRUE)
points(landings, pch = 19, col = 'red')  ## landings points
## legend
points(x = 5.5, y = 4.8, pch = 19, xpd = TRUE, col = 'red')
text(x = 5.7, y = 4.8, labels = 'landing', adj = c(0, 0.4), xpd = TRUE)
lines(x = c(5.3, 5.6), y = c(4.2, 4.2), lwd = 2, xpd = TRUE)
text(x = 5.75, y = 4.2, labels = 'roads', adj = c(0, 0.3), xpd = TRUE)

## ---- fig.show='hold', fig.width=5, fig.height=4.85---------------------------
## project new roads using the 'DLCP' approach
projRoads_dlcp2 <- roads::projectRoads(rev(landings), 
                                        costRaster, 
                                        roadsLine, 
                                        roadMethod = 'dlcp')

## plot the cost raster and overlay it with new roads
plot(costRaster, col = rastColours, main = "'DLCP' roads")
plot(projRoads_dlcp2$roads, add = TRUE)
points(landings, pch = 19, col = 'red')  ## landings points
## legend
points(x = 5.5, y = 4.8, pch = 19, xpd = TRUE, col = 'red')
text(x = 5.7, y = 4.8, labels = 'landing', adj = c(0, 0.4), xpd = TRUE)
lines(x = c(5.3, 5.6), y = c(4.2, 4.2), lwd = 2, xpd = TRUE)
text(x = 5.75, y = 4.2, labels = 'roads', adj = c(0, 0.3), xpd = TRUE)

## ---- fig.show='hold',  fig.width=5, fig.height=4.85--------------------------
## project new roads using the 'MST' approach
projRoads_mst <- roads::projectRoads(landings, 
                                        costRaster,
                                        roadsLine, 
                                        roadMethod = 'mst')

## plot the cost raster and overlay it with new roads
plot(costRaster, col = rastColours, main = "'MST' roads")
plot(projRoads_mst$roads, add = TRUE)
points(landings, pch = 19, col = 'red')  ## landings points
## legend
points(x = 5.5, y = 4.8, pch = 19, xpd = TRUE, col = 'red')
text(x = 5.7, y = 4.8, labels = 'landing', adj = c(0, 0.4), xpd = TRUE)
lines(x = c(5.3, 5.6), y = c(4.2, 4.2), lwd = 2, xpd = TRUE)
text(x = 5.75, y = 4.2, labels = 'roads', adj = c(0, 0.3), xpd = TRUE)

## ---- fig.show='hold', fig.width=7,fig.height=6.5-----------------------------
## colours for displaying cost raster
if(requireNamespace("viridis", quietly = TRUE)){
  # Use colour blind friendly palette if available
  rastColours2 <- c('grey50', viridis::viridis(30))
} else {
  rastColours2 <- c('grey50', terrain.colors(30))
}

## scenario 
scen <- demoScen[[1]]
## landing sets 1 to 4 of this scenario 
land.pnts <- scen$landings.points[scen$landings.points$set %in% c(1:4),]
## plot the cost raster and landings
par(mar=par('mar')/2)
plot(scen$cost.rast, col = rastColours2, main = 'Cost and landings (by set)')
plot(land.pnts, add = TRUE, pch = 21, cex = 2, bg = 'white')
text(land.pnts@coords, labels = land.pnts$set, cex = 0.6, adj = c(0.5, 0.3),
     xpd = TRUE)

## ---- fig.show='hold', fig.width=7,fig.height=6.86----------------------------
## project roads for landing sets 1 to 4, with independent one-time simulations
oneTime_sim <- list() ## empty list 
for (i in 1:4){
  oneTime_sim <- c(oneTime_sim,
                       roads::projectRoads(land.pnts[land.pnts$set==i,],
                                              scen$cost.rast,
                                              scen$cost.rast==0,
                                              roadMethod='mst')$roads)
}

## plot
oldpar <- par(mfrow = c(2, 2), mar = par('mar')/2)
for (i in 1:4){
  oneTime_sim[[i]][!oneTime_sim[[i]]] <- NA 
  plot(scen$cost.rast, col = rastColours2, 
       main = paste0('Landings set ', i),
       legend = FALSE)
  plot(oneTime_sim[[i]], add = TRUE, col = "grey50", legend = FALSE)
  plot(land.pnts[land.pnts$set == i, ], add = TRUE,
       pch = 21, cex = 1.5, bg = 'white')
}


## ---- fig.show='hold', fig.width=5,fig.height=4.85----------------------------
## raster representing the union of completely independent simulations for multiple sets
oneTime_sim <- rast(oneTime_sim)
independent <- any(oneTime_sim == 1)
## set non-road to NA for display purposes
independent[!independent] <- NA 

## plot 
plot(scen$cost.rast, col = rastColours2,
     main = 'Union of independent sim results',
     legend = FALSE)

plot(independent, col = 'grey30', add = TRUE, legend = FALSE)

plot(land.pnts, add = TRUE, pch = 21, cex = 1.5, bg = 'white')

## ---- fig.show='hold', fig.width=7, fig.height=9.97---------------------------
## continuing on with demo scenario 1
## landing sets 1 to 4 of this scenario as a raster stack
land.stack <- scen$landings.stack[[1:4]]

# initialize sim list with first landings set
multiTime_sim <- list(projectRoads(land.stack[[1]], scen$cost.rast, 
                                               scen$road.line))

# iterate over landings sets using the sim list from the previous run as input
for (i in 2:raster::nlayers(land.stack)) {
  multiTime_sim <- c(
    multiTime_sim,
    list(projectRoads(sim =  multiTime_sim[[i-1]], landings = land.stack[[i]]))
  ) 
}

par(mfrow = c(3, 2))
par(mar = par('mar')/2)
  plot(scen$cost.rast, col = rastColours2, main = 'Roads at time t = 0', 
       legend = FALSE)
  plot(scen$road.line, col = 'grey30', add = TRUE, legend = FALSE)
  
for (i in 1:length(multiTime_sim)){
  plot(multiTime_sim[[i]]$costSurface, col = rastColours2, 
       main = paste0('Roads at time t = ', i), legend = FALSE)
  plot(multiTime_sim[[i]]$roads, col = 'grey30', add = TRUE, legend = FALSE)
  plot(land.pnts[land.pnts$set == i, ], add = TRUE, pch = 21, 
         cex = 1.5, bg = 'white')
  if (i >= 2){
    plot(land.pnts[land.pnts$set < i, ], add = TRUE, pch = 1, cex = 1.5)
    plot(land.pnts[land.pnts$set == i, ], add = TRUE, pch = 21, 
         cex = 1.5, bg = 'white')
  }
}

## ----fig.width=6,fig.height=5-------------------------------------------------
harvPoly <- demoScen[[1]]$landings.poly

outCent <- getLandingsFromTarget(harvPoly)
raster::plot(harvPoly)
plot(outCent, col = "red", add = TRUE)

# Get random sample with density 0.02 pts per unit area
outRand <- getLandingsFromTarget(harvPoly, 0.02, sampleType = "random")
prRand <- projectRoads(outRand, scen$cost.rast, scen$road.line)

plot(scen$cost.rast, main = "Random Landings in Harvest Blocks",
     col = rastColours2)
plot(harvPoly, add = TRUE)
plot(prRand$roads, add = TRUE,  col = "grey50")
plot(outRand, col = "red", add = TRUE)

# Get regular sample with density 0.02 pts per unit area
outReg <- getLandingsFromTarget(harvPoly, 0.02, sampleType = "regular")
prReg <- projectRoads(outReg, scen$cost.rast,scen$road.line)


plot(scen$cost.rast, main = "Regular Landings in Harvest Blocks",
     col = rastColours2)
plot(harvPoly, add = TRUE)
plot(prReg$roads, add = TRUE, col = "grey50")
plot(outReg, col = "red", add = TRUE)


# clean up 
par(oldpar)

