# simple example test case - compare to results from kylesCLUSExample.Rmd

library(dplyr)
library(sf)

doPlot <- interactive()
###########################################
# basic test case from kylesCLUSExample.Rmd
#   - 5 by 5 raster representing cost, populated with uniform random numbers based on seed value 1
#   - first/top row (cost[1:5]) identified as existing roads (cost is 0 for these cells)
#   - 4 landings are cells: 11, 13, 22, 25
##############################################
# hard code expected results from CLUS example

CLUS.snap.roads <- c(1:25)[-c(9,14,16,18,19,21,23,24)]  

CLUS.lcp.roads <- c(1:25)[-c(6,7,9,15,17,18,20,21,23,24)] 

CLUS.mst.roads <- c(1:25)[-c(6:9,15,17,18,20,21,23,24)] 
###############################################
# generate the results from roads::projectRoads
CLUSexample <- prepExData(CLUSexample)
landingsC <- CLUSexample$landings
costC     <- CLUSexample$cost
roadsC    <- CLUSexample$roads

pR_snap <- projectRoads(landings = landingsC, 
                           weightRaster = costC,
                           roads = roadsC,
                           roadMethod = "snap", roadsOut = "sf")

pR_lcp <- projectRoads(landings = landingsC,
                          weightRaster = costC,
                          roads = roadsC,
                          roadMethod = "lcp", 
                          neighbourhood = "queen", roadsOut = "sf")

pR_mst <- projectRoads(landings = landingsC,
                          weightRaster = costC,
                          roads = roadsC,
                          roadMethod="mst", 
                          neighbourhood = "queen", roadsOut = "sf")

# ilcp tested below

getRoadCells <- function(rast, roads, method){
  # browser()
  ex_line <- terra::extract(rast, terra::vect(roads %>% sf::st_collection_extract("LINESTRING")), 
                            cells = TRUE, touches = FALSE)  %>% pull(cell) 
  ex_pt <- terra::extract(rast, terra::vect(roads %>% sf::st_collection_extract("POINT")), 
                          cells = TRUE)  %>% pull(cell) 
  
  union(ex_line, ex_pt) %>% sort()
}
###############################################
# perform tests
test_that("Projected roads results match CLUS example results for the 'snap' method",{
  expect_equal(getRoadCells(costC, pR_snap$roads, "snap"), CLUS.snap.roads)
})
test_that("Projected roads results match CLUS example results for the 'lcp' method",{
  expect_equal(getRoadCells(costC, pR_lcp$roads, "lcp"), CLUS.lcp.roads)
})
test_that("Projected roads results match CLUS example results for the 'mst' method",{
  expect_equal(getRoadCells(costC, pR_mst$roads, "mst"), CLUS.mst.roads)
})

test_that("Iterative LCP works",{
  # by iterating works but should be possible to make much faster
  land.pnts2 <- landingsC %>% st_as_sf() %>% 
    mutate(ID = c(1:4)) %>% st_set_agr("constant")
  
  iterLands_sim <- list(projectRoads(land.pnts2[land.pnts2$ID==1,],
                                     costC,
                                     costC==0,
                                     roadMethod='lcp', roadsOut = "sf")) 
  for (i in 2:max(land.pnts2$ID)){
    iterLands_sim <- c(iterLands_sim,
                       list(projectRoads(sim = iterLands_sim[[i-1]], 
                                         landings = land.pnts2[land.pnts2$ID==i,], 
                                         roadsOut = "sf")))
  }
  
  ## plot
  if(doPlot){
    plotRoads(iterLands_sim[[4]])
    plot(land.pnts2, add = TRUE, pch = letters[land.pnts2$ID], cex = 1.5, col = 'black')
  }

  
  # with iterative LCP
  
  # add a landing that is touching the road for testing
  land.pnts3 <- land.pnts2 %>%
    bind_rows(land.pnts2 %>% slice(1) %>%
                mutate(geometry = geometry + c(0,1.5)) %>%
                sf::st_set_crs(sf::st_crs(land.pnts2))) %>%
    mutate(ID = 1:5) %>% arrange(ID) %>% st_set_agr("constant")

  dyLCP <- projectRoads(land.pnts3,
               costC,
               costC==0,
               roadMethod='ilcp', roadsOut = "sf", ordering = "none")

  if(doPlot){
    plotRoads(dyLCP)
  }
  
  # not really needed but potentially useful for getting total cost
  # start_edge <- getGraph(list(weightRaster = costC), "octagon") %>% 
  #   igraph::edge_attr(name = "weight") %>% sum()
  # 
  # it_end_edge <- iterLands_sim[[4]]$g %>% igraph::edge_attr(name = "weight") %>% sum()
  # dy_end_edge <- dyLCP$g %>% igraph::edge_attr(name = "weight") %>% sum()
  
  expect_identical(getRoadCells(costC, dyLCP$roads),
                   getRoadCells(costC, iterLands_sim[[4]]$roads))
  
})
###############################################
# end of tests

