# test getLandingsFromTarget works for different scenarios
demoScen <- prepExData(demoScen)


lndsDenTest <- list(1, 0.5, 0.25, 0.1, 0.01, 0.001)

lndPoly <- demoScen[[1]]$landings.poly %>% sf::st_as_sf() %>% 
  sf::st_set_agr("constant")

test_that("sf input polygons work", {
  # replicate because some errors only happened with specific samples
  outsReg <- replicate(20, 
                       {lapply(lndsDenTest, function(x){
                         getLandingsFromTarget(lndPoly, 
                                               landingDens = x, 
                                               sampleType = "regular")
                       })
                       },
                       simplify = FALSE)
  expect_type(outsReg, "list")
  
  if(interactive()){
    plot(lndPoly %>% sf::st_geometry())
    plot(outsReg[[1]][[4]], col = "red", add = T)
  }
})

test_that("sf input polygons work for random", {
  # replicate because some errors only happened with specific samples
  outsRand <- replicate(20, 
                        {lapply(lndsDenTest, function(x){
                          getLandingsFromTarget(lndPoly, 
                                                landingDens = x, 
                                                sampleType = "random")
                        })
                        },
                        simplify = FALSE)
  expect_type(outsRand, "list")
  
  if(interactive()){
    plot(lndPoly %>% sf::st_geometry())
    plot(outsRand[[1]][[4]], col = "red", add = T)
  }
  
})


test_that("sp polygon input works for centroid",{
  outCent <- getLandingsFromTarget(demoScen[[1]]$landings.poly)
  expect_type(outCent, "list")
  
  if(interactive()){
    plot(sf::st_geometry(demoScen[[1]]$landings.poly))
    plot(outCent, col = "red", add = T)
  }
  
})

test_that("raster no clumps input works",{
 outRast1 <- getLandingsFromTarget(demoScen[[1]]$landings.stack[[1]])
 
 expect_warning(getLandingsFromTarget(demoScen[[1]]$landings.stack[[1]], 0.5, 
                                      sampleType = "regular"),
                "landingDens is ignored")
 if(interactive()){
   plot(sf::st_geometry(demoScen[[1]]$landings.stack[[1]]))
   plot(outRast1, col = "red", add = T)
 }
})

test_that("raster with clumps input works no ID",{
  rast <- demoScen[[1]]$landings.poly %>%
    terra::rasterize(demoScen[[1]]$cost.rast) %>% 
    terra::`crs<-`(value = "EPSG:5070")
  
  # make sure that a single celled harvest block will work with clumps
  rast[10,10] <- 6

  # Show effect of ID
  rast[78:88, 4:5] <- 7
  
  outRastCent <- getLandingsFromTarget(rast > 0)
  outRastRand <- getLandingsFromTarget(rast > 0, landingDens = 0.1, 
                                      sampleType = "random")
  outRastReg <- getLandingsFromTarget(rast > 0, landingDens = 0.1, 
                                      sampleType = "regular")
  
  expect_type(outRastCent, "list")
  
  if(interactive()){
    terra::plot(rast)
    plot(outRastCent, col = "red", add = T)
    
    terra::plot(rast)
    plot(outRastRand, col = "red", add = T)
    
    terra::plot(rast)
    plot(outRastReg, col = "red", add = T)
  }
})

test_that("raster with clumps input works with ID",{
  rast <- demoScen[[1]]$landings.poly %>% terra::vect() %>%
    terra::rasterize(terra::rast(demoScen[[1]]$cost.rast), field = "ID") %>% 
    terra::`crs<-`(value = "EPSG:5070")
  
  # make sure that a single celled havest block will work with clumps
  rast[10,10] <- 6
  
  # Show effect of ID
  rast[78:88, 4:5] <- 7
  
  outRastCent <- getLandingsFromTarget(rast)
  outRastRand <- getLandingsFromTarget(rast, landingDens = 0.1, 
                                       sampleType = "random")
  outRastReg <- getLandingsFromTarget(rast, landingDens = 0.1, 
                                      sampleType = "regular")
  
  expect_type(outRastCent, "list")
  
  if(interactive()){
    terra::plot(rast)
    plot(outRastCent, col = "red", add = T)
    
    terra::plot(rast)
    plot(outRastRand, col = "red", add = T)
    
    terra::plot(rast)
    plot(outRastReg, col = "red", add = T)
  }
})

