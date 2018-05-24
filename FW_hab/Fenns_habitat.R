library(raster)

ukgrid = "+init=epsg:27700"
fenn.ext <- extent(c(348000, 350000, 335000, 337000))

moss <- raster(as.matrix(read.csv("Fenns_Moss.csv", header = FALSE)), crs = crs(ukgrid))
extent(moss) <- fenn.ext               
writeRaster(moss, "Fenns_moss.img", overwrite = TRUE)

scrub <- raster(as.matrix(read.csv("Fenns_Scrub.csv", header = FALSE)), crs = crs(ukgrid))
extent(scrub) <- fenn.ext               
writeRaster(scrub, "Fenns_scrub.img", overwrite = TRUE)

scrubMoss <- raster(as.matrix(read.csv("Fenns_ScrubMoss.csv", header = FALSE)), crs = crs(ukgrid))
extent(scrubMoss) <- fenn.ext               
writeRaster(scrubMoss, "Fenns_scrubmoss.img", overwrite = TRUE)

water <- raster(as.matrix(read.csv("Fenns_Water.csv", header = FALSE)), crs = crs(ukgrid))
extent(water) <- fenn.ext               
writeRaster(water, "Fenns_water.img", overwrite = TRUE)

wood <- raster(as.matrix(read.csv("Fenns_Wood.csv", header = FALSE)), crs = crs(ukgrid))
extent(wood) <- fenn.ext               
writeRaster(wood, "Fenns_wood.img", overwrite = TRUE)



