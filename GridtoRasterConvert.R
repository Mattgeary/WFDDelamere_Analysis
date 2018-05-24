WFD.rast.v13 <- raster(as.matrix(WFD.v13), crs = crs(ukgrid))
extent(WFD.rast.v13) <- fenn.ext
writeRaster(WFD.rast.v13, "FW_hab/occ_mat/grids/WFD.v13.asc", overwrite = TRUE)
