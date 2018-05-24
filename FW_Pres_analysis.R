library(raster)
library(reshape2)
library(unmarked)


latlong <- "+init=epsg:4326"
ukgrid <- "+init=epsg:27700"


# Load presence data
### Wind score of 6+ changed to 7 for ease of interpretation
WFD.pres <- read.csv("FW_presdata.csv")
plot(WFD.pres$Long, WFD.pres$Lat)

## Add 'presence' column
WFD.pres$pres <- 1

## Create spatial object for presence points
WFD.pres.latlong <- SpatialPointsDataFrame(data.frame(WFD.pres$Long, WFD.pres$Lat), data = WFD.pres, proj4string = CRS(latlong))

## Convert to BNG
WFD.pres.BNG <-  spTransform(WFD.pres.latlong,  CRS(ukgrid))
## Add trasnsformed x and y values to data frame
WFD.pres.BNG@data$x <- WFD.pres.BNG@coords[,1]
WFD.pres.BNG@data$y <- WFD.pres.BNG@coords[,2]


# Create habitat rasters
## Define extent
#fenn.ext <- extent(c(348000, 350000, 335000, 337000)) ## Previous extent needs correcting
fenn.ext <- extent(c(347890, 349890, 335000, 337000)) ## Corrected visually using Google maps

moss <- raster(as.matrix(read.csv("FW_hab/Fenns_Moss.csv", header = FALSE)), crs = crs(ukgrid))
extent(moss) <- fenn.ext               
writeRaster(moss, "FW_hab/Fenns_moss.img", overwrite = TRUE)

scrub <- raster(as.matrix(read.csv("FW_hab/Fenns_Scrub.csv", header = FALSE)), crs = crs(ukgrid))
extent(scrub) <- fenn.ext               
writeRaster(scrub, "FW_hab/Fenns_scrub.img", overwrite = TRUE)

scrubMoss <- raster(as.matrix(read.csv("FW_hab/Fenns_ScrubMoss.csv", header = FALSE)), crs = crs(ukgrid))
extent(scrubMoss) <- fenn.ext               
writeRaster(scrubMoss, "FW_hab/Fenns_scrubmoss.img", overwrite = TRUE)

water <- raster(as.matrix(read.csv("FW_hab/Fenns_Water.csv", header = FALSE)), crs = crs(ukgrid)) ## Water matrix corrected visually using aerial photographs (Google maps)
extent(water) <- fenn.ext               
writeRaster(water, "FW_hab/Fenns_water.img", overwrite = TRUE)

wood <- raster(as.matrix(read.csv("FW_hab/Fenns_Wood.csv", header = FALSE)), crs = crs(ukgrid))
extent(wood) <- fenn.ext               
writeRaster(wood, "FW_hab/Fenns_wood.img", overwrite = TRUE)

### Split WFD data by visit
### Will create matrix of presence then edit by hand to include absences (0s) along transect lines
WFD.pres.v1 <- WFD.pres[WFD.pres.BNG@data$Date == "16/05/2017",] 
WFD.pres.v1 <- SpatialPointsDataFrame(data.frame(WFD.pres.v1$E, WFD.pres.v1$N), data = WFD.pres.v1, proj4string = CRS(ukgrid))
WFD.pres.v2 <- WFD.pres[WFD.pres.BNG@data$Date == "22/05/2017",]
WFD.pres.v2 <- SpatialPointsDataFrame(data.frame(WFD.pres.v2$E, WFD.pres.v2$N), data = WFD.pres.v2, proj4string = CRS(ukgrid))
WFD.pres.v3 <- WFD.pres[WFD.pres.BNG@data$Date == "25/05/2017",]
WFD.pres.v3 <- SpatialPointsDataFrame(data.frame(WFD.pres.v3$E, WFD.pres.v3$N), data = WFD.pres.v3, proj4string = CRS(ukgrid))
WFD.pres.v4 <- WFD.pres[WFD.pres.BNG@data$Date == "30/05/2017",]
WFD.pres.v4 <- SpatialPointsDataFrame(data.frame(WFD.pres.v4$E, WFD.pres.v4$N), data = WFD.pres.v4, proj4string = CRS(ukgrid))
WFD.pres.v5 <- WFD.pres[WFD.pres.BNG@data$Date == "01/06/2017",]
WFD.pres.v5 <- SpatialPointsDataFrame(data.frame(WFD.pres.v5$E, WFD.pres.v5$N), data = WFD.pres.v5, proj4string = CRS(ukgrid))
WFD.pres.v6 <- WFD.pres[WFD.pres.BNG@data$Date == "07/06/2017",]
WFD.pres.v6 <- SpatialPointsDataFrame(data.frame(WFD.pres.v6$E, WFD.pres.v6$N), data = WFD.pres.v6, proj4string = CRS(ukgrid))
WFD.pres.v7 <- WFD.pres[WFD.pres.BNG@data$Date == "09/06/2017",]
WFD.pres.v7 <- SpatialPointsDataFrame(data.frame(WFD.pres.v7$E, WFD.pres.v7$N), data = WFD.pres.v7, proj4string = CRS(ukgrid))
WFD.pres.v8 <- WFD.pres[WFD.pres.BNG@data$Date == "13/06/2017",]
WFD.pres.v8 <- SpatialPointsDataFrame(data.frame(WFD.pres.v8$E, WFD.pres.v8$N), data = WFD.pres.v8, proj4string = CRS(ukgrid))
WFD.pres.v9 <- WFD.pres[WFD.pres.BNG@data$Date == "19/06/2017",]
WFD.pres.v9 <- SpatialPointsDataFrame(data.frame(WFD.pres.v9$E, WFD.pres.v9$N), data = WFD.pres.v9, proj4string = CRS(ukgrid))
WFD.pres.v10 <- WFD.pres[WFD.pres.BNG@data$Date == "23/06/2017",]
WFD.pres.v10 <- SpatialPointsDataFrame(data.frame(WFD.pres.v10$E, WFD.pres.v10$N), data = WFD.pres.v10, proj4string = CRS(ukgrid))
WFD.pres.v11 <- WFD.pres[WFD.pres.BNG@data$Date == "26/06/2017",]
WFD.pres.v11 <- SpatialPointsDataFrame(data.frame(WFD.pres.v11$E, WFD.pres.v11$N), data = WFD.pres.v11, proj4string = CRS(ukgrid))
WFD.pres.v12 <- WFD.pres[WFD.pres.BNG@data$Date == "04/07/2017",]
WFD.pres.v12 <- SpatialPointsDataFrame(data.frame(WFD.pres.v12$E, WFD.pres.v12$N), data = WFD.pres.v12, proj4string = CRS(ukgrid))
WFD.pres.v13 <- WFD.pres[WFD.pres.BNG@data$Date == "07/07/2017",]
WFD.pres.v13 <- SpatialPointsDataFrame(data.frame(WFD.pres.v13$E, WFD.pres.v13$N), data = WFD.pres.v13, proj4string = CRS(ukgrid))

WFD.occ.v1 <- rasterize(WFD.pres.v1, water, "pres")
WFD.occ.v2 <- rasterize(WFD.pres.v2, water, "pres")
WFD.occ.v3 <- rasterize(WFD.pres.v3, water, "pres")
WFD.occ.v4 <- rasterize(WFD.pres.v4, water, "pres")
WFD.occ.v5 <- rasterize(WFD.pres.v5, water, "pres")
WFD.occ.v6 <- rasterize(WFD.pres.v6, water, "pres")
WFD.occ.v7 <- rasterize(WFD.pres.v7, water, "pres")
WFD.occ.v8 <- rasterize(WFD.pres.v8, water, "pres")
WFD.occ.v9 <- rasterize(WFD.pres.v9, water, "pres")
WFD.occ.v10 <- rasterize(WFD.pres.v10, water, "pres")
WFD.occ.v11 <- rasterize(WFD.pres.v11, water, "pres")
WFD.occ.v12 <- rasterize(WFD.pres.v12, water, "pres")
WFD.occ.v13 <- rasterize(WFD.pres.v13, water, "pres")

### Write occupancy matrices
# write.csv(as.matrix(WFD.occ.v1), "FW_hab/occ_mat/WFD.occ.v1.csv")
# write.csv(as.matrix(WFD.occ.v2), "FW_hab/occ_mat/WFD.occ.v2.csv")
# write.csv(as.matrix(WFD.occ.v3), "FW_hab/occ_mat/WFD.occ.v3.csv")
# write.csv(as.matrix(WFD.occ.v4), "FW_hab/occ_mat/WFD.occ.v4.csv")
# write.csv(as.matrix(WFD.occ.v5), "FW_hab/occ_mat/WFD.occ.v5.csv")
# write.csv(as.matrix(WFD.occ.v6), "FW_hab/occ_mat/WFD.occ.v6.csv")
# write.csv(as.matrix(WFD.occ.v7), "FW_hab/occ_mat/WFD.occ.v7.csv")
# write.csv(as.matrix(WFD.occ.v8), "FW_hab/occ_mat/WFD.occ.v8.csv")
# write.csv(as.matrix(WFD.occ.v9), "FW_hab/occ_mat/WFD.occ.v9.csv")
# write.csv(as.matrix(WFD.occ.v10), "FW_hab/occ_mat/WFD.occ.v10.csv")
# write.csv(as.matrix(WFD.occ.v11), "FW_hab/occ_mat/WFD.occ.v11.csv")
# write.csv(as.matrix(WFD.occ.v12), "FW_hab/occ_mat/WFD.occ.v12.csv")
# write.csv(as.matrix(WFD.occ.v13), "FW_hab/occ_mat/WFD.occ.v13.csv")

## Rasters are edited by hand to include absence values along transects
## Transects were standard routes and are implied by presence points
### Zeroes were recorded as straight (diagonal) lines between presence 
### points roughly following the footpath route at F & W
### Only locations with at least two 0s will be used as absence locations

WFD.v1 <-read.csv("FW_hab/occ_mat/WFD.occ.v1.csv", header = FALSE)
WFD.v2 <-read.csv("FW_hab/occ_mat/WFD.occ.v2.csv", header = FALSE)
WFD.v3 <-read.csv("FW_hab/occ_mat/WFD.occ.v3.csv", header = FALSE)
WFD.v4 <-read.csv("FW_hab/occ_mat/WFD.occ.v4.csv", header = FALSE)
WFD.v5 <-read.csv("FW_hab/occ_mat/WFD.occ.v5.csv", header = FALSE)
WFD.v6 <-read.csv("FW_hab/occ_mat/WFD.occ.v6.csv", header = FALSE)
WFD.v7 <-read.csv("FW_hab/occ_mat/WFD.occ.v7.csv", header = FALSE)
WFD.v8 <-read.csv("FW_hab/occ_mat/WFD.occ.v8.csv", header = FALSE)
WFD.v9 <-read.csv("FW_hab/occ_mat/WFD.occ.v9.csv", header = FALSE)
WFD.v10 <-read.csv("FW_hab/occ_mat/WFD.occ.v10.csv", header = FALSE)
WFD.v11 <-read.csv("FW_hab/occ_mat/WFD.occ.v11.csv", header = FALSE)
WFD.v12 <-read.csv("FW_hab/occ_mat/WFD.occ.v12.csv", header = FALSE)
WFD.v13 <-read.csv("FW_hab/occ_mat/WFD.occ.v13.csv", header = FALSE)

## Create data frame of occupancy per visit
#WFD.occ.dat <- data.frame('visit'= character(), 'row' = numeric(), 'col' = numeric(), 'pres' = numeric())
# Visit 1
WFD.occ.dat <- data.frame('visit'= character(length(which(!is.na(WFD.v1)))), 'row' = numeric(length(which(!is.na(WFD.v1)))), 'col' = numeric(length(which(!is.na(WFD.v1)))), 'pres' = numeric(length(which(!is.na(WFD.v1)))))
WFD.occ.dat$visit <- rep("v1", length(which(!is.na(WFD.v1))))
WFD.occ.dat[,2:3] <- which(!is.na(WFD.v1), arr.ind = TRUE)
WFD.occ.dat$pres <- WFD.v1[which(!is.na(WFD.v1), arr.ind = TRUE)]
# Visit 2
WFD.occ.temp <- data.frame('visit'= character(length(which(!is.na(WFD.v2)))), 'row' = numeric(length(which(!is.na(WFD.v2)))), 'col' = numeric(length(which(!is.na(WFD.v2)))), 'pres' = numeric(length(which(!is.na(WFD.v2)))))
WFD.occ.temp$visit <- rep("v2", length(which(!is.na(WFD.v2))))
WFD.occ.temp[,2:3] <- which(!is.na(WFD.v2), arr.ind = TRUE)
WFD.occ.temp$pres <- WFD.v2[which(!is.na(WFD.v2), arr.ind = TRUE)]
WFD.occ.dat <- rbind(WFD.occ.dat, WFD.occ.temp)
# Visit 3
WFD.occ.temp <- data.frame('visit'= character(length(which(!is.na(WFD.v3)))), 'row' = numeric(length(which(!is.na(WFD.v3)))), 'col' = numeric(length(which(!is.na(WFD.v3)))), 'pres' = numeric(length(which(!is.na(WFD.v3)))))
WFD.occ.temp$visit <- rep("v3", length(which(!is.na(WFD.v3))))
WFD.occ.temp[,2:3] <- which(!is.na(WFD.v3), arr.ind = TRUE)
WFD.occ.temp$pres <- WFD.v3[which(!is.na(WFD.v3), arr.ind = TRUE)]
WFD.occ.dat <- rbind(WFD.occ.dat, WFD.occ.temp)
# Visit 4
WFD.occ.temp <- data.frame('visit'= character(length(which(!is.na(WFD.v4)))), 'row' = numeric(length(which(!is.na(WFD.v4)))), 'col' = numeric(length(which(!is.na(WFD.v4)))), 'pres' = numeric(length(which(!is.na(WFD.v4)))))
WFD.occ.temp$visit <- rep("v4", length(which(!is.na(WFD.v4))))
WFD.occ.temp[,2:3] <- which(!is.na(WFD.v4), arr.ind = TRUE)
WFD.occ.temp$pres <- WFD.v4[which(!is.na(WFD.v4), arr.ind = TRUE)]
WFD.occ.dat <- rbind(WFD.occ.dat, WFD.occ.temp)
# Visit 5
WFD.occ.temp <- data.frame('visit'= character(length(which(!is.na(WFD.v5)))), 'row' = numeric(length(which(!is.na(WFD.v5)))), 'col' = numeric(length(which(!is.na(WFD.v5)))), 'pres' = numeric(length(which(!is.na(WFD.v5)))))
WFD.occ.temp$visit <- rep("v5", length(which(!is.na(WFD.v5))))
WFD.occ.temp[,2:3] <- which(!is.na(WFD.v5), arr.ind = TRUE)
WFD.occ.temp$pres <- WFD.v5[which(!is.na(WFD.v5), arr.ind = TRUE)]
WFD.occ.dat <- rbind(WFD.occ.dat, WFD.occ.temp)
# Visit 6
WFD.occ.temp <- data.frame('visit'= character(length(which(!is.na(WFD.v6)))), 'row' = numeric(length(which(!is.na(WFD.v6)))), 'col' = numeric(length(which(!is.na(WFD.v6)))), 'pres' = numeric(length(which(!is.na(WFD.v6)))))
WFD.occ.temp$visit <- rep("v6", length(which(!is.na(WFD.v6))))
WFD.occ.temp[,2:3] <- which(!is.na(WFD.v6), arr.ind = TRUE)
WFD.occ.temp$pres <- WFD.v6[which(!is.na(WFD.v6), arr.ind = TRUE)]
WFD.occ.dat <- rbind(WFD.occ.dat, WFD.occ.temp)
# Visit 7
WFD.occ.temp <- data.frame('visit'= character(length(which(!is.na(WFD.v7)))), 'row' = numeric(length(which(!is.na(WFD.v7)))), 'col' = numeric(length(which(!is.na(WFD.v7)))), 'pres' = numeric(length(which(!is.na(WFD.v7)))))
WFD.occ.temp$visit <- rep("v7", length(which(!is.na(WFD.v7))))
WFD.occ.temp[,2:3] <- which(!is.na(WFD.v7), arr.ind = TRUE)
WFD.occ.temp$pres <- WFD.v7[which(!is.na(WFD.v7), arr.ind = TRUE)]
WFD.occ.dat <- rbind(WFD.occ.dat, WFD.occ.temp)
# Visit 8
WFD.occ.temp <- data.frame('visit'= character(length(which(!is.na(WFD.v8)))), 'row' = numeric(length(which(!is.na(WFD.v8)))), 'col' = numeric(length(which(!is.na(WFD.v8)))), 'pres' = numeric(length(which(!is.na(WFD.v8)))))
WFD.occ.temp$visit <- rep("v8", length(which(!is.na(WFD.v8))))
WFD.occ.temp[,2:3] <- which(!is.na(WFD.v8), arr.ind = TRUE)
WFD.occ.temp$pres <- WFD.v8[which(!is.na(WFD.v8), arr.ind = TRUE)]
WFD.occ.dat <- rbind(WFD.occ.dat, WFD.occ.temp)
# Visit 9
WFD.occ.temp <- data.frame('visit'= character(length(which(!is.na(WFD.v9)))), 'row' = numeric(length(which(!is.na(WFD.v9)))), 'col' = numeric(length(which(!is.na(WFD.v9)))), 'pres' = numeric(length(which(!is.na(WFD.v9)))))
WFD.occ.temp$visit <- rep("v9", length(which(!is.na(WFD.v9))))
WFD.occ.temp[,2:3] <- which(!is.na(WFD.v9), arr.ind = TRUE)
WFD.occ.temp$pres <- WFD.v9[which(!is.na(WFD.v9), arr.ind = TRUE)]
WFD.occ.dat <- rbind(WFD.occ.dat, WFD.occ.temp)
# Visit 10
WFD.occ.temp <- data.frame('visit'= character(length(which(!is.na(WFD.v10)))), 'row' = numeric(length(which(!is.na(WFD.v10)))), 'col' = numeric(length(which(!is.na(WFD.v10)))), 'pres' = numeric(length(which(!is.na(WFD.v10)))))
WFD.occ.temp$visit <- rep("v10", length(which(!is.na(WFD.v10))))
WFD.occ.temp[,2:3] <- which(!is.na(WFD.v10), arr.ind = TRUE)
WFD.occ.temp$pres <- WFD.v10[which(!is.na(WFD.v10), arr.ind = TRUE)]
WFD.occ.dat <- rbind(WFD.occ.dat, WFD.occ.temp)
# Visit 11
WFD.occ.temp <- data.frame('visit'= character(length(which(!is.na(WFD.v11)))), 'row' = numeric(length(which(!is.na(WFD.v11)))), 'col' = numeric(length(which(!is.na(WFD.v11)))), 'pres' = numeric(length(which(!is.na(WFD.v11)))))
WFD.occ.temp$visit <- rep("v11", length(which(!is.na(WFD.v11))))
WFD.occ.temp[,2:3] <- which(!is.na(WFD.v11), arr.ind = TRUE)
WFD.occ.temp$pres <- WFD.v11[which(!is.na(WFD.v11), arr.ind = TRUE)]
WFD.occ.dat <- rbind(WFD.occ.dat, WFD.occ.temp)
#Visit 12
WFD.occ.temp <- data.frame('visit'= character(length(which(!is.na(WFD.v12)))), 'row' = numeric(length(which(!is.na(WFD.v12)))), 'col' = numeric(length(which(!is.na(WFD.v12)))), 'pres' = numeric(length(which(!is.na(WFD.v12)))))
WFD.occ.temp$visit <- rep("v12", length(which(!is.na(WFD.v12))))
WFD.occ.temp[,2:3] <- which(!is.na(WFD.v12), arr.ind = TRUE)
WFD.occ.temp$pres <- WFD.v12[which(!is.na(WFD.v12), arr.ind = TRUE)]
WFD.occ.dat <- rbind(WFD.occ.dat, WFD.occ.temp)
# Visit 13
WFD.occ.temp <- data.frame('visit'= character(length(which(!is.na(WFD.v13)))), 'row' = numeric(length(which(!is.na(WFD.v13)))), 'col' = numeric(length(which(!is.na(WFD.v13)))), 'pres' = numeric(length(which(!is.na(WFD.v13)))))
WFD.occ.temp$visit <- rep("v13", length(which(!is.na(WFD.v13))))
WFD.occ.temp[,2:3] <- which(!is.na(WFD.v13), arr.ind = TRUE)
WFD.occ.temp$pres <- WFD.v13[which(!is.na(WFD.v13), arr.ind = TRUE)]
WFD.occ.dat <- rbind(WFD.occ.dat, WFD.occ.temp)

### Creat occupancy matrix
WFD.occ.mat <- dcast(WFD.occ.dat, row + col ~ visit, value.var = "pres", fun.aggregate = max)
WFD.occ.mat[WFD.occ.mat == -Inf] <- NA

## Check for rows with no values
apply(WFD.occ.mat[,3:15], 1, sum, na.rm = TRUE)

## Remove rows with less than two 1/0 values and sum of 1/0 values is 0
WFD.occ.mat <- WFD.occ.mat[-c(which(apply(WFD.occ.mat[,3:15], 1, FUN = function(x) length(x[!is.na(x)])) <= 2 & apply(WFD.occ.mat[,3:15], 1, sum, na.rm = TRUE) < 1)),]


### Create observation covariates
obsCov.cloud <- data.frame("cloud.v1" = rep(WFD.pres$Cloud[WFD.pres.BNG@data$Date == "16/05/2017"][1], length(WFD.occ.mat[,1])), "cloud.v2" = rep(WFD.pres$Cloud[WFD.pres.BNG@data$Date == "22/05/2017"][1], length(WFD.occ.mat[,1])),"cloud.v3" = rep(WFD.pres$Cloud[WFD.pres.BNG@data$Date == "25/05/2017"][1], length(WFD.occ.mat[,1])),
                           "cloud.v4" = rep(WFD.pres$Cloud[WFD.pres.BNG@data$Date == "30/05/2017"][1], length(WFD.occ.mat[,1])), "cloud.v5" = rep(WFD.pres$Cloud[WFD.pres.BNG@data$Date == "01/06/2017"][1], length(WFD.occ.mat[,1])), "cloud.v6" = rep(WFD.pres$Cloud[WFD.pres.BNG@data$Date == "07/06/2017"][1], length(WFD.occ.mat[,1])),
                           "cloud.v7" = rep(WFD.pres$Cloud[WFD.pres.BNG@data$Date == "09/06/2017"][1], length(WFD.occ.mat[,1])), "cloud.v8" = rep(WFD.pres$Cloud[WFD.pres.BNG@data$Date == "13/06/2017"][1], length(WFD.occ.mat[,1])), "cloud.v9" = rep(WFD.pres$Cloud[WFD.pres.BNG@data$Date == "19/06/2017"][1], length(WFD.occ.mat[,1])), "cloud.v10" = rep(WFD.pres$Cloud[WFD.pres.BNG@data$Date == "23/06/2017"][1], length(WFD.occ.mat[,1])), "cloud.v11" = rep(WFD.pres$Cloud[WFD.pres.BNG@data$Date == "26/06/2017"][1], length(WFD.occ.mat[,1])), "cloud.v12" = rep(WFD.pres$Cloud[WFD.pres.BNG@data$Date == "04/07/2017"][1], length(WFD.occ.mat[,1])),
                           "cloud.v13" = rep(WFD.pres$Cloud[WFD.pres.BNG@data$Date == "07/07/2017"][1], length(WFD.occ.mat[,1])))


obsCov.temp <- data.frame("temp.v1" = rep(WFD.pres$Temp[WFD.pres.BNG@data$Date == "16/05/2017"][1], length(WFD.occ.mat[,1])) ,"temp.v2" = rep(WFD.pres$Temp[WFD.pres.BNG@data$Date == "22/05/2017"][1], length(WFD.occ.mat[,1])),"temp.v3" = rep(WFD.pres$Temp[WFD.pres.BNG@data$Date == "25/05/2017"][1], length(WFD.occ.mat[,1])),
                           "temp.v4" = rep(WFD.pres$Temp[WFD.pres.BNG@data$Date == "30/05/2017"][1], length(WFD.occ.mat[,1])), "temp.v5" = rep(WFD.pres$Temp[WFD.pres.BNG@data$Date == "01/06/2017"][1], length(WFD.occ.mat[,1])), "temp.v6" = rep(WFD.pres$Temp[WFD.pres.BNG@data$Date == "07/06/2017"][1], length(WFD.occ.mat[,1])),
                           "temp.v7" = rep(WFD.pres$Temp[WFD.pres.BNG@data$Date == "09/06/2017"][1], length(WFD.occ.mat[,1])), "temp.v8" = rep(WFD.pres$Temp[WFD.pres.BNG@data$Date == "13/06/2017"][1], length(WFD.occ.mat[,1])), "temp.v9" = rep(WFD.pres$Temp[WFD.pres.BNG@data$Date == "19/06/2017"][1], length(WFD.occ.mat[,1])),
                          "temp.v10" = rep(WFD.pres$Temp[WFD.pres.BNG@data$Date == "23/06/2017"][1], length(WFD.occ.mat[,1])), "temp.v11" = rep(WFD.pres$Temp[WFD.pres.BNG@data$Date == "26/06/2017"][1], length(WFD.occ.mat[,1])), "temp.v12" = rep(WFD.pres$Temp[WFD.pres.BNG@data$Date == "04/07/2017"][1], length(WFD.occ.mat[,1])),
"temp.v13" = rep(WFD.pres$Temp[WFD.pres.BNG@data$Date == "07/07/2017"][1], length(WFD.occ.mat[,1]))) 

obsCov.wind <- data.frame("wind.v1" = rep(WFD.pres$Wind[WFD.pres.BNG@data$Date == "16/05/2017"][1], length(WFD.occ.mat[,1])) ,"wind.v2" = rep(WFD.pres$Wind[WFD.pres.BNG@data$Date == "22/05/2017"][1], length(WFD.occ.mat[,1])),"wind.v3" = rep(WFD.pres$Wind[WFD.pres.BNG@data$Date == "25/05/2017"][1], length(WFD.occ.mat[,1])),
                           "wind.v4" = rep(WFD.pres$Wind[WFD.pres.BNG@data$Date == "30/05/2017"][1], length(WFD.occ.mat[,1])), "wind.v5" = rep(WFD.pres$Wind[WFD.pres.BNG@data$Date == "01/06/2017"][1], length(WFD.occ.mat[,1])), "wind.v6" = rep(WFD.pres$Wind[WFD.pres.BNG@data$Date == "07/06/2017"][1], length(WFD.occ.mat[,1])),
                           "wind.v7" = rep(WFD.pres$Wind[WFD.pres.BNG@data$Date == "09/06/2017"][1], length(WFD.occ.mat[,1])), "wind.v8" = rep(WFD.pres$Wind[WFD.pres.BNG@data$Date == "13/06/2017"][1], length(WFD.occ.mat[,1])), "wind.v9" = rep(WFD.pres$Wind[WFD.pres.BNG@data$Date == "19/06/2017"][1], length(WFD.occ.mat[,1])),
                          "wind.v10" = rep(WFD.pres$Wind[WFD.pres.BNG@data$Date == "23/06/2017"][1], length(WFD.occ.mat[,1])), "wind.v11" = rep(WFD.pres$Wind[WFD.pres.BNG@data$Date == "26/06/2017"][1], length(WFD.occ.mat[,1])), "wind.v12" = rep(WFD.pres$Wind[WFD.pres.BNG@data$Date == "04/07/2017"][1], length(WFD.occ.mat[,1])),
"wind.v13" = rep(WFD.pres$Wind[WFD.pres.BNG@data$Date == "07/07/2017"][1], length(WFD.occ.mat[,1]))) 

obsCov.effort <- data.frame("effort.v1" = rep(WFD.pres$Effort[WFD.pres.BNG@data$Date == "16/05/2017"][1], length(WFD.occ.mat[,1])) ,"effort.v2" = rep(WFD.pres$Effort[WFD.pres.BNG@data$Date == "22/05/2017"][1], length(WFD.occ.mat[,1])),"effort.v3" = rep(WFD.pres$Effort[WFD.pres.BNG@data$Date == "25/05/2017"][1], length(WFD.occ.mat[,1])),
                           "effort.v4" = rep(WFD.pres$Effort[WFD.pres.BNG@data$Date == "30/05/2017"][1], length(WFD.occ.mat[,1])), "effort.v5" = rep(WFD.pres$Effort[WFD.pres.BNG@data$Date == "01/06/2017"][1], length(WFD.occ.mat[,1])), "effort.v6" = rep(WFD.pres$Effort[WFD.pres.BNG@data$Date == "07/06/2017"][1], length(WFD.occ.mat[,1])),
                           "effort.v7" = rep(WFD.pres$Effort[WFD.pres.BNG@data$Date == "09/06/2017"][1], length(WFD.occ.mat[,1])), "effort.v8" = rep(WFD.pres$Effort[WFD.pres.BNG@data$Date == "13/06/2017"][1], length(WFD.occ.mat[,1])), "effort.v9" = rep(WFD.pres$Effort[WFD.pres.BNG@data$Date == "19/06/2017"][1], length(WFD.occ.mat[,1])), 
                           "effort.v10" = rep(WFD.pres$Effort[WFD.pres.BNG@data$Date == "23/06/2017"][1], length(WFD.occ.mat[,1])), "effort.v11" = rep(WFD.pres$Effort[WFD.pres.BNG@data$Date == "26/06/2017"][1], length(WFD.occ.mat[,1])), "effort.v12" = rep(WFD.pres$Effort[WFD.pres.BNG@data$Date == "04/07/2017"][1], length(WFD.occ.mat[,1])),
"effort.v13" = rep(WFD.pres$Effort[WFD.pres.BNG@data$Date == "07/07/2017"][1], length(WFD.occ.mat[,1])))

### Create site covariates
WFD.occ.mat$moss <- as.matrix(moss)[as.matrix(WFD.occ.mat[,1:2])]
WFD.occ.mat$scrub <- as.matrix(scrub)[as.matrix(WFD.occ.mat[,1:2])]
WFD.occ.mat$scrubMoss <- as.matrix(scrubMoss)[as.matrix(WFD.occ.mat[,1:2])]
WFD.occ.mat$water <- as.matrix(water)[as.matrix(WFD.occ.mat[,1:2])]
WFD.occ.mat$wood <- as.matrix(wood)[as.matrix(WFD.occ.mat[,1:2])]


########## Occupancy model #############


#### Build UMF using data from csv files
# Creat observation covariates list
#WFD.obsCovs <- list(cloud=obsCov.cloud[-c(40, 41, 47, 49, 57, 58, 62:64),], temp=obsCov.temp[-c(40, 41, 47, 49, 57, 58, 62:64),], wind=obsCov.wind[-c(40, 41, 47, 49, 57, 58, 62:64),], effort=obsCov.effort[-c(40, 41, 47, 49, 57, 58, 62:64),])
WFD.obsCovs <- list(cloud=obsCov.cloud[-c(40, 41, 47, 49, 57, 58, 62:64),-c(5:8)], temp=obsCov.temp[-c(40, 41, 47, 49, 57, 58, 62:64), -c(5:8)], wind=obsCov.wind[-c(40, 41, 47, 49, 57, 58, 62:64), -c(5:8)], effort=obsCov.effort[-c(40, 41, 47, 49, 57, 58, 62:64), -c(5:8)])

#WFD.UMF <- unmarkedFrameOccu(y = WFD.occ.mat[-c(40, 41, 47, 49, 57, 58, 62:64),3:15], siteCovs = WFD.occ.mat[-c(40, 41, 47, 49, 57, 58, 62:64),16:20], obsCovs = WFD.obsCovs)
WFD.UMF <- unmarkedFrameOccu(y = WFD.occ.mat[-c(40, 41, 47, 49, 57, 58, 62:64), c(3:6, 11:15)], siteCovs = WFD.occ.mat[-c(40, 41, 47, 49, 57, 58, 62:64),16:20], obsCovs = WFD.obsCovs)
# obsCovs(WFD.UMF) <- scale(obsCovs(WFD.UMF))
# oC.scale <- scale(obsCovs(WFD.UMF))
#siteCovs(WFD.UMF) <- scale(siteCovs(WFD.UMF))
#sC.scale <- scale(siteCovs(WFD.UMF))

WFD.occu <- occu(~1 ~1, WFD.UMF)
summary(WFD.occu)

WFD.occu.1 <- occu(~ wind + effort ~1, WFD.UMF)
summary(WFD.occu.1)

WFD.occu.2 <- occu(~temp + effort ~1, WFD.UMF)
summary(WFD.occu.2)

WFD.occu.3 <- occu(~temp ~1, WFD.UMF)
summary(WFD.occu.3)

WFD.occu.4 <- occu(~cloud + effort ~1, WFD.UMF)
summary(WFD.occu.4)

WFD.occu.5 <- occu(~temp + effort ~ water + scrub, WFD.UMF)
summary(WFD.occu.5)
head(siteCovs(WFD.UMF))

WFD.occu.1 <- occu(~1 ~ moss + scrubMoss + water, WFD.UMF)
WFD.occu.2 <- occu(~1 ~ moss + scrub + water, WFD.UMF)
WFD.occu.3 <- occu(~1 ~ moss + scrubMoss, WFD.UMF)
WFD.occu.4 <- occu(~1 ~ moss + scrub, WFD.UMF)
WFD.occu.5 <- occu(~1 ~ scrubMoss + water, WFD.UMF)
WFD.occu.6 <- occu(~1 ~ scrub + water, WFD.UMF)
WFD.occu.7 <- occu(~1 ~ moss + water, WFD.UMF)
WFD.occu.8 <- occu(~1 ~ moss, WFD.UMF)
WFD.occu.9 <- occu(~1 ~ scrub, WFD.UMF)
WFD.occu.10 <- occu(~1 ~ scrubMoss, WFD.UMF)
WFD.occu.11 <- occu(~1 ~ moss + scrub + water + wood, WFD.UMF)
WFD.occu.12 <- occu(~1 ~ moss + scrub + wood, WFD.UMF)
WFD.occu.13 <- occu(~1 ~ scrub + water + wood, WFD.UMF)
WFD.occu.14 <- occu(~1 ~ moss + water + wood, WFD.UMF)
WFD.occu.15 <- occu(~1 ~ moss + wood, WFD.UMF)
WFD.occu.16 <- occu(~1 ~ scrub + wood, WFD.UMF)
WFD.occu.17 <- occu(~1 ~ wood, WFD.UMF)
WFD.occu.18 <- occu(~wind ~ moss + scrubMoss + water, WFD.UMF)
WFD.occu.19 <- occu(~wind ~ moss + scrub + water, WFD.UMF)
WFD.occu.20 <- occu(~wind ~ moss + scrubMoss, WFD.UMF)
WFD.occu.21 <- occu(~wind ~ moss + scrub, WFD.UMF)
WFD.occu.22 <- occu(~wind ~ scrubMoss + water, WFD.UMF)
WFD.occu.23 <- occu(~wind ~ scrub + water, WFD.UMF)
WFD.occu.24 <- occu(~wind ~ moss + water, WFD.UMF)
WFD.occu.25 <- occu(~wind ~ moss, WFD.UMF)
WFD.occu.26 <- occu(~wind ~ scrub, WFD.UMF)
WFD.occu.27 <- occu(~wind ~ scrubMoss, WFD.UMF)
WFD.occu.28 <- occu(~wind ~ moss + scrub + water + wood, WFD.UMF)
WFD.occu.29 <- occu(~wind ~ moss + scrub + wood, WFD.UMF)
WFD.occu.30 <- occu(~wind ~ scrub + water + wood, WFD.UMF)
WFD.occu.31 <- occu(~wind ~ moss + water + wood, WFD.UMF)
WFD.occu.32 <- occu(~wind ~ moss + wood, WFD.UMF)
WFD.occu.33 <- occu(~wind ~ scrub + wood, WFD.UMF)
WFD.occu.34 <- occu(~wind ~ wood, WFD.UMF)
WFD.occu.35 <- occu(~temp ~ moss + scrubMoss + water, WFD.UMF)
WFD.occu.36 <- occu(~temp ~ moss + scrub + water, WFD.UMF)
WFD.occu.37 <- occu(~temp ~ moss + scrubMoss, WFD.UMF)
WFD.occu.38 <- occu(~temp ~ moss + scrub, WFD.UMF)
WFD.occu.39 <- occu(~temp ~ scrubMoss + water, WFD.UMF)
WFD.occu.40 <- occu(~temp ~ scrub + water, WFD.UMF)
WFD.occu.41 <- occu(~temp ~ moss + water, WFD.UMF)
WFD.occu.42 <- occu(~temp ~ moss, WFD.UMF)
WFD.occu.43 <- occu(~temp ~ scrub, WFD.UMF)
WFD.occu.44 <- occu(~temp ~ scrubMoss, WFD.UMF)
WFD.occu.45 <- occu(~temp ~ moss + scrub + water + wood, WFD.UMF)
WFD.occu.46 <- occu(~temp ~ moss + scrub + wood, WFD.UMF)
WFD.occu.47 <- occu(~temp ~ scrub + water + wood, WFD.UMF)
WFD.occu.48 <- occu(~temp ~ moss + water + wood, WFD.UMF)
WFD.occu.49 <- occu(~temp ~ moss + wood, WFD.UMF)
WFD.occu.50 <- occu(~temp ~ scrub + wood, WFD.UMF)
WFD.occu.51 <- occu(~temp ~ wood, WFD.UMF)

### Doesn't work when predictors not scaled but works for best model
#WFD.mods <- fitList(WFD.occu, WFD.occu.1, WFD.occu.2, WFD.occu.3, WFD.occu.4, WFD.occu.5, WFD.occu.6, WFD.occu.7, WFD.occu.8, WFD.occu.9, WFD.occu.10, WFD.occu.11,WFD.occu.12, WFD.occu.13, WFD.occu.14, WFD.occu.15, WFD.occu.16, WFD.occu.17, WFD.occu.18, WFD.occu.19, WFD.occu.20, WFD.occu.21, WFD.occu.22, WFD.occu.23, WFD.occu.24, WFD.occu.25, WFD.occu.26, WFD.occu.27, WFD.occu.28, WFD.occu.29, WFD.occu.30, WFD.occu.31, WFD.occu.32, WFD.occu.33, WFD.occu.34, WFD.occu.35, WFD.occu.36, WFD.occu.37, WFD.occu.38, WFD.occu.39, WFD.occu.40, WFD.occu.41, WFD.occu.42, WFD.occu.43, WFD.occu.44, WFD.occu.45, WFD.occu.46, WFD.occu.47, WFD.occu.48, WFD.occu.49, WFD.occu.50, WFD.occu.51)
WFD.mods <- fitList(WFD.occu, WFD.occu.1, WFD.occu.2, WFD.occu.3, WFD.occu.4, WFD.occu.5, WFD.occu.6, WFD.occu.7, WFD.occu.9, WFD.occu.10, WFD.occu.11,WFD.occu.12, WFD.occu.13, WFD.occu.14, WFD.occu.16, WFD.occu.17, WFD.occu.18, WFD.occu.19, WFD.occu.20, WFD.occu.21, WFD.occu.22, WFD.occu.23, WFD.occu.24, WFD.occu.26, WFD.occu.27, WFD.occu.28, WFD.occu.29, WFD.occu.30, WFD.occu.31, WFD.occu.33, WFD.occu.34, WFD.occu.35, WFD.occu.36, WFD.occu.37, WFD.occu.38, WFD.occu.39, WFD.occu.40, WFD.occu.41, WFD.occu.42, WFD.occu.43, WFD.occu.44, WFD.occu.45, WFD.occu.46, WFD.occu.47, WFD.occu.48, WFD.occu.49, WFD.occu.50, WFD.occu.51)
modSel(WFD.mods)

### Best model has positive relationship with moss, negligible relationship with scrubMoss and no covariate on detectablity (3)

## Occupancy
backTransform(linearComb(WFD.occu.3, coefficients = c(1,0), type = 'state'))
## Detectability
backTransform(linearComb(WFD.occu.3, coefficients = c(1), type = 'det'))

### Relationship with Moss


new.dat<- data.frame(moss=seq(min(siteCovs(WFD.UMF)$moss),max(siteCovs(WFD.UMF)$moss),,20), scrubMoss = 0)
WFD.pred<-predict(WFD.occu.3,type="state",newdata=new.dat,appendData=TRUE)

#### Scale and center not working here - max wood is 90%
#WFD.pred$wood<-(WFD.pred$wood*attr(sC.scale,"scaled:scale")[5]+attr(sC.scale,"scaled:center")[5])
plot(Predicted ~ moss, WFD.pred,type="l",xlab="Proportion of moss in 100 m square",
     ylab="Probability of Occupancy",ylim=c(0,1))
lines(lower ~ moss, WFD.pred,type="l",col="red")
lines(upper ~ moss, WFD.pred,type="l",col="red")

summary(WFD.occ.mat$wood)

