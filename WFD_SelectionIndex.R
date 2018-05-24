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

hab.dat <- data.frame(moss = getValues(moss), scrub = getValues(scrub), scrubMoss = getValues(scrubMoss), water = getValues(water), wood = getValues(wood))

### Funtion to select and print dominant habitat
## x is data frame of habitat proportions
## n is names of columns
dom.hab <- function(x, n){
   if(is.na(max(x))){
     y <- NA
   }
  else{
   y <- n[which.max(x)]
  }
    return(y)
}
hab.names <- names(hab.dat)
dom.hab.vals <- character(400)
for(i in 1:length(hab.dat[,1])){
  dom.hab.vals[i] <- dom.hab(hab.dat[i,], hab.names)
}
hab.dat$dom <- as.factor(dom.hab.vals)

pres.hab <- data.frame(moss = extract(moss, WFD.pres.BNG), scrub = extract(scrub, WFD.pres.BNG), scrubMoss = extract(scrubMoss, WFD.pres.BNG), water = extract(water, WFD.pres.BNG), wood = extract(wood, WFD.pres.BNG))
pres.hab.vals <- character(258)
for(i in 1:length(pres.hab[,1])){
  pres.hab.vals[i] <- dom.hab(pres.hab[i,], hab.names)
}
pres.hab$dom <- as.factor(pres.hab.vals)

w.i <- table(pres.hab$dom)/sum(table(pres.hab$dom))
pi.i <- table(hab.dat$dom)[-4]/sum(table(hab.dat$dom)[-4])


selection.index <- as.vector(w.i)/as.vector(pi.i)
names(selection.index) <- c("Open moss", "Scrub", "Scrub & Moss", "Woodland")
selection.index
barplot(selection.index, ylab = "Neu's selection index", xlab = "Habitat type")
