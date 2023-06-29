
# ------------------------------------------------------------------------------------------------ #

### Title: M hypothesis and calibration area ####
### Author: Patron-Rivero, C. ####
### Date: 06/22/2023 ###
### Project: 'Hybridization proccess of Caiman latirostris and C. yucare' ###

# ------------------------------------------------------------------------------------------------ #

# Packages #

# ------------------------------------------------------------------------------------------------ #

library(rgdal)
library(raster)
library(ellipsenm)

# ------------------------------------------------------------------------------------------------ #

# Inputs #

# ------------------------------------------------------------------------------------------------ #

setwd("D:/3_Caiman")
jjm <- readOGR("./ecoreg/Ecoregions2017.shp")
pred_p <- list.files("D:/3_Caiman/2_env/2_america", pattern = ".tif", full.names = TRUE)
stack <- raster::stack(pred_p)
spp <- c("c_lat", "c_yac")
setwd("D:/3_Caiman/1_occ/3_clean_joint")
press <- list.files(pattern = ".csv")
press <- press[-1]

# ------------------------------------------------------------------------------------------------ #

# Calibration area #

# ------------------------------------------------------------------------------------------------ #

for (i in 1:length(press)) {
	setwd("D:/3_Caiman/1_occ/3_clean_joint")
	data <- read.delim(press[[i]], header = TRUE, sep = ",")
	buf <- buffer_area(data = data, longitude = "Long", latitude = "Lat", buffer_distance = 100)
	env <- crop(stack, buf)
	env <- mask(env, buf)
	setwd("D:/3_Caiman/2_env/4_calib")
	writeRaster(env, filename = paste(spp[[i]], "_", names(stack), sep = ""), bylayer = TRUE, format = "GTiff")
	writeRaster(env, filename = paste(spp[[i]], "_", names(stack), sep = ""), bylayer = TRUE, format = "ascii")
	setwd("D:/3_Caiman/1_occ/3_clean_joint")
}

# ------------------------------------------------------------------------------------------------ #

# Area M #

# ------------------------------------------------------------------------------------------------ #

memory.limit(size = 1000000)

for(i in 1:length(press)){
	setwd("D:/3_Caiman/1_occ/3_clean_joint")
	occ <- read.delim(press[[i]], header = T, sep = ",")
	occ1 <- occ[c(2, 3)]
	occ2 <- SpatialPoints(occ1)
	CRS.new <- CRS ("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
	proj4string(occ2) <- CRS.new
	jjm_subset <- jjm[occ2, ]
	polys1 <- aggregate(jjm_subset, dissolve = TRUE)
	raster::shapefile(polys1, paste0("D:/3_Caiman/2_env/3_eco/", press[[i]], ".shp"))
	Ms <- mask(crop(stack, jjm_subset), jjm_subset)
	setwd("D:/3_Caiman/2_env/3_eco")
	writeRaster(Ms, filename = paste(spp[[i]], "_", names(stack), sep = ""), bylayer = T, format = "GTiff")
	writeRaster(Ms, filename = paste(spp[[i]], "_", names(stack), sep = ""), bylayer = T, format = "ascii")
	setwd("D:/3_Caiman/1_occ/3_clean_joint")
}

# ------------------------------------------------------------------------------------------------ #

### EndNotRun
