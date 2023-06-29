
# ------------------------------------------------------------------------------------------------ #

### Title: Bias layer and background points ####
### Author: Patron-Rivero, C. ####
### Date: 06/22/2023 ###
### Project: 'Hybridization proccess of Caiman latirostris and C. yucare' ###

# ------------------------------------------------------------------------------------------------ #

# Packages #

# ------------------------------------------------------------------------------------------------ #

library(raster)
library(dplyr)
library(adehabitatHR)
library(flexsdm)

# ------------------------------------------------------------------------------------------------ #

# Clean data #

# ------------------------------------------------------------------------------------------------ #

r_l <- raster("D:/3_Caiman/2_env/4_calib/C_lat/c_lat_bio_1.tif")
r_y <- raster("D:/3_Caiman/2_env/4_calib/C_yac/c_yac_bio_1.tif")
r_merge <- merge(r_l, r_y)
setwd("D:/3_Caiman/1_occ/4_bias")
press <- list.files(pattern = ".csv")
test <- read.csv("D:/3_Caiman/1_occ/2_eval_genetic/all.csv", header = TRUE, sep = ",", row.names = NULL)
all <- read.csv("D:/3_Caiman/1_occ/3_clean_joint/all.csv", header = TRUE, sep = ",", row.names = NULL)
data_list <- list()

for (i in 1:length(press)) {
	setwd("D:/3_Caiman/1_occ/4_bias")
		if (grepl("\t", readLines(press[[i]], n = 1))) {
			data <- read.csv(press[[i]], header = TRUE, sep = "\t", row.names = NULL)
		} else if (grepl(",", readLines(press[[i]], n = 1))) {
			data <- read.csv(press[[i]], header = TRUE, sep = ",", row.names = NULL)
			}
	if ("species" %in% colnames(data) && "decimalLongitude" %in% colnames(data) && "decimalLatitude" %in% colnames(data)) {
		data <- data[, c("species", "decimalLongitude", "decimalLatitude")]
	} else if ("scientificname" %in% colnames(data) && "decimallongitude" %in% colnames(data) && "decimallatitude" %in% colnames(data)) {
			data <- data[, c("scientificname", "decimallongitude", "decimallatitude")]
		}
	data_1 <- na.omit(data)
	colnames(data_1) <- c("Spp", "Long", "Lat")
	data_2 <- data_1[!duplicated(data_1[, c("Lat", "Long")]), ]
	env <- raster::extract(r_merge, data_2[, c("Long", "Lat")])
	f_occ_env <- data.frame(data_2, env)
	occ_env <- na.omit(f_occ_env)
	data_clean <- occ_env[, c("Spp", "Long", "Lat")]
	data_list[[i]] <- data_clean
}

combined_data <- do.call(rbind, data_list)
data_filtered <- rbind(combined_data, all, test)
env <- raster::extract(r_merge, data_filtered[, c("Long", "Lat")])
f_occ_env <- data.frame(data_filtered, env)
occ_env <- na.omit(f_occ_env)
data_clean <- occ_env[, c("Spp", "Long", "Lat")]
write.csv(data_clean, "D:/3_Caiman/1_occ/4_bias/bias.csv", row.names = F)

# ------------------------------------------------------------------------------------------------ #

# Inputs #

# ------------------------------------------------------------------------------------------------ #

den <- data_clean[, c("Long", "Lat")]
den <- SpatialPoints(den)
r <- rasterToPoints(r_merge)
r1 <- SpatialPoints(r[, 1:2])
r2 <- SpatialPixels(r1)

# ------------------------------------------------------------------------------------------------ #

# Kernel Utilization Distribution probability #

# ------------------------------------------------------------------------------------------------ #

kde <- kernelUD(den, h = "href", grid = r2)
k <- raster(kde)
projection(k) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
k1 <- crop(k, r_merge)
projection(r_merge) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
r_merge2 <- crop(r_merge, k1)
k3 <- mask(k1, r_merge2)

setwd("D:/3_Caiman/2_env/5_bias")
writeRaster(k3, filename = "bias.tif", format = "GTiff", overwrite = TRUE)

# ------------------------------------------------------------------------------------------------ #

# Background points #

# ------------------------------------------------------------------------------------------------ #

bg <- function(r_layer, output_file) {
	r_layer <- crop(r_layer, k3)
	k_mask <- mask(crop(k3, r_layer), r_layer)
  		bg_data <- sample_background(
			data = data_clean,
			x = "Long",
			y = "Lat",
			n = 10000,
			method = "biased",
			rlayer = r_layer,
			rbias = k_mask)
	write.csv(bg_data, output_file, row.names = FALSE)
}

bg(r_l, "D:/3_Caiman/2_env/5_bias/bg_lat.csv")
bg(r_y, "D:/3_Caiman/2_env/5_bias/bg_yac.csv")

# ------------------------------------------------------------------------------------------------ #

### EndNotRun
