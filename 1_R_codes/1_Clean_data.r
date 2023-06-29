
# ------------------------------------------------------------------------------------------------ #

### Title: Clean databases ####
### Author: Patron-Rivero, C. ####
### Date: 06/21/2023 ###
### Project: 'Hybridization proccess of Caiman latirostris and C. yucare' ###

# ------------------------------------------------------------------------------------------------ #

# Packages #

# ------------------------------------------------------------------------------------------------ #

library(raster)
library(dplyr)

# ------------------------------------------------------------------------------------------------ #

# Inputs #

# ------------------------------------------------------------------------------------------------ #

pred <- list.files("D:/3_Caiman/2_env/2_america", pattern = ".tif", full.names = TRUE)
stack <- raster::stack(pred)
setwd("D:/3_Caiman/1_occ/1_raw")
press <- list.files(pattern = ".csv")
data_list <- list()
test <- read.csv("D:/3_Caiman/1_occ/2_eval_genetic/all.csv", header = TRUE, sep = ",", row.names = NULL)

# ------------------------------------------------------------------------------------------------ #

# Cleaning duplicated and obvious errors #

# ------------------------------------------------------------------------------------------------ #

for (i in 1:length(press)) {
	setwd("D:/3_Caiman/1_occ/1_raw")
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
	env <- raster::extract(stack, data_2[, c("Long", "Lat")])
	f_occ_env <- data.frame(data_2, env)
	occ_env <- na.omit(f_occ_env)
	data_clean <- occ_env[, c("Spp", "Long", "Lat")]
	data_list[[i]] <- data_clean
}

combined_data <- do.call(rbind, data_list)
data_filtered <- anti_join(combined_data, test, by = c("Long", "Lat"))
write.csv(data_filtered, "D:/3_Caiman/1_occ/3_clean_joint/all.csv", row.names = F)

# ------------------------------------------------------------------------------------------------ #

# Cleaning taxonomic errors by experts #

# ------------------------------------------------------------------------------------------------ #

split_data <- split(data_filtered, data_filtered$Spp)
c_lat <- split_data[[1]]
c_yac <- split_data[[2]]

leaflet_map <- function(data) {
	map <- leaflet(data) %>%
		addTiles() %>%
		addCircleMarkers(
		lng = ~Long,
		lat = ~Lat,
		label = ~rownames(data),
		labelOptions = labelOptions(noHide = TRUE)
		)
	return(map)
}

map_lat <- leaflet_map(c_lat)
map_yac <- leaflet_map(c_yac)

c_lat <- c_lat[-c(which(rownames(c_lat) %in% c(37, 48, 50, 54, 889, 948, 1264))), ]
c_yac <- c_yac[-c(which(rownames(c_yac) %in% c(32, 33, 42, 132, 413, 2049, 2113, 2217, 2238, 2255, 2930, 3077))), ]

map_l <- leaflet_map(c_lat)
map_y <- leaflet_map(c_yac)

# ------------------------------------------------------------------------------------------------ #

# Cleaning statistical outiers #

# ------------------------------------------------------------------------------------------------ #

caiman_data <- list()
caiman_data[[1]] <- c_lat
caiman_data[[2]] <- c_yac
spp <- c("c_lat.csv", "c_yac.csv")

for(i in 1:length(caiman_data)){
occ_1 <- caiman_data[[i]]
dups <- rep(0, nrow(occ_1))
occ_e <- raster::extract(stack, occ_1[, 2:3], df = TRUE)
full <- occ_1
	for (j in 2:13) {
		outliers <- outliers(occ_e$ID, occ_1$Spp, dups, occ_e[, j])
		full <- cbind(full, outliers)
	}
column_indices <- 4:27
final <- full
	for (k in column_indices) {
		final <- final[!(final[, k] == 1), ]
	}
setwd("D:/3_Caiman/1_occ/3_clean_joint")
write.csv(final[, 1:3], spp[[i]], row.names = F)
}

# ------------------------------------------------------------------------------------------------ #

### EndNotRun
