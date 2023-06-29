
# ------------------------------------------------------------------------------------------------ #

### Title: Variable selection ####
### Author: Patron-Rivero, C. ####
### Date: 06/24/2023 ###
### Project: 'Hybridization proccess of Caiman latirostris and C. yucare' ###

# ------------------------------------------------------------------------------------------------ #

# Packages #

# ------------------------------------------------------------------------------------------------ #

# library(devtools)
# install_github("PatronRiveroC/VariablePermutation")
library(VariablePermutation)
library(raster)
library(ntbox)

# ------------------------------------------------------------------------------------------------ #

# Inputs #

# ------------------------------------------------------------------------------------------------ #

load_data <- function(csv_dir, tif_dir) {
	csv_data <- read.csv(csv_dir, header = TRUE, sep = ",", row.names = NULL)
	tif_files <- list.files(tif_dir, pattern = ".tif", full.names = TRUE)
	raster_stack <- stack(tif_files)
	extracted_data <- extract(raster_stack, cbind(csv_data$Long, csv_data$Lat), df = TRUE)
	return(extracted_data)
}

env_l <- load_data("D:/3_Caiman/1_occ/3_clean_joint/c_lat.csv", "D:/3_Caiman/2_env/4_calib/C_lat")
env_y <- load_data("D:/3_Caiman/1_occ/3_clean_joint/c_yac.csv", "D:/3_Caiman/2_env/4_calib/C_yac")

env_l_1 <- env_l[, -c(1, 11, 13)]
env_l_2 <- env_l[, -c(1, 11, 12)]
env_l_3 <- env_l[, -c(1, 10, 13)]
env_l_4 <- env_l[, -c(1, 10, 12)]

env_y_1 <- env_y[, -c(1, 11, 13)]
env_y_2 <- env_y[, -c(1, 11, 12)]
env_y_3 <- env_y[, -c(1, 10, 13)]
env_y_4 <- env_y[, -c(1, 10, 12)]

# ------------------------------------------------------------------------------------------------ #

# Correlation process #

# ------------------------------------------------------------------------------------------------ #

c_lat_1 <- variable_permutation(10, data = env_l_1)
c_lat_2 <- variable_permutation(10, data = env_l_2)
c_lat_3 <- variable_permutation(10, data = env_l_3)
c_lat_4 <- variable_permutation(10, data = env_l_4)

c_lat <- rbind(c_lat_1, c_lat_2, c_lat_3, c_lat_4)
c_lat <- unique(c_lat)
write.csv(c_lat, "D:/3_Caiman/2_env/c_lat_var_set.csv", row.names = FALSE)

c_yac_1 <- variable_permutation(10, data = env_y_1)
c_yac_2 <- variable_permutation(10, data = env_y_2)
c_yac_3 <- variable_permutation(10, data = env_y_3)
c_yac_4 <- variable_permutation(10, data = env_y_4)

c_yac <- rbind(c_yac_1, c_yac_2, c_yac_3, c_yac_4)
c_yac <- unique(c_yac)
write.csv(c_yac, "D:/3_Caiman/2_env/c_yac_var_set.csv", row.names = FALSE)

# ------------------------------------------------------------------------------------------------ #

### EndNotRun
