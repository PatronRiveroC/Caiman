
# ------------------------------------------------------------------------------------------------ #

### Title: Minimum volume ellipsoid models ####
### Author: Patron-Rivero, C. ####
### Date: 06/25/2023 ###
### Project: 'Hybridization proccess of Caiman latirostris and C. yucare' ###

# ------------------------------------------------------------------------------------------------ #

# Packages #

# ------------------------------------------------------------------------------------------------ #

library(raster)
library(ntbox)
library(rgl)
library(png)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(grid)
library(patchwork)

# ------------------------------------------------------------------------------------------------ #

# Inputs #

# ------------------------------------------------------------------------------------------------ #

load_data <- function(csv_dir, tif_dir) {
	csv_data <- read.csv(csv_dir, header = TRUE, sep = ",", row.names = NULL)
	tif_files <- list.files(tif_dir, pattern = ".tif", full.names = TRUE)
	raster_stack <- stack(tif_files)
	extracted_data <- extract(raster_stack, cbind(csv_data$Long, csv_data$Lat), df = TRUE)
	extracted_data <- extracted_data[, -1]
	colnames(extracted_data) <- gsub("^c_lat_|^c_yac_", "", colnames(extracted_data))
	return(extracted_data)
}

ext_data <- function(csv_dir, env_data) {
	csv_data <- read.csv(csv_dir, header = TRUE, sep = ",", row.names = NULL)
	extracted_data <- extract(env_data, cbind(csv_data$Long, csv_data$Lat), df = TRUE)
	extracted_data <- extracted_data[, -1]
	colnames(extracted_data) <- gsub("^c_lat_|^c_yac_", "", colnames(extracted_data))
	return(extracted_data)
}

c_lat_train <- load_data("D:/3_Caiman/1_occ/3_clean_joint/c_lat.csv", "D:/3_Caiman/2_env/4_calib/C_lat")
c_lat_test <- load_data("D:/3_Caiman/1_occ/2_eval_genetic/C_lat.csv", "D:/3_Caiman/2_env/4_calib/C_lat")
c_yac_train <- load_data("D:/3_Caiman/1_occ/3_clean_joint/c_yac.csv", "D:/3_Caiman/2_env/4_calib/C_yac")
c_yac_test <- load_data("D:/3_Caiman/1_occ/2_eval_genetic/C_yac.csv", "D:/3_Caiman/2_env/4_calib/C_yac")

r_l <- raster("D:/3_Caiman/2_env/3_eco/C_lat/c_lat_bio_1.tif")
r_y <- raster("D:/3_Caiman/2_env/3_eco/C_yac/c_yac_bio_1.tif")
r_merge <- merge(r_l, r_y)
env <- list.files("D:/3_Caiman/2_env/2_america", pattern = ".tif", full.names = TRUE)
env <- stack(env)
env_proj <- mask(crop(env, r_merge), r_merge)

env_bg_lat <- ext_data("D:/3_Caiman/2_env/5_bias/bg_lat.csv", env_proj)
env_bg_yac <- ext_data("D:/3_Caiman/2_env/5_bias/bg_yac.csv", env_proj)

# ------------------------------------------------------------------------------------------------ #

# Non-correlated varible sets #

# ------------------------------------------------------------------------------------------------ #

convert_rows_to_vectors <- function(data) {
	num_rows <- nrow(data)
	num_cols <- ncol(data)
	sets <- vector("list", num_rows)
  	for (i in 1:num_rows) {
		row_vector <- as.character(data[i, ])
		row_vector <- row_vector[!is.na(row_vector)]
		row_vector <- gsub("'", "", row_vector)
		row_vector <- gsub("^c_lat_|^c_yac_", "", row_vector)
		sets[[i]] <- row_vector
	}
  	sets
}

set_lat <- read.csv("D:/3_Caiman/2_env/c_lat_var_set.csv", header = TRUE, sep = ",", row.names = NULL)
set_yac <- read.csv("D:/3_Caiman/2_env/c_yac_var_set.csv", header = TRUE, sep = ",", row.names = NULL)
colnames(set_lat) <- NULL
colnames(set_yac) <- NULL
set_lat <- convert_rows_to_vectors(set_lat)
set_yac <- convert_rows_to_vectors(set_yac)

# ------------------------------------------------------------------------------------------------ #

# Ellipsoid candidate models #

# ------------------------------------------------------------------------------------------------ #

set.seed(20)

results <- list()
for (i in 1:length(set_lat)) {
	nvarstest <- length(set_lat[[i]])
    level <- 0.95
    env_bg_lat_subset <- env_bg_lat[, set_lat[[i]]]
    omr_criteria <- 0.10
    proc <- TRUE
    eval <- c("om_rate_test", "env_bg_paucratio")
		output <- capture.output({
			result <- ntbox::ellipsoid_selection(env_train = c_lat_train,
				env_test = c_lat_test,
				env_vars = set_lat[[i]],
				level = level,
				nvarstest = nvarstest,
				env_bg = env_bg_lat_subset,
				omr_criteria = omr_criteria,
				proc = proc,
				rseed = TRUE)
		})
    results[[i]] <- result[, eval]
}

combined_results <- do.call(rbind, results)
rownames(combined_results) <- NULL
combined_results$ID <- 1:nrow(combined_results)
c_lat <- combined_results
c_lat <- c_lat[order(-c_lat$om_rate_test, c_lat$env_bg_paucratio, decreasing = TRUE), ]
c_lat <- c_lat[, c(3, 1, 2)]
write.csv(c_lat, "D:/3_Caiman/4_eval/1_ellip/c_lat.csv", row.names = FALSE)

results <- list()
for (i in 1:length(set_yac)) {
	nvarstest <- length(set_yac[[i]])
    level <- 0.95
    env_bg_yac_subset <- env_bg_yac[, set_yac[[i]]]
    omr_criteria <- 0.10
    proc <- TRUE
    eval <- c("om_rate_test", "env_bg_paucratio")
		output <- capture.output({
			result <- ntbox::ellipsoid_selection(env_train = c_yac_train,
				env_test = c_yac_test,
				env_vars = set_yac[[i]],
				level = level,
				nvarstest = nvarstest,
				env_bg = env_bg_yac_subset,
				omr_criteria = omr_criteria,
				proc = proc,
				rseed = TRUE)
		})
    results[[i]] <- result[, eval]
}

combined_results <- do.call(rbind, results)
rownames(combined_results) <- NULL
combined_results$ID <- 1:nrow(combined_results)
c_yac <- combined_results
c_yac <- c_yac[order(-c_yac$om_rate_test, c_yac$env_bg_paucratio, decreasing = TRUE), ]
c_yac <- c_yac[, c(3, 1, 2)]
write.csv(c_yac, "D:/3_Caiman/4_eval/1_ellip/c_yac.csv", row.names = FALSE)

# ------------------------------------------------------------------------------------------------ #

# Ellipsoid Final model #

# ------------------------------------------------------------------------------------------------ #

memory.limit(size = 1000000)

bestvarcomb <- set_lat[[c_lat$ID[1]]]
all_lat <- rbind(c_lat_train, c_lat_test)
best_mod <- ntbox::cov_center(all_lat[, bestvarcomb], mve = TRUE, level = 0.95, vars = 1:length(bestvarcomb))
mProj <- ntbox::ellipsoidfit(env_proj[[bestvarcomb]], centroid = best_mod$centroid, covar = best_mod$covariance, level = 0.95, size = 3, plot = TRUE)

writeRaster(mProj$suitRaster, filename = "D:/3_Caiman/4_eval/1_ellip/c_lat_suit.tif", format = "GTiff", overwrite = TRUE)
occ_lat <- read.csv("D:/3_Caiman/1_occ/3_clean_joint/c_lat.csv", header = TRUE, sep = ",", row.names = NULL)
bin <- ntbox::bin_model(model = mProj$suitRaster, occs = occ_lat[, 2:3], percent = 10)
writeRaster(bin, filename = "D:/3_Caiman/4_eval/1_ellip/c_lat_bin.tif", format = "GTiff", overwrite = TRUE)

bestvarcomb <- set_yac[[c_yac$ID[1]]]
all_yac <- rbind(c_yac_train, c_yac_test)
best_mod <- ntbox::cov_center(all_yac[, bestvarcomb], mve = TRUE, level = 0.95, vars = 1:length(bestvarcomb))
mProj <- ntbox::ellipsoidfit(env_proj[[bestvarcomb]], centroid = best_mod$centroid, covar = best_mod$covariance, level = 0.95, size = 3, plot = TRUE)

writeRaster(mProj$suitRaster, filename = "D:/3_Caiman/4_eval/1_ellip/c_yac_suit.tif", format = "GTiff", overwrite = TRUE)
occ_yac <- read.csv("D:/3_Caiman/1_occ/3_clean_joint/c_yac.csv", header = TRUE, sep = ",", row.names = NULL)
bin <- ntbox::bin_model(model = mProj$suitRaster, occs = occ_yac[, 2:3], percent = 10)
writeRaster(bin, filename = "D:/3_Caiman/4_eval/1_ellip/c_yac_bin.tif", format = "GTiff", overwrite = TRUE)

# ------------------------------------------------------------------------------------------------ #

# Ellipsoid plotting #

# ------------------------------------------------------------------------------------------------ #

set.seed(20)
memory.limit(size = 1000000)

bestvarcomb <- set_lat[[2]]
all_lat <- rbind(c_lat_train, c_lat_test)
best_mod <- ntbox::cov_center(all_lat[, bestvarcomb], mve = TRUE, level = 0.95, vars = 1:length(bestvarcomb))

lat <- rgl::ellipse3d(best_mod$covariance, centre = best_mod$centroid, level = 0.95)
rgl::plot3d(lat, col = "white", alpha = 0.1)
rgl::plot3d(lat, col = "red", alpha = 0.3, add = TRUE, lit = FALSE, type = "wire")
rgl::points3d(env_bg_lat[, bestvarcomb], col = "grey")
rgl::points3d(c_lat_train[, bestvarcomb], col = "black")
rgl::points3d(c_lat_test[, bestvarcomb], col = "red", size = 5)
setwd("D:/3_Caiman/4_eval/1_ellip")
#rgl.postscript("C_lat.pdf", fmt = "pdf")
rgl.snapshot("C_lat.png", fmt = "png", top = TRUE)

bestvarcomb <- set_yac[[2]]
all_yac <- rbind(c_yac_train, c_yac_test)
best_mod <- ntbox::cov_center(all_yac[, bestvarcomb], mve = TRUE, level = 0.95, vars = 1:length(bestvarcomb))

yac <- rgl::ellipse3d(best_mod$covariance, centre = best_mod$centroid, level = 0.95)
rgl::plot3d(yac, col = "white", alpha = 0.1)
rgl::plot3d(yac, col = "red", alpha = 0.3, add = TRUE, lit = FALSE, type = "wire")
rgl::points3d(env_bg_yac[, bestvarcomb], col = "grey")
rgl::points3d(c_yac_train[, bestvarcomb], col = "black")
rgl::points3d(c_yac_test[, bestvarcomb], col = "red", size = 5)
setwd("D:/3_Caiman/4_eval/1_ellip")
#rgl.postscript("C_yac.pdf", fmt = "pdf")
rgl.snapshot("C_yac.png", fmt = "png", top = TRUE)

# ------------------------------------------------------------------------------------------------ #

# Final plot #

# ------------------------------------------------------------------------------------------------ #

r_suit <- function(raster_data, colors = c("white", "#e31a1c")) {
	points_df <- as.data.frame(rasterToPoints(raster_data))
	colnames(points_df) <- c("x", "y", "layer")
	world_map <- map_data("world")
	extent <- extent(raster_data)

	p <- ggplot() +
		geom_raster(data = points_df, aes(x = x, y = y, fill = layer)) +
		scale_fill_gradient(low = colors[1], high = colors[2], name = NULL,
        guide = "none") +
		geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "transparent", color = "black", size = 0.1) +
		theme_bw() +
		theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank(), plot.margin = margin(0, 0, 0, 0),
			plot.background = element_rect(fill = "transparent", color = NA)) +
		coord_sf(xlim = c(extent[1], extent[2]), ylim = c(extent[3], extent[4]))
		return(p)
}

r_bin <- function(raster_data, colors = c("white", "#fc9272")) {
	points_df <- as.data.frame(rasterToPoints(raster_data))
	colnames(points_df) <- c("x", "y", "layer")
	world_map <- map_data("world")
	extent <- extent(raster_data)

	p <- ggplot() +
		geom_raster(data = points_df, aes(x = x, y = y, fill = layer)) +
		scale_fill_gradient(low = colors[1], high = colors[2], name = NULL, guide = "none") +
		geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "transparent", color = "black", size = 0.1) +
    theme_bw() +
    theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank(), plot.margin = margin(0, 0, 0, 0),
		plot.background = element_rect(fill = "transparent", color = NA)) +
    coord_sf(xlim = c(extent[1], extent[2]), ylim = c(extent[3], extent[4]))
    return(p)
}

suit_l <- raster("D:/3_Caiman/4_eval/1_ellip/c_lat_suit.tif")
bin_l <- raster("D:/3_Caiman/4_eval/1_ellip/c_lat_bin.tif")
l_train <- read.csv("D:/3_Caiman/1_occ/3_clean_joint/c_lat.csv", header = TRUE, sep = ",", row.names = NULL)
l_test <- read.csv("D:/3_Caiman/1_occ/2_eval_genetic/C_lat.csv", header = TRUE, sep = ",", row.names = NULL)

a <- r_suit(suit_l)
a <- a + geom_point(data = l_train, aes(x = Long, y = Lat), colour = "#000000", size = 0.5, shape = 1)
a <- a + geom_point(data = l_test, aes(x = Long, y = Lat),  colour = "#e31a1c", size = 0.5, shape = 4)

b <- r_bin(bin_l)
b <- b + geom_point(data = l_train, aes(x = Long, y = Lat), colour = "#000000", size = 0.5, shape = 1)
b <- b + geom_point(data = l_test, aes(x = Long, y = Lat), colour = "#e31a1c", size = 0.5, shape = 4)

img <- readPNG("D:/3_Caiman/4_eval/1_ellip/c_lat.png")
grob <- rasterGrob(img, interpolate = TRUE)
p <- ggplot() + xlim(0, 1) + ylim(0, 1) + theme_void()
p <- p + annotation_custom(grob, xmin = 0, xmax = 1, ymin = 0, ymax = 1)

a <- a + theme(plot.margin = margin(0, 10, 0, 10))
p <- p + theme(plot.margin = margin(0, 10, 0, 10))
b <- b + theme(plot.margin = margin(0, 10, 0, 10))

c <- a + p + b 

p1 <- c + plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold")), 
			tag_levels = 'A', tag_suffix = ')')
			
setwd("D:/3_Caiman/5_Figures")
ggsave(file = "c_lat.tiff", plot = p1, width = 30, height = 10, dpi = 300, units = "cm", device = "tiff")
ggsave(file = "c_lat.pdf", plot = p1, width = 30, height = 10, dpi = 600, units = "cm", device = "pdf")

suit_y <- raster("D:/3_Caiman/4_eval/1_ellip/c_yac_suit.tif")
bin_y <- raster("D:/3_Caiman/4_eval/1_ellip/c_yac_bin.tif")
y_train <- read.csv("D:/3_Caiman/1_occ/3_clean_joint/c_yac.csv", header = TRUE, sep = ",", row.names = NULL)
y_test <- read.csv("D:/3_Caiman/1_occ/2_eval_genetic/C_yac.csv", header = TRUE, sep = ",", row.names = NULL)

a <- r_suit(suit_y)
b <- r_bin(bin_y)

a <- a + geom_point(data = y_train, aes(x = Long, y = Lat), colour = "#000000", size = 0.5, shape = 1)
a <- a + geom_point(data = y_test, aes(x = Long, y = Lat),  colour = "#e31a1c", size = 0.5, shape = 4)
b <- b + geom_point(data = y_train, aes(x = Long, y = Lat), colour = "#000000", size = 0.5, shape = 1)
b <- b + geom_point(data = y_test, aes(x = Long, y = Lat), colour = "#e31a1c", size = 0.5, shape = 4)

img <- readPNG("D:/3_Caiman/4_eval/1_ellip/c_yac.png")
grob <- rasterGrob(img, interpolate = TRUE)
p <- ggplot() + xlim(0, 1) + ylim(0, 1) + theme_void()
p <- p + annotation_custom(grob, xmin = 0, xmax = 1, ymin = 0, ymax = 1)

a <- a + theme(plot.margin = margin(0, 10, 0, 10))
p <- p + theme(plot.margin = margin(0, 10, 0, 10))
b <- b + theme(plot.margin = margin(0, 10, 0, 10))
c <- a + p + b 

p1 <- c + plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold")), 
			tag_levels = 'A', tag_suffix = ')')
			
setwd("D:/3_Caiman/5_Figures")
ggsave(file = "c_yac.tiff", plot = p1, width = 30, height = 10, dpi = 300, units = "cm", device = "tiff")
ggsave(file = "c_yac.pdf", plot = p1, width = 30, height = 10, dpi = 600, units = "cm", device = "pdf")

# ------------------------------------------------------------------------------------------------ #

### EndNotRun
