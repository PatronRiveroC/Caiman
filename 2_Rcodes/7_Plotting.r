
# ------------------------------------------------------------------------------------------------ #

### Title: Final plot ####
### Author: Patron-Rivero, C. ####
### Date: 06/29/2023 ###
### Project: "Hybridization patterns, ecological niches, and conservation implications in Caiman yacare and
### 			Caiman latirostris: Insights from Phylogeographic Analysis and Ecological Niche Modeling" ###

# ------------------------------------------------------------------------------------------------ #

# Packages #

# ------------------------------------------------------------------------------------------------ #

library(raster)
library(png)
library(ggplot2)
library(rnaturalearth)
library(viridis)
library(grid)
library(patchwork)

# ------------------------------------------------------------------------------------------------ #

# Inputs #

# ------------------------------------------------------------------------------------------------ #

eco_lat <- raster("D:/3_Caiman/2_env/3_eco/C_lat/c_lat_bio_1.tif")
eco_yac <- raster("D:/3_Caiman/2_env/3_eco/C_yac/c_yac_bio_1.tif")
e_lat <- raster("D:/3_Caiman/4_eval/1_ellip/c_lat_bin.tif")
e_yac <- raster("D:/3_Caiman/4_eval/1_ellip/c_yac_bin.tif")
m_lat <- raster("D:/3_Caiman/4_eval/2_max/c_lat_bin_max.tif")
m_yac <- raster("D:/3_Caiman/4_eval/2_max/c_yac_bin_max.tif")
c_lat_train <- read.csv("D:/3_Caiman/4_eval/2_max/c_lat/c_lat_train.csv",  header = TRUE, sep = ",", row.names = NULL)
c_yac_train <- read.csv("D:/3_Caiman/4_eval/2_max/c_yac/c_yac_train.csv",  header = TRUE, sep = ",", row.names = NULL)
c_lat_test <- read.csv("D:/3_Caiman/4_eval/2_max/c_lat/c_lat_test.csv",  header = TRUE, sep = ",", row.names = NULL)
c_yac_test <- read.csv("D:/3_Caiman/4_eval/2_max/c_yac/c_yac_test.csv",  header = TRUE, sep = ",", row.names = NULL)

# ------------------------------------------------------------------------------------------------ #

# Functions #

# ------------------------------------------------------------------------------------------------ #

zero <- e_lat * 0
eco_lat <- (eco_lat * 0) + 1
eco_yac <- (eco_yac * 0) + 2
eco_lat <- merge(eco_lat, zero)
eco_yac <- merge(eco_yac, zero)
M <- eco_lat + eco_yac

e_lat <- e_lat * 10
e_yac <- e_yac * 10
m_lat <- m_lat * 10
m_yac <- m_yac * 10

e_lat <- e_lat + M
e_yac <- e_yac + M
m_lat <- m_lat + M
m_yac <- m_yac + M

reclass_rules <- cbind(c(1, 2, 3), c(NA))
e_lat <- reclassify(e_lat, reclass_rules)
e_yac <- reclassify(e_yac, reclass_rules)
m_lat <- reclassify(m_lat, reclass_rules)
m_yac <- reclassify(m_yac, reclass_rules)

reclass_rules <- cbind(c(12), c(14))
e_lat <- reclassify(e_lat, reclass_rules)
e_yac <- reclassify(e_yac, reclass_rules)
m_lat <- reclassify(m_lat, reclass_rules)
m_yac <- reclassify(m_yac, reclass_rules)

bin_l <- function(raster_data) {
	points_df <- as.data.frame(rasterToPoints(raster_data))
	colnames(points_df) <- c("x", "y", "layer")
	world_map <- map_data("world")
	extent <- extent(raster_data)

	p <- ggplot() +
		geom_raster(data = points_df, aes(x = x, y = y, fill = factor(layer))) +
		scale_fill_viridis_d(name = NULL, guide = "none") +
		geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "transparent", color = "black", size = 0.1) +
		theme_bw() +
		theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank(), plot.margin = margin(0, 0, 0, 0),
			plot.background = element_rect(fill = "transparent", color = NA)) +
		coord_sf(xlim = c(extent[1], extent[2]), ylim = c(extent[3], extent[4]))

  return(p)
}

bin_r <- function(raster_data) {
	points_df <- as.data.frame(rasterToPoints(raster_data))
	colnames(points_df) <- c("x", "y", "layer")
	world_map <- map_data("world")
	extent <- extent(raster_data)

	labels <- c("C. lat's M", "M's overlap", "C. yac's M")
  
	p <- ggplot() +
		geom_raster(data = points_df, aes(x = x, y = y, fill = factor(layer))) +
		scale_fill_viridis_d(name = NULL, breaks = c(11, 13, 14), labels = labels, guide = "legend") +
		geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "transparent", color = "black", size = 0.1) +
		theme_bw() +
		theme(legend.position = c(0.98, 0.01), legend.justification = c(1, 0), axis.title.x = element_blank(), axis.title.y = element_blank(),
			plot.margin = margin(0, 0, 0, 0), plot.background = element_rect(fill = "transparent", color = NA)) +
		coord_sf(xlim = c(extent[1], extent[2]), ylim = c(extent[3], extent[4]))

  return(p)
}

# ------------------------------------------------------------------------------------------------ #

# Final plot #

# ------------------------------------------------------------------------------------------------ #

img <- readPNG("D:/3_Caiman/4_eval/1_ellip/c_lat.png")
grob <- rasterGrob(img, interpolate = TRUE)
p <- ggplot() + xlim(0, 1) + ylim(0, 1) + theme_void()
p <- p + annotation_custom(grob, xmin = 0, xmax = 1, ymin = 0, ymax = 1)

a <- bin_l(m_lat)
a <- a + geom_point(data = c_lat_train, aes(x = Long, y = Lat), colour = "#000000", size = 0.5, shape = 1)
a <- a + geom_point(data = c_lat_test, aes(x = Long, y = Lat),  colour = "#e31a1c", size = 0.5, shape = 4)

b <- bin_l(e_lat)
b <- b + geom_point(data = c_lat_train, aes(x = Long, y = Lat), colour = "#000000", size = 0.5, shape = 1)
b <- b + geom_point(data = c_lat_test, aes(x = Long, y = Lat), colour = "#e31a1c", size = 0.5, shape = 4)

a <- a + theme(plot.margin = margin(0, 10, 0, 10))
b <- b + theme(plot.margin = margin(0, 0, 0, 0))

c <- p + a + b

img <- readPNG("D:/3_Caiman/4_eval/1_ellip/c_yac.png")
grob <- rasterGrob(img, interpolate = TRUE)
p <- ggplot() + xlim(0, 1) + ylim(0, 1) + theme_void()
p <- p + annotation_custom(grob, xmin = 0, xmax = 1, ymin = 0, ymax = 1)

a <- bin_l(m_yac)
a <- a + geom_point(data = c_yac_train, aes(x = Long, y = Lat), colour = "#000000", size = 0.5, shape = 1)
a <- a + geom_point(data = c_yac_test, aes(x = Long, y = Lat),  colour = "#e31a1c", size = 0.5, shape = 4)

b <- bin_r(e_yac)
b <- b + geom_point(data = c_yac_train, aes(x = Long, y = Lat), colour = "#000000", size = 0.5, shape = 1)
b <- b + geom_point(data = c_yac_test, aes(x = Long, y = Lat), colour = "#e31a1c", size = 0.5, shape = 4)

a <- a + theme(plot.margin = margin(0, 10, 0, 10))
b <- b + theme(plot.margin = margin(0, 0, 0, 0))

d <- p + a + b

f <- c / d

p1 <- f + plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold")), 
			tag_levels = 'A', tag_suffix = ')')
			
setwd("D:/3_Caiman/5_Figures")
ggsave(file = "Fig1.tiff", plot = p1, width = 30, height = 20, dpi = 300, units = "cm", device = "tiff")
ggsave(file = "Fig1.pdf", plot = p1, width = 30, height = 20, dpi = 600, units = "cm", device = "pdf")

# ------------------------------------------------------------------------------------------------ #

# Suitability plot for SM #

# ------------------------------------------------------------------------------------------------ #

e_lat <- raster("D:/3_Caiman/4_eval/1_ellip/c_lat_suit.tif")
e_yac <- raster("D:/3_Caiman/4_eval/1_ellip/c_yac_suit.tif")
m_lat <- raster("D:/3_Caiman/4_eval/2_max/C_lat_median.asc")
m_yac <- raster("D:/3_Caiman/4_eval/2_max/C_yac_median.asc")

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

a <- r_suit(e_lat)
a <- a + geom_point(data = c_lat_train, aes(x = Long, y = Lat), colour = "#000000", size = 0.5, shape = 1)
a <- a + geom_point(data = c_lat_test, aes(x = Long, y = Lat),  colour = "#e31a1c", size = 0.5, shape = 4)

b <- r_suit(m_lat)
b <- b + geom_point(data = c_lat_train, aes(x = Long, y = Lat), colour = "#000000", size = 0.5, shape = 1)
b <- b + geom_point(data = c_lat_test, aes(x = Long, y = Lat),  colour = "#e31a1c", size = 0.5, shape = 4)

c <- r_suit(e_yac)
c <- c + geom_point(data = c_yac_train, aes(x = Long, y = Lat), colour = "#000000", size = 0.5, shape = 1)
c <- c + geom_point(data = c_yac_test, aes(x = Long, y = Lat), colour = "#e31a1c", size = 0.5, shape = 4)

d <- r_suit(m_yac)
d <- d + geom_point(data = c_yac_train, aes(x = Long, y = Lat), colour = "#000000", size = 0.5, shape = 1)
d <- d + geom_point(data = c_yac_test, aes(x = Long, y = Lat), colour = "#e31a1c", size = 0.5, shape = 4)

f <- (a + b) / (c + d)

p1 <- f + plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold")), 
			tag_levels = 'A', tag_suffix = ')')
			
setwd("D:/3_Caiman/5_Figures")
ggsave(file = "FigureSM1.tiff", plot = p1, width = 20, height = 20, dpi = 300, units = "cm", device = "tiff")
ggsave(file = "FIgureSM1.pdf", plot = p1, width = 20, height = 20, dpi = 600, units = "cm", device = "pdf")

# ------------------------------------------------------------------------------------------------ #

### EndNotRun
