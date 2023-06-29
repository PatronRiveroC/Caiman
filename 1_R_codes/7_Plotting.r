
# ------------------------------------------------------------------------------------------------ #

### Title: Final plot ####
### Author: Patron-Rivero, C. ####
### Date: 06/29/2023 ###
### Project: 'Hybridization proccess of Caiman latirostris and C. yucare' ###

# ------------------------------------------------------------------------------------------------ #

# Packages #

# ------------------------------------------------------------------------------------------------ #

library(raster)
library(png)
library(ggplot2)
library(rnaturalearth)
library(grid)
library(patchwork)

# ------------------------------------------------------------------------------------------------ #

# Inputs #

# ------------------------------------------------------------------------------------------------ #

e_lat_bin <- raster("D:/3_Caiman/4_eval/1_ellip/c_lat_bin.tif")
e_yac_bin <- raster("D:/3_Caiman/4_eval/1_ellip/c_yac_bin.tif")
m_lat_bin <- raster("D:/3_Caiman/4_eval/2_max/c_lat_bin_max.tif")
m_yac_bin <- raster("D:/3_Caiman/4_eval/2_max/c_yac_bin_max.tif")
c_lat_train <- read.csv("D:/3_Caiman/4_eval/2_max/c_lat/c_lat_train.csv",  header = TRUE, sep = ",", row.names = NULL)
c_yac_train <- read.csv("D:/3_Caiman/4_eval/2_max/c_yac/c_yac_train.csv",  header = TRUE, sep = ",", row.names = NULL)
c_lat_test <- read.csv("D:/3_Caiman/4_eval/2_max/c_lat/c_lat_test.csv",  header = TRUE, sep = ",", row.names = NULL)
c_yac_test <- read.csv("D:/3_Caiman/4_eval/2_max/c_yac/c_yac_test.csv",  header = TRUE, sep = ",", row.names = NULL)

# ------------------------------------------------------------------------------------------------ #

# Functions #

# ------------------------------------------------------------------------------------------------ #

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

# ------------------------------------------------------------------------------------------------ #

# Individual plots #

# ------------------------------------------------------------------------------------------------ #

img <- readPNG("D:/3_Caiman/4_eval/1_ellip/c_lat.png")
grob <- rasterGrob(img, interpolate = TRUE)
p <- ggplot() + xlim(0, 1) + ylim(0, 1) + theme_void()
p <- p + annotation_custom(grob, xmin = 0, xmax = 1, ymin = 0, ymax = 1)

a <- r_bin(e_lat_bin)
a <- a + geom_point(data = c_lat_train, aes(x = Long, y = Lat), colour = "#000000", size = 0.5, shape = 1)
a <- a + geom_point(data = c_lat_test, aes(x = Long, y = Lat),  colour = "#e31a1c", size = 0.5, shape = 4)

b <- r_bin(m_lat_bin)
b <- b + geom_point(data = c_lat_train, aes(x = Long, y = Lat), colour = "#000000", size = 0.5, shape = 1)
b <- b + geom_point(data = c_lat_test, aes(x = Long, y = Lat), colour = "#e31a1c", size = 0.5, shape = 4)

a <- a + theme(plot.margin = margin(0, 10, 0, 10))
b <- b + theme(plot.margin = margin(0, 0, 0, 0))

c <- p + b + a 

p1 <- c + plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold")), 
			tag_levels = 'A', tag_suffix = ')')
			
setwd("D:/3_Caiman/5_Figures")
ggsave(file = "Fig_2.tiff", plot = p1, width = 30, height = 10, dpi = 300, units = "cm", device = "tiff")
ggsave(file = "Fig_2.pdf", plot = p1, width = 30, height = 10, dpi = 600, units = "cm", device = "pdf")

img <- readPNG("D:/3_Caiman/4_eval/1_ellip/c_yac.png")
grob <- rasterGrob(img, interpolate = TRUE)
p <- ggplot() + xlim(0, 1) + ylim(0, 1) + theme_void()
p <- p + annotation_custom(grob, xmin = 0, xmax = 1, ymin = 0, ymax = 1)

a <- r_bin(e_yac_bin)
a <- a + geom_point(data = c_yac_train, aes(x = Long, y = Lat), colour = "#000000", size = 0.5, shape = 1)
a <- a + geom_point(data = c_yac_test, aes(x = Long, y = Lat),  colour = "#e31a1c", size = 0.5, shape = 4)

b <- r_bin(m_yac_bin)
b <- b + geom_point(data = c_yac_train, aes(x = Long, y = Lat), colour = "#000000", size = 0.5, shape = 1)
b <- b + geom_point(data = c_yac_test, aes(x = Long, y = Lat), colour = "#e31a1c", size = 0.5, shape = 4)

a <- a + theme(plot.margin = margin(0, 10, 0, 10))
b <- b + theme(plot.margin = margin(0, 0, 0, 0))

d <- p + b + a 

p1 <- d + plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold")), 
			tag_levels = 'A', tag_suffix = ')')
			
setwd("D:/3_Caiman/5_Figures")
ggsave(file = "Fig_3.tiff", plot = p1, width = 30, height = 10, dpi = 300, units = "cm", device = "tiff")
ggsave(file = "Fig_3.pdf", plot = p1, width = 30, height = 10, dpi = 600, units = "cm", device = "pdf")

# ------------------------------------------------------------------------------------------------ #

# Final plot #

# ------------------------------------------------------------------------------------------------ #

f <- c / d

p1 <- f + plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold")), 
			tag_levels = 'A', tag_suffix = ')')
			
setwd("D:/3_Caiman/5_Figures")
ggsave(file = "Fig_1.tiff", plot = p1, width = 30, height = 20, dpi = 300, units = "cm", device = "tiff")
ggsave(file = "Fig_1.pdf", plot = p1, width = 30, height = 20, dpi = 600, units = "cm", device = "pdf")

# ------------------------------------------------------------------------------------------------ #

### EndNotRun
