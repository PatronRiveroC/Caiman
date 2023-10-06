
# ------------------------------------------------------------------------------------------------ #

### Title: Sdm final plot ####
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
library(terra)
library(RColorBrewer)

# ------------------------------------------------------------------------------------------------ #

# Inputs #

# ------------------------------------------------------------------------------------------------ #

c_lat_train <- read.csv("D:/3_Caiman/6_maxent/c_lat/c_lat_train.csv")
c_lat_test <- read.csv("D:/3_Caiman/6_maxent/c_lat/c_lat_test.csv")
c_yac_train <- read.csv("D:/3_Caiman/6_maxent/c_yac/c_yac_train.csv")
c_yac_test <- read.csv("D:/3_Caiman/6_maxent/c_yac/c_yac_test.csv")

C_lat <- raster("D:/3_Caiman/6_maxent/c_lat_e/bin_final.tif")
C_lat_E <- raster("D:/3_Caiman/6_maxent/c_lat_e/E.tif")
C_lat_EC <- raster("D:/3_Caiman/6_maxent/c_lat_e/EC.tif")
C_lat_NE <- raster("D:/3_Caiman/6_maxent/c_lat_e/NE.tif")
C_yac <- raster("D:/3_Caiman/6_maxent/c_yac_e/bin_final.tif")
C_yac_E <- raster("D:/3_Caiman/6_maxent/c_yac_e/E.tif")
C_yac_EC <- raster("D:/3_Caiman/6_maxent/c_yac_e/EC.tif")
C_yac_NE <- raster("D:/3_Caiman/6_maxent/c_yac_e/NE.tif")

M_lat <- raster("D:/3_Caiman/6_maxent/c_lat_e/M_Var/set1/bio10.asc")
M_yac <- raster("D:/3_Caiman/6_maxent/c_lat_e/G_Var/set1/bio10.asc")

M_lat <- (M_lat * 0) + 10
M_yac <- (M_yac * 0)
Mlat <- merge(M_lat, M_yac)
M_yac <- M_yac + 20
M_lat <- (M_lat * 0)
Myac <- merge(M_yac, M_lat)
M <- Mlat + Myac
zero <- (M * 0)

lat_bin <- merge(C_lat, zero) * 100
lat_E <- ((merge(C_lat_E, zero)) + M) + lat_bin
lat_EC <- (merge(C_lat_EC, zero) + M) + lat_bin
lat_NE <- (merge(C_lat_NE, zero) + M) + lat_bin
yac_bin <- merge(C_yac, zero) * 100
yac_E <- ((merge(C_yac_E, zero)) + M) + yac_bin
yac_EC <- (merge(C_yac_EC, zero) + M) + yac_bin
yac_NE <- (merge(C_yac_NE, zero) + M) + yac_bin

# ------------------------------------------------------------------------------------------------ #

# Functions #

# ------------------------------------------------------------------------------------------------ #

bin_lat <- function(raster_data) {
	
	points_df <- as.data.frame(rasterToPoints(raster_data))
	colnames(points_df) <- c("x", "y", "layer")
	points_df$layer <- factor(round(points_df$layer))
	points_df$layer[points_df$layer %in% c(10, 20, 30)] <- NA
	points_df <- na.omit(points_df)
	world_map <- map_data("world")
	extent <- extent(raster_data)
	custom_order <- c("120", "21", "121", "131", "31", "130", "11", "110", "111")

	p <- ggplot() +
	
			geom_raster(data = points_df, aes(x = x, y = y, fill = factor(layer))) +
			
			scale_fill_manual(name = NULL, 
							  values = c("11" = "#440154", "21" = "#fde725", "31" = "#2c728e",
								   	     "110" = "#57106e", "120" = "#fcffa4", "130" = "#3b528b",
										 "111" = "#7e03a8", "121" = "#f0f921", "131" = "#21918c"),
							  breaks = custom_order) +
			
			geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "transparent", 
						 color = "black", linewidth = 0.1) +
			
			theme_bw() +
			
			theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank(),
				  plot.margin = margin(0, 0, 0, 0), plot.background = element_rect(fill = "transparent", color = NA)) +
			
			coord_sf(xlim = c(extent[1], extent[2]), ylim = c(extent[3], extent[4]))

	return(p)
}

bin_yac <- function(raster_data) {
	
	points_df <- as.data.frame(rasterToPoints(raster_data))
	colnames(points_df) <- c("x", "y", "layer")
	points_df$layer <- factor(round(points_df$layer))
	points_df$layer[points_df$layer %in% c(10, 20, 30)] <- NA
	points_df <- na.omit(points_df)
	world_map <- map_data("world")
	extent <- extent(raster_data)
	custom_order <- c("120", "21", "121", "131", "31", "130", "11", "110", "111")

	p <- ggplot() +
	
			geom_raster(data = points_df, aes(x = x, y = y, fill = factor(layer))) +
			
			scale_fill_manual(name = NULL, 
							  values = c("11" = "#57106e", "21" = "#fcffa4", "31" = "#2c728e",
										 "110" = "#440154", "120" = "#fde725", "130" = "#3b528b",
										 "111" = "#7e03a8", "121" = "#f0f921", "131" = "#21918c"),
							  breaks = custom_order) +
			
			geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "transparent", 
						 color = "black", linewidth = 0.1) +
			
			theme_bw() +
			
			theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank(),
				  plot.margin = margin(0, 0, 0, 0), plot.background = element_rect(fill = "transparent", color = NA)) +
			
			coord_sf(xlim = c(extent[1], extent[2]), ylim = c(extent[3], extent[4]))

	return(p)
}

bin_r_lat <- function(raster_data) {
  
	points_df <- as.data.frame(rasterToPoints(raster_data))
	colnames(points_df) <- c("x", "y", "layer")
	points_df$layer <- factor(round(points_df$layer))
	points_df$layer[points_df$layer %in% c(10, 20, 30)] <- NA
	points_df <- na.omit(points_df)
	world_map <- map_data("world")
	extent <- extent(raster_data)
	custom_order <- c("120", "21", "121", "131", "31", "130", "11", "110", "111")

	p <- ggplot() +
			geom_raster(data = points_df, aes(x = x, y = y, fill = factor(layer))) +
    
			scale_fill_manual(name = NULL,
							  values = c("11" = "#440154", "21" = "#fde725", "31" = "#2c728e",
										 "110" = "#57106e", "120" = "#fcffa4", "130" = "#3b528b",
										 "111" = "#7e03a8", "121" = "#f0f921", "131" = "#21918c"),
							  labels = c("11" = "C. lat (proj)", "21" = "C. yac (proj)",
										 "31" = "Both (proj)", "110" = "C. lat (sdm)",
										 "120" = "C. yac (sdm)", "130" = "Overlap (sdm)",
										 "111" = "C. lat (sdm and proj)", "122" = "C. yac (sdm and proj)", 
										 "131" = "Both (sdm and proj)"),
							  breaks = custom_order) +
							  
			geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "transparent", color = "black", linewidth = 0.1) +
    
			theme_bw() +
    
			theme(legend.position = c(0.98, 0.01), legend.justification = c(1, 0), axis.title.x = element_blank(), 
				  axis.title.y = element_blank(), plot.margin = margin(0, 0, 0, 0), plot.background = element_rect(fill = "transparent", 
				  color = NA), legend.key.size = unit(0.5, "lines")) +
    
			coord_sf(xlim = c(extent[1], extent[2]), ylim = c(extent[3], extent[4])) +
	
			theme(legend.background = element_blank()) +
	
			guides(fill = guide_legend(ncol = 1, keywidth = 0.5, keyheight = 0.5))
  
  return(p)
  
}

bin_r_yac <- function(raster_data) {
	
	points_df <- as.data.frame(rasterToPoints(raster_data))
	colnames(points_df) <- c("x", "y", "layer")
	points_df$layer <- factor(round(points_df$layer))
	points_df$layer[points_df$layer %in% c(10, 20, 30)] <- NA
	points_df <- na.omit(points_df)
	world_map <- map_data("world")
	extent <- extent(raster_data)
	custom_order <- c("120", "21", "121", "131", "31", "130", "11", "110", "111")

	p <- ggplot() +
	
			geom_raster(data = points_df, aes(x = x, y = y, fill = factor(layer))) +
    
			scale_fill_manual(name = NULL,
							  values = c("11" = "#57106e", "21" = "#fcffa4", "31" = "#2c728e",
										 "110" = "#440154", "120" = "#fde725", "130" = "#3b528b",
										 "111" = "#7e03a8", "121" = "#f0f921", "131" = "#21918c"),
							  labels = c("11" = "C. lat (proj)", "21" = "C. yac (proj)",
										 "31" = "Both (proj)", "110" = "C. lat (sdm)",
										 "120" = "C. yac (sdm)", "130" = "Overlap (sdm)",
										 "111" = "C. lat (sdm and proj)", "121" = "C. yac (sdm and proj)",
										 "131" = "Both (sdm and proj)"),
							  breaks = custom_order) +
							  
			geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "transparent", color = "black", linewidth = 0.1) +
    
			theme_bw() +
    
			theme(legend.position = c(0.98, 0.01), legend.justification = c(1, 0), axis.title.x = element_blank(), 
				  axis.title.y = element_blank(), plot.margin = margin(0, 0, 0, 0), plot.background = element_rect(fill = "transparent", 
				  color = NA), legend.key.size = unit(0.5, "lines")) +
    
			coord_sf(xlim = c(extent[1], extent[2]), ylim = c(extent[3], extent[4])) +
	
			theme(legend.background = element_blank()) +
	
			guides(fill = guide_legend(ncol = 1, keywidth = 0.5, keyheight = 0.5))
  
  return(p)

}

# ------------------------------------------------------------------------------------------------ #

# Final plot #

# ------------------------------------------------------------------------------------------------ #

a <- bin_lat(lat_NE)
b <- bin_lat(lat_E)
c <- bin_r_lat(lat_EC)
d <- bin_yac(yac_NE)
e <- bin_yac(yac_E)
f <- bin_r_yac(yac_EC)

a <- a + theme(plot.margin = margin(0, 10, 0, 10))
b <- b + theme(plot.margin = margin(0, 10, 0, 10))
c <- c + theme(plot.margin = margin(0, 10, 0, 10))
d <- d + theme(plot.margin = margin(0, 10, 0, 10))
e <- e + theme(plot.margin = margin(0, 10, 0, 10))
f <- f + theme(plot.margin = margin(0, 10, 0, 10))

f <- (a + b + c) / (d + e + f)

p1 <- f + plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold")), 
			tag_levels = 'A', tag_suffix = ')')
			
setwd("D:/3_Caiman/Patron_Modelling/4_Figures")
ggsave(file = "FigX1.jpg", plot = p1, width = 30, height = 20, dpi = 300, units = "cm", device = "jpg")
ggsave(file = "FigX1.tiff", plot = p1, width = 30, height = 20, dpi = 300, units = "cm", device = "tiff")
ggsave(file = "FigX1.pdf", plot = p1, width = 30, height = 20, dpi = 600, units = "cm", device = "pdf")

# ------------------------------------------------------------------------------------------------ #

# Suitability plot for SM #

# ------------------------------------------------------------------------------------------------ #

setwd("D:/3_Caiman/6_maxent/c_lat_e/Final_Models")
lat <- list.files(pattern = ".asc")
suit_lat <- rast(lat[1:3])
suit_lat <- median(suit_lat)
sd_lat <- rast(lat[4:6])
sd_lat <- median(sd_lat)
suit_lat <- raster(suit_lat)
sd_lat <- raster(sd_lat)

setwd("D:/3_Caiman/6_maxent/c_yac_e/Final_Models")
yac <- list.files(pattern = ".asc")
suit_yac <- rast(yac[4:6])
suit_yac <- median(suit_yac)
sd_yac <- rast(yac[7:9])
sd_yac <- median(sd_yac)
suit_yac <- raster(suit_yac)
sd_yac <- raster(sd_yac)

r_suit <- function(raster_data) {
	
	points_df <- as.data.frame(rasterToPoints(raster_data))
	colnames(points_df) <- c("x", "y", "layer")
	world_map <- map_data("world")
	extent <- extent(raster_data)

	p <- ggplot() +
	
			geom_raster(data = points_df, aes(x = x, y = y, fill = layer)) +
			
			scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu")), name = NULL, limits = c(0, 1)) +
			
			geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "transparent", color = "black", linewidth = 0.1) +
			
			theme_bw() +
			
			theme(legend.position = c(0.98, 0.01), legend.justification = c(1, 0), axis.title.x = element_blank(), 
				  axis.title.y = element_blank(), plot.margin = margin(0, 0, 0, 0), plot.background = element_rect(fill = "transparent", 
				  color = NA)) +
    
			theme(legend.background = element_blank()) +
	
			coord_sf(xlim = c(extent[1], extent[2]), ylim = c(extent[3], extent[4]))

	return(p)
	
}

r_sd <- function(raster_data) {
	
	points_df <- as.data.frame(rasterToPoints(raster_data))
	colnames(points_df) <- c("x", "y", "layer")
	world_map <- map_data("world")
	extent <- extent(raster_data)

	p <- ggplot() +
	
			geom_raster(data = points_df, aes(x = x, y = y, fill = layer)) +
	
			scale_fill_gradientn(colors = brewer.pal(5, "Reds"), name = NULL, limits = c(0, max(points_df$layer))) +
    
			geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "transparent", color = "black", 
						 linewidth = 0.1) +
    
			theme_bw() +
			
			coord_sf(xlim = c(extent[1], extent[2]), ylim = c(extent[3], extent[4])) +
			
			theme(legend.position = c(0.98, 0.01), legend.justification = c(1, 0), axis.title.x = element_blank(), 
				  axis.title.y = element_blank(), plot.margin = margin(0, 0, 0, 0), plot.background = element_rect(fill = "transparent", 
				  color = NA)) +
			
			theme(legend.background = element_blank())  
				  
	return(p)

}

a <- r_suit(suit_lat)
b <- r_sd(sd_lat)
c <- r_suit(suit_yac)
d <- r_sd(sd_yac)

a <- a + theme(plot.margin = margin(0, 10, 0, 10))
b <- b + theme(plot.margin = margin(0, 10, 0, 10))
c <- c + theme(plot.margin = margin(0, 10, 0, 10))
d <- d + theme(plot.margin = margin(0, 10, 0, 10))

g <- (a + b) / (c + d)

p1 <- g + plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold")), 
			tag_levels = 'A', tag_suffix = ')')
			
setwd("D:/3_Caiman/Patron_Modelling/5_SM")
ggsave(file = "FIgureSM1.pdf", plot = p1, width = 25, height = 25, dpi = 600, units = "cm", device = "pdf")
#ggsave(file = "FigureSM1.jpg", plot = p1, width = 25, height = 25, dpi = 300, units = "cm", device = "jpg")
#ggsave(file = "FigureSM1.tiff", plot = p1, width = 25, height = 25, dpi = 300, units = "cm", device = "tiff")

# ------------------------------------------------------------------------------------------------ #

### EndNotRun
