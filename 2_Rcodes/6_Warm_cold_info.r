
# ------------------------------------------------------------------------------------------------ #

### Title: Warmest and Coldest quarter information ####
### Author: Patron-Rivero, C. ####
### Date: 06/29/2023 ###
### Project: "Hybridization patterns, ecological niches, and conservation implications in Caiman yacare and
### 			Caiman latirostris: Insights from Phylogeographic Analysis and Ecological Niche Modeling" ###

# ------------------------------------------------------------------------------------------------ #

# Packages #

# ------------------------------------------------------------------------------------------------ #

library(raster)
library(ggplot2)
library(RColorBrewer)
library(rnaturalearth)
library(tidyr)
library(patchwork)

# ------------------------------------------------------------------------------------------------ #

# Inputs #

# ------------------------------------------------------------------------------------------------ #

warm <- raster("D:/3_Caiman/2_env/1_new_full/bio10.asc")
cold <- raster("D:/3_Caiman/2_env/1_new_full/bio11.asc")

w <- rasterToPoints(warm)
c <- rasterToPoints(cold)
data <- cbind(w[, 3], c[, 3])
colnames(data)[2] <- "cold"
colnames(data)[1] <- "warm"
data <- as.data.frame(data)

# ------------------------------------------------------------------------------------------------ #

# Map plot #

# ------------------------------------------------------------------------------------------------ #

map_l <- function(raster_data) {
	
	points_df <- as.data.frame(rasterToPoints(raster_data))
	colnames(points_df) <- c("x", "y", "layer")
	points_df <- na.omit(points_df)
	world_map <- map_data("world")
	extent <- extent(raster_data)
  
	p <- ggplot() +
			
			geom_raster(data = points_df, aes(x = x, y = y, fill = layer/100)) +
			
			scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu")), name = NULL, guide = "none", breaks = seq(25, 31, by = 2), limits = c(25, 31)) +
			
			geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "transparent", color = "black", linewidth = 0.1) +
			
			theme_bw() +
			
			theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank(), plot.margin = margin(0, 0, 0, 0),
				  plot.background = element_rect(fill = "transparent", color = NA)) +
			
			coord_sf(xlim = c(extent[1], extent[2]), ylim = c(extent[3], extent[4])) +
			
			theme(legend.background = element_blank())
  
		return(p)
		
}

map_r <- function(raster_data) {
	
	points_df <- as.data.frame(rasterToPoints(raster_data))
	colnames(points_df) <- c("x", "y", "layer")
	points_df <- na.omit(points_df)
	world_map <- map_data("world")
	extent <- extent(raster_data)
  
	p <- ggplot() +
			
			geom_raster(data = points_df, aes(x = x, y = y, fill = layer/100)) +
			
			scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu")), name = NULL, breaks = seq(25, 31, by = 2), limits = c(25, 31)) +
			
			geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "transparent", color = "black", linewidth = 0.1) +
			
			theme_bw() +
			
			theme(legend.position = c(0.98, 0.01), legend.justification = c(1, 0), axis.title.x = element_blank(), axis.title.y = element_blank(),
			   	  plot.margin = margin(0, 0, 0, 0), plot.background = element_rect(fill = "transparent", color = NA)) +
			
			coord_sf(xlim = c(extent[1], extent[2]), ylim = c(extent[3], extent[4])) +
			
			theme(legend.background = element_blank())
  
		return(p)
		
}

a <- map_l(cold)
b <- map_r(warm)

# ------------------------------------------------------------------------------------------------ #

# Density plot #

# ------------------------------------------------------------------------------------------------ #

data <- pivot_longer(data, cols = c(warm, cold), names_to = "variable", values_to = "value")

c <- ggplot(data, aes(x = value/100, fill = variable)) +
		
		geom_density(alpha = 0.5, color = "black", lwd = 0.2) +
		
		labs(x = "Temperature (°C)", y = "") +
		
		scale_fill_manual(values = c("warm" = "#e31a1c", "cold" = "#2171b5"), name = NULL,
			              labels = c("warm" = "Mean Temperature of the Warmest Quarter", "cold" = "Mean Temperature of the Coldest Quarter")) +
		
		theme_bw() +
		
		theme(legend.position = c(0.02, 0.8), legend.justification = c(0, 0)) +
			
		theme(legend.background = element_blank())

# ------------------------------------------------------------------------------------------------ #

# final plot #

# ------------------------------------------------------------------------------------------------ #

a <- a + theme(plot.margin = margin(0, 10, 0, 10))
b <- b + theme(plot.margin = margin(0, 10, 0, 10))
c <- c + theme(plot.margin = margin(0, 10, 0, 10))

f <- (a + b) / c

p1 <- f + plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold")), 
			tag_levels = 'A', tag_suffix = ')')

setwd("D:/3_Caiman/5_Figures")
ggsave(file = "Fig2.jpg", plot = p1, width = 22, height = 22, dpi = 300, units = "cm", device = "jpg")
ggsave(file = "Fig2.tiff", plot = p1, width = 22, height = 22, dpi = 300, units = "cm", device = "tiff")
ggsave(file = "Fig2.pdf", plot = p1, width = 22, height = 22, dpi = 600, units = "cm", device = "pdf")
	
# ------------------------------------------------------------------------------------------------ #

### EndNotRun
