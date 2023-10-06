
# ------------------------------------------------------------------------------------------------ #

### Title: Ecological information ####
### Author: Patron-Rivero, C. ####
### Date: 06/29/2023 ###
### Project: "Hybridization patterns, ecological niches, and conservation implications in Caiman yacare and
### 			Caiman latirostris: Insights from Phylogeographic Analysis and Ecological Niche Modeling" ###

# ------------------------------------------------------------------------------------------------ #

# Packages #

# ------------------------------------------------------------------------------------------------ #

library(raster)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(forcats)
library(ggdist)
library(gghalves)
library(patchwork)

# ------------------------------------------------------------------------------------------------ #

# Inputs #

# ------------------------------------------------------------------------------------------------ #

load_data <- function(csv_dir, dir_env) {
	
	csv_data <- read.csv(csv_dir, header = TRUE, sep = ",", row.names = NULL)
	tif_files <- list.files(dir_env, pattern = ".asc", full.names = TRUE)
	raster_stack <- raster::stack(tif_files)
	extracted_data <- raster::extract(raster_stack, cbind(csv_data$Long, csv_data$Lat), df = TRUE)
	extracted_data <- extracted_data[, -1]
	colnames(extracted_data) <- gsub("^c_lat_|^c_yac_", "", colnames(extracted_data))
	extracted_data <- cbind(csv_data$Spp, extracted_data)
	return(extracted_data)

}

test <- load_data("D:/3_Caiman/1_occ/2_eval_genetic/all.csv", "D:/3_Caiman/2_env/1_new_full")
train <- load_data("D:/3_Caiman/1_occ/3_clean_joint/all.csv", "D:/3_Caiman/2_env/1_new_full")
colnames(test)[1] <- "Spp"
colnames(train)[1] <- "Spp"
test <- train %>% mutate(Spp = recode(Spp, "C_lat" = "Caiman latirostris", "C_yac" = "Caiman yacare"))
all <- rbind(train, test)

# ------------------------------------------------------------------------------------------------ #

# Plot for SM #

# ------------------------------------------------------------------------------------------------ #

vars <- names(all)[2:16]
plots_l <- vector("list", length(vars))
plots_r <- vector("list", length(vars))
labs_title <- c("Mean Annual Air Temperature (°C)",
				"Mean Daily Mean Air Temperatures of the Warmest Quarter (°C)",
				"Mean Daily Mean Air Temperatures of the Coldest Quarter (°C)",
				"Annual Precipitation Amount (kg m^-2)",
				"Precipitation seasonality (kg m^-2)",
				"Mean Monthly Precipitation Amount of the Wettest Quarter (kg m^-2)",
				"Mean Monthly Precipitation Amount of the Driest Quarter (kg m^-2)",
				"Temperature Seasonality (°C/100)",
				"Elevation",
				"Distance to Large Lakes and Reservoirs",
				"Distance to Permanent Smaller Water Bodies",
				"Slope",
				"Topographic position index",
				"Above-ground Vegetal Biomass",
				"Below-ground Vegetal Biomass")

for (i in 1:length(vars)) {
	
	all$Spp <- factor(all$Spp, levels = rev(unique(all$Spp)))
	plot <- ggplot(all, aes(x = Spp, y = .data[[vars[i]]], fill = Spp)) +
				
				ggdist::stat_halfeye(adjust = 0.5, width = 0.6, .width = 0, justification = -0.2, point_colour = NA, lwd = 0.2) +
				
				scale_fill_viridis_d(alpha = 0.75, guide = FALSE) +
				
				geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.2, lwd = 0.2, fatten = 1) +
				
				coord_flip() +
				
				labs(x = NULL, y = labs_title[i]) +
				
				theme_bw() +
				
				theme(axis.text.y = element_text(face = "italic"), axis.title.y = element_blank())
	
	plots_l[[i]] <- plot

}

for (i in 1:length(vars)) {
	
	all$Spp <- factor(all$Spp, levels = rev(unique(all$Spp)))
	
	plot <- ggplot(all, aes(x = Spp, y = .data[[vars[i]]], fill = Spp)) +
				
				ggdist::stat_halfeye(adjust = 0.5, width = 0.6, .width = 0, justification = -0.2, point_colour = NA, lwd = 0.2) +
				
				scale_fill_viridis_d(alpha = 0.75, guide = FALSE) +
				
				geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.2, lwd = 0.2, fatten = 1) +
				
				coord_flip() +
				
				labs(x = NULL, y = labs_title[i]) +
				
				theme_bw() +
				
				theme(axis.text.y = element_blank(), axis.title.y = element_blank())
	
	plots_r[[i]] <- plot

}
		
f <- (plots_l[[1]] + plots_r[[8]]) / (plots_l[[2]] + plots_r[[3]]) / (plots_l[[4]] + plots_r[[5]]) / (plots_l[[6]] + plots_r[[7]]) / 
	 (plots_l[[10]] + plots_r[[11]]) / (plots_l[[14]] + plots_r[[15]]) / (plots_l[[9]] + plots_r[[12]]) / plots_r[[13]]
		
setwd("D:/3_Caiman/5_Figures")
ggsave(file = "FigSM2.pdf", plot = f, width = 30, height = 36, dpi = 600, units = "cm", device = "pdf")
#ggsave(file = "FigSM2.tiff", plot = f, width = 30, height = 36, dpi = 300, units = "cm", device = "tiff")
#ggsave(file = "FigSM2.jpg", plot = f, width = 30, height = 36, dpi = 300, units = "cm", device = "jpg")
	
# ------------------------------------------------------------------------------------------------ #

### EndNotRun