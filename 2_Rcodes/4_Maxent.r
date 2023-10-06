
# ------------------------------------------------------------------------------------------------ #

### Title: Maxent models ####
### Author: Patron-Rivero, C. ####
### Date: 06/27/2023 ###
### Project: "Hybridization patterns, ecological niches, and conservation implications in Caiman yacare and
### 			Caiman latirostris: Insights from Phylogeographic Analysis and Ecological Niche Modeling" ###

# ------------------------------------------------------------------------------------------------ #

# Packages #

# ------------------------------------------------------------------------------------------------ #

library(raster)
library(kuenm)
library(fs)
library(ggplot2)
library(rnaturalearth)
library(patchwork)
library(terra)
library(usdm)
library(rgdal)

# ------------------------------------------------------------------------------------------------ #

# VIF variable selection and crop by M area #

# ------------------------------------------------------------------------------------------------ #


data <- list.files("D:/3_Caiman/1_occ/3_clean_joint", pattern = ".csv")
data <- data[-1]
setwd("D:/3_Caiman/2_env/1_new_full")
env <- list.files(pattern = ".asc")
env_data <- rast(env)
spp <- list()
vif <- list()

	for (i in 1:length(data)) {
	
		setwd("D:/3_Caiman/1_occ/3_clean_joint")
		spp1 <- read.csv(data[i])
		spp_env <- terra::extract(env_data, spp1[, 2:3])
		spp_env <- na.omit(spp_env)
		vif[[i]] <- vifstep(spp_env[-c(1, 14)], th = 10)
			
	}

shp <- list.files("D:/3_Caiman/2_env/3_eco", pattern = ".shp")
sp <- c("c_lat", "c_yac")

	for (i in 1:length(shp)){

		setwd("D:/3_Caiman/2_env/3_eco")
		M <- shapefile(shp[[i]])
		M <-  st_as_sf(M)
		setwd(paste0("D:/3_Caiman/6_maxent/", sp[i], "/M_Var/set1"))
		env <- list.files(pattern = ".asc")
		env <- rast(env)
		env1 <- terra::crop(env, M)
		env2 <- terra::mask(env1, M)
		env2 <- raster::stack(env2)
		writeRaster(env2, filename = names(env2), bylayer = TRUE, format = "ascii", overwrite = TRUE)

	}

setwd("D:/3_Caiman/1_occ/4_bias")
press_files <- list.files(pattern = ".csv")
press_data <- list()

for (file in press_files) {
	
	data <- read.csv(file, header = TRUE)
	press_data[[file]] <- data

}

data <- do.call(rbind, press_data)
data_2 <- data
data_2[, "Long"] <- as.numeric(data_2[, "Long"])
data_2[, "Lat"] <- as.numeric(data_2[, "Lat"])
data_2 <- na.omit(data_2)
data_2 <- as.data.frame(data_2)
data_2 <- data_2[, -1]
env <- terra::extract(env_data, data_2[, c("Long", "Lat")])
f_occ_env <- data.frame(data_2, env)
occ_env <- na.omit(f_occ_env)
data_clean <- occ_env[, c("Long", "Lat")]
data_list <- data_clean
den <- data_list[, c(1, 2)]
den <- SpatialPoints(den)

	for (i in 1:length(sp)) {

		setwd(paste0("D:/3_Caiman/6_maxent/", sp[i], "/M_Var/set1"))
		env <- list.files(pattern = ".asc")
		var <- rast(env)
		r <- rasterToPoints(raster(var[[1]]))
		r1 <- SpatialPoints(r[, 1:2])
		r2 <- SpatialPixels(r1)
		kde <- kernelUD(den, h = "href", grid = r2)
		k <- raster(kde)
		kp <- rasterToPoints(k)
		ksf <- st_as_sf(as.data.frame(kp), coords = c("x", "y"), crs = st_crs("+datum=WGS84 +proj=longlat"))
		kr <- raster(crs = crs(ksf), vals = 0, resolution = res(var[[1]])) %>% rasterize(ksf, .)
		a <- crop(kr[[2]], raster(var[[1]]))
		b <- mask(a, raster(var[[1]]))
		setwd(paste0("D:/3_Caiman/6_maxent/", sp[i]))
		writeRaster(b, filename = sp[i], format = "ascii", overwrite = TRUE)

	}

# ------------------------------------------------------------------------------------------------ #

# Maxent C_lat #

# ------------------------------------------------------------------------------------------------ #

setwd("D:/3_Caiman/6_maxent/c_lat_e")
file_name <- "C_lat_kuenm_process"
kuenm_start(file.name = file_name)
occ_joint <- "c_lat_joint.csv"
occ_tra <- "c_lat_train.csv"
M_var_dir <- "M_Var"
batch_cal <- "Candidate_models"
out_dir <- "Candidate_Models" 
reg_mult <- 4 #c(0.1, 0.25, 0.5, 0.75, 1, 2, 3, 4)
args <- "randomseed maximumbackground=10000 biasfile=D:\\3_Caiman\\6_maxent\\c_lat_e\\c_lat.asc biastype=3"
f_clas <- "h" #"all"
maxent_path <- "D:/3_Caiman/6_maxent/c_lat_e"
wait <- FALSE
run <- TRUE

kuenm_cal(occ.joint = occ_joint, occ.tra = occ_tra, M.var.dir = M_var_dir, batch = batch_cal,
          out.dir = out_dir, reg.mult = reg_mult, f.clas = f_clas, args = args,
          maxent.path = maxent_path, wait = wait, run = run)

occ_test <- "c_lat_test.csv"
out_eval <- "Calibration_results"
threshold <- 10
rand_percent <- 80
iterations <- 100
kept <- FALSE
selection <- "OR_AICc"
paral_proc <- FALSE 

cal_eval <- kuenm_ceval(path = out_dir, occ.joint = occ_joint, occ.tra = occ_tra, occ.test = occ_test, batch = batch_cal,
                        out.eval = out_eval, threshold = threshold, rand.percent = rand_percent, iterations = iterations,
                        kept = kept, selection = selection, parallel.proc = paral_proc)
						
batch_fin <- "final_models"
mod_dir <- "Final_Models"
rep_n <- 10
rep_type <- "Bootstrap"
jackknife <- FALSE
out_format <- "cloglog"
project <- TRUE
G_var_dir <- "G_Var"
ext_type <- "all" 
write_mess <- FALSE
write_clamp <- FALSE
wait1 <- FALSE
run1 <- TRUE
args <- "randomseed maximumbackground=10000 biasfile=D:\\3_Caiman\\6_maxent\\c_lat_e\\c_lat.asc biastype=3"

kuenm::kuenm_mod(occ.joint = occ_joint, M.var.dir = M_var_dir, out.eval = out_eval, batch = batch_fin, rep.n = rep_n,
          rep.type = rep_type, jackknife = jackknife, out.dir = mod_dir, out.format = out_format, project = project,
          G.var.dir = G_var_dir, ext.type = ext_type, write.mess = write_mess, write.clamp = write_clamp, 
          maxent.path = maxent_path, args = args, wait = wait1, run = run1)

sets_var <- "set1"
out_mop <- "MOP_results"
percent <- 10
paral <- FALSE

kuenm_mmop(G.var.dir = G_var_dir, M.var.dir = M_var_dir, sets.var = sets_var, out.mop = out_mop,
           percent = percent, parallel = paral)

sp_name <- "c_lat"
fmod_dir <- "Final_Models"
format <- "asc"
project <- TRUE
stats <- c("med", "range")
rep <- TRUE
scenarios <- c("current", "hybrid")
ext_type <- c("E", "EC", "NE") # the type of extrapolation can be selected according to user requirements 
out_dir <- "Final_Model_Stats"

kuenm_modstats(sp.name = sp_name, fmod.dir = fmod_dir, format = format, project = project, 
               statistics = stats, replicated = rep, proj.scenarios = scenarios, 
               ext.type = ext_type, out.dir = out_dir)

# ------------------------------------------------------------------------------------------------ #

# Maxent C_yac #

# ------------------------------------------------------------------------------------------------ #

setwd("D:/3_Caiman/6_maxent/c_yac_e")
file_name <- "C_yac_kuenm_process"
kuenm_start(file.name = file_name)
occ_joint <- "c_yac_joint.csv"
occ_tra <- "c_yac_train.csv"
M_var_dir <- "M_Var"
batch_cal <- "Candidate_models"
out_dir <- "Candidate_Models" 
reg_mult <- c(0.1, 0.25, 0.5, 0.75, 1, 2, 3, 4)
args <- "randomseed maximumbackground=10000 biasfile=D:\\3_Caiman\\6_maxent\\c_yac_e\\bias.asc biastype=3"
f_clas <- "all"
maxent_path <- "D:/3_Caiman/6_maxent/c_yac_e"
wait <- FALSE
run <- TRUE

kuenm_cal(occ.joint = occ_joint, occ.tra = occ_tra, M.var.dir = M_var_dir, batch = batch_cal,
          out.dir = out_dir, reg.mult = reg_mult, f.clas = f_clas, args = args,
          maxent.path = maxent_path, wait = wait, run = run)

occ_test <- "c_yac_test.csv"
out_eval <- "Calibration_results"
threshold <- 10
rand_percent <- 80
iterations <- 100
kept <- FALSE
selection <- "OR_AICc"
paral_proc <- FALSE 

cal_eval <- kuenm_ceval(path = out_dir, occ.joint = occ_joint, occ.tra = occ_tra, occ.test = occ_test, batch = batch_cal,
                        out.eval = out_eval, threshold = threshold, rand.percent = rand_percent, iterations = iterations,
                        kept = kept, selection = selection, parallel.proc = paral_proc)
						
batch_fin <- "final_models"
mod_dir <- "Final_Models"
rep_n <- 10
rep_type <- "Bootstrap"
jackknife <- FALSE
out_format <- "cloglog"
project <- TRUE
G_var_dir <- "G_Var"
ext_type <- "all" 
write_mess <- FALSE
write_clamp <- TRUE
wait1 <- FALSE
run1 <- TRUE
args <- "randomseed maximumbackground=10000 biasfile=D:\\3_Caiman\\6_maxent\\c_yac_e\\bias.asc biastype=3"

kuenm::kuenm_mod(occ.joint = occ_joint, M.var.dir = M_var_dir, out.eval = out_eval, batch = batch_fin, rep.n = rep_n,
          rep.type = rep_type, jackknife = jackknife, out.dir = mod_dir, out.format = out_format, project = project,
          G.var.dir = G_var_dir, ext.type = ext_type, write.mess = write_mess, write.clamp = write_clamp, 
          maxent.path = maxent_path, args = args, wait = wait1, run = run1)

# ------------------------------------------------------------------------------------------------ #

# Binarization optimization process #

# ------------------------------------------------------------------------------------------------ #

### C_lat ###

setwd("D:/3_Caiman/6_maxent/c_lat_e/Final_Models")
a <- list.files("D:/3_Caiman/6_maxent/c_lat_e/Final_Models", pattern = ".asc")
a <- rast(a)
b <- median(a)
suit <- raster::raster(b)
train <- read.csv("D:/3_Caiman/6_maxent/c_lat_e/c_lat_train.csv")
joint <- read.csv("D:/3_Caiman/6_maxent/c_lat_e/c_lat_joint.csv")

set.seed(20)
E <- seq(1, 20, by = 1)	
or <- vector()
sensitivity <- vector()

for (j in 1:length(E)) {
		
		t <- train[, 2:3]
		bin_m <- ntbox::bin_model(model = suit, occs = t, percent = E[j])
		area <- raster::extract(bin_m, t)
		area <- na.omit(area)
		area <- sum(area == 1) / length(area)
		or[[j]] <- 1-area
		bin_m <- bin_m[!is.na(bin_m)]
		prop <- mean(bin_m == 1)
		acc <- area / (area + prop)
		sensitivity[[j]] <- acc

}
	
rank <- cbind(E, sensitivity, or)
rank <- as.data.frame(rank)
ranking <- rank[order(-rank$sensitivity, rank$or), ]
print(ranking)
n <- as.numeric(readline(prompt = "Choose the best E: "))
bin_best <- ntbox::bin_model(model = suit, occs = joint[, 2:3], percent = n)
writeRaster(bin_best, filename = "D:/3_Caiman/6_maxent/c_lat_e/bin_final.tif", format = "GTiff", overwrite = TRUE)
	
t <- train[, 2:3]
e <- extract(suit, t)
e <- na.omit(e)
p <- quantile(e, probs = n/100)
setwd("D:/3_Caiman/6_maxent/c_lat_e/Final_Models/proj")
a <- list.files(pattern = ".asc")
a <- raster::stack(a)
reclasstable <- matrix(c(NA, p, 0, p, 100, 1), ncol = 3, byrow = TRUE)
r <- reclassify(a, reclasstable)
setwd("D:/3_Caiman/6_maxent/c_lat_e")
writeRaster(r, filename = names(r), bylayer = TRUE, format = "GTiff", overwrite = TRUE)

### C_yac ###

setwd("D:/3_Caiman/6_maxent/c_yac_e/Final_Models")
a <- list.files(pattern = ".asc")
a <- a[4:6]
a <- rast(a)
b <- median(a)
suit <- raster::raster(b)
train <- read.csv("D:/3_Caiman/6_maxent/c_yac_e/c_yac_train.csv")
joint <- read.csv("D:/3_Caiman/6_maxent/c_yac_e/c_yac_joint.csv")

set.seed(20)
E <- seq(1, 20, by = 1)	
or <- vector()
sensitivity <- vector()

for (j in 1:length(E)) {
		
		t <- train[, 2:3]
		bin_m <- ntbox::bin_model(model = suit, occs = t, percent = E[j])
		area <- raster::extract(bin_m, t)
		area <- na.omit(area)
		area <- sum(area == 1) / length(area)
		or[[j]] <- 1-area
		bin_m <- bin_m[!is.na(bin_m)]
		prop <- mean(bin_m == 1)
		acc <- area / (area + prop)
		sensitivity[[j]] <- acc

}
	
rank <- cbind(E, sensitivity, or)
rank <- as.data.frame(rank)
ranking <- rank[order(-rank$sensitivity, rank$or), ]
print(ranking)
n <- as.numeric(readline(prompt = "Choose the best E: "))
bin_best <- ntbox::bin_model(model = suit, occs = joint[, 2:3], percent = n)
writeRaster(bin_best, filename = "D:/3_Caiman/6_maxent/c_yac_e/bin_final.tif", format = "GTiff", overwrite = TRUE)
	
t <- train[, 2:3]
e <- extract(suit, t)
e <- na.omit(e)
p <- quantile(e, probs = n/100)
setwd("D:/3_Caiman/6_maxent/c_yac_e/Final_Models")
a <- list.files(pattern = ".asc")
a <- a[1:3]
a <- rast(a)
reclasstable <- matrix(c(NA, p, 0, p, 100, 1), ncol = 3, byrow = TRUE)
r <- classify(a, reclasstable)
r <- raster::stack(r)
setwd("D:/3_Caiman/6_maxent/c_yac_e")
writeRaster(r, filename = names(r), bylayer = TRUE, format = "GTiff", overwrite = TRUE)

# ------------------------------------------------------------------------------------------------ #

### EndNotRun

