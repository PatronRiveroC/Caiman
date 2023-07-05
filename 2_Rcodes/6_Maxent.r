
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

# ------------------------------------------------------------------------------------------------ #

# Create all variable sets for calibration and projection #

# ------------------------------------------------------------------------------------------------ #

# C_lat calibration 
vars <- "D:/3_Caiman/2_env/4_calib/C_lat"
new <- "D:/3_Caiman/6_maxent/c_lat/M_Var"

for (i in seq_along(set_lat)) {
	carpeta_set <- file.path(new, paste0("set", i))
	dir_create(carpeta_set, showWarnings = FALSE)
	variables <- set_lat[[i]]
	for (variable in variables) {
		archivo_origen <- file.path(vars, paste0(variable, ".asc"))
		archivo_destino <- file.path(carpeta_set, paste0(variable, ".asc"))
		file_copy(archivo_origen, archivo_destino, overwrite = TRUE)
	}
}

# C_yac calibration 
vars <- "D:/3_Caiman/2_env/4_calib/C_yac"
new <- "D:/3_Caiman/6_maxent/c_yac/M_Var"

for (i in seq_along(set_yac)) {
	carpeta_set <- file.path(new, paste0("set", i))
	dir_create(carpeta_set, showWarnings = FALSE)
	variables <- set_yac[[i]]
	for (variable in variables) {
		archivo_origen <- file.path(vars, paste0(variable, ".asc"))
		archivo_destino <- file.path(carpeta_set, paste0(variable, ".asc"))
		file_copy(archivo_origen, archivo_destino, overwrite = TRUE)
	}
}

# C_lat projection 
vars <- "D:/3_Caiman/2_env/3_eco/full"
new <- "D:/3_Caiman/6_maxent/c_lat/G_Var/var"

for (i in seq_along(set_lat)) {
	carpeta_set <- file.path(new, paste0("set", i))
	dir_create(carpeta_set, showWarnings = FALSE)
	variables <- set_lat[[i]]
	for (variable in variables) {
		archivo_origen <- file.path(vars, paste0(variable, ".asc"))
		archivo_destino <- file.path(carpeta_set, paste0(variable, ".asc"))
		file_copy(archivo_origen, archivo_destino, overwrite = TRUE)
	}
}

# C_yac projection 
vars <- "D:/3_Caiman/2_env/3_eco/full"
new <- "D:/3_Caiman/6_maxent/c_yac/G_Var/var"

for (i in seq_along(set_yac)) {
	carpeta_set <- file.path(new, paste0("set", i))
	dir_create(carpeta_set, showWarnings = FALSE)
	variables <- set_yac[[i]]
	for (variable in variables) {
		archivo_origen <- file.path(vars, paste0(variable, ".asc"))
		archivo_destino <- file.path(carpeta_set, paste0(variable, ".asc"))
		file_copy(archivo_origen, archivo_destino, overwrite = TRUE)
	}
}

# ------------------------------------------------------------------------------------------------ #

# Maxent C_lat #

# ------------------------------------------------------------------------------------------------ #

setwd("D:/3_Caiman/6_maxent/c_lat")
file_name <- "C_lat_kuenm_process"
kuenm_start(file.name = file_name)
occ_joint <- "c_lat_joint.csv"
occ_tra <- "c_lat_train.csv"
M_var_dir <- "M_Var"
batch_cal <- "Candidate_models"
out_dir <- "Candidate_Models" 
reg_mult <- c(0.1, 0.25, 0.5, 0.75, 1, 2, 3, 4)
args <- "maximumbackground=10000 biasfile=D:\\3_Caiman\\6_maxent\\c_lat\\bias_lat.asc biastype=3"
f_clas <- "no.t.h"
maxent_path <- "D:/3_Caiman/4_eval/2_max/c_lat"
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
project <- FALSE
G_var_dir <- "G_Var"
ext_type <- "all" 
write_mess <- FALSE
write_clamp <- TRUE
wait1 <- FALSE
run1 <- TRUE
args <- "randomseed maximumbackground=10000 biasfile=D:\\3_Caiman\\6_maxent\\c_lat\\bias.asc biastype=3"

kuenm::kuenm_mod(occ.joint = occ_joint, M.var.dir = M_var_dir, out.eval = out_eval, batch = batch_fin, rep.n = rep_n,
          rep.type = rep_type, jackknife = jackknife, out.dir = mod_dir, out.format = out_format, project = project,
          G.var.dir = G_var_dir, ext.type = ext_type, write.mess = write_mess, write.clamp = write_clamp, 
          maxent.path = maxent_path, args = args, wait = wait1, run = run1)

# ------------------------------------------------------------------------------------------------ #

# Maxent C_yac #

# ------------------------------------------------------------------------------------------------ #

setwd("D:/3_Caiman/6_maxent/c_yac")
file_name <- "C_yac_kuenm_process"
kuenm_start(file.name = file_name)
occ_joint <- "c_yac_joint.csv"
occ_tra <- "c_yac_train.csv"
M_var_dir <- "M_Var"
batch_cal <- "Candidate_models"
out_dir <- "Candidate_Models" 
reg_mult <- c(0.1, 0.25, 0.5, 0.75, 1, 2, 3, 4)
args <- "maximumbackground=10000 biasfile=D:\\3_Caiman\\6_maxent\\c_yac\\bias_yac.asc biastype=3"
f_clas <- "no.t.h"
maxent_path <- "D:/3_Caiman/6_maxent/c_yac"
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
project <- FALSE
G_var_dir <- "G_Var"
ext_type <- "all" 
write_mess <- FALSE
write_clamp <- TRUE
wait1 <- FALSE
run1 <- TRUE
args <- "randomseed maximumbackground=10000 biasfile=D:\\3_Caiman\\6_maxent\\c_yac\\bias.asc biastype=3"

kuenm::kuenm_mod(occ.joint = occ_joint, M.var.dir = M_var_dir, out.eval = out_eval, batch = batch_fin, rep.n = rep_n,
          rep.type = rep_type, jackknife = jackknife, out.dir = mod_dir, out.format = out_format, project = project,
          G.var.dir = G_var_dir, ext.type = ext_type, write.mess = write_mess, write.clamp = write_clamp, 
          maxent.path = maxent_path, args = args, wait = wait1, run = run1)

# ------------------------------------------------------------------------------------------------ #

### EndNotRun
