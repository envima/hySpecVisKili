# Compile species richness dataset containing residuals from some previous modelling
if(Sys.info()["sysname"] == "Windows"){
  filepath_base = "C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R"
} else {
  filepath_base = "/mnt/sd19006/data/users/tnauss/KI-Hyperspec/HySpec_KiLi/src/000_set_environment_linux.R"
}

source(filepath_base)


dir.create(paste0(path_comb_gpm_sr_res), showWarnings = FALSE)


# Compile elevation residuals for gam model using eleveation as only predictor
pt = "*elev*"
mt = "*gam*"

comb_sr = readRDS(paste0(path_comb_gpm_sr, "ki_hyperspec_biodiv_non_scaled.rds"))
comb_sr_elev_res = compResData(comb_sr, pt, mt)

saveRDS(comb_sr_elev_res, 
        file = file.path(path_comb_gpm_sr_res, 
                         paste0("ki_hyperspec_biodiv_non_scaled",
                                gsub("[*]", "", paste0("_", mt, "_", pt, "_res.rds")))))



# Compile elevation residuals for pls model using elevation and LUI as only predictor
pt = "*elui*"
mt = "*pls*"

comb_sr = readRDS(paste0(path_comb_gpm_sr, "ki_hyperspec_biodiv_non_scaled.rds"))
comb_sr_elev_res = compResData(comb_sr, pt, mt)

saveRDS(comb_sr_elev_res, 
        file = file.path(path_comb_gpm_sr_res, 
                         paste0("ki_hyperspec_biodiv_non_scaled",
                                gsub("[*]", "", paste0("_", mt, "_", pt, "_res.rds")))))



# Compile elevation residuals for rf model using elevation and LUI as only predictor
pt = "*elui*"
mt = "*rf*"

comb_sr = readRDS(paste0(path_comb_gpm_sr, "ki_hyperspec_biodiv_non_scaled.rds"))
comb_sr_elev_res = compResData(comb_sr, pt, mt)

saveRDS(comb_sr_elev_res, 
        file = file.path(path_comb_gpm_sr_res, 
                         paste0("ki_hyperspec_biodiv_non_scaled",
                                gsub("[*]", "", paste0("_", mt, "_", pt, "_res.rds")))))

