# Recompute elevation residual models using best variable subset and completely independent validation

# Predict species richness using different models and predictor sets
if(Sys.info()["sysname"] == "Windows"){
  filepath_base = "D:/plygrnd/hySpecVisKili/hySpecVisKili/src/000_set_environment.R"
} else {
  filepath_base = "/mnt/sd19006/data/users/tnauss/KI-Hyperspec/HySpec_KiLi/src/000_set_environment_linux.R"
}
source(filepath_base)

if(length(showConnections()) == 0){
  cores = 3
  cl = parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
}

dir.create(path_model_gpm_sr_elev_res_indp, showWarnings = FALSE)

elev_res_indp = readRDS(file.path(path_comb_gpm_sr_elev_res_indp, "ki_hyperspec_biodiv_non_scaled_rf_elui_res_indp.rds"))
all_models_res = readRDS(file.path(path_compile_analysis_sr_elev_res, 
                                   "models_sr_elev_res.rds"))

mt = "rf"

# Predict variables again using best predictors and independent validation.
for(pt in names(all_models_res[[mt]])){
  comb_res = all_models_res[[mt]][[pt]]
  var_imp <- compVarImp(comb_res@model[[1]], scale = FALSE)
  com_res_indp = elev_res_indp
  
  for(rs in seq(length(var_imp))){
    com_res_indp@meta$input$RESPONSE = paste0(as.character(var_imp[[rs]]$RESPONSE[1]), "_indp")
    com_res_indp@meta$input$PREDICTOR_FINAL = as.character(var_imp[[rs]]$VARIABLE)
    
    compModels(model = com_res_indp, pt = pt, mt = mt, rs = rs, outpath = path_model_gpm_sr_elev_res_indp, nested_cv = TRUE)
  }
}


stopCluster(cl)

