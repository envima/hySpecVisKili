# Recompute models using best variable subset and completely independent validation

# Predict species richness using different models and predictor sets
if(Sys.info()["sysname"] == "Windows"){
  filepath_base = "C:/Users/Thomas Nauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R"
} else {
  filepath_base = "/mnt/sd19006/data/users/tnauss/KI-Hyperspec/HySpec_KiLi/src/000_set_environment_linux.R"
}
source(filepath_base)

if(length(showConnections()) == 0){
  cores = 3
  cl = parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
}

dir.create(path_comb_gpm_sr_indp, showWarnings = FALSE)

all_models = readRDS(file.path(path_compile_analysis_sr, "models_sr.rds"))

mt = "rf"

# Predict variables again using best predictors and independent validation.
for(pt in names(all_models[[mt]])){
  comb = all_models[[mt]][[pt]]
  var_imp <- compVarImp(comb@model[[1]], scale = FALSE)
  
  for(rs in seq(length(var_imp))){
    comb@meta$input$RESPONSE = as.character(var_imp[[rs]]$RESPONSE[1])
    comb@meta$input$PREDICTOR_FINAL = var_imp[[rs]]$VARIABLE

    compModels(model = comb, pt = pt, mt = mt, rs = rs, outpath = path_comb_gpm_sr_indp, nested_cv = TRUE)
  }
}


stopCluster(cl)

