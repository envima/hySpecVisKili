# comb_elev_resine hyperspectral predictores and biodiversity variables in gpm class.
if(Sys.info()["sysname"] == "Windows"){
  filepath_base = "D:/plygrnd/hySpecVisKili/hySpecVisKili/src/000_set_environment.R"
} else {
  filepath_base = "/mnt/sd19006/data/users/tnauss/KI-Hyperspec/HySpec_KiLi/src/000_set_environment_linux.R"
}
source(filepath_base)

if(length(showConnections()) == 0){
  cores = 30
  cl = parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
}

dir.create(paste0(path_model_gpm_sr_res), showWarnings = FALSE)

# Predict gam, pls and rf elevation and elevation/lui based residuals using 
# pls and rf models with hyperspectral data only
# res_suffixes = c("_gam_elev_res", "_pls_elui_res", "_rf_elui_res")
res_suffixes = c("_rf_elui_res")
# mtypes = c("pls", "rf")
mtypes = c("rf")
ptypes = c("*spec*", "*kmra*")


for(res_suffix in res_suffixes){
  comb_elev_res = readRDS(paste0(path_comb_gpm_sr_res, "ki_hyperspec_biodiv_non_scaled",
                                 res_suffix, ".rds"))
  for(mt in mtypes){
    for(pt in ptypes){
      
      if(pt == "*spec*"){ 
        comb_elev_res@meta$input$PREDICTOR_FINAL = comb_elev_res@meta$input$PREDICTOR[-c(1,2)]
      } else if(pt == "*kmra*"){
        comb_elev_res@meta$input$PREDICTOR_FINAL = unique(comb_elev_res@meta$input$PREDICTOR[
          c(grep("kmdc", comb_elev_res@meta$input$PREDICTOR), 
            grep("raoq", comb_elev_res@meta$input$PREDICTOR))])
      }
      
      compModels(model = comb_elev_res, pt = pt, mt = mt, outpath = path_model_gpm_sr_res)
    }
  }
}

stopCluster(cl)