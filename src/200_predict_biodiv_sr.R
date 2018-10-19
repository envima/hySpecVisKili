# Combine hyperspectral predictores and biodiversity variables in gpm class.
if(Sys.info()["sysname"] == "Windows"){
  filepath_base = "C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R"
} else {
  filepath_base = "/mnt/sd19006/data/users/tnauss/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R"
}
source(filepath_base)

if(length(showConnections()) == 0){
  cores = 3
  cl = parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
}

dir.create(paste0(path_model_gpm_sr), showWarnings = FALSE)

comb = readRDS(paste0(path_comb_gpm_sr, "ki_hyperspec_biodiv_non_scaled.rds"))
comb@meta$input$PREDICTOR_FINAL = comb@meta$input$PREDICTOR[-c(1:7)]

lapply(comb@meta$input$RESPONSE, function(r){
  comb@meta$input$RESPONSE_FINAL = r
  comb@data$input = comb@data$input[complete.cases(comb@data$input[, c(comb@meta$input$RESPONSE_FINAL, comb@meta$input$PREDICTOR_FINAL)]), ]
  comb = createIndexFolds(x = comb, nested_cv = FALSE)
  comb = trainModel(x = comb,
                    metric = "RMSE",
                    n_var = NULL, 
                    mthd = "rf",
                    mode = "ffs",
                    seed_nbr = 11, 
                    cv_nbr = NULL,
                    var_selection = "indv",
                    filepath_tmp = NULL)
  saveRDS(comb, file = paste0(path_model_gpm_sr, 
                              "ki_hs_bd_sr_non_scaled_rf_rso_", 
                              r, 
                              ".rds"))
})






# saveRDS(comb, file = paste0(path_model_gpm_sr, "ki_hyperspec_biodiv_non_scaled_modell.rds"))

stopCluster(cl)