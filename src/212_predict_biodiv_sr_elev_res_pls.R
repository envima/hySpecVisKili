# comb_elev_resine hyperspectral predictores and biodiversity variables in gpm class.
if(Sys.info()["sysname"] == "Windows"){
  filepath_base = "C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R"
} else {
  filepath_base = "/mnt/sd19006/data/users/tnauss/KI-Hyperspec/HySpec_KiLi/src/000_set_environment_linux.R"
}
source(filepath_base)

if(length(showConnections()) == 0){
  cores = 20
  cl = parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
}

dir.create(paste0(path_model_gpm_sr_elev_res), showWarnings = FALSE)

comb_elev_res = readRDS(paste0(path_comb_gpm_sr_elev_res, "ki_hyperspec_biodiv_non_scaled_elev_res.rds"))


# Predict with all elevation and lui information only
comb_elev_res@meta$input$PREDICTOR_FINAL = comb_elev_res@meta$input$PREDICTOR[c(1:7)]


foreach (i = seq(length(comb_elev_res@meta$input$RESPONSE)), .packages = c("gpm", "caret", "pls", "CAST")) %dopar% {
  
  model = comb_elev_res
  model@meta$input$RESPONSE_FINAL = model@meta$input$RESPONSE[i]
  model@data$input = model@data$input[complete.cases(model@data$input[, c(model@meta$input$RESPONSE_FINAL, model@meta$input$PREDICTOR_FINAL)]), ]
  model = createIndexFolds(x = model, nested_cv = FALSE)
  model = trainModel(x = model,
                     metric = "RMSE",
                     n_var = NULL, 
                     mthd = "pls",
                     mode = "ffs",
                     seed_nbr = 11, 
                     cv_nbr = NULL,
                     var_selection = "indv",
                     filepath_tmp = NULL)
  
  saveRDS(model, file = paste0(path_model_gpm_sr_elev_res, 
                               "ki_sr_elui_non_scaled_elev_res_rf_", 
                               model@meta$input$RESPONSE_FINAL, 
                               ".rds"))
}

# Predict with hyperspectral data only
comb_elev_res@meta$input$PREDICTOR_FINAL = comb_elev_res@meta$input$PREDICTOR[-c(1:7)]

foreach (i = seq(length(comb_elev_res@meta$input$RESPONSE)), .packages = c("gpm", "caret", "pls", "CAST")) %dopar% {
  
  model = comb_elev_res
  model@meta$input$RESPONSE_FINAL = model@meta$input$RESPONSE[i]
  model@data$input = model@data$input[complete.cases(model@data$input[, c(model@meta$input$RESPONSE_FINAL, model@meta$input$PREDICTOR_FINAL)]), ]
  model = createIndexFolds(x = model, nested_cv = FALSE)
  model = trainModel(x = model,
                    metric = "RMSE",
                    n_var = NULL, 
                    mthd = "pls",
                    mode = "ffs",
                    seed_nbr = 11, 
                    cv_nbr = NULL,
                    var_selection = "indv",
                    filepath_tmp = NULL)
  
    saveRDS(model, file = paste0(path_model_gpm_sr_elev_res, 
                              "ki_sr_spec_non_scaled_elev_res_rf_", 
                              model@meta$input$RESPONSE_FINAL, 
                              ".rds"))
}


# Predict with all data
comb_elev_res@meta$input$PREDICTOR_FINAL = comb_elev_res@meta$input$PREDICTOR

foreach (i = seq(length(comb_elev_res@meta$input$RESPONSE)), .packages = c("gpm", "caret", "pls", "CAST")) %dopar% {
  
  model = comb_elev_res
  model@meta$input$RESPONSE_FINAL = model@meta$input$RESPONSE[i]
  model@data$input = model@data$input[complete.cases(model@data$input[, c(model@meta$input$RESPONSE_FINAL, model@meta$input$PREDICTOR_FINAL)]), ]
  model = createIndexFolds(x = model, nested_cv = FALSE)
  model = trainModel(x = model,
                     metric = "RMSE",
                     n_var = NULL, 
                     mthd = "pls",
                     mode = "ffs",
                     seed_nbr = 11, 
                     cv_nbr = NULL,
                     var_selection = "indv",
                     filepath_tmp = NULL)
  
  saveRDS(model, file = paste0(path_model_gpm_sr_elev_res, 
                               "ki_sr_elsp_non_scaled_elev_res_rf_", 
                               model@meta$input$RESPONSE_FINAL, 
                               ".rds"))
}



# Predict with kmdc and raoq only
comb_elev_res@meta$input$PREDICTOR_FINAL = unique(comb@meta$input$PREDICTOR[
  c(grep("kmdc", comb@meta$input$PREDICTOR), 
    grep("raoq", comb@meta$input$PREDICTOR))])

foreach (i = seq(length(comb_elev_res@meta$input$RESPONSE)), .packages = c("gpm", "caret", "pls", "CAST")) %dopar% {
  
  model = comb_elev_res
  model@meta$input$RESPONSE_FINAL = model@meta$input$RESPONSE[i]
  model@data$input = model@data$input[complete.cases(model@data$input[, c(model@meta$input$RESPONSE_FINAL, model@meta$input$PREDICTOR_FINAL)]), ]
  model = createIndexFolds(x = model, nested_cv = FALSE)
  model = trainModel(x = model,
                     metric = "RMSE",
                     n_var = NULL, 
                     mthd = "pls",
                     mode = "ffs",
                     seed_nbr = 11, 
                     cv_nbr = NULL,
                     var_selection = "indv",
                     filepath_tmp = NULL)
  
  saveRDS(model, file = paste0(path_model_gpm_sr_elev_res, 
                               "ki_sr_kmra_non_scaled_elev_res_rf_", 
                               model@meta$input$RESPONSE_FINAL, 
                               ".rds"))
}


stopCluster(cl)