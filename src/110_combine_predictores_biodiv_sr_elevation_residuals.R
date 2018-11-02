# Combine hyperspectral predictores and biodiversity variables in gpm class.

source("C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R")

if(length(showConnections()) == 0){
  cores = 20
  cl = parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
}

comb_elev_res = readRDS(paste0(path_comb_gpm_sr, "ki_hyperspec_biodiv_non_scaled.rds"))

comb_elev_res@meta$input$PREDICTOR_FINAL = comb_elev_res@meta$input$PREDICTOR[c(1)]

for (i in seq(length(comb@meta$input$RESPONSE))){
  print(i)  
  comb_elev_res@meta$input$RESPONSE_FINAL = comb_elev_res@meta$input$RESPONSE[i]
  comb_elev_res@data$input = comb_elev_res@data$input[complete.cases(comb_elev_res@data$input[, c(comb_elev_res@meta$input$RESPONSE_FINAL, comb_elev_res@meta$input$PREDICTOR_FINAL)]), ]
  comb_elev_res = createIndexFolds(x = comb_elev_res, nested_cv = FALSE)
  
  
  comb_elev_res = trainModel(x = comb_elev_res,
                     metric = "RMSE",
                     n_var = NULL, 
                     mthd = "gam",
                     mode = "none",
                     seed_nbr = 11, 
                     cv_nbr = NULL,
                     var_selection = "indv",
                     filepath_tmp = NULL)
  
  comb_elev_res@model$gam_none[[1]][[1]]$model
  
  comb_elev_res@data$input[, paste0(comb_elev_res@meta$input$RESPONSE_FINAL, "_elev_res")] = 
    as.vector(comb_elev_res@data$input[, comb_elev_res@meta$input$RESPONSE_FINAL] - 
    predict(comb_elev_res@model$gam_none[[1]][[1]]$model, comb_elev_res@data$input))
  
}

# Remove original biodiversity information and update meta information
comb_elev_res@data$input[, comb_elev_res@meta$input$RESPONSE] = NULL
comb_elev_res@meta$input$RESPONSE = paste0(comb_elev_res@meta$input$RESPONSE, "_elev_res")

dir.create(paste0(path_comb_gpm_sr_elev_res), showWarnings = FALSE)

saveRDS(comb_elev_res, file = paste0(path_comb_gpm_sr_elev_res, "ki_hyperspec_biodiv_non_scaled_elev_res.rds"))

