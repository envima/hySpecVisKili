# Combine hyperspectral predictores and biodiversity variables in gpm class.

source("C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R")
if(length(showConnections()) == 0){
  cores = 3
  cl = parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
}

dir.create(paste0(path_model_gpm), showWarnings = FALSE)

comb = readRDS(paste0(path_comb_gpm, "ki_hyperspec_biodiv_non_scaled.rds"))

comb@meta$input$RESPONSE_FINAL = "SRsnails"
comb@data$input = comb@data$input[complete.cases(comb@data$input[, c(comb@meta$input$RESPONSE_FINAL, comb@meta$input$PREDICTOR_FINAL)]), ]

comb = createIndexFolds(x = comb, nested_cv = FALSE)

comb = trainModel(x = comb,
                  metric = "RMSE",
                  n_var = NULL, 
                  mthd = "pls",
                  mode = "ffs",
                  seed_nbr = 11, 
                  cv_nbr = NULL,
                  var_selection = "indv",
                  filepath_tmp = NULL)



stopCluster(cl)