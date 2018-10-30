# Combine hyperspectral predictores and biodiversity variables in gpm class.
if(Sys.info()["sysname"] == "Windows"){
  filepath_base = "C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R"
} else {
  filepath_base = "/mnt/sd19006/data/users/tnauss/KI-Hyperspec/HySpec_KiLi/src/000_set_environment_linux.R"
}
source(filepath_base)

if(length(showConnections()) == 0){
  cores = 3
  cl = parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
}

dir.create(path_analysis_sr, showWarnings = FALSE)


# Combine all models into one gpm object


mtypes = c("*elui*", "*eall*", "*spec*")

mtypes = c("*eall*", "*spec*")

model_results = lapply(mtypes, function(mt){
  model_files = list.files(path_model_gpm_sr, full.names = TRUE,
                           pattern = glob2rx(mt))
  
  all_models = readRDS(model_files[[1]])
  
  for(i in (seq(2, length(model_files)))){
    all_models@model[[1]][[i]] = readRDS(model_files[[i]])@model[[1]][[1]]
  }
  
  smr = lapply(all_models@model[[1]], function(m){
    data.frame(mtype = mt,
               resp = m[[1]]$response,
               m[[1]]$model$results[m[[1]]$model$results$mtry == 
                                      m[[1]]$model$bestTune$mtry,]
    )
  })
  smr = do.call("rbind", smr)
  return(smr)
})

model_results = do.call("rbind", model_results)

model_results[order(model_results$resp),]







# varImp(all_models@model[[1]][[1]][[1]]$model)
# 
# caret::varImp(all_models@model[[1]][[1]][[1]]$model)
# 
# tune = m@meta$input$RESPONSE_FINAL
# perf_mean = m@model[[1]][[1]][[1]]$model$results[m@model[[1]][[1]][[1]]$model$results$mtry == m@model[[1]][[1]][[1]]$model$bestTune[, 1],]
# perf_resmpls = m@model[[1]][[1]][[1]]$model$resample
# return(data.frame(SR = n, Results = t))
# 
# 
# model_files
# 
# ms = do.call("rbind", ms)
# ms[, c(1, 4)]
# 
# 
# varImp(m@model$rf_ffs[[1]][[1]]$model$finalModel$importance)
# 
# var_imp <- compVarImp(m@model, scale = FALSE)
# var_imp_scale <- compVarImp(models, scale = TRUE)
# plotVarImp(var_imp)
# plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")
# tstat <- compContTests(models, mean = TRUE)
# summary(tstat[[2]])