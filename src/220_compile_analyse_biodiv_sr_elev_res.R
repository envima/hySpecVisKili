# Combine species richness residual model results in one variable.
source("D:/plygrnd/hySpecVisKili/hySpecVisKili/src/000_set_environment.R")


dir.create(path_compile_analysis_sr_elev_res, showWarnings = FALSE)


# Combine all models into one gpm object
pt = "*spec*"
mtypes = c("*rf*")
rtypes = c("*gam_elev_res*", "*pls_elui_res*", "*rf_elui_res*")
rtypes = c("*pls_elui_res*", "*rf_elui_res*")

all_models = lapply(mtypes, function(mt){
  all_pmodels = lapply(rtypes, function(rt){
    model_files = list.files(path_model_gpm_sr_res, full.names = TRUE,
                             pattern = glob2rx(paste0(mt, rt)))
    
    l = 0
    i = 0
    while(l == 0){
      i = i + 1
      all_models = readRDS(model_files[[i]])  
      l = length(all_models@model)
    }
    
    
    for(i in (seq(2, length(model_files)))){
      act_model = readRDS(model_files[[i]])
      if(length(act_model@model) > 0){
      all_models@model[[1]][[i]] = act_model@model[[1]][[1]]
      } else {
        all_models@model[[1]][[i]] = NA
      }
    }
    
    return(all_models)
  })
  names(all_pmodels) = gsub("[*]", "", rtypes)
  return(all_pmodels)
})
names(all_models) = gsub("[*]", "", gsub("_", "", mtypes))

saveRDS(all_models, file = file.path(path_compile_analysis_sr_elev_res, 
                                     "models_sr_elev_res.rds"))
