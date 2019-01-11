# Combine hyperspectral predictores and biodiversity variables in gpm class.
if(Sys.info()["sysname"] == "Windows"){
  filepath_base = "C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R"
} else {
  filepath_base = "/mnt/sd19006/data/users/tnauss/KI-Hyperspec/HySpec_KiLi/src/000_set_environment_linux.R"
}
source(filepath_base)

dir.create(path_compile_analysis_sr_elev_res, showWarnings = FALSE)


# Combine all models into one gpm object
ptypes = c("*elui*", "*kmra*", "*spec*", "*elsp*")
mtypes = c("gam_none", "*_gam_*", "*_pls_*", "*_rf_*")

all_models = lapply(mtypes, function(mt){
  all_pmodels = lapply(ptypes, function(pt){
    use_model_nbr = 2
    if(mt == "gam_none"){
      use_model_nbr = 1
      model_files = list.files(path_model_gpm_sr_elev_res, full.names = TRUE,
                               pattern = glob2rx(paste0(pt, "*_gam_*")))
    } else {
      
      model_files = list.files(path_model_gpm_sr_elev_res, full.names = TRUE,
                               pattern = glob2rx(paste0(pt, mt)))
    }
    
    
    all_models = readRDS(model_files[[1]])
    if(mt == "gam_none"){
      all_models@model$gam_ffs = NULL
    } else {
      all_models@model$gam_none = NULL
    }
    
    for(i in (seq(2, length(model_files)))){
      all_models@model[[1]][[i]] = readRDS(model_files[[i]])@model[[use_model_nbr]][[1]]
    }
    
    return(all_models)
  })
  names(all_pmodels) = gsub("[*]", "", ptypes)
  return(all_pmodels)
})
names(all_models) = gsub("[*]", "", gsub("_", "", mtypes))

saveRDS(all_models, file = file.path(path_compile_analysis_sr_elev_res, 
                                     "models_sr_elev_res.rds"))


