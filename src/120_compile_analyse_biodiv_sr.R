# Combine species richness model results in one variable.

require(envimaR)
root_folder = path.expand("~/plygrnd/hySpecVisKili/")
source(file.path(root_folder, "hySpecVisKili/src/000_setup_windows.R"))
dir.create(envrmt$path_120_compile_analysis_sr, showWarnings = FALSE)


# Combine all models into one gpm object
# mtypes = c("gam", "pls", "rf")
mtypes = c("gam", "pls", "rf")



all_models = lapply(mtypes, function(mt){
  ptypes = c("*elui*", 
             "*spec*", "*elsp*", 
             "*lidr*", "*eldr*", 
             "*splr*", "*esld*", 
             "*kmra*")
  if(mt == "gam"){
    ptypes = c("*elev*", ptypes)
  }
  all_pmodels = lapply(ptypes, function(pt){
    model_files = list.files(envrmt$path_110_model_gpm_sr, full.names = TRUE,
                             pattern = glob2rx(paste0(pt, mt, "*")))
    
    all_models = readRDS(model_files[[1]])
    
    for(i in (seq(2, length(model_files)))){
      all_models@model[[1]][[i]] = readRDS(model_files[[i]])@model[[1]][[1]]
    }
    
    return(all_models)
  })
  names(all_pmodels) = gsub("[*]", "", ptypes)
  return(all_pmodels)
})
names(all_models) = gsub("[*]", "", gsub("_", "", mtypes))

saveRDS(all_models, file = file.path(envrmt$path_120_compile_analysis_sr, 
                                     "models_sr.rds"))


