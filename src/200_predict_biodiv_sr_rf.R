# Predict species richness using different models and predictor sets
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


dir.create(paste0(path_model_gpm_sr), showWarnings = FALSE)

comb = readRDS(paste0(path_comb_gpm_sr, "ki_hyperspec_biodiv_non_scaled.rds"))


# Predict with all elevation and lui information, hyperspectral data only, 
# all data, and  kmdc and raoq only using gam, pls and rf models.
mtypes = c("*gam*", "*pls*", "*rf*")
ptypes = c("*elui*", "*spec*", "*elsp*", "*kmra*")

mt = mtypes[3]
pt = ptypes[4]

for(mt in mtypes){
  for(pt in ptypes){
    
    if(pt == "*elui*"){
      comb@meta$input$PREDICTOR_FINAL = comb@meta$input$PREDICTOR[c(1:7)]
    } else if(pt == "*spec*"){ 
      comb@meta$input$PREDICTOR_FINAL = comb@meta$input$PREDICTOR[-c(1:7)]
    } else if(pt ==  "*elsp*"){ 
      comb@meta$input$PREDICTOR_FINAL = comb@meta$input$PREDICTOR    
    } else if(pt == "*kmra*"){
      comb@meta$input$PREDICTOR_FINAL = unique(comb@meta$input$PREDICTOR[
        c(grep("kmdc", comb@meta$input$PREDICTOR), 
          grep("raoq", comb@meta$input$PREDICTOR))])
    }
    
    compModels(model = comb, pt = pt, mt = mt, outpath = path_model_gpm_sr)
  }
}


stopCluster(cl)
