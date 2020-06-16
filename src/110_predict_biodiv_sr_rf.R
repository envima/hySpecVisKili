# Predict species richness using different models and predictor sets
if(Sys.info()["sysname"] == "Windows"){
  filepath_base = "D:/plygrnd/hySpecVisKili/hySpecVisKili/src/000_set_environment.R"
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

comb = readRDS(paste0(path_comb_gpm_sr, "ki_hyperspec_lidar_biodiv_non_scaled.rds"))


# Predict with all elevation and lui information, hyperspectral data only, 
# all data, and  kmdc and raoq only using gam, pls and rf models.
mtypes = c("gam", "pls", "rf")
mtypes = c("rf")
ptypes = c("*elui*", 
           "*spec*", "*elsp*", 
           "*lidr*", "*eldr*", 
           "*splr*", "*esld*", 
           "*kmra*")

mt = mtypes[1]
pt = ptypes[1]


elui_cols = seq(which(comb@meta$input$PREDICTOR == "elevation"), 
                which(comb@meta$input$PREDICTOR == "lui"))

spec_cols = seq(which(comb@meta$input$PREDICTOR == "CARI_mean"), 
                which(comb@meta$input$PREDICTOR == "pcai_kmdc_raoq_sd"))

ldr_cols = seq(which(comb@meta$input$PREDICTOR == "AGB"), 
                which(comb@meta$input$PREDICTOR == "scl_elevsq"))

for(mt in mtypes){
  for(pt in ptypes){
    
    if(pt == "*elui*"){ 
      comb@meta$input$PREDICTOR_FINAL = comb@meta$input$PREDICTOR[elui_cols]
      
    } else if(pt == "*spec*"){ 
      comb@meta$input$PREDICTOR_FINAL = comb@meta$input$PREDICTOR[spec_cols] 
      
    } else if(pt ==  "*elsp*"){
      comb@meta$input$PREDICTOR_FINAL = comb@meta$input$PREDICTOR[c(elui_cols, 
                                                                    spec_cols)] 
      
    } else if(pt ==  "*lidr*"){ 
      comb@meta$input$PREDICTOR_FINAL = comb@meta$input$PREDICTOR[ldr_cols]
      
    } else if(pt ==  "*eldr*"){ 
      comb@meta$input$PREDICTOR_FINAL = comb@meta$input$PREDICTOR[c(elui_cols, 
                                                                    ldr_cols)]
    
    } else if(pt ==  "*splr*"){ 
      comb@meta$input$PREDICTOR_FINAL = comb@meta$input$PREDICTOR[c(spec_cols,  
                                                                    ldr_cols)]
                                                                    
    } else if(pt ==  "*esld*"){ 
      comb@meta$input$PREDICTOR_FINAL = comb@meta$input$PREDICTOR[c(elui_cols,
                                                                    spec_cols,  
                                                                    ldr_cols)]
    } else if(pt == "*kmra*"){
      comb@meta$input$PREDICTOR_FINAL = unique(comb@meta$input$PREDICTOR[
        c(grep("kmdc", comb@meta$input$PREDICTOR), 
          grep("raoq", comb@meta$input$PREDICTOR))])
    }
    
    compModels(model = comb, pt = pt, mt = mt, outpath = path_model_gpm_sr)
  }
}


stopCluster(cl)
