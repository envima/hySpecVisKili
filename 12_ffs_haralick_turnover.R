library(gpm)
library(CAST)

setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean")

## Read files and build GPM object
filepath_gpm = "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/"
mrg_tbl = read.table(paste0(filepath_gpm, 
                            "veg_beta_diversity_all_taxa_ffs_TO_NE_AC_df.csv"), 
                     header = TRUE, sep = ";", dec = ",")

mrg_tbl$SelCat = substr(mrg_tbl$plotID, 1, 3)
mrg_tbl$SelNbr = as.numeric(substr(mrg_tbl$plotID, 4, 4))
col_selector = which(names(mrg_tbl) %in% c("SelCat", "SelNbr"))
col_diversity = seq(grep("ants_jtu_NMDS1", names(mrg_tbl)), 
                      grep("rosids_jac_NMDS2", names(mrg_tbl)))
col_predictors = seq(grep("mean_Carter", names(mrg_tbl)), 
                    grep("sd_TGI", names(mrg_tbl)))
col_meta = seq(length(mrg_tbl))[-c(col_selector, col_diversity, col_predictors)]


meta = createGPMMeta(mrg_tbl, type = "input",
                      selector = col_selector, 
                      response = col_diversity,
                      predictor = col_predictors, 
                      meta = NULL)

mrg_tbl_gpm = gpm(mrg_tbl, meta, scale = TRUE)
#mrg_tbl_gpm = createIndexFolds(x = mrg_tbl_gpm, nbr = 1)


## Iterate over all response variables
responses = mrg_tbl_gpm@meta$input$RESPONSE_FINAL


mrg_tbl_gpm_list = lapply(responses, function(rsp){
  mrg_tbl_gpm@meta$input$RESPONSE_FINAL = rsp
  ## Remove NAs and compute resamples
  mrg_tbl_gpm@data$input = mrg_tbl_gpm@data$input[complete.cases(mrg_tbl_gpm@data$input[, mrg_tbl_gpm@meta$input$RESPONSE_FINAL]), ]
  #mrg_tbl_gpm = splitMultRespLSO(x = mrg_tbl_gpm, nbr = 1)
  mrg_tbl_gpm = createIndexFolds(x = mrg_tbl_gpm, nbr = 1) #nested_cv = FALSE)
  
  mrg_tbl_gpm_ffs_model = trainModel(x = mrg_tbl_gpm,
                                  metric = "RMSE",
                                  n_var = NULL, 
                                  mthd = "pls",
                                  mode = "ffs",
                                  seed_nbr = 11, 
                                  cv_nbr = 5,
                                  var_selection = "indv")
  
  filepath = paste0(filepath_gpm, rsp, ".rds")
  saveRDS(mrg_tbl_gpm_ffs_model, file = filepath)
})

saveRDS(mrg_tbl_gpm, file = "mrg_tbl_gpm_ffs_all_taxa.rds")

filepath = paste(filepath_gpm, "gpm_ffs_40", sep = "")

## Combine models in gpm objct
models = list.files(filepath, pattern = glob2rx("*.rds"), full.names = TRUE)

mrg_tbl_gpm_model = readRDS(models[1])

for(i in seq(2, length(models))){
  temp = readRDS(models[i])
  mrg_tbl_gpm_model@model$pls_rfe[[i]] = temp@model$pls_ffs[[1]]
}
saveRDS(mrg_tbl_gpm_model, file = paste0(filepath, "gpm_ffs_40_2018-05-11.rds"))


#filepath = paste(filepath_gpm, "residuen", sep = "")
#mrg_tbl_res_gpm_model <- readRDS(paste0(filepath_gpm, "gpm_pci_hara_res_model_pls_2018-03-12.rds"))
mrg_tbl_gpm_model <- readRDS(paste0(filepath_gpm, "gpm_ffs_40_2018-05-11.rds"))

compVarImp <- function(models, scale = FALSE){
  lapply(models, function(x){
    vi_species1 <- lapply(x, function(y){
      # vi <- varImp(y$model$fit, scale = FALSE)   #war: var_Imp(y$model$fit, scale = FALSE)
      if(inherits(y$model, "try-error")){
        NULL
      } else {
        vi <- caret::varImp(y$model)
        class(vi)
        if(inherits(vi, "varImp.train")){
          vi = vi$importance
        }
        if(scale == TRUE){
          vi <- vi / max(vi)
        }
        variables <- rownames(vi)
        vi <- data.frame(RESPONSE = y$response,
                         VARIABLE = variables,
                         IMPORTANCE = vi$Overall)
      }
    })
    
    vi_species <- do.call("rbind", vi_species1)
    if(is.null(vi_species)){
      vi <- NULL
    } else {
      #return(vi_species)
      #}
      #})
      #}
      #vi_count <- vi_species %>% count(VARIABLE)
      vi_count <- vi_species %>% dplyr::count(VARIABLE)
      #vi_mean <- vi_species %>% group_by(RESPONSE) %>% summarise(avg = mean(IMPORTANCE))
      vi_mean <- ddply(vi_species, "VARIABLE", summarise, mean = mean(IMPORTANCE))
      vi <- merge(vi_count, vi_mean)
      vi$RESPONSE <- vi_species$RESPONSE[1]
      vi <- vi[order(vi$mean, decreasing = TRUE), ,drop = FALSE]
    }
    return(vi)
  })
}

#mrg_tbl_gpm_model = mrg_tbl_res_gpm_model

var_imp <- compVarImp(mrg_tbl_gpm_model@model[[2]], scale = FALSE)
var_imp_scale <- compVarImp(mrg_tbl_gpm_model@model[[2]], scale = TRUE)

tst = do.call(rbind, var_imp_scale)
library(stringr)
tst2 = str_sort(tst$VARIABLE)
unique(tst2)

var_imp_plot <- plotVarImp(var_imp)

var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")

tstat <- compRegrTests(mrg_tbl_gpm_model@model[[2]])

#tstat_mean <- merge(tstat[[1]], mrg_tbl_res_gpm_model@model[[1]], 
#                     by.x = "Response", by.y="names")

overview = aggregate(tstat$r_squared, by = list(tstat$model_response), mean)
colnames(overview) = c("Species Richness", "r.sq")
#overview$r.sq_residuen_rd = round(overview$r.sq_residuen, 3)
overview[order(overview$r.sq),]
