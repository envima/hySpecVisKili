library(gpm)
library(doParallel)
library(mgcv)

ele = read.csv2("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/Biodiversity_Data_Marcel.csv")
ele = ele[-c(6:11),]
ele = ele[-c(7,24),]

setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean")

## Read files and build GPM object
filepath_gpm = "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean"
mrg_tbl = read.table(paste0(filepath_gpm, "/rs_veg_hara_biodiv_df_NEW.csv"), 
                     header = TRUE, sep = ";", dec = ",")
mrg_tbl_tst = read.table(paste0(filepath_gpm, "/CopyOfrs_veg_hara_biodiv_df.csv"), 
                     header = TRUE, sep = ";", dec = ",")

########################################################
# build trophic level
biodiv = (mrg_tbl[, 1236:1262]) #1700:1726
biodiv[is.na(biodiv)] <- 0

biodiv$generalist = biodiv$SRmammals + biodiv$SRsyrphids + biodiv$SRants + biodiv$SRsnails
biodiv$decomposer = biodiv$SRdungbeetles + biodiv$SRmillipedes + biodiv$SRcollembola
biodiv$herbivore = biodiv$SRorthoptera + biodiv$SRbees + biodiv$SRmoths
biodiv$predator = biodiv$SRspiders + biodiv$SRheteroptera + biodiv$SRotheraculeata + 
  biodiv$SRparasitoids + biodiv$SRothercoleoptera
biodiv$bb = biodiv$SRbats + biodiv$SRbirds
biodiv$plants = biodiv$SRasterids + biodiv$SRconifers + biodiv$SReudicots + 
  biodiv$SRlycopodiopsida + biodiv$SRrosids + biodiv$SRferns + biodiv$SRmagnoliids + 
  biodiv$SRmonocots 

mrg_tbl = (mrg_tbl[, -c(1236:1262)]) #1700:1726
biodiv_smll = biodiv[, -c(1:27)]
mrg_tbl = cbind(mrg_tbl, biodiv_smll)

mrg_tbl = mrg_tbl[,-c(1224:1235)] #1688:1699
########################################################
test100 <- colSums(mrg_tbl[,c(2:1223)]) #1111 #1687 #1095 #1802 #1143 #1687
test100_4 <- which(is.na(test100))
test101 <- as.numeric(test100_4)
mrg_tbl_no = seq(2:1223) #1802 #1687
mrg_tbl_tst <- mrg_tbl[,][-which(mrg_tbl_no %in% test101)]

any(is.na(mrg_tbl_tst))
which(is.na(mrg_tbl_tst))

test100 <- colSums(mrg_tbl_tst[,c(2:1159)]) #1143 #1111
test100_4 <- which(is.na(test100))
test101 <- as.numeric(test100_4)
#mrg_tbl_res <- mrg_tbl_tst[,-c(339, 759, 783, 785, 807, 809, 833, 855, 879, 881, 903, 905,
#                               927, 929, 951, 953, 975, 977, 999, 1001, 1023, 1025, 1047,
#                               1049, 1071, 1073, 1095, 1097, 1119, 1121)]

#mrg_tbl_res <- mrg_tbl_tst[,-c(759, 781, 803, 825, 847, 869, 891, 913, 
#                               935, 957, 979, 1001, 1023, 1045, 1067, 1089)] 

mrg_tbl_res <- mrg_tbl_tst[,-c(779, 829, 879, 929, 979, 1029, 1079, 1129)] 

any(is.na(mrg_tbl_res[,2:1151])) #1095
which(is.na(mrg_tbl_res[,2:1151])) #1095

mrg_tbl = mrg_tbl_res

########################################################
mrg_tbl$SelCat = substr(mrg_tbl$plotID, 1, 3)
mrg_tbl$SelNbr = as.numeric(substr(mrg_tbl$plotID, 4, 4))
col_selector = which(names(mrg_tbl) %in% c("SelCat", "SelNbr"))

#col_diversity = seq(grep("generalist", names(mrg_tbl)), 
#                      grep("plants", names(mrg_tbl)))
col_diversity = seq(grep("SRmammals", names(mrg_tbl)), 
                      grep("SRallplants", names(mrg_tbl)))
col_diversity = c(col_diversity[1]-1, col_diversity)
col_precitors = seq(grep("rs_sd_miv", names(mrg_tbl)), 
                    grep("X16_smpl_inverse_difference_moment_30_sd", names(mrg_tbl)))
col_meta <- seq(length(mrg_tbl))[-c(col_selector, col_diversity, col_precitors)]

########################################################
## Compute resiudals
mrg_tbl_res = mrg_tbl 
mrg_tbl_res = subset(mrg_tbl_res, substr(mrg_tbl_res$plotID, 1, 3) != "fed" &
                       mrg_tbl_res$plotID != "gra6")

#mrg_tbl_res_tst = mrg_tbl_res[,c(1122:1149)]
mrg_tbl_res$elevation = ele$elevation

results = list()
for(i in col_diversity){      #ncol(mrg_tbl_res_tst)
  r = i 
  lmod = gam(mrg_tbl_res[, i] ~ s(mrg_tbl_res$elevation, k = 5))
  results[[r]] = summary(lmod, na.rm = TRUE)
  names(results)[[r]] = colnames(mrg_tbl_res)[i]
  mrg_tbl_res[!is.na(mrg_tbl_res[, i]), i] = lmod$residuals
  }

r = do.call("rbind", lapply(seq(length(results)), function(i){
  data.frame(NAME = names(results)[i], 
             R = results[[i]]$r.sq)
}))
r = r[order(r$R),]


meta <- createGPMMeta(mrg_tbl_res, type = "input",
                      selector = col_selector,
                      response = col_diversity,
                      predictor = col_precitors,
                      meta = col_meta)

mrg_tbl_res_gpm <- gpm(mrg_tbl_res, meta, scale = TRUE)
mrg_tbl_gpm = mrg_tbl_res_gpm

########################################################
meta <- createGPMMeta(mrg_tbl, type = "input",
                      selector = col_selector,
                      response = col_diversity,
                      predictor = col_precitors,
                      meta = col_meta)

mrg_tbl_gpm <- gpm(mrg_tbl, meta, scale = TRUE)

########################################################
## Set predictor variables
sel = c(colnames(mrg_tbl_gpm@data$input)[
  grep(glob2rx("*mean*"), 
       colnames(mrg_tbl_gpm@data$input))],
  colnames(mrg_tbl_gpm@data$input)[
    grep(glob2rx("*sd*"), 
         colnames(mrg_tbl_gpm@data$input))])

mrg_tbl_gpm@meta$input$PREDICTOR_FINAL = sel
mrg_tbl_gpm@meta$input$PREDICTOR_FINAL


mrg_tbl_gpm <- cleanPredictors(x = mrg_tbl_gpm, nzv = TRUE,
                               highcor = TRUE, cutoff = 0.70,
                               rmvna = TRUE)

# Add mean and standard deviation for all leftover precitors
# Add rs_sd and spec_div_mean_dis in any case
wmean = which(substr(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL, 1, 4) == "mean")
wmeans = substr(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[wmean], 
                6, nchar(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[wmean]))

wsd = which(substr(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL, 1, 2) == "sd")
wsds = substr(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[wsd], 
              4, nchar(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[wsd]))

wharam = which(str_sub(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL, start = -4) == "mean")
whara = unlist(strsplit(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[wharam], "_mean",
                        nchar(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[wharam])))
####
whara[1] <- "X08_adv_mean_02"
whara <- whara[-2]
whara[2] <- "X08_adv_mean_05"
whara <- whara[-3]
whara[6] <- "X08_adv_mean_30"
whara <- whara[-7]
whara[18] <- "X16_adv_mean_10"
whara <- whara[-19]

#####
wharas = which(str_sub(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL, start = -2) == "sd")
wharasd = unlist(strsplit(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[wharas], "_sd",
                          nchar(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[wharas])))

####
mrg_tbl_gpm@meta$input$PREDICTOR_FINAL = 
  c(paste0("mean_", unique(c(wmeans, wsds))), 
    paste0("sd_", unique(c(wmeans, wsds))),
    paste0(unique(c(whara, wharasd)), "_mean"),
    paste0(unique(c(whara, wharasd)), "_sd"))


mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[101:110] 
head(mrg_tbl_gpm@data$input[, mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[100:104]])

mrg_tbl_gpm@meta$input$PREDICTOR_FINAL = 
  mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[-c(37,38,71,72)] #"NDVI_smpl_inverse_difference_moment_5_sd" (150)
                                                  #"SR_smpl_inverse_difference_moment_10_sd" (res) 139


## Check for NAs in predictor values
any(is.na(mrg_tbl_gpm@data$input[, mrg_tbl_gpm@meta$input$PREDICTOR_FINAL]))
which(is.na(mrg_tbl_gpm@data$input[, mrg_tbl_gpm@meta$input$PREDICTOR_FINAL]))

#mrg_tbl_gpm@meta$input$PREDICTOR_FINAL = 
#  c("sd_PWI", "sd_Datt", "mean_TCARI2.OSAVI2", "sd_MTCI", "mean_SIPI",
#    "X08_adv_sum_entropy_30_sd", "X16_adv_dissimilarity_10_sd", "X08_adv_sum_entropy_5_sd",
#    "X08_adv_sum_entropy_10_mean", "sd_TGI", "sd_DWSI4", "mean_PWI",
#    "X08_hgr_run_percentage_5_sd", "mean_Green.NDVI", "sd_mSR", "X08_adv_sum_entropy_30_mean",
#    "X16_adv_dissimilarity_5_sd", "X08_adv_dissimilarity_5_sd", "sd_SPVI", 
#    "X16_smpl_haralick_correlation_5_sd", "X16_smpl_inverse_difference_moment_5_mean",
#    "X08_hgr_run_percentage_30_mean", "X16_adv_dissimilarity_2_mean", "X16_adv_sum_average_5_sd",
#    "mean_TGI", "mean_REP_Li", "sd_mSR705", "mean_PRI_norm", "X16_adv_dissimilarity_10_mean",
#    "mean_SPVI", "mean_mSR705", "X16_smpl_inverse_difference_moment_30_sd", "mean_PRI",
#    "X08_hgr_run_percentage_2_mean", "X16_smpl_haralick_correlation_30_sd", 
#    "X08_adv_sum_entropy_10_sd", "X16_smpl_haralick_correlation_10_mean",
#    "X08_smpl_entropy_30_sd", "X08_hgr_run_length_nonuniformity_2_mean",
#    "X08_hgr_run_percentage_5_mean", "X08_smpl_energy_30_mean", "X08_adv_sum_entropy_2_mean",
#    "X16_smpl_cluster_prominence_5_mean", "X08_adv_sum_entropy_5_mean", "sd_PRI", "sd_SIPI",
#    "X16_adv_dissimilarity_5_mean", "sd_EVI", "X08_smpl_entropy_30_mean", "sd_Datt2", 
#    "X16_smpl_cluster_prominence_5_sd", "X08_hgr_run_length_nonuniformity_10_sd",
#    "X16_smpl_cluster_shade_10_mean", "X08_hgr_run_percentage_2_sd", 
#    "X08_adv_dissimilarity_30_mean", "X08_hgr_run_length_nonuniformity_10_mean",
#    "mean_mNDVI", "X16_smpl_cluster_shade_10_sd", "sd_mNDVI", 
#    "X16_smpl_haralick_correlation_10_sd", "sd_REP_Li", "X08_adv_dissimilarity_30_sd")


mrg_tbl_gpm@meta$input$PREDICTOR_FINAL = 
  c("X16_smpl_haralick_correlation_5_mean", "X08_adv_dissimilarity_30_sd", "mean_TCARI2.OSAVI2",
    "X16_smpl_inverse_difference_moment_5_sd", "sd_Green.NDVI", 
    "X16_smpl_haralick_correlation_30_mean", "X16_smpl_haralick_correlation_30_sd",
    "X16_smpl_haralick_correlation_10_sd", "mean_Datt", "X08_adv_sum_entropy_10_sd",                 
    "X16_smpl_haralick_correlation_10_mean", "X16_adv_dissimilarity_5_sd", "sd_PRI",                                     
    "sd_DWSI4", "sd_REP_Li", "mean_mNDVI", "mean_PRI_norm", "mean_SR8", "mean_PWI", 
    "sd_Vogelmann", "sd_mSR705", "mean_PRI", "X16_smpl_haralick_correlation_2_sd", 
    "mean_mSR705", "sd_MTCI", "X16_smpl_cluster_shade_30_sd", "mean_MTCI", 
    "X08_smpl_inverse_difference_moment_30_mean", "X08_hgr_run_percentage_5_sd", 
    "X16_smpl_inverse_difference_moment_5_mean", "X16_adv_dissimilarity_10_mean", 
    "sd_SPVI", "X08_hgr_run_length_nonuniformity_5_sd", "X08_adv_diff_variances_2_mean", 
    "X16_adv_dissimilarity_2_mean", "sd_TCARI2.OSAVI2", "sd_PWI",                                    
    "X08_smpl_entropy_30_mean", "sd_mSR", "X08_adv_sum_variance_5_sd", "sd_Carter",                                 
    "sd_SR8", "sd_Datt", "X08_hgr_run_percentage_5_mean", "sd_EVI", 
    "X08_hgr_run_percentage_30_sd", "sd_mNDVI", "X16_adv_dissimilarity_2_sd",
    "X08_hgr_run_length_nonuniformity_5_mean", "X08_adv_sum_entropy_5_mean", 
    "X08_adv_dissimilarity_5_sd", "sd_PRI_norm", "X08_smpl_energy_2_mean",                    
    "X08_smpl_inverse_difference_moment_30_sd", "mean_DWSI4", "X08_adv_sum_average_30_sd",
    "mean_TGI", "sd_TGI", "X08_smpl_energy_2_sd", "X16_adv_dissimilarity_5_mean",
    "X08_hgr_run_percentage_30_mean", "X08_adv_diff_variances_2_sd")





########################################################
## Iterate over all response variables
responses = mrg_tbl_gpm@meta$input$RESPONSE_FINAL


cl = makePSOCKcluster(28L)
jnk = clusterEvalQ(cl, library(gpm))

clusterExport(cl, c("mrg_tbl_gpm", "filepath_gpm"))

mrg_tbl_gpm_list = parLapply(cl, responses, function(rsp){
  mrg_tbl_gpm@meta$input$RESPONSE_FINAL = rsp
  ## Remove NAs and compute resamples
  mrg_tbl_gpm@data$input = mrg_tbl_gpm@data$input[complete.cases(mrg_tbl_gpm@data$input[, mrg_tbl_gpm@meta$input$RESPONSE_FINAL]), ]
  mrg_tbl_gpm = createIndexFolds(x = mrg_tbl_gpm, nbr = 1)
  
  mrg_tbl_gpm_model <- trainModel(x = mrg_tbl_gpm,
                                  metric = "RMSE",
                                  n_var = NULL, 
                                  mthd = "pls",
                                  mode = "ffs",
                                  seed_nbr = 11, 
                                  cv_nbr = 5,
                                  var_selection = "indv")
  
  filepath = paste0(filepath_gpm, "/hara_SD_SR_alphadiv_RES_control", rsp, ".rds")
  saveRDS(mrg_tbl_gpm_model, file = filepath)
})

#filepath = paste(filepath, "residuen", sep = "")

## Combine models in gpm objct
models = list.files(filepath_gpm, pattern = glob2rx("*mrg_tbl_gpm*"), full.names = TRUE)

mrg_tbl_res_gpm_model = readRDS(models[1])

for(i in seq(2, length(models))){
  temp = readRDS(models[i])
  mrg_tbl_res_gpm_model@model$pls_rfe[[i]] = temp@model$pls_rfe[[1]]
}
saveRDS(mrg_tbl_res_gpm_model, file = paste0(filepath, "gpm_pci_hara_model_pls_2018-03-12.rds"))


filepath = paste(filepath_gpm, "residuen", sep = "")
mrg_tbl_res_gpm_model <- readRDS(paste0(filepath, "gpm_pci_hara_res_model_pls_2018-03-12.rds"))
mrg_tbl_gpm_model <- readRDS(paste0(filepath_gpm, "gpm_pci_hara_model_pls_2018-03-11.rds"))

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

var_imp <- compVarImp(mrg_tbl_gpm_model@model[[1]], scale = FALSE)
var_imp_scale <- compVarImp(mrg_tbl_gpm_model@model[[1]], scale = TRUE)

var_imp_plot <- plotVarImp(var_imp)

var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")

tstat <- compRegrTests(mrg_tbl_gpm_model@model[[1]])

#tstat_mean <- merge(tstat[[1]], mrg_tbl_res_gpm_model@model[[1]], 
#                     by.x = "Response", by.y="names")

overview = aggregate(tstat$r_squared, by = list(tstat$model_response), mean)
colnames(overview) = c("Species Richness", "r.sq")
#overview$r.sq_residuen_rd = round(overview$r.sq_residuen, 3)
overview[order(overview$r.sq),]

tstat_mean[order(tstat_mean$Kappa_mean, decreasing = TRUE),]
