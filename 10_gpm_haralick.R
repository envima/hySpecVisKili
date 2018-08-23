library(gpm)
library(doParallel)
library(mgcv)

setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/gpm_re_norm_pci_hara")

## Read files and build GPM object
filepath = "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/gpm_re_norm_pci_hara"
mrg_tbl = read.table(paste0(filepath, "/rs_veg_hara_biodiv_df.csv"), 
                     header = TRUE, sep = ";", dec = ",")

test100 <- colSums(mrg_tbl[,c(2:1802)]) #1111 #1687 #1095 #1802 #1143
test100_4 <- which(is.na(test100))
test101 <- as.numeric(test100_4)
mrg_tbl_no = seq(2:1802) #1802
#mrg_tbl <- mrg_tbl[,][-which(mrg_tbl_no %in% test101)]
mrg_tbl_tst <- mrg_tbl[,][-which(mrg_tbl_no %in% test101)]

any(is.na(mrg_tbl_tst))
which(is.na(mrg_tbl_tst))

test100 <- colSums(mrg_tbl_tst[,c(2:1143)]) #1143
test100_4 <- which(is.na(test100))
test101 <- as.numeric(test100_4)
mrg_tbl_res <- mrg_tbl_tst[,-c(339, 759, 783, 785, 807, 809, 833, 855, 879, 881, 903, 905,
                               927, 929, 951, 953, 975, 977, 999, 1001, 1023, 1025, 1047,
                               1049, 1071, 1073, 1095, 1097, 1119, 1121)]

mrg_tbl_no = seq(2:1143) #1802
#mrg_tbl <- mrg_tbl[,][-which(mrg_tbl_no %in% test101)]
mrg_tbl_tst2 <- mrg_tbl_tst[,][-which(mrg_tbl_no %in% test101)]

any(is.na(mrg_tbl_res[,2:1113]))
which(is.na(mrg_tbl_res[,2:1113]))
test100 <- colSums(mrg_tbl_res[,c(2:1113)])
test100_5 <- which(is.na(test100))

mrg_tbl <- mrg_tbl_res[,-c(759, 825, 849)]
any(is.na(mrg_tbl[,2:1083]))


mrg_tbl$selID = paste0("id_", substr(mrg_tbl$plotID, 4, 4))
col_selector = which(names(mrg_tbl) == "selID")
col_diversity = seq(grep("SRmammals", names(mrg_tbl)), 
                      grep("SRallplants", names(mrg_tbl)))
col_diversity = c(col_diversity[1]-1, col_diversity)
col_precitors = seq(grep("rs_sd_miv", names(mrg_tbl)), 
                    grep("SR_smpl_inverse_difference_moment_30_sd", names(mrg_tbl)))
#col_precitors = col_precitors[-which(col_precitors %in% 28)] # "IQR_M_OSAVI" || "min_GDVI_4"
col_meta <- seq(length(mrg_tbl))[-c(col_selector, col_diversity, col_precitors)]

## Compute resiudals
#mrg_tbl_res = mrg_tbl 
#results = list()

#for(i in col_diversity){
#  r = i  #luidich -1028
#  lmod = gam(mrg_tbl_res[, i] ~ s(mrg_tbl_res$elevation, k = 5))
#  results[[r]] = summary(lmod)
#  names(results)[[r]] = colnames(mrg_tbl_res)[i]
#  mrg_tbl_res[!is.na(mrg_tbl_res[, i]), i] = lmod$residuals
#  }

#r = do.call("rbind", lapply(seq(length(results)), function(i){
#  data.frame(NAME = names(results)[i], 
#             R = results[[i]]$r.sq)
#}))
#r = r[order(r$R),]

#meta <- createGPMMeta(mrg_tbl_res, type = "input",
#                      selector = col_selector,
#                      response = col_diversity,
#                      predictor = col_precitors,
#                      meta = col_meta)

#mrg_tbl_res_gpm <- gpm(mrg_tbl_res, meta, scale = TRUE)

meta <- createGPMMeta(mrg_tbl, type = "input",
                      selector = col_selector,
                      response = col_diversity,
                      predictor = col_precitors,
                      meta = col_meta)

mrg_tbl_gpm <- gpm(mrg_tbl, meta, scale = TRUE)

## Set predictor variables
sel = c(colnames(mrg_tbl_gpm@data$input)[
          grep(glob2rx("*mean*"), 
               colnames(mrg_tbl_gpm@data$input))],
        colnames(mrg_tbl_gpm@data$input)[
          grep(glob2rx("*sd*"), 
               colnames(mrg_tbl_gpm@data$input))])

mrg_tbl_gpm@meta$input$PREDICTOR_FINAL = sel
mrg_tbl_gpm@meta$input$PREDICTOR_FINAL

#huch <- mrg_tbl_gpm@data$input[, mrg_tbl_gpm@meta$input$PREDICTOR_FINAL][-which(is.na(mrg_tbl_gpm@data$input[, mrg_tbl_gpm@meta$input$PREDICTOR_FINAL]))]
#mrg_tbl_gpm <- gpm(huch, meta, scale = TRUE)

mrg_tbl_gpm <- cleanPredictors(x = mrg_tbl_gpm, nzv = TRUE,
                               highcor = TRUE, cutoff = 0.70,
                               rmvna = TRUE)

# Add mean and standard deviation for all leftover precitors
# Add rs_sd and spec_div_mean_dis in any case
wmean = which(substr(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL, 1, 4) == "mean")
wmeans = substr(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[wmean], 
               6, nchar(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[wmean]))
#test_wmean = substr(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[wmean], 
#               6, nchar(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[wmean]))
wsd = which(substr(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL, 1, 2) == "sd")
wsds = substr(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[wsd], 
             4, nchar(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[wsd]))

####
mrg_tbl_gpm@meta$input$PREDICTOR_FINAL = 
  c(paste0("mean_", unique(c(wmeans, wsds))), 
    paste0("sd_", unique(c(wmeans, wsds))))

## Check for NAs in predictor values
any(is.na(mrg_tbl_gpm@data$input[, mrg_tbl_gpm@meta$input$PREDICTOR_FINAL]))
which(is.na(mrg_tbl_gpm@data$input[, mrg_tbl_gpm@meta$input$PREDICTOR_FINAL]))

## Iterate over all response variables
responses = mrg_tbl_gpm@meta$input$RESPONSE_FINAL


#cl <- makeCluster(detectCores())
#cl <- makeCluster(16)
#registerDoParallel(cl)
library(pls)

mrg_tbl_gpm_list = lapply(responses, function(rsp){
  mrg_tbl_gpm@meta$input$RESPONSE_FINAL = rsp
  #rsp = mrg_tbl_res_gpm@meta$input$RESPONSE_FINAL 
  ## Remove NAs and compute resamples
  mrg_tbl_gpm@data$input = mrg_tbl_gpm@data$input[complete.cases(mrg_tbl_gpm@data$input[, mrg_tbl_gpm@meta$input$RESPONSE_FINAL]), ]
  mrg_tbl_gpm <- splitMultRespLSO(x = mrg_tbl_gpm, nbr = 1)
  
  mrg_tbl_gpm_model <- trainModel(x = mrg_tbl_gpm,
                                  n_var = NULL, 
                                  mthd = "pls",
                                  mode = "rfe",
                                  seed_nbr = 11, 
                                  cv_nbr = 5,
                                  var_selection = "indv",
                                  filepath_tmp = NULL,
                                  rerank = FALSE)
  
  filepath = paste0(filepath, "mrg_tbl_gpm_model_pci_hara_", rsp, "_.rds")
  saveRDS(mrg_tbl_gpm_model, file = filepath)
})


## Combine models in gpm objct
models = list.files(filepath, pattern = glob2rx("*mrg_tbl_gpm_model_*"), full.names = TRUE)

mrg_tbl_res_gpm_model = readRDS(models[1])

for(i in seq(2, length(models))){
  temp = readRDS(models[i])
  mrg_tbl_res_gpm_model@model$pls_rfe[[i]] = temp@model$pls_rfe[[1]]
}
saveRDS(mrg_tbl_res_gpm_model, file = paste0(filepath, "gpm_haralick_model_pls_2018-02-28.rds"))


var_imp <- compVarImp(mrg_tbl_res_gpm_model@model[[1]], scale = FALSE)

var_imp_scale <- compVarImp(mrg_tbl_res_gpm_model@model[[1]], scale = TRUE)

var_imp_plot <- plotVarImp(var_imp)

var_imp_heat <- plotVarImpHeatmap(var_imp_scale, xlab = "Species", ylab = "Band")

tstat <- compRegrTests(mrg_tbl_res_gpm_model@model[[1]])

# tstat_mean <- merge(tstat[[1]], mrg_tbl_res_gpm_model@model[[1]], 
#                     by.x = "Response", by.y="names")
overview = aggregate(tstat$r_squared, by = list(tstat$model_response), mean)
overview[order(overview$x),]
tstat_mean[order(tstat_mean$Kappa_mean, decreasing = TRUE),]
