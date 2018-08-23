library(gpm)
library(parallel)

ele = read.csv2("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/Biodiversity_Data_Marcel.csv")
#ele = ele[-c(6:11),]
#ele = ele[-c(7,24),]

setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/results/")

############### read file ##############################
filepath_gpm = "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean"

#mrg_tbl = read.table(paste0(filepath_gpm, 
#                            "/rs_veg_hara_species_turnover_moths_TO_NE_AC_df.csv"), 
#                     header = TRUE, sep = ";", dec = ",")
mrg_tbl = read.table(paste0(filepath_gpm, 
                            "/rs_veg_hara_species_turnover_moths_TO_NE_AC_df.csv"), 
                     header = TRUE, sep = ";", dec = ",")
########################################################
# build trophic level
head(mrg_tbl[, 1800:1819])

############### data manipulation|rm na ################
test100 <- colSums(mrg_tbl[,c(2:1687)]) 
test100_4 <- which(is.na(test100))
test101 <- as.numeric(test100_4)
mrg_tbl_no = seq(2:1687) 
mrg_tbl_tst <- mrg_tbl[,][-which(mrg_tbl_no %in% test101)]

any(is.na(mrg_tbl_tst))
which(is.na(mrg_tbl_tst))

test100 <- colSums(mrg_tbl_tst[,c(2:1143)]) #1111 #1143
test100_4 <- which(is.na(test100))
test101 <- as.numeric(test100_4)
#mrg_tbl_res <- mrg_tbl_tst[,-c(759,781,803,825,847,869,891,913,935,
#                               957,979,1001,1023,1045,1067,1089)]

mrg_tbl_res <- mrg_tbl_tst[,-c(759,761,783,785,807,809,831,833,855,857,
                               879,881,903,905,927,929,951,953,975,977,
                               999,1001,1023,1025,1047,1049,1071,1073,1095,1097,1119,1121)]

#any(is.na(mrg_tbl_res[,2:1095]))
#which(is.na(mrg_tbl_res[,2:1095]))

any(is.na(mrg_tbl_res[,2:1111]))
which(is.na(mrg_tbl_res[,2:1111]))
########################################################

mrg_tbl = mrg_tbl_res

############### data manipulation|define col ########### 
mrg_tbl$SelCat = substr(mrg_tbl$plotID, 1, 3)
mrg_tbl$SelNbr = as.numeric(substr(mrg_tbl$plotID, 4, 4))
col_selector = which(names(mrg_tbl) %in% c("SelCat", "SelNbr"))
#col_diversity = seq(grep("SRmammals", names(mrg_tbl)), 
#                      grep("SRallplants", names(mrg_tbl)))
col_diversity = seq(grep("ants_jtu_NMDS1", names(mrg_tbl)), 
                    grep("rosids_jac_NMDS2", names(mrg_tbl)))
col_diversity = c(col_diversity[1]-1, col_diversity)
#col_precitors = seq(grep("rs_sd_miv", names(mrg_tbl)), 
#                    grep("SR_smpl_inverse_difference_moment_5_sd", 
#                         names(mrg_tbl)))
col_precitors = seq(grep("rs_sd_miv", names(mrg_tbl)), 
                    grep("SR_smpl_inverse_difference_moment_30_sd", 
                         names(mrg_tbl)))
col_meta <- seq(length(mrg_tbl))[-c(col_selector, col_diversity, 
                                    col_precitors)]
########################################################


############### compute residues #######################
mrg_tbl_res = mrg_tbl 
mrg_tbl_res = subset(mrg_tbl_res, substr(mrg_tbl_res$plotID, 1, 3) != "fed" &
                       mrg_tbl_res$plotID != "gra6")

mrg_tbl_res_tst = mrg_tbl_res[,c(1122:1149)]
mrg_tbl_res$elevation = ele$elevation

results = list()
for(i in col_diversity){      
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
########################################################


############### create GPM object ######################
meta <- createGPMMeta(mrg_tbl, type = "input",
                      selector = col_selector,
                      response = col_diversity,
                      predictor = col_precitors,
                      meta = col_meta)

mrg_tbl_gpm <- gpm(mrg_tbl, meta, scale = TRUE)
########################################################

mrg_tbl_gpm = mrg_tbl_res_gpm

############### set predictor variables ################
sel = c(colnames(mrg_tbl_gpm@data$input)[
          grep(glob2rx("*mean*"), 
               colnames(mrg_tbl_gpm@data$input))],
        colnames(mrg_tbl_gpm@data$input)[
          grep(glob2rx("*sd*"), 
               colnames(mrg_tbl_gpm@data$input))])

mrg_tbl_gpm@meta$input$PREDICTOR_FINAL = sel

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

wharas = which(str_sub(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL, start = -2) == "sd")
wharasd = unlist(strsplit(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[wharas], "_sd",
                          nchar(mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[wharas])))

####
mrg_tbl_gpm@meta$input$PREDICTOR_FINAL = 
  c(paste0("mean_", unique(c(wmeans, wsds))), 
    paste0("sd_", unique(c(wmeans, wsds))),
    paste0(unique(c(whara, wharasd)), "_mean"),
    paste0(unique(c(whara, wharasd)), "_sd"),
    "rs_sd_miv", "spec_div_mean_dis")

#mrg_tbl_gpm@meta$input$PREDICTOR_FINAL = 
#  mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[-c(101,139)] #c(112,150) #101,139

#mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[1]
#mrg_tbl_gpm@data$input$mean_CRI1

#mrg_tbl_gpm@meta$input$PREDICTOR_FINAL = 
#  mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[-c(42,43,45,46,48,49,51,52,54,55,
#                                            57,58,60,61,64,65,67,68,70,71,
#                                            73,74,76,77,79,80,82,83,85,86,
#                                            88,89,90,
#                                            92,93,95,96,98,99,101,102,104,105,
#                                            107,108,110,111,114,115,117,118,120,121,
#                                            123,124,126,127,129,130,132,133,135,136,
#                                            138,139,140)]

#mrg_tbl_gpm@meta$input$PREDICTOR_FINAL = 
#  mrg_tbl_gpm@meta$input$PREDICTOR_FINAL[-c(41,44,47,50,53,56,59,62,63,
#                                            66,69,72,75,78,81,84,87,
#                                            92,95,98,101,104,
#                                            107,110,113,114,117,120,
#                                            123,126,129,132,135,138)]


#mrg_tbl_gpm@meta$input$PREDICTOR_FINAL =
#  c("SR_smpl_inertia_10_mean","mean_PSRI","sd_ClAInt","sd_GDVI_2",
#    "NDVI_hgr_short_run_low_grey_level_emphasis_5_mean","NDVI_smpl_inertia_2_mean",
#    "mean_Gitelson","SR_smpl_inertia_10_sd","CARI_hgr_short_run_emphasis_5_sd",
#    "NDVI_smpl_energy_10_mean","mean_mND705","mean_RDVI","SR_smpl_energy_5_sd","sd_RDVI",
#    "sd_EVI","SR_hgr_short_run_low_grey_level_emphasis_5_mean","mean_TCARI2.OSAVI2",
#    "SR_hgr_short_run_emphasis_5_sd","sd_Green.NDVI","SIPI_smpl_haralick_correlation_5_mean",
#    "rs_sd_miv","spec_div_mean_dis","sd_Carter","sd_DD","mean_GDVI_2",
#    "CARI_hgr_short_run_low_grey_level_emphasis_30_mean","SR_smpl_haralick_correlation_2_mean",
#    "SR_smpl_haralick_correlation_2_sd","mean_DD","mean_Carter4","mean_GI",
#    "sd_Vogelmann4")

mrg_tbl_gpm@meta$input$PREDICTOR_FINAL =
c("sd_PWI","SIPI_hgr_grey_level_nonuniformity_30_mean","sd_SPVI",
  "NDVI_hgr_grey_level_nonuniformity_2_mean","sd_Datt",
  "SIPI_hgr_short_run_emphasis_5_sd","mean_SPVI","sd_mSR705",
  "NDVI_hgr_short_run_emphasis_5_sd","NDVI_smpl_haralick_correlation_30_sd",
  "SR_smpl_energy_2_sd","SR_hgr_grey_level_nonuniformity_2_mean",
  "SIPI_smpl_inverse_difference_moment_5_mean",
  "NDVI_hgr_grey_level_nonuniformity_30_mean","CARI_hgr_grey_level_nonuniformity_5_mean",
  "mean_EVI","mean_SR8","mean_Datt","mean_TGI","SR_smpl_haralick_correlation_2_sd",
  "NDVI_hgr_grey_level_nonuniformity_10_mean","CARI_smpl_haralick_correlation_30_sd",
  "CARI_smpl_haralick_correlation_2_sd","CARI_smpl_haralick_correlation_10_sd",
  "sd_Carter","mean_Green.NDVI","sd_mSR","mean_mSR","mean_CRI2",
  "sd_EVI","sd_DWSI4","mean_PRI","CARI_hgr_grey_level_nonuniformity_2_mean","sd_TGI",
  "mean_PWI","SR_hgr_grey_level_nonuniformity_5_mean",
  "SIPI_hgr_grey_level_nonuniformity_10_mean","CARI_hgr_grey_level_nonuniformity_30_mean",
  "SR_hgr_short_run_emphasis_5_sd","mean_mSR705","SIPI_hgr_grey_level_nonuniformity_5_mean",
  "mean_DWSI4","SR_hgr_grey_level_nonuniformity_30_mean","mean_MTCI",
  "NDVI_smpl_haralick_correlation_2_sd","CARI_hgr_grey_level_nonuniformity_10_mean","sd_Green.NDVI",
  "SR_hgr_grey_level_nonuniformity_10_mean","SIPI_hgr_short_run_emphasis_10_sd","sd_mNDVI",
  "mean_PRI_norm","sd_MTCI","SIPI_smpl_cluster_shade_30_sd","SIPI_smpl_inertia_5_sd",
  "SIPI_smpl_haralick_correlation_2_sd","sd_CRI2","sd_TCARI2.OSAVI2",
  "SR_smpl_haralick_correlation_10_sd","mean_mNDVI","mean_REP_Li","SR_smpl_inertia_2_mean",
  "SR_hgr_grey_level_nonuniformity_5_sd","NDVI_hgr_grey_level_nonuniformity_5_sd",
  "CARI_smpl_haralick_correlation_2_mean","mean_Carter",
  "SIPI_hgr_grey_level_nonuniformity_5_sd","CARI_hgr_short_run_emphasis_5_sd",
  "SR_smpl_inertia_5_mean","SIPI_hgr_grey_level_nonuniformity_2_mean",
  "sd_PRI","CARI_smpl_cluster_shade_30_mean","NDVI_hgr_grey_level_nonuniformity_5_mean")  
    
## Check for NAs in predictor values
any(is.na(mrg_tbl_gpm@data$input[, mrg_tbl_gpm@meta$input$PREDICTOR_FINAL]))
which(is.na(mrg_tbl_gpm@data$input[, mrg_tbl_gpm@meta$input$PREDICTOR_FINAL]))
########################################################


############### iterate over all response variables ####
responses = mrg_tbl_gpm@meta$input$RESPONSE_FINAL
#responses = c("moths_jtu_NMDS1", "moths_jtu_NMDS2",
#              "moths_jne_NMDS1", "moths_jne_NMDS2",
#              "moths_jac_NMDS1", "moths_jac_NMDS2")
#########################################################


############### GPM ####################################
cl = makePSOCKcluster(15L)
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
  
  filepath = paste0(filepath_gpm, "hara_residues_species_richness_final_selection", rsp, ".rds")
  #filepath = paste0(filepath_gpm, "hara_res_species_betadiv_moths", rsp, ".rds")
  saveRDS(mrg_tbl_gpm_model, file = filepath)
})
########################################################

filepath_gpm_ffs_res = paste(filepath_gpm, 
                             "/gpm_ffs_res_hara_species_betadiv", 
                             sep = "")

filepath_gpm_ffs = paste(filepath_gpm, 
                             "/gpm_ffs_hara_species_betadiv", 
                             sep = "")

## Combine models in gpm objct
setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/gpm_ffs_res_hara_species_betadiv")
models = list.files(filepath_gpm_ffs_res)

mrg_tbl_res_gpm_model = readRDS(models[1])

for(i in seq(2, length(models))){
  temp = readRDS(models[i])
  mrg_tbl_res_gpm_model@model$pls_ffs[[i]] = temp@model$pls_ffs[[1]]
}
saveRDS(mrg_tbl_res_gpm_model, 
        file = "gpm_hara_species_betadiv_model_pls_ffs_res_180528.rds")


>mrg_tbl_res_gpm_model <- readRDS(paste0
                                 (filepath_gpm_ffs_res,
                                   "gpm_hara_species_betadiv_model_pls_ffs_res_moths_180528.rds"))

mrg_tbl_gpm_model = mrg_tbl_res_gpm_model

var_imp <- compVarImp(mrg_tbl_gpm_model@model[[1]], scale = FALSE)
var_imp_scale <- compVarImp(mrg_tbl_gpm_model@model[[1]], scale = TRUE)

#tst = do.call(rbind, var_imp_scale)
#library(stringr)
#tst2 = str_sort(tst$VARIABLE)
#unique(tst2)

var_imp_plot <- plotVarImp(var_imp)
var_imp_heat <- plotVarImpHeatmap(var_imp_scale, 
                                  xlab = "Species", ylab = "Band")

mrg_tbl_gpm_model@model$pls_ffs[[67]][[1]]$response
mrg_tbl_gpm_model@model$pls_ffs[[67]][[1]]$model

tstat <- compRegrTests(mrg_tbl_gpm_model@model[[1]], 
                       per_selector = TRUE)

#tstat_mean <- merge(tstat[[1]], mrg_tbl_res_gpm_model@model[[1]], 
#                     by.x = "Response", by.y="names")

overview = aggregate(tstat$r_squared, by = list(tstat$model_response), mean)
colnames(overview) = c("Species Richness", "r.sq")
#overview$r.sq_residuen_rd = round(overview$r.sq_residuen, 3)
overview[order(overview$r.sq),]

tstat_mean[order(tstat_mean$Kappa_mean, decreasing = TRUE),]
