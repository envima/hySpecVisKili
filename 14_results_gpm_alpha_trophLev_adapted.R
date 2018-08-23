library(gpm)
library(foreach)

#setwd("C:/Users/iotte/Desktop/test_control")
setwd("C:/Users/iotte/Desktop/results/final_run/alpha")

####
#filepath_gpm = "C:/Users/iotte/Desktop/test_control"
filepath_gpm = "C:/Users/iotte/Desktop/results/final_run/alpha"

###### alpha diversity ######
alpha = readRDS(paste0(filepath_gpm,
                       #"/alphadiv_Model_control.rds"))
                       "/gpm_results_trophLev_SD_hara_alpha_test_ffs.rds"))
# residues
alpha_res = readRDS(paste0(filepath_gpm,
                           #"/alphadiv_RES_Model_control.rds"))
                           "/gpm_results_trophLev_SD_hara_alpha_res_ffs.rds"))


#### results
mrg_tbl_gpm_model = alpha

var_imp_scale = compVarImp(mrg_tbl_gpm_model@model[[1]], scale = TRUE)

#mrg_tbl_gpm_model@model[[1]][[27]][[1]]$response
#mrg_tbl_gpm_model@model[[1]][[27]][[1]]$model

temp <- do.call("rbind", var_imp_scale)

##############
temp_3 = temp

#### rename levels for plotting
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_CARI"] <- "cellulose_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_MCARI"] <- "cellulose_mean"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Carter"] <- "plant_stress_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Carter2"] <- "plant_stress_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Carter3"] <- "plant_stress_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Carter4"] <- "plant_stress_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Carter6"] <- "plant_stress_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_DWSI4"] <- "plant_stress_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_SR2"] <- "plant_stress_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_SR3"] <- "plant_stress_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_SR6"] <- "plant_stress_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_SR7"] <- "plant_stress_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_SR8"] <- "plant_stress_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_PWI"] <- "plant_stress_mean"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_CRI1"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_CRI2"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_CRI3"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_CRI4"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_ClAInt"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Datt"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Datt2"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Datt4"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Datt5"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Datt6"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_DD"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_GDVI_2"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_GDVI_3"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_GDVI_4"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_GI"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Gitelson"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Gitelson2"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Green.NDVI"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Maccioni"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_mND705"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_mNDVI"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_mSR705"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_MTCI"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_NDVI2"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_NPCI"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_OSAVI"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_SPVI"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_TCARI.OSAVI"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_TCARI2"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_TCARI2.OSAVI2"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_TGI"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Vogelmann"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Vogelmann2"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_Vogelmann4"] <- "chlorophyll_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_REP_Li"] <- "chlorophyll_mean"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_PRI.CI2"] <- "carotenoid_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_PSRI"] <- "carotenoid_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_PSSR"] <- "carotenoid_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_SRPI"] <- "carotenoid_mean"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_PSND"] <- "pigment_mean"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_EVI"] <- "structure_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_RDVI"] <- "structure_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_SIPI"] <- "structure_mean"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_mSR"] <- "photosynthese_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_mSR2"] <- "photosynthese_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_PARS"] <- "photosynthese_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_PRI"] <- "photosynthese_mean"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "mean_PRI_norm"] <- "photosynthese_mean"


levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_CARI"] <- "cellulose_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_MCARI"] <- "cellulose_sd"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Carter"] <- "plant_stress_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Carter2"] <- "plant_stress_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Carter3"] <- "plant_stress_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Carter4"] <- "plant_stress_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Carter6"] <- "plant_stress_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_DWSI4"] <- "plant_stress_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_SR2"] <- "plant_stress_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_SR3"] <- "plant_stress_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_SR6"] <- "plant_stress_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_SR7"] <- "plant_stress_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_SR8"] <- "plant_stress_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_PWI"] <- "plant_stress_sd"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_CRI1"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_CRI2"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_CRI3"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_CRI4"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_ClAInt"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Datt"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Datt2"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Datt4"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Datt5"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Datt6"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_DD"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_GDVI_2"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_GDVI_3"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_GDVI_4"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_GI"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Gitelson"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Gitelson2"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Green.NDVI"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Maccioni"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_mND705"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_mNDVI"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_mSR705"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_MTCI"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_NDVI2"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_NPCI"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_OSAVI"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_SPVI"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_TCARI.OSAVI"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_TCARI2"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_TCARI2.OSAVI2"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_TGI"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Vogelmann"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Vogelmann2"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_Vogelmann4"] <- "chlorophyll_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_REP_Li"] <- "chlorophyll_sd"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_PRI.CI2"] <- "carotenoid_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_PSRI"] <- "carotenoid_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_PSSR"] <- "carotenoid_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_SRPI"] <- "carotenoid_sd"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_PSND"] <- "pigment_sd"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_EVI"] <- "structure_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_RDVI"] <- "structure_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_SIPI"] <- "structure_sd"

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_mSR"] <- "photosynthese_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_mSR2"] <- "photosynthese_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_PARS"] <- "photosynthese_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_PRI"] <- "photosynthese_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_PRI_norm"] <- "photosynthese_sd"

###############################################
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_adv_dissimilarity_2_mean"] <- "dissim_16A_mean_02"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_adv_dissimilarity_2_sd"] <- "dissim_16A_sd_02"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_adv_dissimilarity_10_mean"] <- "dissim_16A_mean_10"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_sum_average_2_sd"] <- "sumAvrg_08A_sd_02"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_adv_dissimilarity_5_mean"] <- "dissim_16A_mean_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_haralick_correlation_10_mean"] <- "corr_16S_mean_10"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_adv_mean_10_sd"] <- "mean_16A_sd_10"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_sum_entropy_5_mean"] <- "sumEntrpy_08A_mean_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_hgr_run_percentage_5_mean"] <- "runPer_08H_mean_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_smpl_energy_2_mean"] <- "enrgy_08S_mean_02"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_dissimilarity_5_sd"] <- "dissim_08A_sd_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_smpl_inverse_difference_moment_30_sd"] <- "invDiffMom_08S_sd_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_mean_30_sd"] <- "mean_08A_sd_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_sum_average_30_sd"] <- "sumAvrg_08A_sd_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_sum_average_30_mean"] <- "sumAvrg_08A_mean_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_haralick_correlation_30_sd"] <- "corr_16S_sd_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_haralick_correlation_10_sd"] <- "corr_16S_sd_10"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_hgr_run_length_nonuniformity_30_sd"] <- "runLNonuni_08H_sd_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_inverse_difference_moment_5_sd"] <- "invDiffMom_16S_sd_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_sum_entropy_10_sd"] <- "sumEntrpy_08A_sd_10"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_sum_entropy_5_sd"] <- "sumEntrpy_08A_sd_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_hgr_run_length_nonuniformity_5_sd"] <- "runLNonuni_08H_sd_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_smpl_entropy_30_mean"] <- "entrpy_08S_mean_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_sum_variance_5_sd"] <- "sumVar_08A_sd_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_smpl_haralick_correlation_30_mean"] <- "corr_08S_mean_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_cluster_shade_5_mean"] <- "clstrShade_16S_mean_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_adv_sum_entropy_5_sd"] <- "sumEntrpy_16A_sd_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_smpl_energy_10_sd"] <- "enrgy_08S_sd_10"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_hgr_run_length_nonuniformity_5_mean"] <- "runLNonuni_08H_mean_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_adv_sum_entropy_5_mean"] <- "sumEntrpy_16A_mean_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_dissimilarity_30_sd"] <- "dissim_08A_sd_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_smpl_entropy_30_sd"] <- "entrpy_08S_sd_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_adv_dissimilarity_5_sd"] <- "dissim_16A_sd_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_hgr_run_length_nonuniformity_30_mean"] <- "runLNonuni_08H_mean_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_haralick_correlation_30_mean"] <- "corr_16S_mean_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_cluster_shade_30_mean"] <- "clstrShade_16S_mean_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_smpl_energy_30_mean"] <- "enrgy_08S_mean_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_inverse_difference_moment_5_mean"] <- "invDiffMom_16S_mean_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_cluster_shade_5_sd"] <- "clstrShade_16S_sd_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_smpl_haralick_correlation_30_sd"] <- "corr_08S_sd_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_smpl_inverse_difference_moment_30_mean"] <- "invDiffMom_08S_mean_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_diff_variances_2_sd"] <- "diffVar_08A_sd_02"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_diff_variances_2_mean"] <- "diffVar_08A_mean_02"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_adv_dissimilarity_10_sd"] <- "dissim_16A_sd_10"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_haralick_correlation_5_mean"] <- "corr_16S_mean_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_hgr_run_percentage_30_sd"] <- "runPer_16H_sd_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_haralick_correlation_2_sd"] <- "corr_16S_sd_02"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_mean_30_mean"] <- "mean_08A_mean_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_dissimilarity_30_mean"] <- "dissim_08A_mean_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_haralick_correlation_2_mean"] <- "corr_16S_mean_02"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_sum_variance_5_mean"] <- "sumVar_08A_mean_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_haralick_correlation_5_sd"] <- "corr_16S_sd_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_adv_mean_10_mean"] <- "mean_16A_mean_10"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_hgr_run_percentage_5_sd"] <- "runPer_08H_sd_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_sum_entropy_30_mean"] <- "sumEntrpy_08A_mean_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_sum_entropy_10_mean"] <- "sumEntrpy_08A_mean_10"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_sum_entropy_30_sd"] <- "sumEntrpy_08A_sd_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_smpl_energy_2_sd"] <- "enrgy_08S_sd_02"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_dissimilarity_5_mean"] <- "dissim_08A_mean_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_smpl_energy_10_mean"] <- "enrgy_08S_mean_10"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_cluster_shade_30_sd"] <- "clstrShade_16S_sd_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_sum_average_2_mean"] <- "sumAvrg_08A_mean_02"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_hgr_run_percentage_30_mean"] <- "runPer_08H_mean_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_cluster_shade_10_sd"] <- "clstrShade_16S_sd_10"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_correlation_2_mean"] <- "corrS_16S_mean_02"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_cluster_prominence_5_sd"] <- "clstrProm_16S_sd_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_hgr_run_length_nonuniformity_10_sd"] <- "runLNonuni_08H_sd_10"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_hgr_run_length_nonuniformity_10_mean"] <- "runLNonuni_08H_mean_10"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_energy_5_sd"] <- "enrgy_16S_sd_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_cluster_shade_10_mean"] <- "clstrShade_16S_mean_10"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_sum_entropy_2_mean"] <- "sumEntrpy_08A_mean_02"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_smpl_energy_30_sd"] <- "enrgy_08S_sd_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_cluster_prominence_5_mean"] <- "clstrProm_16S_mean_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_energy_5_mean"] <- "enrgy_16S_mean_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_inverse_difference_moment_30_sd"] <- "invDiffMom_16S_sd_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_hgr_run_percentage_2_mean"] <- "runPer_08H_mean_02"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_correlation_2_sd"] <- "corrS_16S_sd_02"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_adv_sum_average_5_sd"] <- "sumAvrg_16A_sd_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_sum_average_5_sd"] <- "sumAvrg_08A_sd_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_hgr_run_length_nonuniformity_2_sd"] <- "runLNonuni_08H_sd_02"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_hgr_run_length_nonuniformity_2_mean"] <- "runLNonuni_08H_mean_02"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_hgr_run_percentage_10_mean"] <- "runPer_08H_mean_10"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_sum_average_5_mean"] <- "sumAvrg_08A_mean_05"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_smpl_inverse_difference_moment_30_mean"] <- "invDiffMom_16S_mean_30"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_hgr_run_percentage_10_sd"] <- "runPer_08H_sd_10"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_hgr_run_percentage_2_sd"] <- "runPer_08H_sd_02"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X08_adv_sum_entropy_2_sd"] <- "sumEntrpy_08A_sd_02"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "X16_adv_sum_average_5_mean"] <- "sumAvrg_16A_mean_05"

###############################################
temp_4 = subset(temp_3, temp_3$RESPONSE != "lui")
var_imp_scale = temp_4

plotVarImpHeatmapAlpha <- function (var_imp_scale, xlab = "Variable", ylab = "Method", vis_range = "minmax") 
{
  #temp <- do.call("rbind", var_imp_scale)
  temp <- var_imp_scale  
  temp$VARIABLE <- factor(temp$VARIABLE, levels = sort(as.character(unique(temp$VARIABLE))))
  temp$RESPONSE <- factor(temp$RESPONSE, 
                          levels(temp$RESPONSE)[c(3,17,7,21,4,6,
                                                  8:9,11,14,18,20,
                                                  22:23,26,28,24,27,
                                                  2,5,10,12:13,15,
                                                  16,19,25,1)])
  temp$trophLev = ifelse(temp$RESPONSE %in% c("SRmammals","SRanimals",
                                              "SRsyrphids","SRants","SRsnails"), 1,
                         ifelse(temp$RESPONSE %in% c("SRdungbeetles","SRmillipedes","SRcollembola"), 2,
                                ifelse(temp$RESPONSE %in% c("SRbees","SRmoths","SRorthoptera"), 3,
                                       ifelse(temp$RESPONSE %in% c("SRbirds","SRbats"), 5,
                                              ifelse(temp$RESPONSE %in% c("SRallplants",
                                                                          "SRasterids",
                                                                          "SRconifers",
                                                                          "SReudicots",
                                                                          "SRferns",
                                                                          "SRlycopodiopsida",
                                                                          "SRmagnoliids",
                                                                          "SRmonocots",
                                                                          "SRrosids"), 6, 
                                                     ifelse(temp$RESPONSE %in% "lui", 6, 4))))))
  temp$trophLev <- as.factor(temp$trophLev)
  levels(temp$trophLev)[levels(temp$trophLev) == 1] <- "generalist"
  levels(temp$trophLev)[levels(temp$trophLev) == 2] <- "decomposer"
  levels(temp$trophLev)[levels(temp$trophLev) == 3] <- "herbivore"
  levels(temp$trophLev)[levels(temp$trophLev) == 4] <- "predator"
  levels(temp$trophLev)[levels(temp$trophLev) == 5] <- "predator flying"
  levels(temp$trophLev)[levels(temp$trophLev) == 6] <- "plants"
  
  temp = temp %>%
    group_by(VARIABLE,trophLev) %>%
    summarise(avg = mean(mean)) %>%
    arrange(avg)
  colnames(temp) = c("VARIABLE", "trophLev", "mean")
  
  tempTrop = temp %>%
    group_by(trophLev) %>%
    summarise(avg = mean(mean)) %>%
    arrange(avg)
  levels_trophLev = as.character(tempTrop$trophLev)
  temp$trophLev <- factor(temp$trophLev,levels = levels_trophLev)
  
  tempVar = temp %>%
    group_by(VARIABLE) %>%
    summarise(avg = mean(mean)) %>%
    arrange(avg)
  levels_Var = as.character(tempVar$VARIABLE)
  temp$VARIABLE <- factor(temp$VARIABLE, levels = c(levels_Var))
  
  if (vis_range == "minmax") {
    #vis_range <- c(min(temp$mean), max(temp$mean))
    vis_range <- c(0, 1)
  }
  clr <- colorRampPalette(brewer.pal(9, "YlOrRd"))
  #clr <- colorRampPalette(rev(brewer.pal(10, "Spectral")))
  lattice::levelplot(mean ~ trophLev * VARIABLE, data = temp, 
                     #lattice::levelplot(mean ~ trophLev * VARIABLE, data = temp, 
                     col.regions = clr(20), at = seq(vis_range[1], vis_range[2], 
                                                      length.out = 20), asp = 1, 
                     as.table = TRUE, ylab = ylab, 
                     xlab = xlab, scales = list(x = list(rot = 0)), 
                     main = "Variable Importance > 0.75 Alpha Diversity Trophic Levels", 
                     cex.title = 0.5, 
                     cex.axis.text = .01, 
                     colorkey = list(space = "top", width = 1, 
                                     height = 0.75), 
                     panel = function(...) {
                       grid::grid.rect(gp = grid::gpar(col = NA, fill = "grey60"))
                       panel.levelplot(...)
                       })
}

#### var imp > .75
var_imp_scale_75 = subset(var_imp_scale, var_imp_scale$mean >= .75)


var_imp_heat = plotVarImpHeatmapAlpha(var_imp_scale, 
                                      xlab = "", 
                                      ylab = "")

tiff("alpha075_grouped_trophLev.tiff", 
     width = 40, height = 30, units = "cm", res = 1200, 
     compression = "lzw")
plot.new()
print(var_imp_heat, newpage = FALSE)
dev.off()

png("alpha075_grouped_trophLev.png", 
    width = 40, height = 30, units = "cm", res = 1200)
plot.new()
print(var_imp_heat, newpage = FALSE)
dev.off()
vars = as.character(unique(var_imp_scale$VARIABLE))

##### trophic levels
# 1 generalist
var_imp_scale_gen = subset(var_imp_scale,
                           var_imp_scale$RESPONSE == "SRmammals")

var_imp_scale_gen_75 = subset(var_imp_scale_gen, var_imp_scale_gen$mean >= .75)

var_imp_heat_gen = plotVarImpHeatmapAlpha(var_imp_scale_gen, 
                                          xlab = "", ylab = "")


# 2 decomposer
var_imp_scale_deco = subset(var_imp_scale,
                             var_imp_scale$RESPONSE == "SRdungbeetles"|
                             var_imp_scale$RESPONSE == "SRmillipedes"|
                             var_imp_scale$RESPONSE == "SRcollembola")

var_imp_scale_deco_75 = subset(var_imp_scale_deco, var_imp_scale_deco$mean >= .75)

var_imp_heat_deco = plotVarImpHeatmapAlpha(var_imp_scale_deco, 
                                          xlab = "", ylab = "")
# 3 herbivore
var_imp_scale_herb = subset(var_imp_scale,
                            var_imp_scale$RESPONSE == "SRbees"|
                              var_imp_scale$RESPONSE == "SRmoths"|
                              var_imp_scale$RESPONSE == "SRorthoptera")

var_imp_scale_herb_75 = subset(var_imp_scale_herb, var_imp_scale_herb$mean >= .75)

var_imp_heat_herb = plotVarImpHeatmapAlpha(var_imp_scale_herb, 
                                           xlab = "", ylab = "")
# 4 predators
var_imp_scale_pred = subset(var_imp_scale,
                            var_imp_scale$RESPONSE == "SRspiders"|
                              var_imp_scale$RESPONSE == "SRheteroptera"|
                              var_imp_scale$RESPONSE == "SRotheraculeata"|
                              var_imp_scale$RESPONSE == "SRparasitoids"|
                              var_imp_scale$RESPONSE == "SRothercoleoptera")

var_imp_scale_pred_75 = subset(var_imp_scale_pred, var_imp_scale_pred$mean >= .75)

var_imp_heat_pred = plotVarImpHeatmapAlpha(var_imp_scale_pred, 
                                           xlab = "", ylab = "")
# 5 flying predators
var_imp_scale_bb = subset(var_imp_scale,
                            var_imp_scale$RESPONSE == "SRbats"|
                              var_imp_scale$RESPONSE == "SRbirds")

var_imp_scale_bb_75 = subset(var_imp_scale_bb, var_imp_scale_bb$mean >= .75)

var_imp_heat_bb = plotVarImpHeatmapAlpha(var_imp_scale_bb, 
                                           xlab = "", ylab = "")

# 6 plants
var_imp_scale_pla = subset(var_imp_scale,
                          var_imp_scale$RESPONSE == "SRallplants"|
                            var_imp_scale$RESPONSE == "SRasterids"|
                            var_imp_scale$RESPONSE == "SRconifers"|
                            var_imp_scale$RESPONSE == "SReudicots"|
                            var_imp_scale$RESPONSE == "SRferns"|
                            var_imp_scale$RESPONSE == "SRlycopodiopsida"|
                            var_imp_scale$RESPONSE == "SRmagnoliids"|
                            var_imp_scale$RESPONSE == "SRmonocots"|
                            var_imp_scale$RESPONSE == "SRrosids")

var_imp_scale_pla_75 = subset(var_imp_scale_pla, var_imp_scale_pla$mean >= .75)

var_imp_heat_pla = plotVarImpHeatmapAlpha(var_imp_scale_pla, 
                                         xlab = "", ylab = "")
