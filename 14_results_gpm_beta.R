library(gpm)
library(foreach)

setwd("C:/Users/iotte/Desktop/test_control")

####
filepath_gpm = "C:/Users/iotte/Desktop/test_control"

###### beta diversity ######
beta = readRDS(paste0(filepath_gpm,
                      "/betadiv_Model_control.rds"))
# residues
beta_res = readRDS(paste0(filepath_gpm,
                          "/betadiv_RES_Model_control.rds"))



#### results
mrg_tbl_gpm_model = beta_res
var_imp_scale = compVarImp(mrg_tbl_gpm_model@model[[1]], scale = TRUE)

#mrg_tbl_gpm_model@model[[1]][[8]][[1]]$response
#mrg_tbl_gpm_model@model[[1]][[8]][[1]]$model


var_imp_scale_jac = var_imp_scale[c(1,2,7,8,13,14,19,20,25,26,
                                    31,32,37,38,43,44,49,50,55,
                                    56,61,62,67,68,73,74,79,80,85,86,91,92,
                                    97,98,103,104,109,110,115,
                                    116,121,122,127,128)]

var_imp_scale_jne = var_imp_scale[c(3,4,9,10,15,16,21,22,27,28,
                                    33,34,39,40,45,46,51,52,57,58,
                                    63,64,69,70,75,76,81,82,87,88,
                                    93,94,99,100,105,106,111,112,
                                    117,118,123,124,129,130)]

var_imp_scale_jtu = var_imp_scale[c(5,6,11,12,17,18,23,24,29,30,
                                    35,36,41,42,47,48,53,54,59,60,
                                    65,66,71,72,77,78,83,84,89,90,
                                    95,96,101,102,107,108,113,114,
                                    119,120,125,126,131,132)]


temp_3 <- do.call("rbind", var_imp_scale_jac)


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

levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_mSR"] <- "photosynthese_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_mSR2"] <- "photosynthese_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_PARS"] <- "photosynthese_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_PRI"] <- "photosynthese_sd"
levels(temp_3$VARIABLE)[levels(temp_3$VARIABLE) == "sd_PRI_norm"] <- "photosynthese_sd"

##############
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "bb_jac_NMDS1"] <- "predator_flying_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "bb_jac_NMDS2"] <- "predator_flying_NMDS2"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "deco_jac_NMDS1"] <- "decomposer_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "deco_jac_NMDS2"] <- "decomposer_NMDS2"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "gen_jac_NMDS1"] <- "generalist_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "gen_jac_NMDS2"] <- "generalist_NMDS2"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "herb_jac_NMDS1"] <- "herbivore_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "herb_jac_NMDS2"] <- "herbivore_NMDS2"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "pla_jac_NMDS1"] <- "plants_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "pla_jac_NMDS2"] <- "plants_NMDS2"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "pred_jac_NMDS1"] <- "predator_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "pred_jac_NMDS2"] <- "predator_NMDS2"



levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "mammals_jtu_NMDS1"] <- "generalist_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "mammals_jtu_NMDS2"] <- "generalist_NMDS2"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "syrphid_jtu_NMDS1"] <- "generalist_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "syrphid_jtu_NMDS2"] <- "generalist_NMDS2"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "ants_jtu_NMDS1"] <- "generalist_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "ants_jtu_NMDS2"] <- "generalist_NMDS2"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "gastropods_jtu_NMDS1"] <- "generalist_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "gastropods_jtu_NMDS2"] <- "generalist_NMDS2"

levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "dungbeetles_jtu_NMDS1"] <- "decomposer_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "dungbeetles_jtu_NMDS2"] <- "decomposer_NMDS2"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "millipedes_jtu_NMDS1"] <- "decomposer_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "millipedes_jtu_NMDS2"] <- "decomposer_NMDS2"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "collembola_jtu_NMDS1"] <- "decomposer_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "collembola_jtu_NMDS2"] <- "decomposer_NMDS2"

levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "bees_jtu_NMDS1"] <- "herbivore_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "bees_jtu_NMDS2"] <- "herbivore_NMDS2"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "orthoptera_jtu_NMDS1"] <- "herbivore_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "orthoptera_jtu_NMDS2"] <- "herbivore_NMDS2"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "moths_jtu_NMDS1"] <- "herbivore_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "moths_jtu_NMDS2"] <- "herbivore_NMDS2"

levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "aculeate_jtu_NMDS1"] <- "predator_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "aculeate_jtu_NMDS2"] <- "predator_NMDS2"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "beetles_jtu_NMDS1"] <- "predator_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "beetles_jtu_NMDS2"] <- "predator_NMDS2"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "parasitoids_jtu_NMDS1"] <- "predator_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "parasitoids_jtu_NMDS2"] <- "predator_NMDS2"

levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "bats_jtu_NMDS1"] <- "predator_flying_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "bats_jtu_NMDS2"] <- "predator_flying_NMDS2"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "birds_jtu_NMDS1"] <- "predator_flying_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "birds_jtu_NMDS2"] <- "predator_flying_NMDS2"

levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "asterids_jtu_NMDS1"] <- "plants_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "asterids_jtu_NMDS2"] <- "plants_NMDS2"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "conifers_jtu_NMDS1"] <- "plants_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "conifers_jtu_NMDS2"] <- "plants_NMDS2"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "eudicots_jtu_NMDS1"] <- "plants_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "eudicots_jtu_NMDS2"] <- "plants_NMDS2"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "ferns_jtu_NMDS1"] <- "plants_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "ferns_jtu_NMDS2"] <- "plants_NMDS2"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "magnoliids_jtu_NMDS1"] <- "plants_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "magnoliids_jtu_NMDS2"] <- "plants_NMDS2"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "monocots_jtu_NMDS1"] <- "plants_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "monocots_jtu_NMDS2"] <- "plants_NMDS2"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "mosses_jtu_NMDS1"] <- "plants_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "mosses_jtu_NMDS2"] <- "plants_NMDS2"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "rosids_jtu_NMDS1"] <- "plants_NMDS1"
levels(temp_3$RESPONSE)[levels(temp_3$RESPONSE) == "rosids_jtu_NMDS2"] <- "plants_NMDS2"

###

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



#######################
var_imp_scale_jac = temp_3

plotVarImpHeatmapBeta <- function (var_imp_scale_jac, 
                                   xlab = "Variable", 
                                   ylab = "Method", 
                                   vis_range = "minmax") 
{
  temp <- var_imp_scale_jac
  temp$VARIABLE <- factor(temp$VARIABLE, levels = sort(as.character(unique(temp$VARIABLE))))
  temp$RESPONSE <- factor(temp$RESPONSE, 
                          levels(temp$RESPONSE)[c(79:84,25:30,109:114,1:12,19:24,31:42,
                                                  49:54,67:72,85:90,103:108,115:120,
                                                  127:132,13:18,43:48,55:66,73:78,97:102,
                                                  91:96,121:126)])
  temp$RESPONSE <- factor(temp$RESPONSE,
                          levels(temp$RESPONSE)[c(3:4,15:16,31:32,
                                                  7:8,9:10,13:14,17:18,
                                                  37:38,41:42,23:24,5:6,
                                                  29:30,33:34,43:44,
                                                  11:12,39:40,19:22,1:2,
                                                  25:26,35:36)])
  #temp$trophLev = temp$RESPONSE
  
  #temp = temp %>%
  #  group_by(VARIABLE,trophLev) %>%
  #  summarise(avg = mean(mean)) %>%
  #  arrange(avg)
  #colnames(temp) = c("VARIABLE", "trophLev", "mean")
  
  #tempTrop = temp %>%
  #  group_by(trophLev) %>%
  #  summarise(avg = mean(mean)) %>%
  #  arrange(avg)
  #levels_trophLev = as.character(tempTrop$trophLev)
  #temp$trophLev <- factor(temp$trophLev,levels = levels_trophLev)
  #levels(temp$trophLev) = c("predator_NMDS1", "predator_NMDS2",
  #                          "herbivore_NMDS1", "herbivore_NMDS2",
  #                          "plants_NMDS1", "plants_NMDS2",
  #                          "predator_flying_NMDS1", "predator_flying_NMDS2",
  #                          "generalist_NMDS1", "generlaist_NMDS2",
  #                          "decomposer_NMDS1", "decomposer_NMDS2")
  
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
  lattice::levelplot(mean ~ RESPONSE * VARIABLE, data = temp, 
                     col.regions = clr(20), at = seq(vis_range[1], vis_range[2], 
                                                      length.out = 20), asp = 1, as.table = TRUE, ylab = ylab, 
                     xlab = xlab, scales = list(x = list(rot = 90)), 
                     main = "Variable Importance Beta Diversity JAC Residuals TAXA Levels", 
                     cex.title = 1, colorkey = list(space = "top", width = 1, 
                                                    height = 0.75), panel = function(...) {
                                                      grid::grid.rect(gp = grid::gpar(col = NA, fill = "grey60"))
                                                      panel.levelplot(...)
                                                    })
}

#### var imp > .75
var_imp_scale_75 = subset(var_imp_scale_jac, 
                          var_imp_scale_jac$mean >= .75)

var_imp_heat = plotVarImpHeatmapBeta(var_imp_scale_jac, 
                                      xlab = "", ylab = "")

tiff("betaRES_JAC_taxaLev.tiff", 
     width = 40, height = 30, units = "cm", res = 1200, 
     compression = "lzw")
plot.new()
print(var_imp_heat, newpage = FALSE)
dev.off()

png("betaRES_JAC_taxaLev.png", 
    width = 40, height = 30, units = "cm", res = 1200)
plot.new()
print(var_imp_heat, newpage = FALSE)
dev.off()
#vars = as.character(unique(var_imp_scale$VARIABLE))


##### trophic levels
###
# 1.1 generalist
var_imp_scale_gen_1 = subset(var_imp_scale_jtu,
                           var_imp_scale_jtu$RESPONSE == "generalist_NMDS1")

var_imp_scale_gen_1_75 = subset(var_imp_scale_gen_1, var_imp_scale_gen_1$mean >= .75)

var_imp_heat_gen_1 = plotVarImpHeatmapBeta(var_imp_scale_gen_1_75, 
                                          xlab = "", ylab = "")

### all

test_sb = var_imp_scale_jtu %>%
  group_by(RESPONSE) %>%
  subset(mean >= .75)

test_sb_df = as.data.frame(test_sb)
test_sb_df = as.list(test_sb)

test_sb_df_plt = foreach(i = seq(levels(test_sb_df$RESPONSE))) %do% {
  plotVarImpHeatmapBeta(test_sb_df[i])
}
  
# 1.2 generalist
var_imp_scale_gen_2 = subset(var_imp_scale_jtu,
                             var_imp_scale_jtu$RESPONSE == "generalist_NMDS2")

var_imp_scale_gen_2_75 = subset(var_imp_scale_gen_2, var_imp_scale_gen_2$mean >= .75)

var_imp_heat_gen_2 = plotVarImpHeatmapBeta(var_imp_scale_gen_2_75, 
                                           xlab = "", ylab = "")

###
# 2.1 decomposer
var_imp_scale_deco_1 = subset(var_imp_scale_jtu,
                             var_imp_scale_jtu$RESPONSE == "decomposer_NMDS1")

var_imp_scale_deco_1_75 = subset(var_imp_scale_deco_1, var_imp_scale_deco_1$mean >= .75)

var_imp_heat_deco_1 = plotVarImpHeatmapBeta(var_imp_scale_deco_1_75, 
                                           xlab = "", ylab = "")

# 2.2 decomposer
var_imp_scale_deco_2 = subset(var_imp_scale_jtu,
                             var_imp_scale_jtu$RESPONSE == "decomposer_NMDS2")

var_imp_scale_deco_2_75 = subset(var_imp_scale_deco_2, var_imp_scale_deco_2$mean >= .75)

var_imp_heat_deco_2 = plotVarImpHeatmapBeta(var_imp_scale_deco_2_75, 
                                           xlab = "", ylab = "")

###
# 3.1 herbivore
var_imp_scale_herb_1 = subset(var_imp_scale_jtu,
                              var_imp_scale_jtu$RESPONSE == "herbivore_NMDS1")

var_imp_scale_herb_1_75 = subset(var_imp_scale_herb_1, var_imp_scale_herb_1$mean >= .75)

var_imp_heat_herb_1 = plotVarImpHeatmapBeta(var_imp_scale_herb_1_75, 
                                            xlab = "", ylab = "")

# 3.2 herbivore
var_imp_scale_herb_2 = subset(var_imp_scale_jtu,
                              var_imp_scale_jtu$RESPONSE == "herbivore_NMDS2")

var_imp_scale_herb_2_75 = subset(var_imp_scale_herb_2, var_imp_scale_herb_2$mean >= .75)

var_imp_heat_herb_2 = plotVarImpHeatmapBeta(var_imp_scale_herb_2_75, 
                                            xlab = "", ylab = "")
###
# 4.1 predator
var_imp_scale_pred_1 = subset(var_imp_scale_jtu,
                              var_imp_scale_jtu$RESPONSE == "predator_NMDS1")

var_imp_scale_pred_1_75 = subset(var_imp_scale_pred_1, var_imp_scale_pred_1$mean >= .75)

var_imp_heat_pred_1 = plotVarImpHeatmapBeta(var_imp_scale_pred_1_75, 
                                            xlab = "", ylab = "")

# 4.2 predator
var_imp_scale_pred_2 = subset(var_imp_scale_jtu,
                              var_imp_scale_jtu$RESPONSE == "predator_NMDS2")

var_imp_scale_pred_2_75 = subset(var_imp_scale_pred_2, var_imp_scale_pred_2$mean >= .75)

var_imp_heat_pred_2 = plotVarImpHeatmapBeta(var_imp_scale_pred_2_75, 
                                            xlab = "", ylab = "")

###
# 5.1 bird_bat
var_imp_scale_bb_1 = subset(var_imp_scale_jtu,
                              var_imp_scale_jtu$RESPONSE == "bird_bat_NMDS1")

var_imp_scale_bb_1_75 = subset(var_imp_scale_bb_1, var_imp_scale_bb_1$mean >= .75)

var_imp_heat_bb_1 = plotVarImpHeatmapBeta(var_imp_scale_bb_1_75, 
                                            xlab = "", ylab = "")

# 5.2 bird_bat
var_imp_scale_bb_2 = subset(var_imp_scale_jtu,
                              var_imp_scale_jtu$RESPONSE == "bird_bat_NMDS2")

var_imp_scale_bb_2_75 = subset(var_imp_scale_bb_2, var_imp_scale_bb_2$mean >= .75)

var_imp_heat_bb_2 = plotVarImpHeatmapBeta(var_imp_scale_bb_2_75, 
                                            xlab = "", ylab = "")

###
# 6.1 plants
var_imp_scale_pla_1 = subset(var_imp_scale_jtu,
                            var_imp_scale_jtu$RESPONSE == "plants_NMDS1")

var_imp_scale_pla_1_75 = subset(var_imp_scale_pla_1, var_imp_scale_pla_1$mean >= .75)

var_imp_heat_pla_1 = plotVarImpHeatmapBeta(var_imp_scale_pla_1_75, 
                                          xlab = "", ylab = "")

# 6.2 plants
var_imp_scale_pla_2 = subset(var_imp_scale_jtu,
                            var_imp_scale_jtu$RESPONSE == "plants_NMDS2")

var_imp_scale_pla_2_75 = subset(var_imp_scale_pla_2, var_imp_scale_pla_2$mean >= .75)

var_imp_heat_pla_2 = plotVarImpHeatmapBeta(var_imp_scale_pla_2_75, 
                                          xlab = "", ylab = "")




