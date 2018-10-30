# Combine hyperspectral predictores and biodiversity variables in gpm class.

source("C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R")

comb = readRDS(paste0(path_comb_gpm_sr, "ki_hyperspec_biodiv_non_scaled.rds"))







dir.create(paste0(path_comb_gpm_sr), showWarnings = FALSE)

saveRDS(comb, file = paste0(path_comb_gpm_sr, "ki_hyperspec_biodiv_non_scaled.rds"))
