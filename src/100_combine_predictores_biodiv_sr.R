# Combine hyperspectral predictores and biodiversity variables in gpm class.

source("C:/Users/Thomas Nauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R")


preds = readRDS(paste0(path_hyp_pred, "hyperspec_preds.rds"))
# bd = readRDS(paste0(path_biodiv, "biodiv.rds"))
species_richness = readRDS(paste0(path_biodiv, "species_richness.rds"))
species_composition_dcor = readRDS(paste0(path_biodiv, "species_composition_dcor.rds"))
species_network_pca = readRDS(paste0(path_biodiv, "species_network_pca.rds"))

comb = data.frame(species_network_pca$scores)
colnames(comb) = paste0("sn_pca", seq(4))
comb$plotID = rownames(species_network_pca$scores)

comb = merge(species_richness, comb, by = c("plotID"), all.x = TRUE, all.y = TRUE)

for(i in seq(length(species_composition_dcor))){
  act = data.frame(species_composition_dcor[[i]]$rproj)
  colnames(act) = paste0("sn_dca", seq(4), "_", names(species_composition_dcor[i]))
  act$plotID = rownames(act)
  comb = merge(comb, act, by = c("plotID"), all.x = TRUE, all.y = TRUE)
}

comb = merge(comb, preds, by = c("plotID"))
comb = droplevels(comb)

comb$SelCat = substr(as.character(comb$plotID), 1, 3)

selnbr = lapply(table(comb$SelCat), function(c){
  seq(c)
})
comb$SelNbr = unlist(selnbr)


col_selector = which(names(comb) %in% c("SelCat", "SelNbr"))

col_diversity = seq(which("SRspiders" == colnames(comb)),
                          which("sn_dca4_Decomposer" == colnames(comb)))

col_precitors = c(which("elevation" == colnames(comb)),
                  which("lui" == colnames(comb)),
                  seq(which("CARI_mean" == colnames(comb)),
                      which("pcai_kmdc_raoq_sd" == colnames(comb))))

col_meta = which(!seq(ncol(comb)) %in% c(col_selector, col_diversity, col_precitors))
  
  
meta <- createGPMMeta(comb, type = "input",
                      selector = col_selector,
                      response = col_diversity,
                      predictor = col_precitors,
                      meta = col_meta)

comb <- gpm(comb, meta, scale = FALSE)

dir.create(paste0(path_comb_gpm_sr), showWarnings = FALSE)

saveRDS(comb, file = paste0(path_comb_gpm_sr, "ki_hyperspec_biodiv_non_scaled.rds"))
