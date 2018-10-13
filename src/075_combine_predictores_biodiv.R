# Combine hyperspectral predictores and biodiversity variables in gpm class.

source("C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R")


preds = readRDS(paste0(path_hyp_pred, "hyperspec_preds.rds"))
bd = readRDS(paste0(path_biodiv, "biodiv.rds"))

comb = merge(bd, preds, by = c("plotID"), all.x = TRUE, all.y = TRUE)

comb$SelCat = substr(as.character(comb$plotID), 1, 3)
comb$SelNbr = substr(as.character(comb$plotID), 4, 4)

col_selector = which(names(comb) %in% c("SelCat", "SelNbr"))

col_diversity = seq(which("SRmammals" == colnames(comb)),
                          which("SRallplants" == colnames(comb)))

col_precitors = c(which("elevation" == colnames(comb)),
                  seq(which("lui_biomass_removal" == colnames(comb)),
                      which("lui" == colnames(comb))),
                  seq(which("CARI_mean" == colnames(comb)),
                      which("pcai_kmdc_raoq_sd" == colnames(comb))))

col_meta = which(!seq(ncol(comb)) %in% c(col_selector, col_diversity, col_precitors))
  
  
meta <- createGPMMeta(comb, type = "input",
                      selector = col_selector,
                      response = col_diversity,
                      predictor = col_precitors,
                      meta = col_meta)

comb <- gpm(comb, meta, scale = FALSE)

dir.create(paste0(path_comb_gpm), showWarnings = FALSE)

saveRDS(comb, file = paste0(path_comb_gpm, "ki_hyperspec_biodiv_non_scaled.rds"))
