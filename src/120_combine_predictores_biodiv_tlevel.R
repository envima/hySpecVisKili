# Combine hyperspectral predictores and biodiversity variables in gpm class
# aggregated by trophic level.

source("C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R")


preds = readRDS(paste0(path_hyp_pred, "hyperspec_preds.rds"))
bd = readRDS(paste0(path_biodiv, "biodiv.rds"))

comb = merge(bd, preds, by = c("plotID"), all.x = TRUE, all.y = TRUE)

trophic_levels = rbind(data.frame(tlevel = "Plants",
                                  groups = c("SRallplants", "SRasterids", "SRconifers", "SReudicots", 
                                             "SRferns", "SRlycopodiopsida", "SRmagnoliids", 
                                             "SRmonocots", "SRrosids")),
                       data.frame(tlevel = "Herbivore",
                                  groups = c("SRbees", "SRmoths", "SRorthoptera")),
                       data.frame(tlevel = "Decomposer",
                                  groups = c("SRdungbeetles", "SRmillipedes", "SRcollembola")),
                       data.frame(tlevel = "Predators",
                                  groups = c("SRspiders", "SRheteroptera", "SRotheraculeata", 
                                             "SRparasitoids", "SRothercoleoptera")),
                       data.frame(tlevel = "Flying predatores",
                                  groups = c("SRbats", "SRbirds")),
                       data.frame(tlevel = "Generalist",
                                  groups = c("SRmammals", "SRanimals", "SRsyrphids", "SRants", "SSRsnails")))

head(comb)







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
