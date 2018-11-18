# Preprocess biodiversity observations.

source("C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R")

# Read species richness dataset
bd = read.table(paste0(path_biodiv, "Biodiversity_Data_Marcel.csv"),
                header = TRUE, sep = ";", dec = ",")

saveRDS(as.character(bd$plotID), file = paste0(path_biodiv, "biodiv_plots.rds"))
saveRDS(bd, file = paste0(path_biodiv, "biodiv.rds"))


