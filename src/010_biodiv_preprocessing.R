# Preprocess biodiversity observations.

source("C:/Users/Thomas Nauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R")


# Read species richness dataset (Peters et al. 2016)
bd = read.table(paste0(path_biodiv, "Biodiversity_Data_Marcel.csv"),
                header = TRUE, sep = ";", dec = ",")

saveRDS(as.character(bd$plotID), file = paste0(path_biodiv, "biodiv_plots.rds"))
saveRDS(bd, file = paste0(path_biodiv, "biodiv.rds"))

# Read animal species dataset
ad = read.table(paste0(path_biodiv, "animals_plotIDcomplete_Syn1.csv"),
                header = TRUE, sep = ";", dec = ",")

ad_diet = lapply(unique(ad$taxon), function(t){
  data.frame(taxon = t,
             ad$diet)  
})



# Set species number to 0/1 and reduce to complete cases
ad[, 14:73][!is.na(ad[, 14:73]) & ad[, 14:73]>1] = 1
adc = ad[complete.cases(ad[, 14:73]),]

# Split into taxon groups
adc_taxl = lapply(unique(adc$taxon), function(t){
  act_level = adc[adc$taxon == t, ]
  act_grp = act_level[, 14:73]
  rownames(act_grp) = act_level$species
  return(t(act_grp))
})

adc_taxl_all = adc[, 14:73]
rownames(adc_all) = adc$species

adc_taxl = c(list(t(adc_taxl_all)), adc_taxl)
names(adc_taxl) = c("Animals", as.character(unique(adc$taxon)))

saveRDS(adc_taxl, file = paste0(path_biodiv, "adc_taxl.rds"))



# Split into trophic levels
adc_tlevels = lapply(unique(adc$diet), function(d){
  act_level = adc[adc$diet == d, ]
  act_grp = act_level[, 14:73]
  rownames(act_grp) = act_level$species
  return(t(act_grp))
})

adc_all =adc[, 14:73]
rownames(adc_all) = adc$species

adc_tlevels = c(list(t(adc_all)), adc_tlevels)
names(adc_tlevels) = c("Animals", as.character(unique(adc$diet)))

saveRDS(adc_tlevels, file = paste0(path_biodiv, "adc_tlevels.rds"))



# Compute species richness from taxon groups and tropich levels
adc_taxl_sr = data.frame(plotID = rownames(adc_taxl[[1]]))
for(i in seq(length(adc_taxl))){
  adc_taxl_sr[, i+1] = rowSums(adc_taxl[[i]])
  names(adc_taxl_sr)[i+1] = paste0("SR", tolower(names(adc_taxl[i])))
}

adc_tlevels_sr = data.frame(plotID = rownames(adc_tlevels[[1]]))
for(i in seq(length(adc_tlevels))){
  adc_tlevels_sr[, i+1] = rowSums(adc_tlevels[[i]])
  names(adc_tlevels_sr)[i+1] = paste0("SR", tolower(names(adc_tlevels[i])))
}

adc_sr = merge(adc_tlevels_sr, 
               adc_taxl_sr[, -grep("SRanimals", colnames(adc_taxl_sr))], 
               by = "plotID")

saveRDS(adc_sr, file = paste0(path_biodiv, "adc_sr.rds"))



# Merge species richness data
s_adc_sr = colnames(adc_sr[-grep("plotID", colnames(adc_sr))])

colnames(bd)[which("SRsyrphids" == colnames(bd))] = "SRsyrphid_flies"
colnames(bd)[which("SRbats" == colnames(bd))] = "SRinsectivorous_bats"
colnames(bd)[which("SRmammals" == colnames(bd))] = "SRlarge_mammals"
colnames(bd)[which("SRparasitoids" == colnames(bd))] = "SRparasitoid_wasps"
colnames(bd)[which("SRotheraculeata" == colnames(bd))] = "SRaculeate_wasps"
colnames(bd)[which("SRmillipedes" == colnames(bd))] = "SRmilipeds"
colnames(bd)[which("SRsnails" == colnames(bd))] = "SRgastropods"

s_bd = colnames(bd[, grep("SR", colnames(bd))])
# which(tolower(s_adc_taxl_sr) %in% tolower(s_bd))

species_richness = merge(bd, adc_sr, by = "plotID")
species_richness = species_richness[, -grep("\\.x", colnames(species_richness))]
colnames(species_richness)[grep("\\.y", colnames(species_richness))] = 
  substr(colnames(species_richness)[grep("\\.y", colnames(species_richness))], 1, 
         (nchar(colnames(species_richness)[grep("\\.y", colnames(species_richness))])-2))

saveRDS(species_richness, file = paste0(path_biodiv, "species_richness.rds"))



# Compute community composition using detrended correspondence analysis
species_composition_dcor = lapply(adc_tlevels, function(l){
  l = l[rowSums(l) > 0, ]
  decorana(l)
})
names(species_composition_dcor) = names(adc_tlevels)
# for(i in seq(5)) plot(species_composition_dcor[[i]], display = "sites")

saveRDS(species_composition_dcor,  file = paste0(path_biodiv, "species_composition_dcor.rds"))



# Compute relative network using PCA
tlevels = colnames(species_richness)[
  seq(grep("SRanimals", colnames(species_richness)), 
      grep("SRanimals", colnames(species_richness))+4)]

adn_matrix = matrix(ncol = 5, nrow = 60)
for(i in seq(5)){
  adn_matrix[, i] = species_richness[, tlevels[i]]
}
rownames(adn_matrix) <- species_richness$plotID
colnames(adn_matrix ) <- tlevels

species_network_pca <- princomp(adn_matrix[,-1], cor=T)

# biplot(species_network_pca, choices = 2:3)
# summary(species_network_pca)

saveRDS(species_network_pca,  file = paste0(path_biodiv, "species_network_pca.rds"))


# Cross check
# sort(colnames(adc_taxl_sr))
# sort(colnames(bd[, c(1, grep("SR", colnames(bd)))]))
# 
# test = merge(adc_taxl_sr, bd, by = "plotID", all = TRUE)
# 
# test_df = lapply(grep("\\.x", colnames(test)), function(t){
#   x = colnames(test)[t]
#   y = paste0(substr(x, 1, (nchar(x)-1)), "y")
#   act_df = data.frame(plotID = test$plotID, test[, x], test[, y])
#   colnames(act_df) = c("plotId", x, y)
#   return(act_df)
# })
# test_df







