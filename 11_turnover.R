library(vegan)

filepath = "E:/hyperspec_clean/clean/"
biodiv_sr = read.csv2(paste0(filepath, "Biodiversity_Data_Marcel.csv")) 
#biodiv_abu = read.csv2(paste0(filepath, "animals_plotIDcomplete_Syn1.csv"))

# dissimilarity matrix for species richness of animals
biodiv_sr_dismat_animals = vegdist(scale(biodiv_sr[14:30]), 
                                   na.rm = TRUE, method = "euclidian")

biodiv_sr_pl = biodiv_sr[-c(7:11,47),]
biodiv_sr_dismat_plants = vegdist(scale(biodiv_sr_pl[32:39]), 
                                  na.rm = TRUE, method = "euclidian")

# ordination
pca_animals = cmdscale(biodiv_sr_dismat_animals) 
pca_animals = as.data.frame(pca_animals)
colnames(pca_animals) = c("1st_NMDS_animals", "2nd_NMDS_animals")
pca_animals$plotID = biodiv_sr$plotID
pca_animals = pca_animals[,c(3,1:2)]

pca_plants = cmdscale(biodiv_sr_dismat_plants) 
pca_plants = as.data.frame(pca_plants)
colnames(pca_plants) = c("1st_NMDS_plants", "2nd_NMDS_plants")
pca_plants$plotID = biodiv_sr[-c(7:11,47),]$plotID
pca_plants = pca_plants[,c(3,1:2)]

NMDS_both_taxa = merge(pca_animals, pca_plants, by = "plotID")

write.csv2(NMDS_both_taxa, 
           "E:/hyperspec_clean/clean/NMDS_both_taxa.csv", row.names = FALSE)

### hier das script zum plotten der ordination
# in dataset2 stehen einfach erklärende variablen: 
# hierfür benötigt werden elevation und cat
fundata_animals = biodiv_sr
fundata_plants = biodiv_sr_pl

plot(pca_plants, type = "n", cex.lab = 1.5, cex.axis = 1.2,las = 1)
with(fundata_plants, ordisurf(pca_plants, elevation, add = TRUE, 
                               col = "black", lwd = 1, nlevels = 10, 
                               method = "ML", labcex = 0.8))
with(fundata_plants, ordihull(pca_plants, cat, draw = "polygon", 
                               col = "cornflowerblue", label = TRUE,
                               show.groups = "sav", alpha = 200))
with(fundata_plants, ordihull(pca_plants, cat, draw = "polygon", 
                               col = "cornflowerblue", label = TRUE,
                               show.groups = "flm", alpha = 200))
with(fundata_plants, ordihull(pca_plants, cat, draw = "polygon", 
                               col = "cornflowerblue", label = TRUE,
                               show.groups = "foc", alpha = 200))
with(fundata_plants, ordihull(pca_plants, cat, draw = "polygon", 
                               col = "cornflowerblue", label = TRUE,
                               show.groups = "fpo", alpha = 200))
with(fundata_plants, ordihull(pca_plants, cat, draw = "polygon", 
                               col = "cornflowerblue", label = TRUE,
                               show.groups = "fer", alpha = 200))
with(fundata_plants, ordihull(pca_plants, cat, draw = "polygon", 
                               col = "cornflowerblue", label = TRUE,
                               show.groups = "hel", alpha = 200))
with(fundata_plants, ordihull(pca_plants, cat, draw = "polygon", label = TRUE,
                               col = "orange", show.groups = "mai",alpha = 200))
with(fundata_plants, ordihull(pca_plants, cat, draw = "polygon", label = TRUE,
                               col = "orange", show.groups = "hom", alpha = 200))
with(fundata_plants, ordihull(pca_plants, cat, draw = "polygon", label = TRUE,
                               col = "orange", show.groups = "gra", alpha = 200))
with(fundata_plants, ordihull(pca_plants, cat, draw = "polygon", label = TRUE,
                               col = "orange", show.groups = "cof", alpha = 200))
with(fundata_plants, ordihull(pca_plants, cat, draw = "polygon", label = TRUE,
                               col = "orange", show.groups = "fod", alpha = 200))
with(fundata_plants, ordihull(pca_plants, cat, draw = "polygon", label = TRUE,
                               col = "orange", show.groups = "fpd", alpha = 200))
points(pca_plants[,2][fundata_plants$cat == "sav"] ~ pca_plants[,1][fundata_plants$cat == "sav"],
       bg = "yellow", cex = 1.5, pch = 21)
points(pca_plants[,2][fundata_plants$cat == "flm"] ~ pca_plants[,1][fundata_plants$cat == "flm"],
       bg = "forestgreen", cex = 1.5, pch = 21)
points(pca_plants[,2][fundata_plants$cat == "foc"] ~ pca_plants[,1][fundata_plants$cat == "foc"],
       bg = "darkolivegreen", cex = 1.5, pch = 21)
points(pca_plants[,2][fundata_plants$cat == "fpo"] ~ pca_plants[,1][fundata_plants$cat == "fpo"],
       bg = "green", cex = 1.5, pch = 21)
points(pca_plants[,2][fundata_plants$cat == "fer"] ~ pca_plants[,1][fundata_plants$cat == "fer"], 
       bg = "lightgreen", cex = 1.5, pch = 21)
points(pca_plants[,2][fundata_plants$cat == "hel"] ~ pca_plants[,1][fundata_plants$cat == "hel"],
       bg = "azure", cex = 1.5, pch = 21)
points(pca_plants[,2][fundata_plants$cat == "mai"] ~ pca_plants[,1][fundata_plants$cat == "mai"],
       bg = "yellow", cex = 1.5, pch = 22)
points(pca_plants[,2][fundata_plants$cat == "hom"] ~ pca_plants[,1][fundata_plants$cat == "hom"],
       bg = "forestgreen", cex = 1.5, pch = 22)
points(pca_plants[,2][fundata_plants$cat == "gra"] ~ pca_plants[,1][fundata_plants$cat == "gra"],
       bg = "forestgreen", cex = 1.5, pch = 24)
points(pca_plants[,2][fundata_plants$cat == "cof"] ~ pca_plants[,1][fundata_plants$cat == "cof"],
       bg = "forestgreen", cex = 1.5, pch = 23)
points(pca_plants[,2][fundata_plants$cat == "fod"] ~ pca_plants[,1][fundata_plants$cat == "fod"],
       bg = "darkolivegreen", cex = 1.5, pch = 22)
points(pca_plants[,2][fundata_plants$cat == "fpd"] ~ pca_plants[,1][fundata_plants$cat == "fpd"],
       bg = "green", cex = 1.5, pch = 22)
