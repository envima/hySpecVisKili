library(vegan)
library(plyr)
################################################################################
filepath = "E:/hyperspec_clean/clean/"
#bd_plt = read.csv2(paste0(filepath, "plantscom_sorted.csv"))
bd_ani = read.csv2(paste0(filepath, "animals_plotIDcomplete_Syn1.csv"))
bd_ani_smll = bd_ani[,c(3:4,14:73)]
bd_ani_smll$type = paste(bd_ani_smll$taxon, bd_ani_smll$species, sep = "_")
names = bd_ani_smll$type

bd_ani_trns <- data.frame(t(bd_ani_smll[,c(3:62)]))
colnames(bd_ani_trns) = names

################################################################################
### calc NMDS for each animal taxon
# ants
ants = bd_ani_trns[,c(1:79)]
any(is.na(ants))
ants.cln = na.omit(ants)
ants_mds = metaMDS(ants, distance = "euclidean", na.rm = TRUE)
#spec = ants_mds$species
#plot(ants_mds, display = c("species", "sites"))
#str(ants_mds)
ants_mds_species = scores(ants_mds, choices = c(1,2), display = "species")
ants_mds_sites = scores(ants_mds, choices = c(1,2), display = "sites")
colnames(ants_mds_sites) = c("ants_NMDS1", "ants_NMDS2")

###
# bees
bees = bd_ani_trns[,c(80:195)]
bees_mds = metaMDS(bees, distance = "euclidean")
bees_mds_species = scores(bees_mds, choices = c(1,2), display = "species")
bees_mds_sites = scores(bees_mds, choices = c(1,2), display = "sites")
colnames(bees_mds_sites) = c("bees_NMDS1", "bees_NMDS2")

taxa_mds_sites = cbind(ants_mds_sites, bees_mds_sites)
plotID = rownames(taxa_mds_sites)
taxa_mds_sites = as.data.frame(taxa_mds_sites)
taxa_mds_sites$plotID = plotID

###
# birds
birds = bd_ani_trns[,c(196:397)]
birds_mds = metaMDS(birds, distance = "euclidean")
birds_mds_species = scores(birds_mds, choices = c(1,2), display = "species")
birds_mds_sites = scores(birds_mds, choices = c(1,2), display = "sites")
colnames(birds_mds_sites) = c("birds_NMDS1", "birds_NMDS2")

taxa_mds_sites = cbind(taxa_mds_sites, birds_mds_sites)

###
# moths
moths = bd_ani_trns[,c(398:777)]
moths_mds = metaMDS(moths, distance = "euclidean", na.rm = TRUE)
moths_mds_species = scores(moths_mds, choices = c(1,2), display = "species")
moths_mds_sites = scores(moths_mds, choices = c(1,2), display = "sites")
colnames(moths_mds_sites) = c("moths_NMDS1", "moths_NMDS2")

taxa_mds_sites = cbind(taxa_mds_sites, moths_mds_sites)

###
# syrphid
syrphid = bd_ani_trns[,c(778:820)]
syrphid_mds = metaMDS(syrphid, distance = "euclidean", na.rm = TRUE)
syrphid_mds_species = scores(syrphid_mds, choices = c(1,2), display = "species")
syrphid_mds_sites = scores(syrphid_mds, choices = c(1,2), display = "sites")
colnames(syrphid_mds_sites) = c("syrphid_NMDS1", "syrphid_NMDS2")

taxa_mds_sites = cbind(taxa_mds_sites, syrphid_mds_sites)

###
# bats
bats = bd_ani_trns[,c(821:840)]
bats_mds = metaMDS(bats, distance = "euclidean", na.rm = TRUE)
bats_mds_species = scores(bats_mds, choices = c(1,2), display = "species")
bats_mds_sites = scores(bats_mds, choices = c(1,2), display = "sites")
colnames(bats_mds_sites) = c("bats_NMDS1", "bats_NMDS2")

taxa_mds_sites = cbind(taxa_mds_sites, bats_mds_sites)

###
# gastropods || snails
gastropods = bd_ani_trns[,c(841:899)]
gastropods_mds = metaMDS(gastropods, distance = "euclidean", na.rm = TRUE)
gastropods_mds_species = scores(gastropods_mds, choices = c(1,2), display = "species")
gastropods_mds_sites = scores(gastropods_mds, choices = c(1,2), display = "sites")
colnames(gastropods_mds_sites) = c("gastropods_NMDS1", "gastropods_NMDS2")

taxa_mds_sites = cbind(taxa_mds_sites, gastropods_mds_sites)

###
# orthoptera
orthoptera = bd_ani_trns[,c(900:1054)]
orthoptera_mds = metaMDS(orthoptera, distance = "euclidean", na.rm = TRUE)
orthoptera_mds_species = scores(orthoptera_mds, choices = c(1,2), display = "species")
orthoptera_mds_sites = scores(orthoptera_mds, choices = c(1,2), display = "sites")
colnames(orthoptera_mds_sites) = c("orthoptera_NMDS1", "orthoptera_NMDS2")

taxa_mds_sites = cbind(taxa_mds_sites, orthoptera_mds_sites)

###
# parasitoids
parasitoids = bd_ani_trns[,c(1055:1574)]
parasitoids_mds = metaMDS(parasitoids, distance = "euclidean", na.rm = TRUE)
parasitoids_mds_species = scores(parasitoids_mds, choices = c(1,2), display = "species")
parasitoids_mds_sites = scores(parasitoids_mds, choices = c(1,2), display = "sites")
colnames(parasitoids_mds_sites) = c("parasitoids_NMDS1", "parasitoids_NMDS2")

taxa_mds_sites = cbind(taxa_mds_sites, parasitoids_mds_sites)

###
# beetles
beetles = bd_ani_trns[,c(1575:1912)]
beetles = na.omit(beetles)
beetles_mds = metaMDS(beetles, distance = "euclidean", na.rm = TRUE)
beetles_mds_species = scores(beetles_mds, choices = c(1,2), display = "species")
beetles_mds_sites = scores(beetles_mds, choices = c(1,2), display = "sites")
colnames(beetles_mds_sites) = c("beetles_NMDS1", "beetles_NMDS2")

plotID_beetles = rownames(beetles_mds_sites) 

beetles_mds_sites = as.data.frame(beetles_mds_sites)
beetles_mds_sites$plotID = plotID_beetles 

beetles_mds_sites = rbind(beetles_mds_sites, c(NA, NA, "fer2"))
beetles_mds_sites = beetles_mds_sites[c(1:7, 60, 8:59),]

taxa_mds_sites = cbind(taxa_mds_sites, beetles_mds_sites[,c(1:2)])

###
# aculeate
aculeate = bd_ani_trns[,c(1913:2031)]
aculeate_mds = metaMDS(aculeate, distance = "euclidean", na.rm = TRUE)
aculeate_mds_species = scores(aculeate_mds, choices = c(1,2), display = "species")
aculeate_mds_sites = scores(aculeate_mds, choices = c(1,2), display = "sites")
colnames(aculeate_mds_sites) = c("aculeate_NMDS1", "aculeate_NMDS2")

taxa_mds_sites = cbind(taxa_mds_sites, aculeate_mds_sites)

###
# milipeds
milipeds = bd_ani_trns[,c(2032:2059)]
milipeds_mds = metaMDS(milipeds, distance = "euclidean", na.rm = TRUE)
milipeds_mds_species = scores(milipeds_mds, choices = c(1,2), display = "species")
milipeds_mds_sites = scores(milipeds_mds, choices = c(1,2), display = "sites")
colnames(milipeds_mds_sites) = c("milipeds_NMDS1", "milipeds_NMDS2")

taxa_mds_sites = cbind(taxa_mds_sites, milipeds_mds_sites)

###
# dungbeetles
dungbeetles = bd_ani_trns[,c(2060:2131)]
dungbeetles_mds = metaMDS(dungbeetles, distance = "euclidean", na.rm = TRUE)
dungbeetles_mds_species = scores(dungbeetles_mds, choices = c(1,2), display = "species")
dungbeetles_mds_sites = scores(dungbeetles_mds, choices = c(1,2), display = "sites")
colnames(dungbeetles_mds_sites) = c("dungbeetles_NMDS1", "dungbeetles_NMDS2")

taxa_mds_sites = cbind(taxa_mds_sites, dungbeetles_mds_sites)

###
# mammals
mammals = bd_ani_trns[,c(2132:2164)]
mammals_mds = metaMDS(mammals, distance = "euclidean", na.rm = TRUE)
mammals_mds_species = scores(mammals_mds, choices = c(1,2), display = "species")
mammals_mds_sites = scores(mammals_mds, choices = c(1,2), display = "sites")
colnames(mammals_mds_sites) = c("mammals_NMDS1", "mammals_NMDS2")

taxa_mds_sites = cbind(taxa_mds_sites, mammals_mds_sites)
taxa_mds_sites = taxa_mds_sites[,c(5,1:4,6:29)]
write.csv2(taxa_mds_sites, 
           paste(filepath, "taxa_animals_NMDS_sites.csv"), 
           row.names = FALSE)


