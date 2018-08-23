library(vegan)
library(plyr)
################################################################################
filepath = "E:/hyperspec_clean/clean/"
bd_plt = read.csv2(paste0(filepath, "plantscom_sorted.csv"))
bd_plt_smll = bd_plt[,c(1,7:67)]
bd_plt_smll = bd_plt_smll[,c(2,1,3:62)]

bd_plt_smll$type = paste(bd_plt_smll$clade, bd_plt_smll$species, sep = "_")
bd_plt_smll = bd_plt_smll[order(bd_plt_smll$clade),]
names = bd_plt_smll$type

bd_plt_trns <- data.frame(t(bd_plt_smll[,c(3:62)]))
colnames(bd_plt_trns) = names

################################################################################
### calc NMDS for each animal taxon
# asterids
asterids = bd_plt_trns[,c(1:317)]

asterids_mds = metaMDS(asterids, distance = "euclidean", na.rm = TRUE)
asterids_mds_tst = metaMDS(comm = asterids, distance = "jaccard", na.rm = TRUE)#k = 4, zerodist = "add") 

asterids_mds_species = scores(asterids_mds, choices = c(1,2), display = "species")
asterids_mds_sites = scores(asterids_mds, choices = c(1,2), display = "sites")
colnames(asterids_mds_sites) = c("asterids_NMDS1", "asterids_NMDS2")

###
# ferns
ferns = bd_plt_trns[,c(318:395)]

ferns_mds = metaMDS(ferns, distance = "euclidean")
#ferns_mds_tst = metaMDS(comm = ferns, distance = "jaccard",  sfgrmin = 1e-7)
#ferns_mds_tst = metaMDS(comm = na.omit(ferns), distance = "bray", k = 4, zerodist = "add")

ferns_mds_species = scores(ferns_mds, choices = c(1,2), display = "species")
ferns_mds_sites = scores(ferns_mds, choices = c(1,2), display = "sites")
colnames(ferns_mds_sites) = c("ferns_NMDS1", "ferns_NMDS2")

taxa_mds_sites = cbind(asterids_mds_sites, ferns_mds_sites)
plotID = rownames(taxa_mds_sites)
taxa_mds_sites = as.data.frame(taxa_mds_sites)
taxa_mds_sites$plotID = plotID

###
# magnoliids
magnoliids = bd_plt_trns[,c(396:405)]
magnoliids_mds = metaMDS(magnoliids, distance = "euclidean")
magnoliids_mds_species = scores(magnoliids_mds, choices = c(1,2), display = "species")
magnoliids_mds_sites = scores(magnoliids_mds, choices = c(1,2), display = "sites")
colnames(magnoliids_mds_sites) = c("magnoliids_NMDS1", "magnoliids_NMDS2")

taxa_mds_sites = cbind(taxa_mds_sites, magnoliids_mds_sites)

###
# monocots
monocots = bd_plt_trns[,c(406:609)]
monocots_mds = metaMDS(monocots, distance = "euclidean", na.rm = TRUE)
monocots_mds_species = scores(monocots_mds, choices = c(1,2), display = "species")
monocots_mds_sites = scores(monocots_mds, choices = c(1,2), display = "sites")
colnames(monocots_mds_sites) = c("monocots_NMDS1", "monocots_NMDS2")

taxa_mds_sites = cbind(taxa_mds_sites, monocots_mds_sites)

###
# mosses
mosses = bd_plt_trns[,c(610:625)]
mosses_mds = metaMDS(mosses, distance = "euclidean", na.rm = TRUE)
mosses_mds_species = scores(mosses_mds, choices = c(1,2), display = "species")
mosses_mds_sites = scores(mosses_mds, choices = c(1,2), display = "sites")
colnames(mosses_mds_sites) = c("mosses_NMDS1", "mosses_NMDS2")

taxa_mds_sites = cbind(taxa_mds_sites, mosses_mds_sites)

###
# eudicots
eudicots = bd_plt_trns[,c(626:678)]
eudicots_mds = metaMDS(eudicots, distance = "euclidean", na.rm = TRUE)
eudicots_mds_species = scores(eudicots_mds, choices = c(1,2), display = "species")
eudicots_mds_sites = scores(eudicots_mds, choices = c(1,2), display = "sites")
colnames(eudicots_mds_sites) = c("eudicots_NMDS1", "eudicots_NMDS2")

taxa_mds_sites = cbind(taxa_mds_sites, eudicots_mds_sites)

###
# conifers
conifers = subset(bd_plt_smll, bd_plt_smll$clade == "Other Tracheophyta'")
conifers = bd_plt_trns[,c(679:687)]
conifers_mds = metaMDS(conifers, distance = "euclidean", na.rm = TRUE)
conifers_mds_species = scores(conifers_mds, choices = c(1,2), display = "species")
conifers_mds_sites = scores(conifers_mds, choices = c(1,2), display = "sites")
colnames(conifers_mds_sites) = c("conifers_NMDS1", "conifers_NMDS2")

taxa_mds_sites = cbind(taxa_mds_sites, conifers_mds_sites)

###
# rosids
rosids = subset(bd_plt_smll, bd_plt_smll$clade == "Rosids")
rosids = bd_plt_trns[,c(688:992)]
rosids_mds = metaMDS(rosids, distance = "euclidean", na.rm = TRUE)
rosids_mds_species = scores(rosids_mds, choices = c(1,2), display = "species")
rosids_mds_sites = scores(rosids_mds, choices = c(1,2), display = "sites")
colnames(rosids_mds_sites) = c("rosids_NMDS1", "rosids_NMDS2")

taxa_mds_sites = cbind(taxa_mds_sites, rosids_mds_sites)
taxa_mds_sites = taxa_mds_sites[,c(5,1:4,6:17)]
write.csv2(taxa_mds_sites, 
           paste(filepath, "taxa_plants_NMDS_sites.csv"), 
           row.names = FALSE)


