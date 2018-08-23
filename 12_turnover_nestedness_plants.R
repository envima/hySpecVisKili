library(vegan)
library(plyr)
library(betapart)
library(foreach)
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
##### calc NMDS for each animal taxon
###
# asterids
asterids = bd_plt_trns[,c(1:317)]
any(is.na(asterids))

# presence absence data
asterids_pa = as.data.frame(ifelse(asterids == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
asterids_pairs = beta.pair(asterids_pa, index.family = "jaccard") 
tst = as.matrix(asterids_pairs[[1]])

asterids_cln = foreach(i = seq(asterids_pairs)) %do% {
  as.matrix(asterids_pairs[[i]])
}

asterids_MDS = foreach(i = seq(asterids_cln), .combine = "cbind") %do% {
  scores(metaMDS(asterids_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(asterids_MDS) = c("asterids_jtu_NMDS1", "asterids_jtu_NMDS2", 
                           "asterids_jne_NMDS1", "asterids_jne_NMDS2",
                           "asterids_jac_NMDS1", "asterids_jac_NMDS2")
asterids_MDS = as.data.frame(asterids_MDS)
asterids_MDS$plotID = rownames(asterids_MDS)

###
# ferns
ferns = bd_plt_trns[,c(318:395)]
any(is.na(ferns))

# presence absence data
ferns_pa = as.data.frame(ifelse(ferns == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
ferns_pairs = beta.pair(ferns_pa, index.family = "jaccard") 
tst = as.matrix(ferns_pairs[[1]])

ferns_cln = foreach(i = seq(ferns_pairs)) %do% {
  as.matrix(ferns_pairs[[i]])[-c(2,4,7,40:45,51:55,58,60),-c(2,4,7,40:45,51:55,58,60)]
}

ferns_MDS = foreach(i = seq(ferns_cln), .combine = "cbind") %do% {
  scores(metaMDS(ferns_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(ferns_MDS) = c("ferns_jtu_NMDS1", "ferns_jtu_NMDS2", 
                        "ferns_jne_NMDS1", "ferns_jne_NMDS2",
                        "ferns_jac_NMDS1", "ferns_jac_NMDS2")
ferns_MDS = as.data.frame(ferns_MDS)
ferns_MDS$plotID = rownames(ferns_MDS)

plnts_MDS_df = merge(asterids_MDS, ferns_MDS, all.x = TRUE)

###
# magnoliids
magnoliids = bd_plt_trns[,c(396:405)]
any(is.na(magnoliids))

# presence absence data
magnoliids_pa = as.data.frame(ifelse(magnoliids == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
magnoliids_pairs = beta.pair(magnoliids_pa, index.family = "jaccard") 
#tst = as.matrix(magnoliids_pairs[[1]])

magnoliids_cln = foreach(i = seq(magnoliids_pairs)) %do% {
  as.matrix(magnoliids_pairs[[i]])[-c(1:4,6:10,27,30,36:38,40:45,47,51:55,57:60),-c(1:4,6:10,27,30,36:38,40:45,47,51:55,57:60)]
}

magnoliids_MDS = foreach(i = seq(magnoliids_cln), .combine = "cbind") %do% {
  scores(metaMDS(magnoliids_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(magnoliids_MDS) = c("magnoliids_jtu_NMDS1", "magnoliids_jtu_NMDS2", 
                       "magnoliids_jne_NMDS1", "magnoliids_jne_NMDS2",
                       "magnoliids_jac_NMDS1", "magnoliids_jac_NMDS2")
magnoliids_MDS = as.data.frame(magnoliids_MDS)
magnoliids_MDS$plotID = rownames(magnoliids_MDS)

plnts_MDS_df = merge(plnts_MDS_df, magnoliids_MDS, all.x = TRUE)

###
# monocots
monocots = bd_plt_trns[,c(406:609)]
any(is.na(monocots))

# presence absence data
monocots_pa = as.data.frame(ifelse(monocots == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
monocots_pairs = beta.pair(monocots_pa, index.family = "jaccard") 
tst = as.matrix(monocots_pairs[[1]])

monocots_cln = foreach(i = seq(monocots_pairs)) %do% {
  as.matrix(monocots_pairs[[i]])
}

monocots_MDS = foreach(i = seq(monocots_cln), .combine = "cbind") %do% {
  scores(metaMDS(monocots_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(monocots_MDS) = c("monocots_jtu_NMDS1", "monocots_jtu_NMDS2", 
                           "monocots_jne_NMDS1", "monocots_jne_NMDS2",
                           "monocots_jac_NMDS1", "monocots_jac_NMDS2")
monocots_MDS = as.data.frame(monocots_MDS)
monocots_MDS$plotID = rownames(monocots_MDS)

plnts_MDS_df = merge(plnts_MDS_df, monocots_MDS, all.x = TRUE)

###
# mosses
mosses = bd_plt_trns[,c(610:625)]
any(is.na(mosses))

# presence absence data
mosses_pa = as.data.frame(ifelse(mosses == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
mosses_pairs = beta.pair(mosses_pa, index.family = "jaccard") 
tst = as.matrix(mosses_pairs[[1]])

mosses_cln = foreach(i = seq(mosses_pairs)) %do% {
  as.matrix(mosses_pairs[[i]])[-c(1:5,9,11:25,31:35,38:40,45:60),-c(1:5,9,11:25,31:35,38:40,45:60)]
}

mosses_MDS = foreach(i = seq(mosses_cln), .combine = "cbind") %do% {
  scores(metaMDS(mosses_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(mosses_MDS) = c("mosses_jtu_NMDS1", "mosses_jtu_NMDS2", 
                         "mosses_jne_NMDS1", "mosses_jne_NMDS2",
                         "mosses_jac_NMDS1", "mosses_jac_NMDS2")
mosses_MDS = as.data.frame(mosses_MDS)
mosses_MDS$plotID = rownames(mosses_MDS)

plnts_MDS_df = merge(plnts_MDS_df, mosses_MDS, all.x = TRUE)

###
# eudicots
eudicots = bd_plt_trns[,c(626:678)]
any(is.na(eudicots))

# presence absence data
eudicots_pa = as.data.frame(ifelse(eudicots == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
eudicots_pairs = beta.pair(eudicots_pa, index.family = "jaccard") 
tst = as.matrix(eudicots_pairs[[1]])

eudicots_cln = foreach(i = seq(eudicots_pairs)) %do% {
  as.matrix(eudicots_pairs[[i]])[-c(7,9,11:12,16,18,21,23,25,43,45),-c(7,9,11:12,16,18,21,23,25,43,45)]
}

eudicots_MDS = foreach(i = seq(eudicots_cln), .combine = "cbind") %do% {
  scores(metaMDS(eudicots_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(eudicots_MDS) = c("eudicots_jtu_NMDS1", "eudicots_jtu_NMDS2", 
                       "eudicots_jne_NMDS1", "eudicots_jne_NMDS2",
                       "eudicots_jac_NMDS1", "eudicots_jac_NMDS2")
eudicots_MDS = as.data.frame(eudicots_MDS)
eudicots_MDS$plotID = rownames(eudicots_MDS)

plnts_MDS_df = merge(plnts_MDS_df, eudicots_MDS, all.x = TRUE)

###
# conifers
conifers = bd_plt_trns[,c(679:687)]
any(is.na(conifers))

# presence absence data
conifers_pa = as.data.frame(ifelse(conifers == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
conifers_pairs = beta.pair(conifers_pa, index.family = "jaccard") 
#tst = as.matrix(conifers_pairs[[1]])

conifers_cln = foreach(i = seq(conifers_pairs)) %do% {
  as.matrix(conifers_pairs[[i]])[-c(1:10,12:14,21,39:45,47,49:60),-c(1:10,12:14,21,39:45,47,49:60)]
}

conifers_MDS = foreach(i = seq(conifers_cln), .combine = "cbind") %do% {
  scores(metaMDS(conifers_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(conifers_MDS) = c("conifers_jtu_NMDS1", "conifers_jtu_NMDS2", 
                             "conifers_jne_NMDS1", "conifers_jne_NMDS2",
                             "conifers_jac_NMDS1", "conifers_jac_NMDS2")
conifers_MDS = as.data.frame(conifers_MDS)
conifers_MDS$plotID = rownames(conifers_MDS)

plnts_MDS_df = merge(plnts_MDS_df, conifers_MDS, all.x = TRUE)

###
# rosids
rosids = bd_plt_trns[,c(688:992)]
any(is.na(rosids))

# presence absence data
rosids_pa = as.data.frame(ifelse(rosids == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
rosids_pairs = beta.pair(rosids_pa, index.family = "jaccard") 
#tst = as.matrix(rosids_pairs[[1]])

rosids_cln = foreach(i = seq(rosids_pairs)) %do% {
  as.matrix(rosids_pairs[[i]])[-7,-7]
}

rosids_MDS = foreach(i = seq(rosids_cln), .combine = "cbind") %do% {
  scores(metaMDS(rosids_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(rosids_MDS) = c("rosids_jtu_NMDS1", "rosids_jtu_NMDS2", 
                         "rosids_jne_NMDS1", "rosids_jne_NMDS2",
                         "rosids_jac_NMDS1", "rosids_jac_NMDS2")
rosids_MDS = as.data.frame(rosids_MDS)
rosids_MDS$plotID = rownames(rosids_MDS)

plnts_MDS_df = merge(plnts_MDS_df, rosids_MDS, all.y = TRUE)

write.csv2(plnts_MDS_df, 
           paste(filepath, "taxa_plants_TO_NE_AC_NMDS_sites.csv"), 
           row.names = FALSE)
  



