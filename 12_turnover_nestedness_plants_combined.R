library(vegan)
library(plyr)
library(betapart)
library(foreach)
################################################################################
filepath = "F:/hyperspec_clean/clean/"
bd_plt = read.csv2(paste0(filepath, "plantscom_sorted.csv"))
bd_plt_smll = bd_plt[,c(1,7:67)]
bd_plt_smll = bd_plt_smll[,c(2,1,3:62)]

bd_plt_smll$type = paste(bd_plt_smll$clade, bd_plt_smll$species, sep = "_")
bd_plt_smll = bd_plt_smll[order(bd_plt_smll$clade),]
names = bd_plt_smll$type

bd_plt_trns <- data.frame(t(bd_plt_smll[,c(3:62)]))
colnames(bd_plt_trns) = names

################################################################################
##### calc NMDS for all plants
# asterids
asterids = bd_plt_trns[,c(1:317)]

# ferns
ferns = bd_plt_trns[,c(318:395)]

# magnoliids
magnoliids = bd_plt_trns[,c(396:405)]

# monocots
monocots = bd_plt_trns[,c(406:609)]

# mosses
mosses = bd_plt_trns[,c(610:625)]

# eudicots
eudicots = bd_plt_trns[,c(626:678)]

# conifers
conifers = bd_plt_trns[,c(679:687)]

# rosids
rosids = bd_plt_trns[,c(688:992)]

# combine all plants
pla_all = cbind(asterids,ferns,magnoliids,monocots,mosses,eudicots,conifers,rosids)

# presence absence data
pla_pa = as.data.frame(ifelse(pla_all == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
pla_pairs = beta.pair(pla_pa, index.family = "jaccard") 
#tst = as.matrix(pla_pairs[[1]])

pla_cln = foreach(i = seq(pla_pairs)) %do% {
  as.matrix(pla_pairs[[i]])[-7,-7]
}

pla_MDS = foreach(i = seq(pla_cln), .combine = "cbind") %do% {
  scores(metaMDS(pla_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(pla_MDS) = c("pla_jtu_NMDS1", "pla_jtu_NMDS2", 
                      "pla_jne_NMDS1", "pla_jne_NMDS2",
                      "pla_jac_NMDS1", "pla_jac_NMDS2")
pla_MDS = as.data.frame(pla_MDS)
pla_MDS$plotID = rownames(pla_MDS)
pla_MDS = pla_MDS[,c(7,1:6)]

write.csv2(pla_MDS, 
           paste(filepath, "plants_TO_NE_AC_NMDS_sites.csv"), 
           row.names = FALSE)
  



