library(vegan)
library(plyr)
library(betapart)
library(foreach)
################################################################################
filepath = "F:/hyperspec_clean/clean/"
bd_ani = read.csv2(paste0(filepath, "animals_plotIDcomplete_Syn1.csv"))
bd_ani_smll = bd_ani[,c(3:4,14:73)]
bd_ani_smll$type = paste(bd_ani_smll$taxon, bd_ani_smll$species, sep = "_")
names = bd_ani_smll$type

bd_ani_trns <- data.frame(t(bd_ani_smll[,c(3:62)]))
colnames(bd_ani_trns) = names

################################################################################
##### calc NMDS for each trophic Level
### generalist: ants|gastropods|mammals|snails
# ants
ants = bd_ani_trns[,c(1:79)]

# gastropods|snails
gastropods = bd_ani_trns[,c(841:899)]

# mammals
mammals = bd_ani_trns[,c(2132:2164)]

# syrphid
syrphid = bd_ani_trns[,c(778:820)]

# combine generalists
gen_all = cbind(ants, gastropods, mammals, syrphid)

# presence absence data
gen_pa = as.data.frame(ifelse(gen_all == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
gen_pairs = beta.pair(gen_pa, index.family = "jaccard") 

gen_cln = foreach(i = seq(gen_pairs)) %do% {
  as.matrix(gen_pairs[[i]])[-c(44),-c(44)]
}

gen_MDS = foreach(i = seq(gen_cln), .combine = "cbind") %do% {  
  scores(metaMDS(gen_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(gen_MDS) = c("gen_jtu_NMDS1", "gen_jtu_NMDS2", 
                       "gen_jne_NMDS1", "gen_jne_NMDS2",
                       "gen_jac_NMDS1", "gen_jac_NMDS2")
gen_MDS = as.data.frame(gen_MDS)
gen_MDS$plotID = rownames(gen_MDS)


### decomposer: dungbeetles|millipedes
# dungbeetles
dungbeetles = bd_ani_trns[,c(2060:2131)]

# millipedes
millipedes = bd_ani_trns[,c(2032:2059)]

# combine decomposer
deco_all = cbind(dungbeetles, millipedes)

# presence absence data
deco_pa = as.data.frame(ifelse(deco_all == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
deco_pairs = beta.pair(deco_pa, index.family = "jaccard") 
#tst = as.matrix(deco_pairs[[1]])

deco_cln = foreach(i = seq(deco_pairs)) %do% {
  as.matrix(deco_pairs[[i]])[-c(6:10,18:24,26:27,31:34,41:45),-c(6:10,18:24,26:27,31:34,41:45)]
}

deco_MDS = foreach(i = seq(deco_cln), .combine = "cbind") %do% {
  scores(metaMDS(deco_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(deco_MDS) = c("deco_jtu_NMDS1", "deco_jtu_NMDS2", 
                       "deco_jne_NMDS1", "deco_jne_NMDS2",
                       "deco_jac_NMDS1", "deco_jac_NMDS2")
deco_MDS = as.data.frame(deco_MDS)
deco_MDS$plotID = rownames(deco_MDS)

ani_MDS_df = merge(gen_MDS, deco_MDS, all.x = TRUE)


### herbivore: bees|moths|orthoptera
# bees
bees = bd_ani_trns[,c(80:195)]

# moths
moths = bd_ani_trns[,c(398:777)]

# orthoptera
orthoptera = bd_ani_trns[,c(900:1054)]

# combine herbivore
herb_all = cbind(bees, moths, orthoptera)

# presence absence data
herb_pa = as.data.frame(ifelse(herb_all == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
herb_pairs = beta.pair(herb_pa, index.family = "jaccard") 
#tst = as.matrix(herb_pairs[[1]])

herb_MDS = foreach(i = seq(herb_pairs), .combine = "cbind") %do% {
  scores(metaMDS(herb_pairs[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(herb_MDS) = c("herb_jtu_NMDS1", "herb_jtu_NMDS2", 
                       "herb_jne_NMDS1", "herb_jne_NMDS2",
                       "herb_jac_NMDS1", "herb_jac_NMDS2")
herb_MDS = as.data.frame(herb_MDS)
herb_MDS$plotID = rownames(herb_MDS)

ani_MDS_df = merge(ani_MDS_df, herb_MDS, all.y = TRUE)


### predator: aculeata|beetles|parasitoids
# aculeate
aculeate = bd_ani_trns[,c(1913:2031)]

# beetles
beetles = bd_ani_trns[,c(1575:1912)]
beetles[8,] = 0

# parasitoids
parasitoids = bd_ani_trns[,c(1055:1574)]

# combine predators
pred_all = cbind(aculeate, beetles, parasitoids)

# presence absence data
pred_pa = as.data.frame(ifelse(pred_all == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
pred_pairs = beta.pair(pred_pa, index.family = "jaccard") 
#tst = as.matrix(pred_pairs[[1]])

pred_cln = foreach(i = seq(pred_pairs)) %do% {
  as.matrix(pred_pairs[[i]])
}

pred_MDS = foreach(i = seq(pred_cln), .combine = "cbind") %do% {
  scores(metaMDS(pred_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(pred_MDS) = c("pred_jtu_NMDS1", "pred_jtu_NMDS2", 
                       "pred_jne_NMDS1", "pred_jne_NMDS2",
                       "pred_jac_NMDS1", "pred_jac_NMDS2")
pred_MDS = as.data.frame(pred_MDS)
pred_MDS$plotID = rownames(pred_MDS)

ani_MDS_df = merge(ani_MDS_df, pred_MDS, all.y = TRUE)


### flying: bats|birds
# bats
bats = bd_ani_trns[,c(821:840)]

# birds
birds = bd_ani_trns[,c(196:397)]

# combine flying
bb_all = cbind(bats, birds)

# presence absence data
bb_pa = as.data.frame(ifelse(bb_all == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
bb_pairs = beta.pair(bb_pa, index.family = "jaccard") 
#tst = as.matrix(bb_pairs[[1]])

bb_cln = foreach(i = seq(bb_pairs)) %do% {
  as.matrix(bb_pairs[[i]])[-c(43),-c(43)]
}

bb_MDS = foreach(i = seq(bb_cln), .combine = "cbind") %do% {
  scores(metaMDS(bb_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(bb_MDS) = c("bb_jtu_NMDS1", "bb_jtu_NMDS2", 
                     "bb_jne_NMDS1", "bb_jne_NMDS2",
                     "bb_jac_NMDS1", "bb_jac_NMDS2")
bb_MDS = as.data.frame(bb_MDS)
bb_MDS$plotID = rownames(bb_MDS)

ani_MDS_df = merge(ani_MDS_df, bb_MDS, all.x = TRUE)


write.csv2(ani_MDS_df, 
           paste(filepath, "trophicLev_animals_TO_NE_AC_NMDS_sites.csv"), 
           row.names = FALSE)
