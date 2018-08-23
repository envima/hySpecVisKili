library(vegan)
library(plyr)
library(betapart)
library(foreach)
################################################################################
filepath = "E:/hyperspec_clean/clean/"
bd_ani = read.csv2(paste0(filepath, "animals_plotIDcomplete_Syn1.csv"))
bd_ani_smll = bd_ani[,c(3:4,14:73)]
bd_ani_smll$type = paste(bd_ani_smll$taxon, bd_ani_smll$species, sep = "_")
names = bd_ani_smll$type

bd_ani_trns <- data.frame(t(bd_ani_smll[,c(3:62)]))
colnames(bd_ani_trns) = names

################################################################################
##### calc NMDS for each animal taxon
###
# ants
ants = bd_ani_trns[,c(1:79)]
any(is.na(ants))

# presence absence data
ants_pa = as.data.frame(ifelse(ants == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
ants_pairs = beta.pair(ants_pa, index.family = "jaccard") 

ants_cln = foreach(i = seq(ants_pairs)) %do% {
  as.matrix(ants_pairs[[i]])[-c(6:10,16:22,24:35,41:45),-c(6:10,16:22,24:35,41:45)]
}

ants_MDS = foreach(i = seq(ants_cln), .combine = "cbind") %do% {
  scores(metaMDS(ants_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(ants_MDS) = c("ants_jtu_NMDS1", "ants_jtu_NMDS2", 
                       "ants_jne_NMDS1", "ants_jne_NMDS2",
                       "ants_jac_NMDS1", "ants_jac_NMDS2")
ants_MDS = as.data.frame(ants_MDS)
ants_MDS$plotID = rownames(ants_MDS)

###
# bees
bees = bd_ani_trns[,c(80:195)]
any(is.na(bees))

# presence absence data
bees_pa = as.data.frame(ifelse(bees == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
bees_pairs = beta.pair(bees_pa, index.family = "jaccard") 
#tst = as.matrix(bees_pairs[[1]])

bees_cln = foreach(i = seq(bees_pairs)) %do% {
  as.matrix(bees_pairs[[i]])[-c(16,21,26:27,31:33),-c(16,21,26:27,31:33)]
}

bees_MDS = foreach(i = seq(bees_cln), .combine = "cbind") %do% {
  scores(metaMDS(bees_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(bees_MDS) = c("bees_jtu_NMDS1", "bees_jtu_NMDS2", 
                       "bees_jne_NMDS1", "bees_jne_NMDS2",
                       "bees_jac_NMDS1", "bees_jac_NMDS2")
bees_MDS = as.data.frame(bees_MDS)
bees_MDS$plotID = rownames(bees_MDS)

ani_MDS_df = merge(ants_MDS, bees_MDS, all.y = TRUE)

###
# birds
birds = bd_ani_trns[,c(196:397)]
any(is.na(birds))

# presence absence data
birds_pa = as.data.frame(ifelse(birds == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
birds_pairs = beta.pair(birds_pa, index.family = "jaccard") 
#tst = as.matrix(birds_pairs[[1]])

birds_cln = foreach(i = seq(birds_pairs)) %do% {
  as.matrix(birds_pairs[[i]])[-c(43),-c(43)]
}

birds_MDS = foreach(i = seq(birds_cln), .combine = "cbind") %do% {
  scores(metaMDS(birds_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(birds_MDS) = c("birds_jtu_NMDS1", "birds_jtu_NMDS2", 
                       "birds_jne_NMDS1", "birds_jne_NMDS2",
                       "birds_jac_NMDS1", "birds_jac_NMDS2")
birds_MDS = as.data.frame(birds_MDS)
birds_MDS$plotID = rownames(birds_MDS)

ani_MDS_df = merge(ani_MDS_df, birds_MDS, all.y = TRUE)

###
# moths
moths = bd_ani_trns[,c(398:777)]
any(is.na(moths))

# presence absence data
moths_pa = as.data.frame(ifelse(moths == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
moths_pairs = beta.pair(moths_pa, index.family = "jaccard") 
#tst = as.matrix(moths_pairs[[1]])

moths_cln = foreach(i = seq(moths_pairs)) %do% {
  as.matrix(moths_pairs[[i]])[-c(8:9,42:45),-c(8:9,42:45)]
}

moths_MDS = foreach(i = seq(moths_cln), .combine = "cbind") %do% {
  scores(metaMDS(moths_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(moths_MDS) = c("moths_jtu_NMDS1", "moths_jtu_NMDS2", 
                        "moths_jne_NMDS1", "moths_jne_NMDS2",
                        "moths_jac_NMDS1", "moths_jac_NMDS2")
moths_MDS = as.data.frame(moths_MDS)
moths_MDS$plotID = rownames(moths_MDS)

ani_MDS_df = merge(ani_MDS_df, moths_MDS, all.x = TRUE)


###
# syrphid
syrphid = bd_ani_trns[,c(778:820)]
any(is.na(syrphid))

# presence absence data
syrphid_pa = as.data.frame(ifelse(syrphid == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
syrphid_pairs = beta.pair(syrphid_pa, index.family = "jaccard") 
#tst = as.matrix(syrphid_pairs[[1]])

syrphid_cln = foreach(i = seq(syrphid_pairs)) %do% {
  as.matrix(syrphid_pairs[[i]])[-c(10,21,26:27,30,36,42,44:45),-c(10,21,26:27,30,36,42,44:45)]
}

syrphid_MDS = foreach(i = seq(syrphid_cln), .combine = "cbind") %do% {
  scores(metaMDS(syrphid_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(syrphid_MDS) = c("syrphid_jtu_NMDS1", "syrphid_jtu_NMDS2", 
                        "syrphid_jne_NMDS1", "syrphid_jne_NMDS2",
                        "syrphid_jac_NMDS1", "syrphid_jac_NMDS2")
syrphid_MDS = as.data.frame(syrphid_MDS)
syrphid_MDS$plotID = rownames(syrphid_MDS)

ani_MDS_df = merge(ani_MDS_df, syrphid_MDS, all.x = TRUE)

###
# bats
bats = bd_ani_trns[,c(821:840)]
any(is.na(bats))

# presence absence data
bats_pa = as.data.frame(ifelse(bats == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
bats_pairs = beta.pair(bats_pa, index.family = "jaccard") 
#tst = as.matrix(bats_pairs[[1]])

bats_cln = foreach(i = seq(bats_pairs)) %do% {
  as.matrix(bats_pairs[[i]])[-c(6,9,42:45),-c(6,9,42:45)]
}

bats_MDS = foreach(i = seq(bats_cln), .combine = "cbind") %do% {
  scores(metaMDS(bats_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(bats_MDS) = c("bats_jtu_NMDS1", "bats_jtu_NMDS2", 
                          "bats_jne_NMDS1", "bats_jne_NMDS2",
                          "bats_jac_NMDS1", "bats_jac_NMDS2")
bats_MDS = as.data.frame(bats_MDS)
bats_MDS$plotID = rownames(bats_MDS)

ani_MDS_df = merge(ani_MDS_df, bats_MDS, all.x = TRUE)

###
# gastropods|snails
gastropods = bd_ani_trns[,c(841:899)]
any(is.na(gastropods))

# presence absence data
gastropods_pa = as.data.frame(ifelse(gastropods == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
gastropods_pairs = beta.pair(gastropods_pa, index.family = "jaccard") 
#tst = as.matrix(gastropods_pairs[[1]])

gastropods_cln = foreach(i = seq(gastropods_pairs)) %do% {
  as.matrix(gastropods_pairs[[i]])[-c(36,39,44:45,51:55),-c(36,39,44:45,51:55)]
}

gastropods_MDS = foreach(i = seq(gastropods_cln), .combine = "cbind") %do% {
  scores(metaMDS(gastropods_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(gastropods_MDS) = c("gastropods_jtu_NMDS1", "gastropods_jtu_NMDS2", 
                       "gastropods_jne_NMDS1", "gastropods_jne_NMDS2",
                       "gastropods_jac_NMDS1", "gastropods_jac_NMDS2")
gastropods_MDS = as.data.frame(gastropods_MDS)
gastropods_MDS$plotID = rownames(gastropods_MDS)

ani_MDS_df = merge(ani_MDS_df, gastropods_MDS, all.x = TRUE)

###
# orthoptera
orthoptera = bd_ani_trns[,c(900:1054)]
any(is.na(orthoptera))

# presence absence data
orthoptera_pa = as.data.frame(ifelse(orthoptera == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
orthoptera_pairs = beta.pair(orthoptera_pa, index.family = "jaccard") 
#tst = as.matrix(orthoptera_pairs[[1]])

orthoptera_cln = foreach(i = seq(orthoptera_pairs)) %do% {
  as.matrix(orthoptera_pairs[[i]])[-c(6:10,20,26:27,30:32,42:43),-c(6:10,20,26:27,30:32,42:43)]
}

orthoptera_MDS = foreach(i = seq(orthoptera_cln), .combine = "cbind") %do% {
  scores(metaMDS(orthoptera_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(orthoptera_MDS) = c("orthoptera_jtu_NMDS1", "orthoptera_jtu_NMDS2", 
                             "orthoptera_jne_NMDS1", "orthoptera_jne_NMDS2",
                             "orthoptera_jac_NMDS1", "orthoptera_jac_NMDS2")
orthoptera_MDS = as.data.frame(orthoptera_MDS)
orthoptera_MDS$plotID = rownames(orthoptera_MDS)

ani_MDS_df = merge(ani_MDS_df, orthoptera_MDS, all.x = TRUE)

###
# parasitoids
parasitoids = bd_ani_trns[,c(1055:1574)]
any(is.na(parasitoids))

# presence absence data
parasitoids_pa = as.data.frame(ifelse(parasitoids == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
parasitoids_pairs = beta.pair(parasitoids_pa, index.family = "jaccard") 
tst = as.matrix(parasitoids_pairs[[1]])

parasitoids_cln = foreach(i = seq(parasitoids_pairs)) %do% {
  as.matrix(parasitoids_pairs[[i]])
}

parasitoids_MDS = foreach(i = seq(parasitoids_cln), .combine = "cbind") %do% {
  scores(metaMDS(parasitoids_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(parasitoids_MDS) = c("parasitoids_jtu_NMDS1", "parasitoids_jtu_NMDS2", 
                             "parasitoids_jne_NMDS1", "parasitoids_jne_NMDS2",
                             "parasitoids_jac_NMDS1", "parasitoids_jac_NMDS2")
parasitoids_MDS = as.data.frame(parasitoids_MDS)
parasitoids_MDS$plotID = rownames(parasitoids_MDS)

ani_MDS_df = merge(ani_MDS_df, parasitoids_MDS, all.y = TRUE)

###
# beetles
beetles = bd_ani_trns[,c(1575:1912)]
any(is.na(beetles))
beetles = na.omit(beetles)

# presence absence data
beetles_pa = as.data.frame(ifelse(beetles == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
beetles_pairs = beta.pair(beetles_pa, index.family = "jaccard") 
#tst = as.matrix(beetles_pairs[[1]])

beetles_cln = foreach(i = seq(beetles_pairs)) %do% {
  as.matrix(beetles_pairs[[i]])[-58,-58]
}

beetles_MDS = foreach(i = seq(beetles_cln), .combine = "cbind") %do% {
  scores(metaMDS(beetles_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(beetles_MDS) = c("beetles_jtu_NMDS1", "beetles_jtu_NMDS2", 
                              "beetles_jne_NMDS1", "beetles_jne_NMDS2",
                              "beetles_jac_NMDS1", "beetles_jac_NMDS2")
beetles_MDS = as.data.frame(beetles_MDS)
beetles_MDS$plotID = rownames(beetles_MDS)

ani_MDS_df = merge(ani_MDS_df, beetles_MDS, all.x = TRUE)

###
# aculeate
aculeate = bd_ani_trns[,c(1913:2031)]
any(is.na(aculeate))

# presence absence data
aculeate_pa = as.data.frame(ifelse(aculeate == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
aculeate_pairs = beta.pair(aculeate_pa, index.family = "jaccard") 
#tst = as.matrix(aculeate_pairs[[1]])

aculeate_cln = foreach(i = seq(aculeate_pairs)) %do% {
  as.matrix(aculeate_pairs[[i]])[-c(9:10,12,14,16:22,26:27,29,31:33,36,42,45),-c(9:10,12,14,16:22,26:27,29,31:33,36,42,45)]
}

aculeate_MDS = foreach(i = seq(aculeate_cln), .combine = "cbind") %do% {
  scores(metaMDS(aculeate_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(aculeate_MDS) = c("aculeate_jtu_NMDS1", "aculeate_jtu_NMDS2", 
                          "aculeate_jne_NMDS1", "aculeate_jne_NMDS2",
                          "aculeate_jac_NMDS1", "aculeate_jac_NMDS2")
aculeate_MDS = as.data.frame(aculeate_MDS)
aculeate_MDS$plotID = rownames(aculeate_MDS)

ani_MDS_df = merge(ani_MDS_df, aculeate_MDS, all.x = TRUE)

###
# millipedes
millipedes = bd_ani_trns[,c(2032:2059)]
any(is.na(millipedes))

# presence absence data
millipedes_pa = as.data.frame(ifelse(millipedes == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
millipedes_pairs = beta.pair(millipedes_pa, index.family = "jaccard") 
#tst = as.matrix(millipedes_pairs[[1]])

millipedes_cln = foreach(i = seq(millipedes_pairs)) %do% {
  as.matrix(millipedes_pairs[[i]])[-c(6:10,12,17:24,26:27,31:34,36:37,41:45,50,52,54:55,59:60),-c(6:10,12,17:24,26:27,31:34,36:37,41:45,50,52,54:55,59:60)]
}

millipedes_MDS = foreach(i = seq(millipedes_cln), .combine = "cbind") %do% {
  scores(metaMDS(millipedes_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(millipedes_MDS) = c("millipedes_jtu_NMDS1", "millipedes_jtu_NMDS2", 
                           "millipedes_jne_NMDS1", "millipedes_jne_NMDS2",
                           "millipedes_jac_NMDS1", "millipedes_jac_NMDS2")
millipedes_MDS = as.data.frame(millipedes_MDS)
millipedes_MDS$plotID = rownames(millipedes_MDS)

ani_MDS_df = merge(ani_MDS_df, millipedes_MDS, all.x = TRUE)

###
# dungbeetles
dungbeetles = bd_ani_trns[,c(2060:2131)]
any(is.na(dungbeetles))

# presence absence data
dungbeetles_pa = as.data.frame(ifelse(dungbeetles == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
dungbeetles_pairs = beta.pair(dungbeetles_pa, index.family = "jaccard") 
#tst = as.matrix(dungbeetles_pairs[[1]])

dungbeetles_cln = foreach(i = seq(dungbeetles_pairs)) %do% {
  as.matrix(dungbeetles_pairs[[i]])[-c(6:10,18:35,41:45),-c(6:10,18:35,41:45)]
}

dungbeetles_MDS = foreach(i = seq(dungbeetles_cln), .combine = "cbind") %do% {
  scores(metaMDS(dungbeetles_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(dungbeetles_MDS) = c("dungbeetles_jtu_NMDS1", "dungbeetles_jtu_NMDS2", 
                             "dungbeetles_jne_NMDS1", "dungbeetles_jne_NMDS2",
                             "dungbeetles_jac_NMDS1", "dungbeetles_jac_NMDS2")
dungbeetles_MDS = as.data.frame(dungbeetles_MDS)
dungbeetles_MDS$plotID = rownames(dungbeetles_MDS)

ani_MDS_df = merge(ani_MDS_df, dungbeetles_MDS, all.x = TRUE)

###
# mammals
mammals = bd_ani_trns[,c(2132:2164)]
any(is.na(mammals))

# presence absence data
mammals_pa = as.data.frame(ifelse(mammals == 0, 0, 1))

# calc turnover and nestedness with betaPART::beta.pair
mammals_pairs = beta.pair(mammals_pa, index.family = "jaccard") 
tst = as.matrix(mammals_pairs[[1]])

mammals_cln = foreach(i = seq(mammals_pairs)) %do% {
  as.matrix(mammals_pairs[[i]])[-c(6,34,40:41,43:44,53),-c(6,34,40:41,43:44,53)]
}

mammals_MDS = foreach(i = seq(mammals_cln), .combine = "cbind") %do% {
  scores(metaMDS(mammals_cln[[i]], distance = "jaccard"), 
         choices = c(1,2), display = "sites")
}
colnames(mammals_MDS) = c("mammals_jtu_NMDS1", "mammals_jtu_NMDS2", 
                              "mammals_jne_NMDS1", "mammals_jne_NMDS2",
                              "mammals_jac_NMDS1", "mammals_jac_NMDS2")
mammals_MDS = as.data.frame(mammals_MDS)
mammals_MDS$plotID = rownames(mammals_MDS)

ani_MDS_df = merge(ani_MDS_df, mammals_MDS, all.x = TRUE)
write.csv2(ani_MDS_df, 
           paste(filepath, "taxa_animals_moths_TO_NE_AC_NMDS_sites.csv"), 
           row.names = FALSE)




