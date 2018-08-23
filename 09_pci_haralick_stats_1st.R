library(hsdar)
library(foreach)
library(doParallel)

setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/1st_sd_hara/pci_sd_16/")
filepath_raster <- "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/1st_sd_hara/pci_sd_16/"

# one plot aim: all plots
cl <- makeCluster(12)
registerDoParallel(cl)

# vegIndices raster files
fls_hps <- list.files(paste0(filepath_raster), full.names = FALSE,
                      recursive = FALSE, pattern = "*.tif")


fls_hps_stck <- foreach(i = seq(fls_hps), .packages = "raster") %dopar% {
  stack(fls_hps[[i]])
}

#vegindex_names <- vegindex() 
#veg <- rep(vegindex_names[c(4,68,87,89)], each = 29)

haralick <- c("smpl_energy", "smpl_entropy", "smpl_correlation", "smpl_inverse_difference_moment", 
              "smpl_inertia", "smpl_cluster_shade", "smpl_cluster_prominence", "smpl_haralick_correlation",
              "adv_mean", "adv_variance", "adv_dissimilarity", "adv_sum_average", "adv_sum_variance", 
              "adv_sum_entropy", "adv_diff_entropies", "adv_diff_variances", "adv_IC1", "adv_IC2",
              "hgr_short_run_emphasis", "hgr_long_run_emphasis", "hgr_grey_level_nonuniformity",
              "hgr_run_length_nonuniformity", "hgr_run_percentage", "hgr_low_grey_level_run_emphasis", 
              "hgr_high_grey_level_run_emphasis", "hgr_short_run_low_grey_level_emphasis", 
              "hgr_short_run_high_grey_level_emphasis", "hgr_long_run_low_grey_level_emphasis", 
              "hgr_long_run_high_grey_level_emphasis")


### create colnames 
# haralick 10
veg_hara_10_mean <- paste("16", haralick, 10, "mean", sep = "_") #veg
veg_hara_10_sd <- paste("16", haralick, 10, "sd", sep = "_") #veg
veg_hara_10 <- sort(c(veg_hara_10_mean, veg_hara_10_sd))

# haralick 02
veg_hara_02_mean <- paste("16", haralick, 02, "mean", sep = "_")
veg_hara_02_sd <- paste("16", haralick, 02, "sd", sep = "_")
veg_hara_02 <- sort(c(veg_hara_02_mean, veg_hara_02_sd))

# haralick 10
veg_hara_05_mean <- paste("16", haralick, 05, "mean", sep = "_")
veg_hara_05_sd <- paste("16", haralick, 05, "sd", sep = "_")
veg_hara_05 <- sort(c(veg_hara_05_mean, veg_hara_05_sd))

# haralick 30
veg_hara_30_mean <- paste("16", haralick, 30, "mean", sep = "_")
veg_hara_30_sd <- paste("16", haralick, 30, "sd", sep = "_")
veg_hara_30 <- sort(c(veg_hara_30_mean, veg_hara_30_sd))


####
####
# cof1 - 02
fls_hps_1.1 <- fls_hps_stck[[1]]
vegIndex.Vars.cof1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.cof1) <- veg_hara_02
vegIndex.Vars.cof1$plotID <- "cof1"
vegIndex.Vars.cof1 <- vegIndex.Vars.cof1[,c(59,1:58)]

# cof1 - 05
fls_hps_1.2 <- fls_hps_stck[[2]]
vegIndex.Vars.cof1.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.cof1.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.cof1, vegIndex.Vars.cof1.2)

# cof1 - 10
fls_hps_1.3 <- fls_hps_stck[[3]]
vegIndex.Vars.cof1.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.cof1.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.cof1.3)

# cof1 - 30
fls_hps_1.5 <- fls_hps_stck[[4]]
vegIndex.Vars.cof1.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.cof1.5) <- veg_hara_30
vegIndex.Vars.cof1 <- cbind(vegIndex.Vars.all, vegIndex.Vars.cof1.5)

##
# cof2 - 02
fls_hps_1.1 <- fls_hps_stck[[5]]
vegIndex.Vars.cof2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.cof2) <- veg_hara_02
vegIndex.Vars.cof2$plotID <- "cof2"
vegIndex.Vars.cof2 <- vegIndex.Vars.cof2[,c(59,1:58)]

# cof2 - 05
fls_hps_1.2 <- fls_hps_stck[[6]]
vegIndex.Vars.cof2.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.cof2.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.cof2, vegIndex.Vars.cof2.2)

# cof2 - 10
fls_hps_1.3 <- fls_hps_stck[[7]]
vegIndex.Vars.cof2.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.cof2.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.cof2.3)

# cof2 - 30
fls_hps_1.5 <- fls_hps_stck[[8]]
vegIndex.Vars.cof2.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.cof2.5) <- veg_hara_30
vegIndex.Vars.cof2 <- cbind(vegIndex.Vars.all, vegIndex.Vars.cof2.5)
vegIndex.Vars <- rbind(vegIndex.Vars.cof1, vegIndex.Vars.cof2)

##
# cof3 - 02
fls_hps_1.1 <- fls_hps_stck[[9]]
vegIndex.Vars.cof3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get neames of vegIndices
colnames(vegIndex.Vars.cof3) <- veg_hara_02
vegIndex.Vars.cof3$plotID <- "cof3"
vegIndex.Vars.cof3 <- vegIndex.Vars.cof3[,c(59,1:58)]

# cof3 - 05
fls_hps_1.2 <- fls_hps_stck[[10]]
vegIndex.Vars.cof3.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.cof3.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.cof3, vegIndex.Vars.cof3.2)

# cof3 - 10
fls_hps_1.3 <- fls_hps_stck[[11]]
vegIndex.Vars.cof3.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.cof3.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.cof3.3)

# cof3 - 30
fls_hps_1.5 <- fls_hps_stck[[12]]
vegIndex.Vars.cof3.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.cof3.5) <- veg_hara_30
vegIndex.Vars.cof3 <- cbind(vegIndex.Vars.all, vegIndex.Vars.cof3.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.cof3)


##
# cof4 - 02
fls_hps_1.1 <- fls_hps_stck[[13]]
vegIndex.Vars.cof4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get neames of vegIndices
colnames(vegIndex.Vars.cof4) <- veg_hara_02
vegIndex.Vars.cof4$plotID <- "cof4"
vegIndex.Vars.cof4 <- vegIndex.Vars.cof4[,c(59, 1:58)]

# cof4 - 05
fls_hps_1.2 <- fls_hps_stck[[14]]
vegIndex.Vars.cof4.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.cof4.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.cof4, vegIndex.Vars.cof4.2)

# cof4 - 10
fls_hps_1.3 <- fls_hps_stck[[15]]
vegIndex.Vars.cof4.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.cof4.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.cof4.3)

# cof4 - 30
fls_hps_1.5 <- fls_hps_stck[[16]]
vegIndex.Vars.cof4.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.cof4.5) <- veg_hara_30
vegIndex.Vars.cof4 <- cbind(vegIndex.Vars.all, vegIndex.Vars.cof4.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.cof4)

##
# cof5 - 02
fls_hps_1.1 <- fls_hps_stck[[17]]
vegIndex.Vars.cof5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get neames of vegIndices
colnames(vegIndex.Vars.cof5) <- veg_hara_02
vegIndex.Vars.cof5$plotID <- "cof5"
vegIndex.Vars.cof5 <- vegIndex.Vars.cof5[,c(59,1:58)]

# cof5 - 05
fls_hps_1.2 <- fls_hps_stck[[18]]
vegIndex.Vars.cof5.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.cof5.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.cof5, vegIndex.Vars.cof5.2)

# cof5 - 10
fls_hps_1.3 <- fls_hps_stck[[19]]
vegIndex.Vars.cof5.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.cof5.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.cof5.3)

# cof5 - 30
fls_hps_1.5 <- fls_hps_stck[[20]]
vegIndex.Vars.cof5.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.cof5.5) <- veg_hara_30
vegIndex.Vars.cof5 <- cbind(vegIndex.Vars.all, vegIndex.Vars.cof5.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.cof5)

##
# flm1 - 02
fls_hps_1.1 <- fls_hps_stck[[21]]
vegIndex.Vars.flm1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get neames of vegIndices
colnames(vegIndex.Vars.flm1) <- veg_hara_02
vegIndex.Vars.flm1$plotID <- "flm1"
vegIndex.Vars.flm1 <- vegIndex.Vars.flm1[,c(59,1:58)]

# flm1 - 05
fls_hps_1.2 <- fls_hps_stck[[22]]
vegIndex.Vars.flm1.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.flm1.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.flm1, vegIndex.Vars.flm1.2)

# flm1 - 10
fls_hps_1.3 <- fls_hps_stck[[23]]
vegIndex.Vars.flm1.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.flm1.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.flm1.3)

# flm1 - 30
fls_hps_1.5 <- fls_hps_stck[[24]]
vegIndex.Vars.flm1.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.flm1.5) <- veg_hara_30
vegIndex.Vars.flm1 <- cbind(vegIndex.Vars.all, vegIndex.Vars.flm1.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.flm1)

##
# flm2 - 02
fls_hps_1.1 <- fls_hps_stck[[25]]
vegIndex.Vars.flm2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get neames of vegIndices
colnames(vegIndex.Vars.flm2) <- veg_hara_02
vegIndex.Vars.flm2$plotID <- "flm2"
vegIndex.Vars.flm2 <- vegIndex.Vars.flm2[,c(59,1:58)]

# flm2 - 05
fls_hps_1.2 <- fls_hps_stck[[26]]
vegIndex.Vars.flm2.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.flm2.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.flm2, vegIndex.Vars.flm2.2)

# flm2 - 10
fls_hps_1.3 <- fls_hps_stck[[27]]
vegIndex.Vars.flm2.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.flm2.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.flm2.3)

# flm2 - 30
fls_hps_1.5 <- fls_hps_stck[[28]]
vegIndex.Vars.flm2.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.flm2.5) <- veg_hara_30
vegIndex.Vars.flm2 <- cbind(vegIndex.Vars.all, vegIndex.Vars.flm2.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.flm2)

##
# flm3 - 02
fls_hps_1.1 <- fls_hps_stck[[29]]
vegIndex.Vars.flm3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get neames of vegIndices
colnames(vegIndex.Vars.flm3) <- veg_hara_02
vegIndex.Vars.flm3$plotID <- "flm3"
vegIndex.Vars.flm3 <- vegIndex.Vars.flm3[,c(59,1:58)]

# flm3 - 05
fls_hps_1.2 <- fls_hps_stck[[30]]
vegIndex.Vars.flm3.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.flm3.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.flm3, vegIndex.Vars.flm3.2)

# flm3 - 10
fls_hps_1.3 <- fls_hps_stck[[31]]
vegIndex.Vars.flm3.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.flm3.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.flm3.3)

# flm3 - 30
fls_hps_1.5 <- fls_hps_stck[[32]]
vegIndex.Vars.flm3.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.flm3.5) <- veg_hara_30
vegIndex.Vars.flm3 <- cbind(vegIndex.Vars.all, vegIndex.Vars.flm3.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.flm3)

##
# flm4 - 02
fls_hps_1.1 <- fls_hps_stck[[33]]
vegIndex.Vars.flm4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.flm4) <- veg_hara_02
vegIndex.Vars.flm4$plotID <- "flm4"
vegIndex.Vars.flm4 <- vegIndex.Vars.flm4[,c(59,1:58)]

# flm4 - 05
fls_hps_1.2 <- fls_hps_stck[[34]]
vegIndex.Vars.flm4.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.flm4.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.flm4, vegIndex.Vars.flm4.2)

# flm4 - 10
fls_hps_1.3 <- fls_hps_stck[[35]]
vegIndex.Vars.flm4.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
colnames(vegIndex.Vars.flm4.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.flm4.3)

# flm4 - 30
fls_hps_1.5 <- fls_hps_stck[[36]]
vegIndex.Vars.flm4.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.flm4.5) <- veg_hara_30
vegIndex.Vars.flm4 <- cbind(vegIndex.Vars.all, vegIndex.Vars.flm4.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.flm4)

##
# flm6 - 02
fls_hps_1.1 <- fls_hps_stck[[37]]
vegIndex.Vars.flm6 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.flm6) <- veg_hara_02
vegIndex.Vars.flm6$plotID <- "flm6"
vegIndex.Vars.flm6 <- vegIndex.Vars.flm6[,c(59,1:58)]

# flm6 - 05
fls_hps_1.2 <- fls_hps_stck[[38]]
vegIndex.Vars.flm6.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.flm6.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.flm6, vegIndex.Vars.flm6.2)

# flm6 - 10
fls_hps_1.3 <- fls_hps_stck[[39]]
vegIndex.Vars.flm6.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
colnames(vegIndex.Vars.flm6.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.flm6.3)

# flm6 - 30
fls_hps_1.5 <- fls_hps_stck[[40]]
vegIndex.Vars.flm6.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.flm6.5) <- veg_hara_30
vegIndex.Vars.flm6 <- cbind(vegIndex.Vars.all, vegIndex.Vars.flm6.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.flm6)

##
# foc1 - 02
fls_hps_1.1 <- fls_hps_stck[[41]]
vegIndex.Vars.foc1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get neames of vegIndices
colnames(vegIndex.Vars.foc1) <- veg_hara_02
vegIndex.Vars.foc1$plotID <- "foc1"
vegIndex.Vars.foc1 <- vegIndex.Vars.foc1[,c(59,1:58)]

# foc1 - 05
fls_hps_1.2 <- fls_hps_stck[[42]]
vegIndex.Vars.foc1.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.foc1.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.foc1, vegIndex.Vars.foc1.2)

# foc1 - 10
fls_hps_1.3 <- fls_hps_stck[[43]]
vegIndex.Vars.foc1.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.foc1.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.foc1.3)

# foc1 - 30
fls_hps_1.5 <- fls_hps_stck[[44]]
vegIndex.Vars.foc1.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.foc1.5) <- veg_hara_30
vegIndex.Vars.foc1 <- cbind(vegIndex.Vars.all, vegIndex.Vars.foc1.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.foc1)


##
# foc6 - 02
fls_hps_1.1 <- fls_hps_stck[[45]]
vegIndex.Vars.foc6 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get neames of vegIndices
colnames(vegIndex.Vars.foc6) <- veg_hara_02
vegIndex.Vars.foc6$plotID <- "foc6"
vegIndex.Vars.foc6 <- vegIndex.Vars.foc6[,c(59,1:58)]

# foc6 - 05
fls_hps_1.2 <- fls_hps_stck[[46]]
vegIndex.Vars.foc6.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.foc6.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.foc6, vegIndex.Vars.foc6.2)

# foc6 - 10
fls_hps_1.3 <- fls_hps_stck[[47]]
vegIndex.Vars.foc6.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.foc6.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.foc6.3)

# foc6 - 30
fls_hps_1.5 <- fls_hps_stck[[48]]
vegIndex.Vars.foc6.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.foc6.5) <- veg_hara_30
vegIndex.Vars.foc6 <- cbind(vegIndex.Vars.all, vegIndex.Vars.foc6.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.foc6)

##
# fod5 - 02
fls_hps_1.1 <- fls_hps_stck[[49]]
vegIndex.Vars.fod5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fod5) <- veg_hara_02
vegIndex.Vars.fod5$plotID <- "fod5"
vegIndex.Vars.fod5 <- vegIndex.Vars.fod5[,c(59,1:58)]

# fod5 - 05
fls_hps_1.2 <- fls_hps_stck[[50]]
vegIndex.Vars.fod5.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fod5.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fod5, vegIndex.Vars.fod5.2)

# fod5 - 10
fls_hps_1.3 <- fls_hps_stck[[51]]
vegIndex.Vars.fod5.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fod5.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fod5.3)

# fod5 - 30
fls_hps_1.5 <- fls_hps_stck[[52]]
vegIndex.Vars.fod5.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fod5.5) <- veg_hara_30
vegIndex.Vars.fod5 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fod5.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.fod5)

##
# gra1 - 02
fls_hps_1.1 <- fls_hps_stck[[53]]
vegIndex.Vars.gra1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.gra1) <- veg_hara_02
vegIndex.Vars.gra1$plotID <- "gra1"
vegIndex.Vars.gra1 <- vegIndex.Vars.gra1[,c(59,1:58)]

# gra1 - 05
fls_hps_1.2 <- fls_hps_stck[[54]]
vegIndex.Vars.gra1.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.gra1.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.gra1, vegIndex.Vars.gra1.2)

# gra1 - 10
fls_hps_1.3 <- fls_hps_stck[[55]]
vegIndex.Vars.gra1.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.gra1.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.gra1.3)

# gra1 - 30
fls_hps_1.5 <- fls_hps_stck[[56]]
vegIndex.Vars.gra1.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.gra1.5) <- veg_hara_30
vegIndex.Vars.gra1 <- cbind(vegIndex.Vars.all, vegIndex.Vars.gra1.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.gra1)

##
# gra2 - 02
fls_hps_1.1 <- fls_hps_stck[[57]]
vegIndex.Vars.gra2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.gra2) <- veg_hara_02
vegIndex.Vars.gra2$plotID <- "gra2"
vegIndex.Vars.gra2 <- vegIndex.Vars.gra2[,c(59,1:58)]

# gra2 - 05
fls_hps_1.2 <- fls_hps_stck[[58]]
vegIndex.Vars.gra2.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.gra2.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.gra2, vegIndex.Vars.gra2.2)

# gra2 - 10
fls_hps_1.3 <- fls_hps_stck[[59]]
vegIndex.Vars.gra2.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.gra2.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.gra2.3)

# gra2 - 30
fls_hps_1.5 <- fls_hps_stck[[60]]
vegIndex.Vars.gra2.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.gra2.5) <- veg_hara_30
vegIndex.Vars.gra2 <- cbind(vegIndex.Vars.all, vegIndex.Vars.gra2.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.gra2)

##
# gra3 - 02
fls_hps_1.1 <- fls_hps_stck[[61]]
vegIndex.Vars.gra3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices 
colnames(vegIndex.Vars.gra3) <- veg_hara_02
vegIndex.Vars.gra3$plotID <- "gra3"
vegIndex.Vars.gra3 <- vegIndex.Vars.gra3[,c(59,1:58)]

# gra3 - 05
fls_hps_1.2 <- fls_hps_stck[[62]]
vegIndex.Vars.gra3.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.gra3.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.gra3, vegIndex.Vars.gra3.2)

# gra3 - 10
fls_hps_1.3 <- fls_hps_stck[[63]]
vegIndex.Vars.gra3.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.gra3.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.gra3.3)

# gra3 - 30
fls_hps_1.5 <- fls_hps_stck[[64]]
vegIndex.Vars.gra3.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.gra3.5) <- veg_hara_30
vegIndex.Vars.gra3 <- cbind(vegIndex.Vars.all, vegIndex.Vars.gra3.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.gra3)

##
# gra4 - 02
fls_hps_1.1 <- fls_hps_stck[[65]]
vegIndex.Vars.gra4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.gra4) <- veg_hara_02
vegIndex.Vars.gra4$plotID <- "gra4"
vegIndex.Vars.gra4 <- vegIndex.Vars.gra4[,c(59,1:58)]

# gra4 - 05
fls_hps_1.2 <- fls_hps_stck[[66]]
vegIndex.Vars.gra4.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.gra4.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.gra4, vegIndex.Vars.gra4.2)

# gra4 - 10
fls_hps_1.3 <- fls_hps_stck[[67]]
vegIndex.Vars.gra4.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.gra4.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.gra4.3)

# gra4 - 30
fls_hps_1.5 <- fls_hps_stck[[68]]
vegIndex.Vars.gra4.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.gra4.5) <- veg_hara_30
vegIndex.Vars.gra4 <- cbind(vegIndex.Vars.all, vegIndex.Vars.gra4.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.gra4)

##
# gra5 - 02
fls_hps_1.1 <- fls_hps_stck[[69]]
vegIndex.Vars.gra5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.gra5) <- veg_hara_02
vegIndex.Vars.gra5$plotID <- "gra5"
vegIndex.Vars.gra5 <- vegIndex.Vars.gra5[,c(59,1:58)]

# gra5 - 05
fls_hps_1.2 <- fls_hps_stck[[70]]
vegIndex.Vars.gra5.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.gra5.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.gra5, vegIndex.Vars.gra5.2)

# gra5 - 10
fls_hps_1.3 <- fls_hps_stck[[71]]
vegIndex.Vars.gra5.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.gra5.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.gra5.3)

# gra5 - 30
fls_hps_1.5 <- fls_hps_stck[[72]]
vegIndex.Vars.gra5.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.gra5.5) <- veg_hara_30
vegIndex.Vars.gra5 <- cbind(vegIndex.Vars.all, vegIndex.Vars.gra5.5)
vegIndex.Vars <- rbind(vegIndex.Vars,vegIndex.Vars.gra5)

##
# gra6 - 02
fls_hps_1.1 <- fls_hps_stck[[73]]
vegIndex.Vars.gra6 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.gra6) <- veg_hara_02
vegIndex.Vars.gra6$plotID <- "gra6"
vegIndex.Vars.gra6 <- vegIndex.Vars.gra6[,c(59,1:58)]

# gra6 - 05
fls_hps_1.2 <- fls_hps_stck[[74]]
vegIndex.Vars.gra6.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.gra6.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.gra6, vegIndex.Vars.gra6.2)

# gra6 - 10
fls_hps_1.3 <- fls_hps_stck[[75]]
vegIndex.Vars.gra6.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.gra6.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.gra6.3)

# gra6 - 30
fls_hps_1.5 <- fls_hps_stck[[76]]
vegIndex.Vars.gra6.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.gra6.5) <- veg_hara_30
vegIndex.Vars.gra6 <- cbind(vegIndex.Vars.all, vegIndex.Vars.gra6.5)
vegIndex.Vars <- rbind(vegIndex.Vars,vegIndex.Vars.gra6)

##
# hom1 - 02
fls_hps_1.1 <- fls_hps_stck[[77]]
vegIndex.Vars.hom1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.hom1) <- veg_hara_02
vegIndex.Vars.hom1$plotID <- "hom1"
vegIndex.Vars.hom1 <- vegIndex.Vars.hom1[,c(59,1:58)]

# hom1 - 05
fls_hps_1.2 <- fls_hps_stck[[78]]
vegIndex.Vars.hom1.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hom1.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.hom1, vegIndex.Vars.hom1.2)

# hom1 - 10
fls_hps_1.3 <- fls_hps_stck[[79]]
vegIndex.Vars.hom1.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hom1.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.hom1.3)

# hom1 - 30
fls_hps_1.5 <- fls_hps_stck[[80]]
vegIndex.Vars.hom1.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hom1.5) <- veg_hara_30
vegIndex.Vars.hom1 <- cbind(vegIndex.Vars.all, vegIndex.Vars.hom1.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.hom1)

##
# hom2 - 02
fls_hps_1.1 <- fls_hps_stck[[81]]
vegIndex.Vars.hom2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get neames of vegIndices
colnames(vegIndex.Vars.hom2) <- veg_hara_02
vegIndex.Vars.hom2$plotID <- "hom2"
vegIndex.Vars.hom2 <- vegIndex.Vars.hom2[,c(59,1:58)]

# hom2 - 05
fls_hps_1.2 <- fls_hps_stck[[82]]
vegIndex.Vars.hom2.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.hom2.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.hom2, vegIndex.Vars.hom2.2)

# hom2 - 10
fls_hps_1.3 <- fls_hps_stck[[83]]
vegIndex.Vars.hom2.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.hom2.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.hom2.3)

# hom2 - 30
fls_hps_1.5 <- fls_hps_stck[[84]]
vegIndex.Vars.hom2.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get neames of vegIndices
colnames(vegIndex.Vars.hom2.5) <- veg_hara_30
vegIndex.Vars.hom2 <- cbind(vegIndex.Vars.all, vegIndex.Vars.hom2.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.hom2)

##
# hom3 - 02
fls_hps_1.1 <- fls_hps_stck[[85]]
vegIndex.Vars.hom3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.hom3) <- veg_hara_02
vegIndex.Vars.hom3$plotID <- "hom3"
vegIndex.Vars.hom3 <- vegIndex.Vars.hom3[,c(59,1:58)]

# hom3 - 05
fls_hps_1.2 <- fls_hps_stck[[86]]
vegIndex.Vars.hom3.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hom3.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.hom3, vegIndex.Vars.hom3.2)

# hom3 - 10
fls_hps_1.3 <- fls_hps_stck[[87]]
vegIndex.Vars.hom3.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hom3.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.hom3.3)

# hom3 - 30
fls_hps_1.5 <- fls_hps_stck[[88]]
vegIndex.Vars.hom3.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hom3.5) <- veg_hara_30
vegIndex.Vars.hom3 <- cbind(vegIndex.Vars.all, vegIndex.Vars.hom3.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.hom3)

##
# hom4 - 02
fls_hps_1.1 <- fls_hps_stck[[89]]
vegIndex.Vars.hom4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.hom4) <- veg_hara_02
vegIndex.Vars.hom4$plotID <- "hom4"
vegIndex.Vars.hom4 <- vegIndex.Vars.hom4[,c(59,1:58)]

# hom4 - 05
fls_hps_1.2 <- fls_hps_stck[[90]]
vegIndex.Vars.hom4.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hom4.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.hom4, vegIndex.Vars.hom4.2)

# hom4 - 10
fls_hps_1.3 <- fls_hps_stck[[91]]
vegIndex.Vars.hom4.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hom4.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.hom4.3)

# hom4 - 30
fls_hps_1.5 <- fls_hps_stck[[92]]
vegIndex.Vars.hom4.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hom4.5) <- veg_hara_30
vegIndex.Vars.hom4 <- cbind(vegIndex.Vars.all, vegIndex.Vars.hom4.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.hom4)

##
# hom5 - 02
fls_hps_1.1 <- fls_hps_stck[[93]]
vegIndex.Vars.hom5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.hom5) <- veg_hara_02
vegIndex.Vars.hom5$plotID <- "hom5"
vegIndex.Vars.hom5 <- vegIndex.Vars.hom5[,c(59,1:58)]

# hom5 - 05
fls_hps_1.2 <- fls_hps_stck[[94]]
vegIndex.Vars.hom5.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hom5.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.hom5, vegIndex.Vars.hom5.2)

# hom5 - 10
fls_hps_1.3 <- fls_hps_stck[[95]]
vegIndex.Vars.hom5.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hom5.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.hom5.3)

# hom5 - 30
fls_hps_1.5 <- fls_hps_stck[[96]]
vegIndex.Vars.hom5.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hom5.5) <- veg_hara_30
vegIndex.Vars.hom5 <- cbind(vegIndex.Vars.all, vegIndex.Vars.hom5.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.hom5)

##
# mai1 - 02
fls_hps_1.1 <- fls_hps_stck[[97]]
vegIndex.Vars.mai1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.mai1) <- veg_hara_02
vegIndex.Vars.mai1$plotID <- "mai1"
vegIndex.Vars.mai1 <- vegIndex.Vars.mai1[,c(59,1:58)]

# mai1 - 05
fls_hps_1.2 <- fls_hps_stck[[98]]
vegIndex.Vars.mai1.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.mai1.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.mai1, vegIndex.Vars.mai1.2)

# mai1 - 10
fls_hps_1.3 <- fls_hps_stck[[99]]
vegIndex.Vars.mai1.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.mai1.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.mai1.3)

# mai1 - 30
fls_hps_1.5 <- fls_hps_stck[[100]]
vegIndex.Vars.mai1.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.mai1.5) <- veg_hara_30
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.mai1.5)
vegIndex.Vars <- rbind(vegIndex.Vars,vegIndex.Vars.all)

##
# mai2 - 02
fls_hps_1.1 <- fls_hps_stck[[101]]
vegIndex.Vars.mai2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.mai2) <- veg_hara_02
vegIndex.Vars.mai2$plotID <- "mai2"
vegIndex.Vars.mai2 <- vegIndex.Vars.mai2[,c(59,1:58)]

# mai2 - 05
fls_hps_1.2 <- fls_hps_stck[[102]]
vegIndex.Vars.mai2.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.mai2.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.mai2, vegIndex.Vars.mai2.2)

# mai2 - 10
fls_hps_1.3 <- fls_hps_stck[[103]]
vegIndex.Vars.mai2.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.mai2.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.mai2.3)

# mai2 - 30
fls_hps_1.5 <- fls_hps_stck[[104]]
vegIndex.Vars.mai2.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get names of vegIndices
colnames(vegIndex.Vars.mai2.5) <- veg_hara_30
vegIndex.Vars.mai2 <- cbind(vegIndex.Vars.all, vegIndex.Vars.mai2.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.mai2)

##
# mai3 - 02
fls_hps_1.1 <- fls_hps_stck[[105]]
vegIndex.Vars.mai3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.mai3) <- veg_hara_02
vegIndex.Vars.mai3$plotID <- "mai3"
vegIndex.Vars.mai3 <- vegIndex.Vars.mai3[,c(59,1:58)]

# mai3 - 05
fls_hps_1.2 <- fls_hps_stck[[106]]
vegIndex.Vars.mai3.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.mai3.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.mai3, vegIndex.Vars.mai3.2)

# mai3 - 10
fls_hps_1.3 <- fls_hps_stck[[107]]
vegIndex.Vars.mai3.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.mai3.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.mai3.3)

# mai3 - 30
fls_hps_1.5 <- fls_hps_stck[[108]]
vegIndex.Vars.mai3.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.mai3.5) <- veg_hara_30
vegIndex.Vars.mai3 <- cbind(vegIndex.Vars.all, vegIndex.Vars.mai3.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.mai3)

##
# mai4 - 02
fls_hps_1.1 <- fls_hps_stck[[109]]
vegIndex.Vars.mai4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.mai4) <- veg_hara_02
vegIndex.Vars.mai4$plotID <- "mai4"
vegIndex.Vars.mai4 <- vegIndex.Vars.mai4[,c(59,1:58)]

# mai4 - 05
fls_hps_1.2 <- fls_hps_stck[[110]]
vegIndex.Vars.mai4.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.mai4.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.mai4, vegIndex.Vars.mai4.2)

# mai4 - 10
fls_hps_1.3 <- fls_hps_stck[[111]]
vegIndex.Vars.mai4.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.mai4.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.mai4.3)

# mai4 - 30
fls_hps_1.5 <- fls_hps_stck[[112]]
vegIndex.Vars.mai4.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.mai4.5) <- veg_hara_30
vegIndex.Vars.mai4 <- cbind(vegIndex.Vars.all, vegIndex.Vars.mai4.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.mai4)

##
# mai5 - 02
fls_hps_1.1 <- fls_hps_stck[[113]]
vegIndex.Vars.mai5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.mai5) <- veg_hara_02
vegIndex.Vars.mai5$plotID <- "mai5"
vegIndex.Vars.mai5 <- vegIndex.Vars.mai5[,c(59,1:58)]

# mai5 - 05
fls_hps_1.2 <- fls_hps_stck[[114]]
vegIndex.Vars.mai5.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.mai5.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.mai5, vegIndex.Vars.mai5.2)

# mai5 - 10
fls_hps_1.3 <- fls_hps_stck[[115]]
vegIndex.Vars.mai5.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.mai5.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.mai5.3)

# mai5 - 30
fls_hps_1.5 <- fls_hps_stck[[116]]
vegIndex.Vars.mai5.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.mai5.5) <- veg_hara_30
vegIndex.Vars.mai5 <- cbind(vegIndex.Vars.all, vegIndex.Vars.mai5.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.mai5)

##
# sav1 - 02
fls_hps_1.1 <- fls_hps_stck[[117]]
vegIndex.Vars.sav1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.sav1) <- veg_hara_02
vegIndex.Vars.sav1$plotID <- "sav1"
vegIndex.Vars.sav1 <- vegIndex.Vars.sav1[,c(59,1:58)]

# sav1 - 05
fls_hps_1.2 <- fls_hps_stck[[118]]
vegIndex.Vars.sav1.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.sav1.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.sav1, vegIndex.Vars.sav1.2)

# sav1 - 10
fls_hps_1.3 <- fls_hps_stck[[119]]
vegIndex.Vars.sav1.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.sav1.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.sav1.3)

# sav1 - 30
fls_hps_1.5 <- fls_hps_stck[[120]]
vegIndex.Vars.sav1.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.sav1.5)<- veg_hara_30
vegIndex.Vars.sav1 <- cbind(vegIndex.Vars.all, vegIndex.Vars.sav1.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.sav1)

##
# sav2 - 02
fls_hps_1.1 <- fls_hps_stck[[121]]
vegIndex.Vars.sav2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.sav2) <- veg_hara_02
vegIndex.Vars.sav2$plotID <- "sav2"
vegIndex.Vars.sav2 <- vegIndex.Vars.sav2[,c(59,1:58)]

# sav2 - 05
fls_hps_1.2 <- fls_hps_stck[[122]]
vegIndex.Vars.sav2.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.sav2.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.sav2, vegIndex.Vars.sav2.2)

# sav2 - 10
fls_hps_1.3 <- fls_hps_stck[[123]]
vegIndex.Vars.sav2.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.sav2.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.sav2.3)

# sav2 - 30
fls_hps_1.5 <- fls_hps_stck[[124]]
vegIndex.Vars.sav2.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.sav2.5) <- veg_hara_30
vegIndex.Vars.sav2 <- cbind(vegIndex.Vars.all, vegIndex.Vars.sav2.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.sav2)

##
# sav3 - 02
fls_hps_1.1 <- fls_hps_stck[[125]]
vegIndex.Vars.sav3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.sav3) <- veg_hara_02
vegIndex.Vars.sav3$plotID <- "sav3"
vegIndex.Vars.sav3 <- vegIndex.Vars.sav3[,c(59,1:58)]

# sav3 - 05
fls_hps_1.2 <- fls_hps_stck[[126]]
vegIndex.Vars.sav3.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.sav3.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.sav3, vegIndex.Vars.sav3.2)

# sav3 - 10
fls_hps_1.3 <- fls_hps_stck[[127]]
vegIndex.Vars.sav3.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.sav3.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.sav3.3)

# sav3 - 30
fls_hps_1.5 <- fls_hps_stck[[128]]
vegIndex.Vars.sav3.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.sav3.5) <- veg_hara_30
vegIndex.Vars.sav3 <- cbind(vegIndex.Vars.all, vegIndex.Vars.sav3.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.sav3)

##
# sav4 - 02
fls_hps_1.1 <- fls_hps_stck[[129]]
vegIndex.Vars.sav4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.sav4) <- veg_hara_02
vegIndex.Vars.sav4$plotID <- "sav4"
vegIndex.Vars.sav4 <- vegIndex.Vars.sav4[,c(59,1:58)]

# sav4 - 05
fls_hps_1.2 <- fls_hps_stck[[130]]
vegIndex.Vars.sav4.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.sav4.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.sav4, vegIndex.Vars.sav4.2)

# sav4 - 10
fls_hps_1.3 <- fls_hps_stck[[131]]
vegIndex.Vars.sav4.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get names of vegIndices
colnames(vegIndex.Vars.sav4.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.sav4.3)

# sav4 - 30
fls_hps_1.5 <- fls_hps_stck[[132]]
vegIndex.Vars.sav4.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.sav4.5) <- veg_hara_30
vegIndex.Vars.sav4 <- cbind(vegIndex.Vars.all, vegIndex.Vars.sav4.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.sav4)

##
# sav5 - 02
fls_hps_1.1 <- fls_hps_stck[[133]]
vegIndex.Vars.sav5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.sav5) <- veg_hara_02
vegIndex.Vars.sav5$plotID <- "sav5"
vegIndex.Vars.sav5 <- vegIndex.Vars.sav5[,c(59,1:58)]

# sav5 - 05
fls_hps_1.2 <- fls_hps_stck[[134]]
vegIndex.Vars.sav5.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.sav5.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.sav5, vegIndex.Vars.sav5.2)

# sav5 - 10
fls_hps_1.3 <- fls_hps_stck[[135]]
vegIndex.Vars.sav5.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.sav5.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.sav5.3)

# sav5 - 30
fls_hps_1.5 <- fls_hps_stck[[136]]
vegIndex.Vars.sav5.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.sav5.5) <- veg_hara_30
vegIndex.Vars.sav5 <- cbind(vegIndex.Vars.all, vegIndex.Vars.sav5.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.sav5)

write.csv2(vegIndex.Vars, 
           file = "pci_16_SD_haralick_stats_1st_wide.csv", 
           row.names = TRUE)

stopCluster(cl)


