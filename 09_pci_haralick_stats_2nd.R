library(hsdar)
library(foreach)
library(doParallel)

setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd_sd_hara/pci_sd_08/")
filepath_raster <- "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd_sd_hara/pci_sd_08/"


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
veg_hara_10_mean <- paste("08", haralick, 10, "mean", sep = "_") #veg
veg_hara_10_sd <- paste("08", haralick, 10, "sd", sep = "_")
veg_hara_10 <- sort(c(veg_hara_10_mean, veg_hara_10_sd))

# haralick 02
veg_hara_02_mean <- paste("08", haralick, 02, "mean", sep = "_")
veg_hara_02_sd <- paste("08", haralick, 02, "sd", sep = "_")
veg_hara_02 <- sort(c(veg_hara_02_mean, veg_hara_02_sd))

# haralick 10
veg_hara_05_mean <- paste("08", haralick, 05, "mean", sep = "_")
veg_hara_05_sd <- paste("08", haralick, 05, "sd", sep = "_")
veg_hara_05 <- sort(c(veg_hara_05_mean, veg_hara_05_sd))

# haralick 30
veg_hara_30_mean <- paste("08", haralick, 30, "mean", sep = "_")
veg_hara_30_sd <- paste("08", haralick, 30, "sd", sep = "_")
veg_hara_30 <- sort(c(veg_hara_30_mean, veg_hara_30_sd))


####
####
# fed1 - 02
fls_hps_1.1 <- fls_hps_stck[[1]]
vegIndex.Vars.fed1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fed1) <- veg_hara_02
vegIndex.Vars.fed1$plotID <- "fed1"
vegIndex.Vars.fed1 <- vegIndex.Vars.fed1[,c(59,1:58)]

# fed1 - 05
fls_hps_1.2 <- fls_hps_stck[[2]]
vegIndex.Vars.fed1.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fed1.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fed1, vegIndex.Vars.fed1.2)

# fed1 - 10
fls_hps_1.3 <- fls_hps_stck[[3]]
vegIndex.Vars.fed1.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fed1.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fed1.3)

# fed1 - 30
fls_hps_1.5 <- fls_hps_stck[[4]]
vegIndex.Vars.fed1.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fed1.5) <- veg_hara_30
vegIndex.Vars.fed1 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fed1.5)

##
# fed2 - 02
fls_hps_1.1 <- fls_hps_stck[[5]]
vegIndex.Vars.fed2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fed2) <- veg_hara_02
vegIndex.Vars.fed2$plotID <- "fed2"
vegIndex.Vars.fed2 <- vegIndex.Vars.fed2[,c(59,1:58)]

# fed2 - 05
fls_hps_1.2 <- fls_hps_stck[[6]]
vegIndex.Vars.fed2.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fed2.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fed2, vegIndex.Vars.fed2.2)

# fed2 - 10
fls_hps_1.3 <- fls_hps_stck[[7]]
vegIndex.Vars.fed2.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fed2.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fed2.3)

# fed2 - 30
fls_hps_1.5 <- fls_hps_stck[[8]]
vegIndex.Vars.fed2.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fed2.5) <- veg_hara_30
vegIndex.Vars.fed2 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fed2.5)
vegIndex.Vars <- rbind(vegIndex.Vars.fed1, vegIndex.Vars.fed2)

##
# fed3 - 02
fls_hps_1.1 <- fls_hps_stck[[9]]
vegIndex.Vars.fed3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fed3) <- veg_hara_02
vegIndex.Vars.fed3$plotID <- "fed3"
vegIndex.Vars.fed3 <- vegIndex.Vars.fed3[,c(59,1:58)]

# fed3 - 05
fls_hps_1.2 <- fls_hps_stck[[10]]
vegIndex.Vars.fed3.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fed3.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fed3, vegIndex.Vars.fed3.2)

# fed3 - 10
fls_hps_1.3 <- fls_hps_stck[[11]]
vegIndex.Vars.fed3.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fed3.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fed3.3)

# fed3 - 30
fls_hps_1.5 <- fls_hps_stck[[12]]
vegIndex.Vars.fed3.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fed3.5) <- veg_hara_30
vegIndex.Vars.fed3 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fed3.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.fed3)

##
# fed4 - 02
fls_hps_1.1 <- fls_hps_stck[[13]]
vegIndex.Vars.fed4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fed4) <- veg_hara_02
vegIndex.Vars.fed4$plotID <- "fed4"
vegIndex.Vars.fed4 <- vegIndex.Vars.fed4[,c(59, 1:58)]

# fed4 - 05
fls_hps_1.2 <- fls_hps_stck[[14]]
vegIndex.Vars.fed4.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fed4.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fed4, vegIndex.Vars.fed4.2)

# fed4 - 10
fls_hps_1.3 <- fls_hps_stck[[15]]
vegIndex.Vars.fed4.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fed4.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fed4.3)

# fed4 - 30
fls_hps_1.5 <- fls_hps_stck[[16]]
vegIndex.Vars.fed4.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fed4.5) <- veg_hara_30
vegIndex.Vars.fed4 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fed4.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.fed4)

##
# fed5 - 02
fls_hps_1.1 <- fls_hps_stck[[17]]
vegIndex.Vars.fed5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fed5) <- veg_hara_02
vegIndex.Vars.fed5$plotID <- "fed5"
vegIndex.Vars.fed5 <- vegIndex.Vars.fed5[,c(59,1:58)]

# fed5 - 05
fls_hps_1.2 <- fls_hps_stck[[18]]
vegIndex.Vars.fed5.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fed5.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fed5, vegIndex.Vars.fed5.2)

# fed5 - 10
fls_hps_1.3 <- fls_hps_stck[[19]]
vegIndex.Vars.fed5.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fed5.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fed5.3)

# fed5 - 30
fls_hps_1.5 <- fls_hps_stck[[20]]
vegIndex.Vars.fed5.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fed5.5) <- veg_hara_30
vegIndex.Vars.fed5 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fed5.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.fed5)

##
# fer0 - 02
fls_hps_1.1 <- fls_hps_stck[[21]]
vegIndex.Vars.fer0 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fer0) <- veg_hara_02
vegIndex.Vars.fer0$plotID <- "fer0"
vegIndex.Vars.fer0 <- vegIndex.Vars.fer0[,c(59,1:58)]

# fer0 - 05
fls_hps_1.2 <- fls_hps_stck[[22]]
vegIndex.Vars.fer0.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fer0.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fer0, vegIndex.Vars.fer0.2)

# fer0 - 10
fls_hps_1.3 <- fls_hps_stck[[23]]
vegIndex.Vars.fer0.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fer0.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fer0.3)

# fer0 - 30
fls_hps_1.5 <- fls_hps_stck[[24]]
vegIndex.Vars.fer0.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fer0.5) <- veg_hara_30
vegIndex.Vars.fer0 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fer0.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.fer0)

##
# fer2 - 02
fls_hps_1.1 <- fls_hps_stck[[25]]
vegIndex.Vars.fer2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fer2) <- veg_hara_02
vegIndex.Vars.fer2$plotID <- "fer2"
vegIndex.Vars.fer2 <- vegIndex.Vars.fer2[,c(59,1:58)]

# fer2 - 05
fls_hps_1.2 <- fls_hps_stck[[26]]
vegIndex.Vars.fer2.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fer2.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fer2, vegIndex.Vars.fer2.2)

# fer2 - 10
fls_hps_1.3 <- fls_hps_stck[[27]]
vegIndex.Vars.fer2.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fer2.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fer2.3)

# fer2 - 30
fls_hps_1.5 <- fls_hps_stck[[28]]
vegIndex.Vars.fer2.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fer2.5) <- veg_hara_30
vegIndex.Vars.fer2 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fer2.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.fer2)

##
# fer3 - 02
fls_hps_1.1 <- fls_hps_stck[[29]]
vegIndex.Vars.fer3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fer3) <- veg_hara_02
vegIndex.Vars.fer3$plotID <- "fer3"
vegIndex.Vars.fer3 <- vegIndex.Vars.fer3[,c(59,1:58)]

# fer3 - 05
fls_hps_1.2 <- fls_hps_stck[[30]]
vegIndex.Vars.fer3.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fer3.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fer3, vegIndex.Vars.fer3.2)

# fer3 - 10
fls_hps_1.3 <- fls_hps_stck[[31]]
vegIndex.Vars.fer3.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fer3.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fer3.3)

# fer3 - 30
fls_hps_1.5 <- fls_hps_stck[[32]]
vegIndex.Vars.fer3.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fer3.5) <- veg_hara_30
vegIndex.Vars.fer3 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fer3.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.fer3)

##
# fer4 - 02
fls_hps_1.1 <- fls_hps_stck[[33]]
vegIndex.Vars.fer4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fer4) <- veg_hara_02
vegIndex.Vars.fer4$plotID <- "fer4"
vegIndex.Vars.fer4 <- vegIndex.Vars.fer4[,c(59,1:58)]

# fer4 - 05
fls_hps_1.2 <- fls_hps_stck[[34]]
vegIndex.Vars.fer4.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fer4.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fer4, vegIndex.Vars.fer4.2)

# fer4 - 10
fls_hps_1.3 <- fls_hps_stck[[35]]
vegIndex.Vars.fer4.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get names of vegIndices
colnames(vegIndex.Vars.fer4.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fer4.3)

# fer4 - 30
fls_hps_1.5 <- fls_hps_stck[[36]]
vegIndex.Vars.fer4.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fer4.5) <- veg_hara_30
vegIndex.Vars.fer4 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fer4.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.fer4)

##
# foc0 - 02
fls_hps_1.1 <- fls_hps_stck[[37]]
vegIndex.Vars.foc0 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.foc0) <- veg_hara_02
vegIndex.Vars.foc0$plotID <- "foc0"
vegIndex.Vars.foc0 <- vegIndex.Vars.foc0[,c(59,1:58)]

# foc0 - 05
fls_hps_1.2 <- fls_hps_stck[[38]]
vegIndex.Vars.foc0.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.foc0.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.foc0, vegIndex.Vars.foc0.2)

# foc0 - 10
fls_hps_1.3 <- fls_hps_stck[[39]]
vegIndex.Vars.foc0.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get names of vegIndices
colnames(vegIndex.Vars.foc0.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.foc0.3)

# foc0 - 30
fls_hps_1.5 <- fls_hps_stck[[40]]
vegIndex.Vars.foc0.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.foc0.5) <- veg_hara_30
vegIndex.Vars.foc0 <- cbind(vegIndex.Vars.all, vegIndex.Vars.foc0.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.foc0)

##
# foc1 - 02
fls_hps_1.1 <- fls_hps_stck[[41]]
vegIndex.Vars.foc1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
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
# get names of vegIndices
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
# get names of vegIndices
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
# get names of vegIndices
colnames(vegIndex.Vars.foc1.5) <- veg_hara_30
vegIndex.Vars.foc1 <- cbind(vegIndex.Vars.all, vegIndex.Vars.foc1.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.foc1)

##
# foc2 - 02
fls_hps_1.1 <- fls_hps_stck[[45]]
vegIndex.Vars.foc2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.foc2) <- veg_hara_02
vegIndex.Vars.foc2$plotID <- "foc2"
vegIndex.Vars.foc2 <- vegIndex.Vars.foc2[,c(59,1:58)]

# foc2 - 05
fls_hps_1.2 <- fls_hps_stck[[46]]
vegIndex.Vars.foc2.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.foc2.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.foc2, vegIndex.Vars.foc2.2)

# foc2 - 10
fls_hps_1.3 <- fls_hps_stck[[47]]
vegIndex.Vars.foc2.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.foc2.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.foc2.3)

# foc2 - 30
fls_hps_1.5 <- fls_hps_stck[[48]]
vegIndex.Vars.foc2.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.foc2.5) <- veg_hara_30
vegIndex.Vars.foc2 <- cbind(vegIndex.Vars.all, vegIndex.Vars.foc2.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.foc2)

##
# foc3 - 02
fls_hps_1.1 <- fls_hps_stck[[49]]
vegIndex.Vars.foc3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.foc3) <- veg_hara_02
vegIndex.Vars.foc3$plotID <- "foc3"
vegIndex.Vars.foc3 <- vegIndex.Vars.foc3[,c(59,1:58)]

# foc3 - 05
fls_hps_1.2 <- fls_hps_stck[[50]]
vegIndex.Vars.foc3.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.foc3.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.foc3, vegIndex.Vars.foc3.2)

# foc3 - 10
fls_hps_1.3 <- fls_hps_stck[[51]]
vegIndex.Vars.foc3.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.foc3.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.foc3.3)

# foc3 - 30
fls_hps_1.5 <- fls_hps_stck[[52]]
vegIndex.Vars.foc3.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.foc3.5) <- veg_hara_30
vegIndex.Vars.foc3 <- cbind(vegIndex.Vars.all, vegIndex.Vars.foc3.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.foc3)

##
# foc4 - 02
fls_hps_1.1 <- fls_hps_stck[[53]]
vegIndex.Vars.foc4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.foc4) <- veg_hara_02
vegIndex.Vars.foc4$plotID <- "foc4"
vegIndex.Vars.foc4 <- vegIndex.Vars.foc4[,c(59,1:58)]

# foc4 - 05
fls_hps_1.2 <- fls_hps_stck[[54]]
vegIndex.Vars.foc4.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.foc4.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.foc4, vegIndex.Vars.foc4.2)

# foc4 - 10
fls_hps_1.3 <- fls_hps_stck[[55]]
vegIndex.Vars.foc4.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.foc4.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.foc4.3)

# foc4 - 30
fls_hps_1.5 <- fls_hps_stck[[56]]
vegIndex.Vars.foc4.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.foc4.5) <- veg_hara_30
vegIndex.Vars.foc4 <- cbind(vegIndex.Vars.all, vegIndex.Vars.foc4.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.foc4)

##
# foc5 - 02
fls_hps_1.1 <- fls_hps_stck[[57]]
vegIndex.Vars.foc5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices 
colnames(vegIndex.Vars.foc5) <- veg_hara_02
vegIndex.Vars.foc5$plotID <- "foc5"
vegIndex.Vars.foc5 <- vegIndex.Vars.foc5[,c(59,1:58)]

# foc5 - 05
fls_hps_1.2 <- fls_hps_stck[[58]]
vegIndex.Vars.foc5.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.foc5.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.foc5, vegIndex.Vars.foc5.2)

# foc5 - 10
fls_hps_1.3 <- fls_hps_stck[[59]]
vegIndex.Vars.foc5.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.foc5.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.foc5.3)

# foc5 - 30
fls_hps_1.5 <- fls_hps_stck[[60]]
vegIndex.Vars.foc5.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.foc5.5) <- veg_hara_30
vegIndex.Vars.foc5 <- cbind(vegIndex.Vars.all, vegIndex.Vars.foc5.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.foc5)

##
# foc6 - 02
fls_hps_1.1 <- fls_hps_stck[[61]]
vegIndex.Vars.foc6 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.foc6) <- veg_hara_02
vegIndex.Vars.foc6$plotID <- "foc6"
vegIndex.Vars.foc6 <- vegIndex.Vars.foc6[,c(59,1:58)]

# foc6 - 05
fls_hps_1.2 <- fls_hps_stck[[62]]
vegIndex.Vars.foc6.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.foc6.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.foc6, vegIndex.Vars.foc6.2)

# foc6 - 10
fls_hps_1.3 <- fls_hps_stck[[63]]
vegIndex.Vars.foc6.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.foc6.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.foc6.3)

# foc6 - 30
fls_hps_1.5 <- fls_hps_stck[[64]]
vegIndex.Vars.foc6.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.foc6.5) <- veg_hara_30
vegIndex.Vars.foc6 <- cbind(vegIndex.Vars.all, vegIndex.Vars.foc6.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.foc6)

##
# fod1 - 02
fls_hps_1.1 <- fls_hps_stck[[65]]
vegIndex.Vars.fod1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fod1) <- veg_hara_02
vegIndex.Vars.fod1$plotID <- "fod1"
vegIndex.Vars.fod1 <- vegIndex.Vars.fod1[,c(59,1:58)]

# fod1 - 05
fls_hps_1.2 <- fls_hps_stck[[66]]
vegIndex.Vars.fod1.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fod1.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fod1, vegIndex.Vars.fod1.2)

# fod1 - 10
fls_hps_1.3 <- fls_hps_stck[[67]]
vegIndex.Vars.fod1.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fod1.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fod1.3)

# fod1 - 30
fls_hps_1.5 <- fls_hps_stck[[68]]
vegIndex.Vars.fod1.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fod1.5) <- veg_hara_30
vegIndex.Vars.fod1 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fod1.5)
vegIndex.Vars <- rbind(vegIndex.Vars,vegIndex.Vars.fod1)

##
# fod2 - 02
fls_hps_1.1 <- fls_hps_stck[[69]]
vegIndex.Vars.fod2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fod2) <- veg_hara_02
vegIndex.Vars.fod2$plotID <- "fod2"
vegIndex.Vars.fod2 <- vegIndex.Vars.fod2[,c(59,1:58)]

# fod2 - 05
fls_hps_1.2 <- fls_hps_stck[[70]]
vegIndex.Vars.fod2.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fod2.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fod2, vegIndex.Vars.fod2.2)

# fod2 - 10
fls_hps_1.3 <- fls_hps_stck[[71]]
vegIndex.Vars.fod2.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fod2.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fod2.3)

# fod2 - 30
fls_hps_1.5 <- fls_hps_stck[[72]]
vegIndex.Vars.fod2.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fod2.5) <- veg_hara_30
vegIndex.Vars.fod2 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fod2.5)
vegIndex.Vars <- rbind(vegIndex.Vars,vegIndex.Vars.fod2)

##
# fod3 - 02
fls_hps_1.1 <- fls_hps_stck[[73]]
vegIndex.Vars.fod3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fod3) <- veg_hara_02
vegIndex.Vars.fod3$plotID <- "fod3"
vegIndex.Vars.fod3 <- vegIndex.Vars.fod3[,c(59,1:58)]

# fod3 - 05
fls_hps_1.2 <- fls_hps_stck[[74]]
vegIndex.Vars.fod3.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fod3.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fod3, vegIndex.Vars.fod3.2)

# fod3 - 10
fls_hps_1.3 <- fls_hps_stck[[75]]
vegIndex.Vars.fod3.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fod3.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fod3.3)

# fod3 - 30
fls_hps_1.5 <- fls_hps_stck[[76]]
vegIndex.Vars.fod3.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fod3.5) <- veg_hara_30
vegIndex.Vars.fod3 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fod3.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.fod3)

##
# fpd0 - 02
fls_hps_1.1 <- fls_hps_stck[[77]]
vegIndex.Vars.fpd0 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fpd0) <- veg_hara_02
vegIndex.Vars.fpd0$plotID <- "fpd0"
vegIndex.Vars.fpd0 <- vegIndex.Vars.fpd0[,c(59,1:58)]

# fpd0 - 05
fls_hps_1.2 <- fls_hps_stck[[78]]
vegIndex.Vars.fpd0.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpd0.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fpd0, vegIndex.Vars.fpd0.2)

# fpd0 - 10
fls_hps_1.3 <- fls_hps_stck[[79]]
vegIndex.Vars.fpd0.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpd0.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fpd0.3)

# fpd0 - 30
fls_hps_1.5 <- fls_hps_stck[[80]]
vegIndex.Vars.fpd0.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpd0.5) <- veg_hara_30
vegIndex.Vars.fpd0 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fpd0.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.fpd0)

##
# fpd2 - 02
fls_hps_1.1 <- fls_hps_stck[[81]]
vegIndex.Vars.fpd2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fpd2) <- veg_hara_02
vegIndex.Vars.fpd2$plotID <- "fpd2"
vegIndex.Vars.fpd2 <- vegIndex.Vars.fpd2[,c(59,1:58)]

# fpd2 - 05
fls_hps_1.2 <- fls_hps_stck[[82]]
vegIndex.Vars.fpd2.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpd2.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fpd2, vegIndex.Vars.fpd2.2)

# fpd2 - 10
fls_hps_1.3 <- fls_hps_stck[[83]]
vegIndex.Vars.fpd2.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpd2.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fpd2.3)

# fpd2 - 30
fls_hps_1.5 <- fls_hps_stck[[84]]
vegIndex.Vars.fpd2.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpd2.5) <- veg_hara_30
vegIndex.Vars.fpd2 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fpd2.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.fpd2)

##
# fpd3 - 02
fls_hps_1.1 <- fls_hps_stck[[85]]
vegIndex.Vars.fpd3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fpd3) <- veg_hara_02
vegIndex.Vars.fpd3$plotID <- "fpd3"
vegIndex.Vars.fpd3 <- vegIndex.Vars.fpd3[,c(59,1:58)]

# fpd3 - 05
fls_hps_1.2 <- fls_hps_stck[[86]]
vegIndex.Vars.fpd3.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpd3.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fpd3, vegIndex.Vars.fpd3.2)

# fpd3 - 10
fls_hps_1.3 <- fls_hps_stck[[87]]
vegIndex.Vars.fpd3.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpd3.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fpd3.3)

# fpd3 - 30
fls_hps_1.5 <- fls_hps_stck[[88]]
vegIndex.Vars.fpd3.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpd3.5) <- veg_hara_30
vegIndex.Vars.fpd3 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fpd3.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.fpd3)

##
# fpd4 - 02
fls_hps_1.1 <- fls_hps_stck[[89]]
vegIndex.Vars.fpd4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fpd4) <- veg_hara_02
vegIndex.Vars.fpd4$plotID <- "fpd4"
vegIndex.Vars.fpd4 <- vegIndex.Vars.fpd4[,c(59,1:58)]

# fpd4 - 05
fls_hps_1.2 <- fls_hps_stck[[90]]
vegIndex.Vars.fpd4.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpd4.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fpd4, vegIndex.Vars.fpd4.2)

# fpd4 - 10
fls_hps_1.3 <- fls_hps_stck[[91]]
vegIndex.Vars.fpd4.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpd4.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fpd4.3)

# fpd4 - 30
fls_hps_1.5 <- fls_hps_stck[[92]]
vegIndex.Vars.fpd4.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpd4.5) <- veg_hara_30
vegIndex.Vars.fpd4 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fpd4.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.fpd4)

##
# fpd5 - 02
fls_hps_1.1 <- fls_hps_stck[[93]]
vegIndex.Vars.fpd5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fpd5) <- veg_hara_02
vegIndex.Vars.fpd5$plotID <- "fpd5"
vegIndex.Vars.fpd5 <- vegIndex.Vars.fpd5[,c(59,1:58)]

# fpd5 - 05
fls_hps_1.2 <- fls_hps_stck[[94]]
vegIndex.Vars.fpd5.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpd5.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fpd5, vegIndex.Vars.fpd5.2)

# fpd5 - 10
fls_hps_1.3 <- fls_hps_stck[[95]]
vegIndex.Vars.fpd5.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpd5.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fpd5.3)

# fpd5 - 30
fls_hps_1.5 <- fls_hps_stck[[96]]
vegIndex.Vars.fpd5.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpd5.5) <- veg_hara_30
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fpd5.5)
vegIndex.Vars <- rbind(vegIndex.Vars,vegIndex.Vars.all)

##
# fpo0 - 02
fls_hps_1.1 <- fls_hps_stck[[97]]
vegIndex.Vars.fpo0 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fpo0) <- veg_hara_02
vegIndex.Vars.fpo0$plotID <- "fpo0"
vegIndex.Vars.fpo0 <- vegIndex.Vars.fpo0[,c(59,1:58)]

# fpo0 - 05
fls_hps_1.2 <- fls_hps_stck[[98]]
vegIndex.Vars.fpo0.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpo0.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fpo0, vegIndex.Vars.fpo0.2)

# fpo0 - 10
fls_hps_1.3 <- fls_hps_stck[[99]]
vegIndex.Vars.fpo0.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpo0.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fpo0.3)

# fpo0 - 30
fls_hps_1.5 <- fls_hps_stck[[100]]
vegIndex.Vars.fpo0.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get names of vegIndices
colnames(vegIndex.Vars.fpo0.5) <- veg_hara_30
vegIndex.Vars.fpo0 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fpo0.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.fpo0)

##
# fpo1 - 02
fls_hps_1.1 <- fls_hps_stck[[101]]
vegIndex.Vars.fpo1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fpo1) <- veg_hara_02
vegIndex.Vars.fpo1$plotID <- "fpo1"
vegIndex.Vars.fpo1 <- vegIndex.Vars.fpo1[,c(59,1:58)]

# fpo1 - 05
fls_hps_1.2 <- fls_hps_stck[[102]]
vegIndex.Vars.fpo1.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpo1.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fpo1, vegIndex.Vars.fpo1.2)

# fpo1 - 10
fls_hps_1.3 <- fls_hps_stck[[103]]
vegIndex.Vars.fpo1.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpo1.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fpo1.3)

# fpo1 - 30
fls_hps_1.5 <- fls_hps_stck[[104]]
vegIndex.Vars.fpo1.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpo1.5) <- veg_hara_30
vegIndex.Vars.fpo1 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fpo1.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.fpo1)

##
# fpo2 - 02
fls_hps_1.1 <- fls_hps_stck[[105]]
vegIndex.Vars.fpo2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fpo2) <- veg_hara_02
vegIndex.Vars.fpo2$plotID <- "fpo2"
vegIndex.Vars.fpo2 <- vegIndex.Vars.fpo2[,c(59,1:58)]

# fpo2 - 05
fls_hps_1.2 <- fls_hps_stck[[106]]
vegIndex.Vars.fpo2.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpo2.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fpo2, vegIndex.Vars.fpo2.2)

# fpo2 - 10
fls_hps_1.3 <- fls_hps_stck[[107]]
vegIndex.Vars.fpo2.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpo2.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fpo2.3)

# fpo2 - 30
fls_hps_1.5 <- fls_hps_stck[[108]]
vegIndex.Vars.fpo2.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpo2.5) <- veg_hara_30
vegIndex.Vars.fpo2 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fpo2.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.fpo2)

##
# fpo3 - 02
fls_hps_1.1 <- fls_hps_stck[[109]]
vegIndex.Vars.fpo3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fpo3) <- veg_hara_02
vegIndex.Vars.fpo3$plotID <- "fpo3"
vegIndex.Vars.fpo3 <- vegIndex.Vars.fpo3[,c(59,1:58)]

# fpo3 - 05
fls_hps_1.2 <- fls_hps_stck[[110]]
vegIndex.Vars.fpo3.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpo3.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fpo3, vegIndex.Vars.fpo3.2)

# fpo3 - 10
fls_hps_1.3 <- fls_hps_stck[[111]]
vegIndex.Vars.fpo3.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpo3.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fpo3.3)

# fpo3 - 30
fls_hps_1.5 <- fls_hps_stck[[112]]
vegIndex.Vars.fpo3.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpo3.5) <- veg_hara_30
vegIndex.Vars.fpo3 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fpo3.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.fpo3)

##
# fpo4 - 02
fls_hps_1.1 <- fls_hps_stck[[113]]
vegIndex.Vars.fpo4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fpo4) <- veg_hara_02
vegIndex.Vars.fpo4$plotID <- "fpo4"
vegIndex.Vars.fpo4 <- vegIndex.Vars.fpo4[,c(59,1:58)]

# fpo4 - 05
fls_hps_1.2 <- fls_hps_stck[[114]]
vegIndex.Vars.fpo4.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpo4.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fpo4, vegIndex.Vars.fpo4.2)

# fpo4 - 10
fls_hps_1.3 <- fls_hps_stck[[115]]
vegIndex.Vars.fpo4.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpo4.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fpo4.3)

# fpo4 - 30
fls_hps_1.5 <- fls_hps_stck[[116]]
vegIndex.Vars.fpo4.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpo4.5)<- veg_hara_30
vegIndex.Vars.fpo4 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fpo4.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.fpo4)

##
# fpo5 - 02
fls_hps_1.1 <- fls_hps_stck[[117]]
vegIndex.Vars.fpo5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.fpo5) <- veg_hara_02
vegIndex.Vars.fpo5$plotID <- "fpo5"
vegIndex.Vars.fpo5 <- vegIndex.Vars.fpo5[,c(59,1:58)]

# fpo5 - 05
fls_hps_1.2 <- fls_hps_stck[[118]]
vegIndex.Vars.fpo5.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpo5.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.fpo5, vegIndex.Vars.fpo5.2)

# fpo5 - 10
fls_hps_1.3 <- fls_hps_stck[[119]]
vegIndex.Vars.fpo5.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpo5.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.fpo5.3)

# fpo5 - 30
fls_hps_1.5 <- fls_hps_stck[[120]]
vegIndex.Vars.fpo5.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.fpo5.5) <- veg_hara_30
vegIndex.Vars.fpo5 <- cbind(vegIndex.Vars.all, vegIndex.Vars.fpo5.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.fpo5)

##
# hel1 - 02
fls_hps_1.1 <- fls_hps_stck[[121]]
vegIndex.Vars.hel1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.hel1) <- veg_hara_02
vegIndex.Vars.hel1$plotID <- "hel1"
vegIndex.Vars.hel1 <- vegIndex.Vars.hel1[,c(59,1:58)]

# hel1 - 05
fls_hps_1.2 <- fls_hps_stck[[122]]
vegIndex.Vars.hel1.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hel1.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.hel1, vegIndex.Vars.hel1.2)

# hel1 - 10
fls_hps_1.3 <- fls_hps_stck[[123]]
vegIndex.Vars.hel1.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hel1.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.hel1.3)

# hel1 - 30
fls_hps_1.5 <- fls_hps_stck[[124]]
vegIndex.Vars.hel1.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hel1.5) <- veg_hara_30
vegIndex.Vars.hel1 <- cbind(vegIndex.Vars.all, vegIndex.Vars.hel1.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.hel1)

##
# hel2 - 02
fls_hps_1.1 <- fls_hps_stck[[125]]
vegIndex.Vars.hel2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.hel2) <- veg_hara_02
vegIndex.Vars.hel2$plotID <- "hel2"
vegIndex.Vars.hel2 <- vegIndex.Vars.hel2[,c(59,1:58)]

# hel2 - 05
fls_hps_1.2 <- fls_hps_stck[[126]]
vegIndex.Vars.hel2.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hel2.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.hel2, vegIndex.Vars.hel2.2)

# hel2 - 10
fls_hps_1.3 <- fls_hps_stck[[127]]
vegIndex.Vars.hel2.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get names of vegIndices
colnames(vegIndex.Vars.hel2.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.hel2.3)

# hel2 - 30
fls_hps_1.5 <- fls_hps_stck[[128]]
vegIndex.Vars.hel2.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hel2.5) <- veg_hara_30
vegIndex.Vars.hel2 <- cbind(vegIndex.Vars.all, vegIndex.Vars.hel2.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.hel2)

##
# hel3 - 02
fls_hps_1.1 <- fls_hps_stck[[129]]
vegIndex.Vars.hel3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.hel3) <- veg_hara_02
vegIndex.Vars.hel3$plotID <- "hel3"
vegIndex.Vars.hel3 <- vegIndex.Vars.hel3[,c(59,1:58)]

# hel3 - 05
fls_hps_1.2 <- fls_hps_stck[[130]]
vegIndex.Vars.hel3.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hel3.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.hel3, vegIndex.Vars.hel3.2)

# hel3 - 10
fls_hps_1.3 <- fls_hps_stck[[131]]
vegIndex.Vars.hel3.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hel3.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.hel3.3)

# hel3 - 30
fls_hps_1.5 <- fls_hps_stck[[132]]
vegIndex.Vars.hel3.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hel3.5) <- veg_hara_30
vegIndex.Vars.hel3 <- cbind(vegIndex.Vars.all, vegIndex.Vars.hel3.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.hel3)

##
# hel4 - 02
fls_hps_1.1 <- fls_hps_stck[[133]]
vegIndex.Vars.hel4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "cbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }
# get names of vegIndices
colnames(vegIndex.Vars.hel4) <- veg_hara_02
vegIndex.Vars.hel4$plotID <- "hel4"
vegIndex.Vars.hel4 <- vegIndex.Vars.hel4[,c(59,1:58)]

# hel4 - 05
fls_hps_1.2 <- fls_hps_stck[[134]]
vegIndex.Vars.hel4.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hel4.2) <- veg_hara_05
vegIndex.Vars.all <- cbind(vegIndex.Vars.hel4, vegIndex.Vars.hel4.2)

# hel4 - 10
fls_hps_1.3 <- fls_hps_stck[[135]]
vegIndex.Vars.hel4.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hel4.3) <- veg_hara_10
vegIndex.Vars.all <- cbind(vegIndex.Vars.all, vegIndex.Vars.hel4.3)

# hel4 - 30
fls_hps_1.5 <- fls_hps_stck[[136]]
vegIndex.Vars.hel4.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "cbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }
# get names of vegIndices
colnames(vegIndex.Vars.hel4.5) <- veg_hara_30
vegIndex.Vars.hel4 <- cbind(vegIndex.Vars.all, vegIndex.Vars.hel4.5)
vegIndex.Vars <- rbind(vegIndex.Vars, vegIndex.Vars.hel4)

write.csv2(vegIndex.Vars, 
           file = "pci_08_haralick_stats_2nd_wide.csv", 
           row.names = TRUE)

stopCluster(cl)


