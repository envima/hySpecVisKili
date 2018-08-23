library(hsdar)
library(foreach)
library(doParallel)

setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd/norm_441/re_norm/pca/pca_inverse/pci_vegIndex/pci_vegIndex_hara/pci_vegIndex_hara_dia_50m/")
filepath_raster <- "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd/norm_441/re_norm/pca/pca_inverse/pci_vegIndex/pci_vegIndex_hara/pci_vegIndex_hara_dia_50m/"

# vegIndices raster files
fls_hps <- list.files(paste0(filepath_raster), full.names = FALSE,
                      recursive = FALSE, pattern = "*dia_50m.tif")

fls_hps_stck <- lapply(seq(fls_hps), function(i){
  stack(fls_hps[[i]])
})

haralick <- c("smpl_energy", "smpl_entropy", "smpl_correlation", "smpl_inverse_difference_moment", 
              "smpl_inertia", "smpl_cluster_shade", "smpl_cluster_prominence", "smpl_haralick_correlation",
              "adv_mean", "adv_variance", "adv_dissimilarity", "adv_sum_average", "adv_sum_variance", 
              "adv_sum_entropy", "adv_diff_entropies", "adv_diff_variances", "adv_IC1", "adv_IC2",
              "hgr_short_run_emphasis", "hgr_long_run_emphasis", "hgr_grey_level_nonuniformity",
              "hgr_run_length_nonuniformity", "hgr_run_percentage", "hgr_low_grey_level_run_emphasis", 
              "hgr_high_grey_level_run_emphasis", "hgr_short_run_low_grey_level_emphasis", 
              "hgr_short_run_high_grey_level_emphasis", "hgr_long_run_low_grey_level_emphasis", 
              "hgr_long_run_high_grey_level_emphasis")
              

# one plot aim: all plots
cl <- makeCluster(detectCores() - 10)
registerDoParallel(cl)

# fed1 - 10
fls_hps_1.1 <- fls_hps_stck[[1]]
vegIndex.Vars.fed1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fed1$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fed1$haralick <- haralick
vegIndex.Vars.fed1$xyrad <- "10"
vegIndex.Vars.fed1$plotID <- "fed1"

# fed1 - 2
fls_hps_1.2 <- fls_hps_stck[[2]]
vegIndex.Vars.fed1.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegIndex.Vars.fed1.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fed1.2$haralick <- haralick
vegIndex.Vars.fed1.2$xyrad <- "02"
vegIndex.Vars.fed1.2$plotID <- "fed1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.fed1, vegIndex.Vars.fed1.2)

# fed1 - 30
fls_hps_1.3 <- fls_hps_stck[[3]]
vegIndex.Vars.fed1.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fed1.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fed1.3$haralick <- haralick
vegIndex.Vars.fed1.3$xyrad <- "30"
vegIndex.Vars.fed1.3$plotID <- "fed1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fed1.3)

# fed1 - 5
fls_hps_1.5 <- fls_hps_stck[[4]]
vegIndex.Vars.fed1.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fed1.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fed1.5$haralick <- haralick
vegIndex.Vars.fed1.5$xyrad <- "05"
vegIndex.Vars.fed1.5$plotID <- "fed1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fed1.5)

##
# fed2 - 10
fls_hps_1.1 <- fls_hps_stck[[5]]
vegIndex.Vars.fed2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fed2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fed2$haralick <- haralick
vegIndex.Vars.fed2$xyrad <- "10"
vegIndex.Vars.fed2$plotID <- "fed2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fed2)

# fed2 - 2
fls_hps_1.2 <- fls_hps_stck[[6]]
vegIndex.Vars.fed2.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fed2.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fed2.2$haralick <- haralick
vegIndex.Vars.fed2.2$xyrad <- "02"
vegIndex.Vars.fed2.2$plotID <- "fed2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fed2.2)

# fed2 - 30
fls_hps_1.3 <- fls_hps_stck[[7]]
vegIndex.Vars.fed2.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fed2.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fed2.3$haralick <- haralick
vegIndex.Vars.fed2.3$xyrad <- "30"
vegIndex.Vars.fed2.3$plotID <- "fed2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fed2.3)

# fed2 - 5
fls_hps_1.5 <- fls_hps_stck[[8]]
vegIndex.Vars.fed2.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fed2.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fed2.5$haralick <- haralick
vegIndex.Vars.fed2.5$xyrad <- "05"
vegIndex.Vars.fed2.5$plotID <- "fed2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fed2.5)


##
# fed3 - 10
fls_hps_1.1 <- fls_hps_stck[[9]]
vegIndex.Vars.fed3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fed3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fed3$haralick <- haralick
vegIndex.Vars.fed3$xyrad <- "10"
vegIndex.Vars.fed3$plotID <- "fed3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fed3)

# fed3 - 2
fls_hps_1.2 <- fls_hps_stck[[10]]
vegIndex.Vars.fed3.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fed3.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fed3.2$haralick <- haralick
vegIndex.Vars.fed3.2$xyrad <- "02"
vegIndex.Vars.fed3.2$plotID <- "fed3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fed3.2)

# fed3 - 30
fls_hps_1.3 <- fls_hps_stck[[11]]
vegIndex.Vars.fed3.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fed3.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fed3.3$haralick <- haralick
vegIndex.Vars.fed3.3$xyrad <- "30"
vegIndex.Vars.fed3.3$plotID <- "fed3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fed3.3)

# fed3 - 5
fls_hps_1.5 <- fls_hps_stck[[12]]
vegIndex.Vars.fed3.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fed3.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fed3.5$haralick <- haralick
vegIndex.Vars.fed3.5$xyrad <- "05"
vegIndex.Vars.fed3.5$plotID <- "fed3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fed3.5)


##
# fed4 - 10
fls_hps_1.1 <- fls_hps_stck[[13]]
vegIndex.Vars.fed4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fed4$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fed4$haralick <- haralick
vegIndex.Vars.fed4$xyrad <- "10"
vegIndex.Vars.fed4$plotID <- "fed4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fed4)

# fed4 - 2
fls_hps_1.2 <- fls_hps_stck[[14]]
vegIndex.Vars.fed4.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fed4.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fed4.2$haralick <- haralick
vegIndex.Vars.fed4.2$xyrad <- "02"
vegIndex.Vars.fed4.2$plotID <- "fed4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fed4.2)

# fed4 - 30
fls_hps_1.3 <- fls_hps_stck[[15]]
vegIndex.Vars.fed4.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fed4.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fed4.3$haralick <- haralick
vegIndex.Vars.fed4.3$xyrad <- "30"
vegIndex.Vars.fed4.3$plotID <- "fed4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fed4.3)

# fed4 - 5
fls_hps_1.5 <- fls_hps_stck[[16]]
vegIndex.Vars.fed4.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fed4.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fed4.5$haralick <- haralick
vegIndex.Vars.fed4.5$xyrad <- "05"
vegIndex.Vars.fed4.5$plotID <- "fed4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fed4.5)


##
# fed5 - 10
fls_hps_1.1 <- fls_hps_stck[[17]]
vegIndex.Vars.fed5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fed5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fed5$haralick <- haralick
vegIndex.Vars.fed5$xyrad <- "10"
vegIndex.Vars.fed5$plotID <- "fed5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fed5)

# fed5 - 2
fls_hps_1.2 <- fls_hps_stck[[18]]
vegIndex.Vars.fed5.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fed5.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fed5.2$haralick <- haralick
vegIndex.Vars.fed5.2$xyrad <- "02"
vegIndex.Vars.fed5.2$plotID <- "fed5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fed5.2)

# fed5 - 30
fls_hps_1.3 <- fls_hps_stck[[19]]
vegIndex.Vars.fed5.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fed5.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fed5.3$haralick <- haralick
vegIndex.Vars.fed5.3$xyrad <- "30"
vegIndex.Vars.fed5.3$plotID <- "fed5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fed5.3)

# fed5 - 5
fls_hps_1.5 <- fls_hps_stck[[20]]
vegIndex.Vars.fed5.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fed5.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fed5.5$haralick <- haralick
vegIndex.Vars.fed5.5$xyrad <- "05"
vegIndex.Vars.fed5.5$plotID <- "fed5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fed5.5)


##
# fer0 - 10
fls_hps_1.1 <- fls_hps_stck[[21]]
vegIndex.Vars.fer0 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fer0$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fer0$haralick <- haralick
vegIndex.Vars.fer0$xyrad <- "10"
vegIndex.Vars.fer0$plotID <- "fer0"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fer0)

# fer0 - 2
fls_hps_1.2 <- fls_hps_stck[[22]]
vegIndex.Vars.fer0.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fer0.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fer0.2$haralick <- haralick
vegIndex.Vars.fer0.2$xyrad <- "02"
vegIndex.Vars.fer0.2$plotID <- "fer0"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fer0.2)

# fer0 - 30
fls_hps_1.3 <- fls_hps_stck[[23]]
vegIndex.Vars.fer0.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fer0.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fer0.3$haralick <- haralick
vegIndex.Vars.fer0.3$xyrad <- "30"
vegIndex.Vars.fer0.3$plotID <- "fer0"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fer0.3)

# fer0 - 5
fls_hps_1.5 <- fls_hps_stck[[24]]
vegIndex.Vars.fer0.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fer0.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fer0.5$haralick <- haralick
vegIndex.Vars.fer0.5$xyrad <- "05"
vegIndex.Vars.fer0.5$plotID <- "fer0"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fer0.5)


##
# fer2 - 10
fls_hps_1.1 <- fls_hps_stck[[25]]
vegIndex.Vars.fer2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fer2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fer2$haralick <- haralick
vegIndex.Vars.fer2$xyrad <- "10"
vegIndex.Vars.fer2$plotID <- "fer2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fer2)

# fer2 - 2
fls_hps_1.2 <- fls_hps_stck[[26]]
vegIndex.Vars.fer2.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fer2.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fer2.2$haralick <- haralick
vegIndex.Vars.fer2.2$xyrad <- "02"
vegIndex.Vars.fer2.2$plotID <- "fer2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fer2.2)

# fer2 - 30
fls_hps_1.3 <- fls_hps_stck[[27]]
vegIndex.Vars.fer2.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fer2.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fer2.3$haralick <- haralick
vegIndex.Vars.fer2.3$xyrad <- "30"
vegIndex.Vars.fer2.3$plotID <- "fer2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fer2.3)

# fer2 - 5
fls_hps_1.5 <- fls_hps_stck[[28]]
vegIndex.Vars.fer2.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fer2.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fer2.5$haralick <- haralick
vegIndex.Vars.fer2.5$xyrad <- "05"
vegIndex.Vars.fer2.5$plotID <- "fer2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fer2.5)


##
# fer3 - 10
fls_hps_1.1 <- fls_hps_stck[[29]]
vegIndex.Vars.fer3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fer3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fer3$haralick <- haralick
vegIndex.Vars.fer3$xyrad <- "10"
vegIndex.Vars.fer3$plotID <- "fer3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fer3)

# fer3 - 2
fls_hps_1.2 <- fls_hps_stck[[30]]
vegIndex.Vars.fer3.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fer3.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fer3.2$haralick <- haralick
vegIndex.Vars.fer3.2$xyrad <- "02"
vegIndex.Vars.fer3.2$plotID <- "fer3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fer3.2)

# fer3 - 30
fls_hps_1.3 <- fls_hps_stck[[31]]
vegIndex.Vars.fer3.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fer3.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fer3.3$haralick <- haralick
vegIndex.Vars.fer3.3$xyrad <- "30"
vegIndex.Vars.fer3.3$plotID <- "fer3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fer3.3)

# fer3 - 5
fls_hps_1.5 <- fls_hps_stck[[32]]
vegIndex.Vars.fer3.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fer3.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fer3.5$haralick <- haralick
vegIndex.Vars.fer3.5$xyrad <- "05"
vegIndex.Vars.fer3.5$plotID <- "fer3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fer3.5)


##
# fer4 - 10
fls_hps_1.1 <- fls_hps_stck[[33]]
vegIndex.Vars.fer4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fer4$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fer4$haralick <- haralick
vegIndex.Vars.fer4$xyrad <- "10"
vegIndex.Vars.fer4$plotID <- "fer4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fer4)

# fer4 - 2
fls_hps_1.2 <- fls_hps_stck[[34]]
vegIndex.Vars.fer4.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fer4.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fer4.2$haralick <- haralick
vegIndex.Vars.fer4.2$xyrad <- "02"
vegIndex.Vars.fer4.2$plotID <- "fer4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fer4.2)

# fer4 - 30
fls_hps_1.3 <- fls_hps_stck[[35]]
vegIndex.Vars.fer4.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fer4.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fer4.3$haralick <- haralick
vegIndex.Vars.fer4.3$xyrad <- "30"
vegIndex.Vars.fer4.3$plotID <- "fer4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fer4.3)

# fer4 - 5
fls_hps_1.5 <- fls_hps_stck[[36]]
vegIndex.Vars.fer4.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fer4.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fer4.5$haralick <- haralick
vegIndex.Vars.fer4.5$xyrad <- "05"
vegIndex.Vars.fer4.5$plotID <- "fer4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fer4.5)


##
# foc0 - 10
fls_hps_1.1 <- fls_hps_stck[[37]]
vegIndex.Vars.foc0 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.foc0$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc0$haralick <- haralick
vegIndex.Vars.foc0$xyrad <- "10"
vegIndex.Vars.foc0$plotID <- "foc0"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc0)

# foc0 - 2
fls_hps_1.2 <- fls_hps_stck[[38]]
vegIndex.Vars.foc0.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.foc0.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc0.2$haralick <- haralick
vegIndex.Vars.foc0.2$xyrad <- "02"
vegIndex.Vars.foc0.2$plotID <- "foc0"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc0.2)

# foc0 - 30
fls_hps_1.3 <- fls_hps_stck[[39]]
vegIndex.Vars.foc0.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.foc0.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc0.3$haralick <- haralick
vegIndex.Vars.foc0.3$xyrad <- "30"
vegIndex.Vars.foc0.3$plotID <- "foc0"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc0.3)

# foc0 - 5
fls_hps_1.5 <- fls_hps_stck[[40]]
vegIndex.Vars.foc0.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.foc0.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc0.5$haralick <- haralick
vegIndex.Vars.foc0.5$xyrad <- "05"
vegIndex.Vars.foc0.5$plotID <- "foc0"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc0.5)


##
# foc1 - 10
fls_hps_1.1 <- fls_hps_stck[[41]]
vegIndex.Vars.foc1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.foc1$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc1$haralick <- haralick
vegIndex.Vars.foc1$xyrad <- "10"
vegIndex.Vars.foc1$plotID <- "foc1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc1)

# foc1 - 2
fls_hps_1.2 <- fls_hps_stck[[42]]
vegIndex.Vars.foc1.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.foc1.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc1.2$haralick <- haralick
vegIndex.Vars.foc1.2$xyrad <- "02"
vegIndex.Vars.foc1.2$plotID <- "foc1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc1.2)

# foc1 - 30
fls_hps_1.3 <- fls_hps_stck[[43]]
vegIndex.Vars.foc1.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.foc1.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc1.3$haralick <- haralick
vegIndex.Vars.foc1.3$xyrad <- "30"
vegIndex.Vars.foc1.3$plotID <- "foc1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc1.3)

# foc1 - 5
fls_hps_1.5 <- fls_hps_stck[[44]]
vegIndex.Vars.foc1.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.foc1.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc1.5$haralick <- haralick
vegIndex.Vars.foc1.5$xyrad <- "05"
vegIndex.Vars.foc1.5$plotID <- "foc1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc1.5)


##
# foc2 - 10
fls_hps_1.1 <- fls_hps_stck[[45]]
vegIndex.Vars.foc2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.foc2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc2$haralick <- haralick
vegIndex.Vars.foc2$xyrad <- "10"
vegIndex.Vars.foc2$plotID <- "foc2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc2)

# foc2 - 2
fls_hps_1.2 <- fls_hps_stck[[46]]
vegIndex.Vars.foc2.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.foc2.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc2.2$haralick <- haralick
vegIndex.Vars.foc2.2$xyrad <- "02"
vegIndex.Vars.foc2.2$plotID <- "foc2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc2.2)

# foc2 - 30
fls_hps_1.3 <- fls_hps_stck[[47]]
vegIndex.Vars.foc2.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.foc2.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc2.3$haralick <- haralick
vegIndex.Vars.foc2.3$xyrad <- "30"
vegIndex.Vars.foc2.3$plotID <- "foc2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc2.3)

# foc2 - 5
fls_hps_1.5 <- fls_hps_stck[[48]]
vegIndex.Vars.foc2.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.foc2.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc2.5$haralick <- haralick
vegIndex.Vars.foc2.5$xyrad <- "05"
vegIndex.Vars.foc2.5$plotID <- "foc2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc2.5)


##
# foc3 - 10
fls_hps_1.1 <- fls_hps_stck[[49]]
vegIndex.Vars.foc3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.foc3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc3$haralick <- haralick
vegIndex.Vars.foc3$xyrad <- "10"
vegIndex.Vars.foc3$plotID <- "foc3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc3)

# foc3 - 2
fls_hps_1.2 <- fls_hps_stck[[50]]
vegIndex.Vars.foc3.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.foc3.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc3.2$haralick <- haralick
vegIndex.Vars.foc3.2$xyrad <- "02"
vegIndex.Vars.foc3.2$plotID <- "foc3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc3.2)

# foc3 - 30
fls_hps_1.3 <- fls_hps_stck[[51]]
vegIndex.Vars.foc3.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.foc3.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc3.3$haralick <- haralick
vegIndex.Vars.foc3.3$xyrad <- "30"
vegIndex.Vars.foc3.3$plotID <- "foc3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc3.3)

# foc3 - 5
fls_hps_1.5 <- fls_hps_stck[[52]]
vegIndex.Vars.foc3.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.foc3.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc3.5$haralick <- haralick
vegIndex.Vars.foc3.5$xyrad <- "05"
vegIndex.Vars.foc3.5$plotID <- "foc3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc3.5)


##
# foc4 - 10
fls_hps_1.1 <- fls_hps_stck[[53]]
vegIndex.Vars.foc4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.foc4$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc4$haralick <- haralick
vegIndex.Vars.foc4$xyrad <- "10"
vegIndex.Vars.foc4$plotID <- "foc4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc4)

# foc4 - 2
fls_hps_1.2 <- fls_hps_stck[[54]]
vegIndex.Vars.foc4.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.foc4.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc4.2$haralick <- haralick
vegIndex.Vars.foc4.2$xyrad <- "02"
vegIndex.Vars.foc4.2$plotID <- "foc4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc4.2)

# foc4 - 30
fls_hps_1.3 <- fls_hps_stck[[55]]
vegIndex.Vars.foc4.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.foc4.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc4.3$haralick <- haralick
vegIndex.Vars.foc4.3$xyrad <- "30"
vegIndex.Vars.foc4.3$plotID <- "foc4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc4.3)

# foc4 - 5
fls_hps_1.5 <- fls_hps_stck[[56]]
vegIndex.Vars.foc4.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.foc4.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc4.5$haralick <- haralick
vegIndex.Vars.foc4.5$xyrad <- "05"
vegIndex.Vars.foc4.5$plotID <- "foc4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc4.5)


##
# foc5 - 10
fls_hps_1.1 <- fls_hps_stck[[57]]
vegIndex.Vars.foc5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.foc5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc5$haralick <- haralick
vegIndex.Vars.foc5$xyrad <- "10"
vegIndex.Vars.foc5$plotID <- "foc5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc5)

# foc5 - 2
fls_hps_1.2 <- fls_hps_stck[[58]]
vegIndex.Vars.foc5.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.foc5.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc5.2$haralick <- haralick
vegIndex.Vars.foc5.2$xyrad <- "02"
vegIndex.Vars.foc5.2$plotID <- "foc5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc5.2)

# foc5 - 30
fls_hps_1.3 <- fls_hps_stck[[59]]
vegIndex.Vars.foc5.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.foc5.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc5.3$haralick <- haralick
vegIndex.Vars.foc5.3$xyrad <- "30"
vegIndex.Vars.foc5.3$plotID <- "foc5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc5.3)

# foc5 - 5
fls_hps_1.5 <- fls_hps_stck[[60]]
vegIndex.Vars.foc5.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.foc5.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc5.5$haralick <- haralick
vegIndex.Vars.foc5.5$xyrad <- "05"
vegIndex.Vars.foc5.5$plotID <- "foc5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc5.5)


##
# foc6 - 10
fls_hps_1.1 <- fls_hps_stck[[61]]
vegIndex.Vars.foc6 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.foc6$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc6$haralick <- haralick
vegIndex.Vars.foc6$xyrad <- "10"
vegIndex.Vars.foc6$plotID <- "foc6"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc6)

# foc6 - 2
fls_hps_1.2 <- fls_hps_stck[[62]]
vegIndex.Vars.foc6.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.foc6.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc6.2$haralick <- haralick
vegIndex.Vars.foc6.2$xyrad <- "02"
vegIndex.Vars.foc6.2$plotID <- "foc6"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc6.2)

# foc6 - 30
fls_hps_1.3 <- fls_hps_stck[[63]]
vegIndex.Vars.foc6.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.foc6.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc6.3$haralick <- haralick
vegIndex.Vars.foc6.3$xyrad <- "30"
vegIndex.Vars.foc6.3$plotID <- "foc6"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc6.3)

# foc6 - 5
fls_hps_1.5 <- fls_hps_stck[[64]]
vegIndex.Vars.foc6.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.foc6.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.foc6.5$haralick <- haralick
vegIndex.Vars.foc6.5$xyrad <- "05"
vegIndex.Vars.foc6.5$plotID <- "foc6"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc6.5)


##
# fod1 - 10
fls_hps_1.1 <- fls_hps_stck[[65]]
vegIndex.Vars.fod1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fod1$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fod1$haralick <- haralick
vegIndex.Vars.fod1$xyrad <- "10"
vegIndex.Vars.fod1$plotID <- "fod1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fod1)

# fod1 - 2
fls_hps_1.2 <- fls_hps_stck[[66]]
vegIndex.Vars.fod1.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fod1.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fod1.2$haralick <- haralick
vegIndex.Vars.fod1.2$xyrad <- "02"
vegIndex.Vars.fod1.2$plotID <- "fod1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fod1.2)

# fod1 - 30
fls_hps_1.3 <- fls_hps_stck[[67]]
vegIndex.Vars.fod1.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fod1.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fod1.3$haralick <- haralick
vegIndex.Vars.fod1.3$xyrad <- "30"
vegIndex.Vars.fod1.3$plotID <- "fod1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fod1.3)

# fod1 - 5
fls_hps_1.5 <- fls_hps_stck[[68]]
vegIndex.Vars.fod1.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fod1.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fod1.5$haralick <- haralick
vegIndex.Vars.fod1.5$xyrad <- "05"
vegIndex.Vars.fod1.5$plotID <- "fod1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fod1.5)


##
# fod2 - 10
fls_hps_1.1 <- fls_hps_stck[[69]]
vegIndex.Vars.fod2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fod2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fod2$haralick <- haralick
vegIndex.Vars.fod2$xyrad <- "10"
vegIndex.Vars.fod2$plotID <- "fod2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fod2)

# fod2 - 2
fls_hps_1.2 <- fls_hps_stck[[70]]
vegIndex.Vars.fod2.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fod2.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fod2.2$haralick <- haralick
vegIndex.Vars.fod2.2$xyrad <- "02"
vegIndex.Vars.fod2.2$plotID <- "fod2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fod2.2)

# fod2 - 30
fls_hps_1.3 <- fls_hps_stck[[71]]
vegIndex.Vars.fod2.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fod2.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fod2.3$haralick <- haralick
vegIndex.Vars.fod2.3$xyrad <- "30"
vegIndex.Vars.fod2.3$plotID <- "fod2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fod2.3)

# fod2 - 5
fls_hps_1.5 <- fls_hps_stck[[72]]
vegIndex.Vars.fod2.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fod2.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fod2.5$haralick <- haralick
vegIndex.Vars.fod2.5$xyrad <- "05"
vegIndex.Vars.fod2.5$plotID <- "fod2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fod2.5)


##
# fod3 - 10
fls_hps_1.1 <- fls_hps_stck[[73]]
vegIndex.Vars.fod3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fod3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fod3$haralick <- haralick
vegIndex.Vars.fod3$xyrad <- "10"
vegIndex.Vars.fod3$plotID <- "fod3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fod3)

# fod3 - 2
fls_hps_1.2 <- fls_hps_stck[[74]]
vegIndex.Vars.fod3.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fod3.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fod3.2$haralick <- haralick
vegIndex.Vars.fod3.2$xyrad <- "02"
vegIndex.Vars.fod3.2$plotID <- "fod3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fod3.2)

# fod3 - 30
fls_hps_1.3 <- fls_hps_stck[[75]]
vegIndex.Vars.fod3.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fod3.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fod3.3$haralick <- haralick
vegIndex.Vars.fod3.3$xyrad <- "30"
vegIndex.Vars.fod3.3$plotID <- "fod3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fod3.3)

# fod3 - 5
fls_hps_1.5 <- fls_hps_stck[[76]]
vegIndex.Vars.fod3.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fod3.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fod3.5$haralick <- haralick
vegIndex.Vars.fod3.5$xyrad <- "05"
vegIndex.Vars.fod3.5$plotID <- "fod3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fod3.5)


##
# fpd0 - 10
fls_hps_1.1 <- fls_hps_stck[[77]]
vegIndex.Vars.fpd0 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fpd0$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpd0$haralick <- haralick
vegIndex.Vars.fpd0$xyrad <- "10"
vegIndex.Vars.fpd0$plotID <- "fpd0"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpd0)

# fpd0 - 2
fls_hps_1.2 <- fls_hps_stck[[78]]
vegIndex.Vars.fpd0.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpd0.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpd0.2$haralick <- haralick
vegIndex.Vars.fpd0.2$xyrad <- "02"
vegIndex.Vars.fpd0.2$plotID <- "fpd0"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpd0.2)

# fpd0 - 30
fls_hps_1.3 <- fls_hps_stck[[79]]
vegIndex.Vars.fpd0.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpd0.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpd0.3$haralick <- haralick
vegIndex.Vars.fpd0.3$xyrad <- "30"
vegIndex.Vars.fpd0.3$plotID <- "fpd0"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpd0.3)

# fpd0 - 5
fls_hps_1.5 <- fls_hps_stck[[80]]
vegIndex.Vars.fpd0.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpd0.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpd0.5$haralick <- haralick
vegIndex.Vars.fpd0.5$xyrad <- "05"
vegIndex.Vars.fpd0.5$plotID <- "fpd0"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpd0.5)


##
# fpd2 - 10
fls_hps_1.1 <- fls_hps_stck[[81]]
vegIndex.Vars.fpd2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fpd2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpd2$haralick <- haralick
vegIndex.Vars.fpd2$xyrad <- "10"
vegIndex.Vars.fpd2$plotID <- "fpd2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpd2)

# fpd2 - 2
fls_hps_1.2 <- fls_hps_stck[[82]]
vegIndex.Vars.fpd2.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpd2.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpd2.2$haralick <- haralick
vegIndex.Vars.fpd2.2$xyrad <- "02"
vegIndex.Vars.fpd2.2$plotID <- "fpd2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpd2.2)

# fpd2 - 30
fls_hps_1.3 <- fls_hps_stck[[83]]
vegIndex.Vars.fpd2.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpd2.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpd2.3$haralick <- haralick
vegIndex.Vars.fpd2.3$xyrad <- "30"
vegIndex.Vars.fpd2.3$plotID <- "fpd2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpd2.3)

# fpd2 - 5
fls_hps_1.5 <- fls_hps_stck[[84]]
vegIndex.Vars.fpd2.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpd2.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpd2.5$haralick <- haralick
vegIndex.Vars.fpd2.5$xyrad <- "05"
vegIndex.Vars.fpd2.5$plotID <- "fpd2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpd2.5)


##
# fpd3 - 10
fls_hps_1.1 <- fls_hps_stck[[85]]
vegIndex.Vars.fpd3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fpd3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpd3$haralick <- haralick
vegIndex.Vars.fpd3$xyrad <- "10"
vegIndex.Vars.fpd3$plotID <- "fpd3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpd3)

# fpd3 - 2
fls_hps_1.2 <- fls_hps_stck[[86]]
vegIndex.Vars.fpd3.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpd3.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpd3.2$haralick <- haralick
vegIndex.Vars.fpd3.2$xyrad <- "02"
vegIndex.Vars.fpd3.2$plotID <- "fpd3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpd3.2)

# fpd3 - 30
fls_hps_1.3 <- fls_hps_stck[[87]]
vegIndex.Vars.fpd3.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpd3.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpd3.3$haralick <- haralick
vegIndex.Vars.fpd3.3$xyrad <- "30"
vegIndex.Vars.fpd3.3$plotID <- "fpd3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpd3.3)

# fpd3 - 5
fls_hps_1.5 <- fls_hps_stck[[88]]
vegIndex.Vars.fpd3.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpd3.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpd3.5$haralick <- haralick
vegIndex.Vars.fpd3.5$xyrad <- "05"
vegIndex.Vars.fpd3.5$plotID <- "fpd3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpd3.5)


##
# fpd4 - 10
fls_hps_1.1 <- fls_hps_stck[[89]]
vegIndex.Vars.fpd4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fpd4$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpd4$haralick <- haralick
vegIndex.Vars.fpd4$xyrad <- "10"
vegIndex.Vars.fpd4$plotID <- "fpd4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpd4)

# fpd4 - 2
fls_hps_1.2 <- fls_hps_stck[[90]]
vegIndex.Vars.fpd4.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpd4.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpd4.2$haralick <- haralick
vegIndex.Vars.fpd4.2$xyrad <- "02"
vegIndex.Vars.fpd4.2$plotID <- "fpd4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpd4.2)

# fpd4 - 30
fls_hps_1.3 <- fls_hps_stck[[91]]
vegIndex.Vars.fpd4.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpd4.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpd4.3$haralick <- haralick
vegIndex.Vars.fpd4.3$xyrad <- "30"
vegIndex.Vars.fpd4.3$plotID <- "fpd4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpd4.3)

# fpd4 - 5
fls_hps_1.5 <- fls_hps_stck[[92]]
vegIndex.Vars.fpd4.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpd4.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpd4.5$haralick <- haralick
vegIndex.Vars.fpd4.5$xyrad <- "05"
vegIndex.Vars.fpd4.5$plotID <- "fpd4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpd4.5)


##
# fdp5 - 10
fls_hps_1.1 <- fls_hps_stck[[93]]
vegIndex.Vars.fdp5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fdp5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fdp5$haralick <- haralick
vegIndex.Vars.fdp5$xyrad <- "10"
vegIndex.Vars.fdp5$plotID <- "fdp5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fdp5)

# fdp5 - 2
fls_hps_1.2 <- fls_hps_stck[[94]]
vegIndex.Vars.fdp5.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fdp5.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fdp5.2$haralick <- haralick
vegIndex.Vars.fdp5.2$xyrad <- "02"
vegIndex.Vars.fdp5.2$plotID <- "fdp5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fdp5.2)

# fdp5 - 30
fls_hps_1.3 <- fls_hps_stck[[95]]
vegIndex.Vars.fdp5.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fdp5.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fdp5.3$haralick <- haralick
vegIndex.Vars.fdp5.3$xyrad <- "30"
vegIndex.Vars.fdp5.3$plotID <- "fdp5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fdp5.3)

# fdp5 - 5
fls_hps_1.5 <- fls_hps_stck[[96]]
vegIndex.Vars.fdp5.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fdp5.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fdp5.5$haralick <- haralick
vegIndex.Vars.fdp5.5$xyrad <- "05"
vegIndex.Vars.fdp5.5$plotID <- "fdp5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fdp5.5)


##
# fpo0 - 10
fls_hps_1.1 <- fls_hps_stck[[97]]
vegIndex.Vars.fpo0 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fpo0$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo0$haralick <- haralick
vegIndex.Vars.fpo0$xyrad <- "10"
vegIndex.Vars.fpo0$plotID <- "fpo0"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo0)

# fpo0 - 2
fls_hps_1.2 <- fls_hps_stck[[98]]
vegIndex.Vars.fpo0.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpo0.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo0.2$haralick <- haralick
vegIndex.Vars.fpo0.2$xyrad <- "02"
vegIndex.Vars.fpo0.2$plotID <- "fpo0"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo0.2)

# fpo0 - 30
fls_hps_1.3 <- fls_hps_stck[[99]]
vegIndex.Vars.fpo0.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpo0.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo0.3$haralick <- haralick
vegIndex.Vars.fpo0.3$xyrad <- "30"
vegIndex.Vars.fpo0.3$plotID <- "fpo0"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo0.3)

# fpo0 - 5
fls_hps_1.5 <- fls_hps_stck[[100]]
vegIndex.Vars.fpo0.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpo0.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo0.5$haralick <- haralick
vegIndex.Vars.fpo0.5$xyrad <- "05"
vegIndex.Vars.fpo0.5$plotID <- "fpo0"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo0.5)


##
# fpo1 - 10
fls_hps_1.1 <- fls_hps_stck[[101]]
vegIndex.Vars.fpo1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fpo1$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo1$haralick <- haralick
vegIndex.Vars.fpo1$xyrad <- "10"
vegIndex.Vars.fpo1$plotID <- "fpo1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo1)

# fpo1 - 2
fls_hps_1.2 <- fls_hps_stck[[102]]
vegIndex.Vars.fpo1.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpo1.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo1.2$haralick <- haralick
vegIndex.Vars.fpo1.2$xyrad <- "02"
vegIndex.Vars.fpo1.2$plotID <- "fpo1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo1.2)

# fpo1 - 30
fls_hps_1.3 <- fls_hps_stck[[103]]
vegIndex.Vars.fpo1.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpo1.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo1.3$haralick <- haralick
vegIndex.Vars.fpo1.3$xyrad <- "30"
vegIndex.Vars.fpo1.3$plotID <- "fpo1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo1.3)

# fpo1 - 5
fls_hps_1.5 <- fls_hps_stck[[104]]
vegIndex.Vars.fpo1.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpo1.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo1.5$haralick <- haralick
vegIndex.Vars.fpo1.5$xyrad <- "05"
vegIndex.Vars.fpo1.5$plotID <- "fpo1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo1.5)


##
# fpo2 - 10
fls_hps_1.1 <- fls_hps_stck[[105]]
vegIndex.Vars.fpo2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fpo2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo2$haralick <- haralick
vegIndex.Vars.fpo2$xyrad <- "10"
vegIndex.Vars.fpo2$plotID <- "fpo2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo2)

# fpo2 - 2
fls_hps_1.2 <- fls_hps_stck[[106]]
vegIndex.Vars.fpo2.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpo2.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo2.2$haralick <- haralick
vegIndex.Vars.fpo2.2$xyrad <- "02"
vegIndex.Vars.fpo2.2$plotID <- "fpo2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo2.2)

# fpo2 - 30
fls_hps_1.3 <- fls_hps_stck[[107]]
vegIndex.Vars.fpo2.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpo2.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo2.3$haralick <- haralick
vegIndex.Vars.fpo2.3$xyrad <- "30"
vegIndex.Vars.fpo2.3$plotID <- "fpo2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo2.3)

# fpo2 - 5
fls_hps_1.5 <- fls_hps_stck[[108]]
vegIndex.Vars.fpo2.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpo2.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo2.5$haralick <- haralick
vegIndex.Vars.fpo2.5$xyrad <- "05"
vegIndex.Vars.fpo2.5$plotID <- "fpo2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo2.5)


##
# fpo3 - 10
fls_hps_1.1 <- fls_hps_stck[[109]]
vegIndex.Vars.fpo3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fpo3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo3$haralick <- haralick
vegIndex.Vars.fpo3$xyrad <- "10"
vegIndex.Vars.fpo3$plotID <- "fpo3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo3)

# fpo3 - 2
fls_hps_1.2 <- fls_hps_stck[[110]]
vegIndex.Vars.fpo3.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpo3.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo3.2$haralick <- haralick
vegIndex.Vars.fpo3.2$xyrad <- "02"
vegIndex.Vars.fpo3.2$plotID <- "fpo3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo3.2)

# fpo3 - 30
fls_hps_1.3 <- fls_hps_stck[[111]]
vegIndex.Vars.fpo3.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpo3.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo3.3$haralick <- haralick
vegIndex.Vars.fpo3.3$xyrad <- "30"
vegIndex.Vars.fpo3.3$plotID <- "fpo3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo3.3)

# fpo3 - 5
fls_hps_1.5 <- fls_hps_stck[[112]]
vegIndex.Vars.fpo3.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpo3.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo3.5$haralick <- haralick
vegIndex.Vars.fpo3.5$xyrad <- "05"
vegIndex.Vars.fpo3.5$plotID <- "fpo3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo3.5)


##
# fpo4 - 10
fls_hps_1.1 <- fls_hps_stck[[113]]
vegIndex.Vars.fpo4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fpo4$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo4$haralick <- haralick
vegIndex.Vars.fpo4$xyrad <- "10"
vegIndex.Vars.fpo4$plotID <- "fpo4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo4)

# fpo4 - 2
fls_hps_1.2 <- fls_hps_stck[[114]]
vegIndex.Vars.fpo4.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpo4.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo4.2$haralick <- haralick
vegIndex.Vars.fpo4.2$xyrad <- "02"
vegIndex.Vars.fpo4.2$plotID <- "fpo4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo4.2)

# fpo4 - 30
fls_hps_1.3 <- fls_hps_stck[[115]]
vegIndex.Vars.fpo4.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpo4.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo4.3$haralick <- haralick
vegIndex.Vars.fpo4.3$xyrad <- "30"
vegIndex.Vars.fpo4.3$plotID <- "fpo4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo4.3)

# fpo4 - 5
fls_hps_1.5 <- fls_hps_stck[[116]]
vegIndex.Vars.fpo4.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpo4.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo4.5$haralick <- haralick
vegIndex.Vars.fpo4.5$xyrad <- "05"
vegIndex.Vars.fpo4.5$plotID <- "fpo4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo4.5)


##
# fpo5 - 10
fls_hps_1.1 <- fls_hps_stck[[117]]
vegIndex.Vars.fpo5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.fpo5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo5$haralick <- haralick
vegIndex.Vars.fpo5$xyrad <- "10"
vegIndex.Vars.fpo5$plotID <- "fpo5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo5)

# fpo5 - 2
fls_hps_1.2 <- fls_hps_stck[[118]]
vegIndex.Vars.fpo5.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpo5.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo5.2$haralick <- haralick
vegIndex.Vars.fpo5.2$xyrad <- "02"
vegIndex.Vars.fpo5.2$plotID <- "fpo5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo5.2)

# fpo5 - 30
fls_hps_1.3 <- fls_hps_stck[[119]]
vegIndex.Vars.fpo5.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpo5.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo5.3$haralick <- haralick
vegIndex.Vars.fpo5.3$xyrad <- "30"
vegIndex.Vars.fpo5.3$plotID <- "fpo5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo5.3)

# fpo5 - 5
fls_hps_1.5 <- fls_hps_stck[[120]]
vegIndex.Vars.fpo5.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.fpo5.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.fpo5.5$haralick <- haralick
vegIndex.Vars.fpo5.5$xyrad <- "05"
vegIndex.Vars.fpo5.5$plotID <- "fpo5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo5.5)


##
# hel1 - 10
fls_hps_1.1 <- fls_hps_stck[[121]]
vegIndex.Vars.hel1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.hel1$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.hel1$haralick <- haralick
vegIndex.Vars.hel1$xyrad <- "10"
vegIndex.Vars.hel1$plotID <- "hel1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hel1)

# hel1 - 2
fls_hps_1.2 <- fls_hps_stck[[122]]
vegIndex.Vars.hel1.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.hel1.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.hel1.2$haralick <- haralick
vegIndex.Vars.hel1.2$xyrad <- "02"
vegIndex.Vars.hel1.2$plotID <- "hel1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hel1.2)

# hel1 - 30
fls_hps_1.3 <- fls_hps_stck[[123]]
vegIndex.Vars.hel1.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.hel1.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.hel1.3$haralick <- haralick
vegIndex.Vars.hel1.3$xyrad <- "30"
vegIndex.Vars.hel1.3$plotID <- "hel1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hel1.3)

# hel1 - 5
fls_hps_1.5 <- fls_hps_stck[[124]]
vegIndex.Vars.hel1.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.hel1.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.hel1.5$haralick <- haralick
vegIndex.Vars.hel1.5$xyrad <- "05"
vegIndex.Vars.hel1.5$plotID <- "hel1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hel1.5)


##
# hel2 - 10
fls_hps_1.1 <- fls_hps_stck[[125]]
vegIndex.Vars.hel2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.hel2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.hel2$haralick <- haralick
vegIndex.Vars.hel2$xyrad <- "10"
vegIndex.Vars.hel2$plotID <- "hel2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hel2)

# hel2 - 2
fls_hps_1.2 <- fls_hps_stck[[126]]
vegIndex.Vars.hel2.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.hel2.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.hel2.2$haralick <- haralick
vegIndex.Vars.hel2.2$xyrad <- "02"
vegIndex.Vars.hel2.2$plotID <- "hel2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hel2.2)

# hel2 - 30
fls_hps_1.3 <- fls_hps_stck[[127]]
vegIndex.Vars.hel2.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.hel2.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.hel2.3$haralick <- haralick
vegIndex.Vars.hel2.3$xyrad <- "30"
vegIndex.Vars.hel2.3$plotID <- "hel2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hel2.3)

# hel2 - 5
fls_hps_1.5 <- fls_hps_stck[[128]]
vegIndex.Vars.hel2.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.hel2.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.hel2.5$haralick <- haralick
vegIndex.Vars.hel2.5$xyrad <- "05"
vegIndex.Vars.hel2.5$plotID <- "hel2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hel2.5)


##
# hel3 - 10
fls_hps_1.1 <- fls_hps_stck[[129]]
vegIndex.Vars.hel3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.hel3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.hel3$haralick <- haralick
vegIndex.Vars.hel3$xyrad <- "10"
vegIndex.Vars.hel3$plotID <- "hel3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hel3)

# hel3 - 2
fls_hps_1.2 <- fls_hps_stck[[130]]
vegIndex.Vars.hel3.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.hel3.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.hel3.2$haralick <- haralick
vegIndex.Vars.hel3.2$xyrad <- "02"
vegIndex.Vars.hel3.2$plotID <- "hel3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hel3.2)

# hel3 - 30
fls_hps_1.3 <- fls_hps_stck[[131]]
vegIndex.Vars.hel3.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.hel3.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.hel3.3$haralick <- haralick
vegIndex.Vars.hel3.3$xyrad <- "30"
vegIndex.Vars.hel3.3$plotID <- "hel3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hel3.3)

# hel3 - 5
fls_hps_1.5 <- fls_hps_stck[[132]]
vegIndex.Vars.hel3.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.hel3.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.hel3.5$haralick <- haralick
vegIndex.Vars.hel3.5$xyrad <- "05"
vegIndex.Vars.hel3.5$plotID <- "hel3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hel3.5)


##
# hel4 - 10
fls_hps_1.1 <- fls_hps_stck[[133]]
vegIndex.Vars.hel4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                data.frame(mean, sd)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegIndex.Vars.hel4$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.hel4$haralick <- haralick
vegIndex.Vars.hel4$xyrad <- "10"
vegIndex.Vars.hel4$plotID <- "hel4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hel4)

# hel4 - 2
fls_hps_1.2 <- fls_hps_stck[[134]]
vegIndex.Vars.hel4.2 <- foreach(i = 1:nlayers(fls_hps_1.2), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.2[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.hel4.2$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.hel4.2$haralick <- haralick
vegIndex.Vars.hel4.2$xyrad <- "02"
vegIndex.Vars.hel4.2$plotID <- "hel4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hel4.2)

# hel4 - 30
fls_hps_1.3 <- fls_hps_stck[[135]]
vegIndex.Vars.hel4.3 <- foreach(i = 1:nlayers(fls_hps_1.3), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.3[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.hel4.3$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.hel4.3$haralick <- haralick
vegIndex.Vars.hel4.3$xyrad <- "30"
vegIndex.Vars.hel4.3$plotID <- "hel4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hel4.3)

# hel4 - 5
fls_hps_1.5 <- fls_hps_stck[[136]]
vegIndex.Vars.hel4.5 <- foreach(i = 1:nlayers(fls_hps_1.5), .packages = "raster", 
                                .combine = "rbind") %dopar% {
                                  mean <- mean(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  sd <- sd(fls_hps_1.5[[i]][], na.rm = TRUE)
                                  data.frame(mean, sd)
                                }

# get neames of vegIndices
vegIndex.Vars.hel4.5$vegIndex <- rep(vegindex_names[c(4,68,87,89)], each=29)
vegIndex.Vars.hel4.5$haralick <- haralick
vegIndex.Vars.hel4.5$xyrad <- "05"
vegIndex.Vars.hel4.5$plotID <- "hel4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hel4.5)


write.csv2(vegIndex.Vars.all, 
           file = "pca_inverse_vegIndex_haralick_stats_2nd.csv", 
           row.names = TRUE)

stopCluster(cl)


