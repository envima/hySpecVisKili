library(hsdar)
library(foreach)
library(doParallel)

setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/1st/norm_441/re_norm/pca/pca_inverse/pci_vegIndex/pci_vegIndex_dia_50m/")
filepath_raster <- "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/1st/norm_441/re_norm/pca/pca_inverse/pci_vegIndex/pci_vegIndex_dia_50m/"


# vegIndices raster files
fls_hps <- list.files(paste0(filepath_raster), full.names = FALSE,
                      recursive = FALSE, pattern = "*vegIndex_dia_50m.tif")

fls_hps_stck <- lapply(seq(fls_hps), function(i){
  stack(fls_hps[[i]])
})

# one plot aim: all plots
cl <- makeCluster(15)
registerDoParallel(cl)

# cof1
fls_hps_1.1 <- fls_hps_stck[[1]]
vegIndex.Vars.cof1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

# get neames of vegIndices
vegindex_names <- vegindex() 
vegindex_names <- vegindex_names[c(4:17, 20:21, 23:25, 28:29, 34, 38:47, 
                                   50:57, 59, 61:65, 68:70, 72:83, 85:98, 
                                   106:113, 115)]
row.names(vegIndex.Vars.cof1) <- vegindex_names
vegIndex.Vars.cof1$plotID <- "cof1"

#cof2
fls_hps_1.1 <- fls_hps_stck[[2]]
vegIndex.Vars.cof2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], probs = .25, 
                                                  na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], probs = .75, 
                                                  na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.cof2) <- vegindex_names
vegIndex.Vars.cof2$plotID <- "cof2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.cof1, vegIndex.Vars.cof2)

# cof3
fls_hps_1.1 <- fls_hps_stck[[3]]
vegIndex.Vars.cof3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.cof3) <- vegindex_names
vegIndex.Vars.cof3$plotID <- "cof3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.cof3)

# cof4
fls_hps_1.1 <- fls_hps_stck[[4]]
vegIndex.Vars.cof4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.cof4) <- vegindex_names
vegIndex.Vars.cof4$plotID <- "cof4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.cof4)

# cof5
fls_hps_1.1 <- fls_hps_stck[[5]]
vegIndex.Vars.cof5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.cof5) <- vegindex_names
vegIndex.Vars.cof5$plotID <- "cof5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.cof5)

# cof6
#fls_hps_1.1 <- fls_hps_stck[[6]]
#vegIndex.Vars.cof6 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
#                              .combine = "rbind") %dopar% {
#                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
#                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
#                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
#                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
#                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
#                                stqrt <- quantile(fls_hps_1.1[[i]][], 
#                                                  probs = .25, na.rm = TRUE)
#                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
#                                                  probs = .75, na.rm = TRUE)
#                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
#                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
#                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
#                                data.frame(min, max, mean, median, sd, stqrt, 
#                                           rdqrt, IQR, IQR_M)
#                              }

#row.names(vegIndex.Vars.cof6) <- vegindex_names
#vegIndex.Vars.cof6$plotID <- "cof6"
#vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.cof6)

# flm1
fls_hps_1.1 <- fls_hps_stck[[6]]
vegIndex.Vars.flm1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.flm1) <- vegindex_names
vegIndex.Vars.flm1$plotID <- "flm1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.flm1)

# flm2
fls_hps_1.1 <- fls_hps_stck[[7]]
vegIndex.Vars.flm2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.flm2) <- vegindex_names
vegIndex.Vars.flm2$plotID <- "flm2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.flm2)

# flm3
fls_hps_1.1 <- fls_hps_stck[[8]]
vegIndex.Vars.flm3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.flm3) <- vegindex_names
vegIndex.Vars.flm3$plotID <- "flm3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.flm3)

# flm4
fls_hps_1.1 <- fls_hps_stck[[9]]
vegIndex.Vars.flm4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.flm4) <- vegindex_names
vegIndex.Vars.flm4$plotID <- "flm4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.flm4)

# flm6
fls_hps_1.1 <- fls_hps_stck[[10]]
vegIndex.Vars.flm6 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.flm6) <- vegindex_names
vegIndex.Vars.flm6$plotID <- "flm6"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.flm6)

# foc1
fls_hps_1.1 <- fls_hps_stck[[11]]
vegIndex.Vars.foc1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.foc1) <- vegindex_names
vegIndex.Vars.foc1$plotID <- "foc1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc1)

# foc6
fls_hps_1.1 <- fls_hps_stck[[12]]
vegIndex.Vars.foc6 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.foc6) <- vegindex_names
vegIndex.Vars.foc6$plotID <- "foc6"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc6)

# fod5
fls_hps_1.1 <- fls_hps_stck[[13]]
vegIndex.Vars.fod5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.fod5) <- vegindex_names
vegIndex.Vars.fod5$plotID <- "fod5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fod5)

# gra1
fls_hps_1.1 <- fls_hps_stck[[14]]
vegIndex.Vars.gra1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.gra1) <- vegindex_names
vegIndex.Vars.gra1$plotID <- "gra1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.gra1)

# gra2
fls_hps_1.1 <- fls_hps_stck[[15]]
vegIndex.Vars.gra2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.gra2) <- vegindex_names
vegIndex.Vars.gra2$plotID <- "gra2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.gra2)

# gra3
fls_hps_1.1 <- fls_hps_stck[[16]]
vegIndex.Vars.gra3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.gra3) <- vegindex_names
vegIndex.Vars.gra3$plotID <- "gra3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.gra3)

# gra4
fls_hps_1.1 <- fls_hps_stck[[17]]
vegIndex.Vars.gra4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.gra4) <- vegindex_names
vegIndex.Vars.gra4$plotID <- "gra4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.gra4)

# gra5
fls_hps_1.1 <- fls_hps_stck[[18]]
vegIndex.Vars.gra5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.gra5) <- vegindex_names
vegIndex.Vars.gra5$plotID <- "gra5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.gra5)

# gra6
fls_hps_1.1 <- fls_hps_stck[[19]]
vegIndex.Vars.gra6 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.gra6) <- vegindex_names
vegIndex.Vars.gra6$plotID <- "gra6"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.gra6)

# hom1
fls_hps_1.1 <- fls_hps_stck[[20]]
vegIndex.Vars.hom1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.hom1) <- vegindex_names
vegIndex.Vars.hom1$plotID <- "hom1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hom1)

# hom2
fls_hps_1.1 <- fls_hps_stck[[21]]
vegIndex.Vars.hom2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.hom2) <- vegindex_names
vegIndex.Vars.hom2$plotID <- "hom2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hom2)

# hom3
fls_hps_1.1 <- fls_hps_stck[[22]]
vegIndex.Vars.hom3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.hom3) <- vegindex_names
vegIndex.Vars.hom3$plotID <- "hom3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hom3)

# hom4
fls_hps_1.1 <- fls_hps_stck[[23]]
vegIndex.Vars.hom4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.hom4) <- vegindex_names
vegIndex.Vars.hom4$plotID <- "hom4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hom4)

# hom5
fls_hps_1.1 <- fls_hps_stck[[24]]
vegIndex.Vars.hom5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.hom5) <- vegindex_names
vegIndex.Vars.hom5$plotID <- "hom5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hom5)

# mai1
fls_hps_1.1 <- fls_hps_stck[[25]]
vegIndex.Vars.mai1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.mai1) <- vegindex_names
vegIndex.Vars.mai1$plotID <- "mai1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.mai1)

# mai2
fls_hps_1.1 <- fls_hps_stck[[26]]
vegIndex.Vars.mai2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.mai2) <- vegindex_names
vegIndex.Vars.mai2$plotID <- "mai2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.mai2)

# mai3
fls_hps_1.1 <- fls_hps_stck[[27]]
vegIndex.Vars.mai3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.mai3) <- vegindex_names
vegIndex.Vars.mai3$plotID <- "mai3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.mai3)

# mai4
fls_hps_1.1 <- fls_hps_stck[[28]]
vegIndex.Vars.mai4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.mai4) <- vegindex_names
vegIndex.Vars.mai4$plotID <- "mai4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.mai4)

# mai5
fls_hps_1.1 <- fls_hps_stck[[29]]
vegIndex.Vars.mai5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.mai5) <- vegindex_names
vegIndex.Vars.mai5$plotID <- "mai5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.mai5)

# sav1
fls_hps_1.1 <- fls_hps_stck[[30]]
vegIndex.Vars.sav1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.sav1) <- vegindex_names
vegIndex.Vars.sav1$plotID <- "sav1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.sav1)

# sav2
fls_hps_1.1 <- fls_hps_stck[[31]]
vegIndex.Vars.sav2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.sav2) <- vegindex_names
vegIndex.Vars.sav2$plotID <- "sav2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.sav2)

# sav3
fls_hps_1.1 <- fls_hps_stck[[32]]
vegIndex.Vars.sav3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.sav3) <- vegindex_names
vegIndex.Vars.sav3$plotID <- "sav3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.sav3)

# sav4
fls_hps_1.1 <- fls_hps_stck[[33]]
vegIndex.Vars.sav4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.sav4) <- vegindex_names
vegIndex.Vars.sav4$plotID <- "sav4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.sav4)

# sav5
fls_hps_1.1 <- fls_hps_stck[[34]]
vegIndex.Vars.sav5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
                              .combine = "rbind") %dopar% {
                                min <- min(fls_hps_1.1[[i]][], na.rm = TRUE)
                                max <- max(fls_hps_1.1[[i]][], na.rm = TRUE)
                                mean <- mean(fls_hps_1.1[[i]][], na.rm = TRUE)
                                median <- median(fls_hps_1.1[[i]][], na.rm = TRUE)
                                sd <- sd(fls_hps_1.1[[i]][], na.rm = TRUE)
                                stqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .25, na.rm = TRUE)
                                rdqrt <- quantile(fls_hps_1.1[[i]][], 
                                                  probs = .75, na.rm = TRUE)
                                IQR <- IQR(fls_hps_1.1[[i]][], na.rm = TRUE)
                                IQR_M <- (IQR(fls_hps_1.1[[i]][], na.rm = TRUE))/
                                  (median(fls_hps_1.1[[i]][], na.rm = TRUE))
                                data.frame(min, max, mean, median, sd, stqrt, 
                                           rdqrt, IQR, IQR_M)
                              }

row.names(vegIndex.Vars.sav5) <- vegindex_names
vegIndex.Vars.sav5$plotID <- "sav5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.sav5)


write.csv2(vegIndex.Vars.all, 
           file = "pci_vegIndex_dia_50m_stats_1st.csv", 
           row.names = TRUE)

stopCluster(cl)


