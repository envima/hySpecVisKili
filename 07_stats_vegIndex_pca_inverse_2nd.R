library(hsdar)
library(foreach)
library(doParallel)

setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd/norm_441/re_norm/pca/pca_inverse/pci_vegIndex/pci_vegIndex_dia_50m/")
filepath_raster <- "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd/norm_441/re_norm/pca/pca_inverse/pci_vegIndex/pci_vegIndex_dia_50m/"


# vegIndices raster files
fls_hps <- list.files(paste0(filepath_raster), full.names = FALSE,
                      recursive = FALSE, pattern = "*vegIndex_dia_50m.tif")

fls_hps_stck <- lapply(seq(fls_hps), function(i){
  stack(fls_hps[[i]])
})

# one plot aim: all plots
#cl <- makeCluster(detectCores() - 1)
cl <- 15
registerDoParallel(cl)

# fed1
fls_hps_1.1 <- fls_hps_stck[[1]]
vegIndex.Vars.fed1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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
row.names(vegIndex.Vars.fed1) <- vegindex_names
vegIndex.Vars.fed1$plotID <- "fed1"

#fed2
fls_hps_1.1 <- fls_hps_stck[[2]]
vegIndex.Vars.fed2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.fed2) <- vegindex_names
vegIndex.Vars.fed2$plotID <- "fed2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.fed1, vegIndex.Vars.fed2)

# fed3
fls_hps_1.1 <- fls_hps_stck[[3]]
vegIndex.Vars.fed3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.fed3) <- vegindex_names
vegIndex.Vars.fed3$plotID <- "fed3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fed3)

# fed4
fls_hps_1.1 <- fls_hps_stck[[4]]
vegIndex.Vars.fed4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.fed4) <- vegindex_names
vegIndex.Vars.fed4$plotID <- "fed4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fed4)

# fed5
fls_hps_1.1 <- fls_hps_stck[[5]]
vegIndex.Vars.fed5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.fed5) <- vegindex_names
vegIndex.Vars.fed5$plotID <- "fed5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fed5)

# fer0
fls_hps_1.1 <- fls_hps_stck[[6]]
vegIndex.Vars.fer0 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.fer0) <- vegindex_names
vegIndex.Vars.fer0$plotID <- "fer0"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fer0)

# fer2
fls_hps_1.1 <- fls_hps_stck[[7]]
vegIndex.Vars.fer2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.fer2) <- vegindex_names
vegIndex.Vars.fer2$plotID <- "fer2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fer2)

# fer3
fls_hps_1.1 <- fls_hps_stck[[8]]
vegIndex.Vars.fer3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.fer3) <- vegindex_names
vegIndex.Vars.fer3$plotID <- "fer3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fer3)

# fer4
fls_hps_1.1 <- fls_hps_stck[[9]]
vegIndex.Vars.fer4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.fer4) <- vegindex_names
vegIndex.Vars.fer4$plotID <- "fer4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fer4)

# foc0
fls_hps_1.1 <- fls_hps_stck[[10]]
vegIndex.Vars.foc0 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.foc0) <- vegindex_names
vegIndex.Vars.foc0$plotID <- "foc0"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc0)

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

# foc2
fls_hps_1.1 <- fls_hps_stck[[12]]
vegIndex.Vars.foc2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.foc2) <- vegindex_names
vegIndex.Vars.foc2$plotID <- "foc2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc2)

# foc3
fls_hps_1.1 <- fls_hps_stck[[13]]
vegIndex.Vars.foc3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.foc3) <- vegindex_names
vegIndex.Vars.foc3$plotID <- "foc3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc3)

# foc4
fls_hps_1.1 <- fls_hps_stck[[13]]
vegIndex.Vars.foc4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.foc4) <- vegindex_names
vegIndex.Vars.foc4$plotID <- "foc4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc4)

# foc5
fls_hps_1.1 <- fls_hps_stck[[15]]
vegIndex.Vars.foc5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.foc5) <- vegindex_names
vegIndex.Vars.foc5$plotID <- "foc5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.foc5)

# foc6
fls_hps_1.1 <- fls_hps_stck[[16]]
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

# fod1
fls_hps_1.1 <- fls_hps_stck[[17]]
vegIndex.Vars.fod1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.fod1) <- vegindex_names
vegIndex.Vars.fod1$plotID <- "fod1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fod1)

# fod2
fls_hps_1.1 <- fls_hps_stck[[18]]
vegIndex.Vars.fod2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.fod2) <- vegindex_names
vegIndex.Vars.fod2$plotID <- "fod2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fod2)

# fod3
fls_hps_1.1 <- fls_hps_stck[[19]]
vegIndex.Vars.fod3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.fod3) <- vegindex_names
vegIndex.Vars.fod3$plotID <- "fod3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fod3)

# fpd0
fls_hps_1.1 <- fls_hps_stck[[20]]
vegIndex.Vars.fpd0 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.fpd0) <- vegindex_names
vegIndex.Vars.fpd0$plotID <- "fpd0"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpd0)

# fpd2
fls_hps_1.1 <- fls_hps_stck[[21]]
vegIndex.Vars.fpd2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.fpd2) <- vegindex_names
vegIndex.Vars.fpd2$plotID <- "fpd2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpd2)

# fpd3
fls_hps_1.1 <- fls_hps_stck[[22]]
vegIndex.Vars.fpd3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.fpd3) <- vegindex_names
vegIndex.Vars.fpd3$plotID <- "fpd3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpd3)

# fpd4
fls_hps_1.1 <- fls_hps_stck[[23]]
vegIndex.Vars.fpd4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.fpd4) <- vegindex_names
vegIndex.Vars.fpd4$plotID <- "fpd4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpd4)

# fpd5
fls_hps_1.1 <- fls_hps_stck[[24]]
vegIndex.Vars.fpd5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.fpd5) <- vegindex_names
vegIndex.Vars.fpd5$plotID <- "fpd5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpd5)

# fpo0
fls_hps_1.1 <- fls_hps_stck[[25]]
vegIndex.Vars.fpo0 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.fpo0) <- vegindex_names
vegIndex.Vars.fpo0$plotID <- "fpo0"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo0)

# fpo1
fls_hps_1.1 <- fls_hps_stck[[26]]
vegIndex.Vars.fpo1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.fpo1) <- vegindex_names
vegIndex.Vars.fpo1$plotID <- "fpo1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo1)

# fpo2
fls_hps_1.1 <- fls_hps_stck[[27]]
vegIndex.Vars.fpo2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.fpo2) <- vegindex_names
vegIndex.Vars.fpo2$plotID <- "fpo2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo2)

# fpo3
fls_hps_1.1 <- fls_hps_stck[[28]]
vegIndex.Vars.fpo3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.fpo3) <- vegindex_names
vegIndex.Vars.fpo3$plotID <- "fpo3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo3)

# fpo4
fls_hps_1.1 <- fls_hps_stck[[29]]
vegIndex.Vars.fpo4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.fpo4) <- vegindex_names
vegIndex.Vars.fpo4$plotID <- "fpo4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo4)

# fpo5
fls_hps_1.1 <- fls_hps_stck[[30]]
vegIndex.Vars.fpo5 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.fpo5) <- vegindex_names
vegIndex.Vars.fpo5$plotID <- "fpo5"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.fpo5)

# hel1
fls_hps_1.1 <- fls_hps_stck[[31]]
vegIndex.Vars.hel1 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.hel1) <- vegindex_names
vegIndex.Vars.hel1$plotID <- "hel1"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hel1)

# hel2
fls_hps_1.1 <- fls_hps_stck[[32]]
vegIndex.Vars.hel2 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.hel2) <- vegindex_names
vegIndex.Vars.hel2$plotID <- "hel2"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hel2)

# hel3
fls_hps_1.1 <- fls_hps_stck[[33]]
vegIndex.Vars.hel3 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.hel3) <- vegindex_names
vegIndex.Vars.hel3$plotID <- "hel3"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hel3)

# hel4
fls_hps_1.1 <- fls_hps_stck[[34]]
vegIndex.Vars.hel4 <- foreach(i = 1:nlayers(fls_hps_1.1), .packages = "raster", 
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

row.names(vegIndex.Vars.hel4) <- vegindex_names
vegIndex.Vars.hel4$plotID <- "hel4"
vegIndex.Vars.all <- rbind(vegIndex.Vars.all, vegIndex.Vars.hel4)


write.csv2(vegIndex.Vars.all, 
           file = "pci_vegIndex_dia_50m_stats_2nd.csv", 
           row.names = TRUE)

stopCluster(cl)


