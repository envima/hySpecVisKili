library(rPointDB)
library(foreach)
library(doParallel)

#####################################
# remote server
remotesensing <- RemoteSensing$new("http://192.168.191.183:8081", "user:password") 

# get one POI group
pois <- remotesensing$poi_group("kili")

# get extent of all plots
ext_all <- foreach(i = 1:nrow(pois)) %do% {
  extent_diameter(pois$x[i], pois$y[i], 50)
}

names(ext_all) <- pois$name

# get RasterStack of all bands at ext
#r <- rasterdb$raster(ext)
#plot(r)

###################################
cl <- makeCluster(15)
registerDoParallel(cl)

setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd/norm_441/re_norm/pca/pca_inverse/pci_vegIndex/pci_vegIndex_hara/")
filepath_raster <- "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd/norm_441/re_norm/pca/pca_inverse/pci_vegIndex/pci_vegIndex_hara/"

fls_hps <- list.files(paste0(filepath_raster), full.names = FALSE,
                      recursive = FALSE, pattern = "*.tif")

# stack and raster files
fls_hps_stck <- foreach(i = seq(fls_hps), .packages = "raster") %dopar% {
  stck <- stack(fls_hps[i])
}

nms_rst <- foreach(i = seq(fls_hps_stck), .packages = "raster") %dopar% {
  substr(names(fls_hps_stck[[i]][[1]]), 8, 11)
}

#ext_1st <- ext_all[c(1:5,17:21,23,28,33,46:51,57:66,70:74)]
#ext_1st_hara <- rep(ext_1st, times = 4)
ext_2nd <- ext_all[c(7:12, 14:16, 22:31, 34, 36:45, 52:55)]
ext_2nd_hara <- rep(ext_2nd, times = 4)

fls_rst_crp <- foreach(i = seq(fls_hps_stck), .packages = "raster") %dopar% {
  crop(fls_hps_stck[[i]], ext_2nd_hara[[i]])
}

#plot(fls_rst_crp[[27]][[1]])

# write raster
#oth_unlist <- unlist(fls_rst_crp)

#ofl <- paste(substr(fls_hps, 1, 24), "dia_50m", sep = "_") #24
ofl_1 <- paste(substr(fls_hps[1:136], 8, 36), substr(fls_hps[1:136], 1, 6), "dia_50m", sep = "_")
#ofl_2 <- paste(substr(fls_hps[103:136], 1, 34), "dia_50m", sep = "_")

#for (i in 1:length(fls_rst_crp)) {
#  writeRaster(fls_rst_crp[[i]], file = ofl[[i]], format='GTiff')
#}

fls_rst_crp_1 <- fls_rst_crp[1:136]
for (i in 1:length(fls_rst_crp_1)) {
  writeRaster(fls_rst_crp_1[[i]], file = ofl_1[[i]], format='GTiff')
}

#fls_rst_crp_2 <- fls_rst_crp[103:136]
#for (i in 1:length(fls_rst_crp_2)) {
#  writeRaster(fls_rst_crp_2[[i]], file = ofl_2[[i]], format='GTiff')
#}

stopCluster()
