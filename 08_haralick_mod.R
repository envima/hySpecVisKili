library(foreach)
library(doParallel)
library(satelliteTools)
initOTB("/usr/share/OTB-5.8/")

setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd/norm_441/re_norm/pca/pca_inverse/pci_vegIndex/")
filepath_raster <- "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd/norm_441/re_norm/pca/pca_inverse/pci_vegIndex/"

# list vegIndices raster files
fls_hps <- list.files(paste0(filepath_raster), full.names = FALSE,
                      recursive = FALSE, pattern = "*vegIndex.tif")

# create output names
rad <- c("rad_02", "rad_05", "rad_10", "rad_30")
odr <- file.path(unique(dirname(fls_hps)), "pci_vegIndex_hara")
ofl <- file.path(odr, gsub(".tif$", "_hara.tif", 
                           paste(rad, rep(substr(basename(fls_hps), 1, 24), each = 4), "hara",
                                 sep = "_")))

# stack files
fls_hps_stck <- lapply(seq(fls_hps), function(i){
  stack(fls_hps[[i]])
})

# use only four vegIndices
fls_veg <- lapply(seq(fls_hps), function(i){
  (fls_hps_stck[[i]][[c(1,47,64,66)]]) 
})


### range min max alle plots spec div
# get minValues -1
minv <- foreach(i = seq(fls_hps_stck)) %do% {
  minValue(fls_hps_stck[[i]][[c(1,47,64,66)]]) 
}

# get maxValues +1
maxv <- foreach(i = seq(fls_hps_stck)) %do% {
  maxValue(fls_hps_stck[[i]][[c(1,47,64,66)]]) 
}

# define radius 
windows <- c(2, 5, 10, 30)


#cl <- 15
#registerDoParallel(cl)

#fls_veg <- fls_veg[1:2]
# get haralick textures
fls_oth_all <- lapply(seq(fls_veg), function(j) {
  oth <- foreach(k = seq(windows)) %do% {
    otbTexturesHaralick(x = fls_veg[[j]], 
                        texture = "all",
                        path_output = filepath_raster, 
                        return_raster = TRUE, 
                        parameters.xyrad = list(c(windows[k], windows[k])),
                        parameters.xyoff = list(c(1,1)),
                        parameters.minmax = c(minv[[j]], maxv[[j]]),
                        parameters.nbbin = 8, #16
                        channel = NULL,
                        verbose = FALSE,
                        ram = "8192")
    }
  })


#saveRDS(fls_oth_all, "haralick_vegIndex_all_1st.rds")
#fls_oth_all <- readRDS("haralick_vegIndex_all_1st.rds")

# write raster
oth_unlist <- unlist(fls_oth_all)

for (i in 1:length(oth_unlist)) {
  writeRaster(oth_unlist[[i]], file = ofl[[i]], format='GTiff')
}

