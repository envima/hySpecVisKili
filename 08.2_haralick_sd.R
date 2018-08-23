library(foreach)
library(doParallel)
library(satelliteTools)
initOTB("/usr/share/OTB-5.8/")

setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/1st_sd_hara/")
filepath_raster <- "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/1st_sd_hara/"

# list vegIndices raster files
fls_hps <- list.files(paste0(filepath_raster), full.names = FALSE,
                      recursive = FALSE, pattern = "pci_sd.tif")

# create output names
rad <- c("rad_02", "rad_05", "rad_10", "rad_30")
odr <- file.path(unique(dirname(fls_hps)), "pci_sd_16")
ofl <- file.path(odr, gsub(".tif$", "_hara.tif", 
                           paste(rad,  "hara", rep(substr(basename(fls_hps), 1, 23), each = 4),
                                 sep = "_")))

# stack files
fls_hps_stck <- lapply(seq(fls_hps), function(i){
  stack(fls_hps[[i]])
})


### range min max alle plots spec div3
# get minValues -1
minv <- foreach(i = seq(fls_hps_stck)) %do% {
  minValue(fls_hps_stck[[i]]) 
}

minv = min(unlist(minv)) -1

# get maxValues +1
maxv <- foreach(i = seq(fls_hps_stck)) %do% {
  maxValue(fls_hps_stck[[i]]) 
}

maxv = max(unlist(maxv)) +1

# define radius 
windows <- c(2, 5, 10, 30)

#cl = makePSOCKcluster(12L)
#jnk = clusterEvalQ(cl, c(library(satelliteTools), library(foreach)))

#clusterExport(cl, c("fls_hps_stck", "filepath_raster",
#                    "windows", "minv", "maxv", "otbPath"))


# get haralick textures
fls_oth_all <- lapply(seq(fls_hps_stck), function(j) {
  oth <- foreach(k = seq(windows)) %do% {
    otbTexturesHaralick(x = fls_hps_stck[[j]], 
                        texture = "all",
                        path_output = filepath_raster, 
                        return_raster = TRUE, 
                        parameters.xyrad = list(c(windows[k], windows[k])),
                        parameters.xyoff = list(c(1,1)),
                        parameters.minmax = c(minv, maxv),
                        parameters.nbbin = 16, #16
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

ofl[[1]]
