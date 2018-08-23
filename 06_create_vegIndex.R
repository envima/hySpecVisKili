library(hsdar)
library(foreach)
library(doParallel)

setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd/norm_441/re_norm/pca/pca_inverse")
filepath_raster <- "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd/norm_441/re_norm/pca/pca_inverse"

band_meta <- read.csv(paste0("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd/", 
                             "band_meta.csv"))
band_meta_1 <- band_meta[-c(8,159:160),]

fls_hps <- list.files(paste0(filepath_raster), full.names = FALSE,
                      recursive = FALSE, pattern = "*pci.tif")

tmp <- stack(fls_hps[[1]])


## get all predifined index names in hsdar
# not all indices are working
vegindex_names <- vegindex() 
vegindex_names <- vegindex_names[c(4:17, 20:21, 23:25, 28:29, 34, 38:47, 
                                   50:57, 59, 61:65, 68:70, 72:83, 85:98, 
                                   106:113, 115)]

#cl <- makePSOCKcluster(detectCores() - 1)
cl <- makePSOCKcluster(15)
jnk <- clusterEvalQ(cl, {library(hsdar)})
clusterExport(cl, c("band_meta_1", "vegindex_names"))

# 1st flight campaign
tst.all <- parLapply(cl, fls_hps[c(1:34)], function(i){
  hsr <- HyperSpecRaster(i, 
                         wavelength = band_meta_1$wavelength, 
                         fwhm = band_meta_1$fwhm, 
                         continuousdata = TRUE)
  
  tr <- blockSize(hsr)
  outfile <- gsub(".GTiff$", i, paste(substr(i, 1, 15), "vegIndex.tif", sep = "_"))
  res <- writeStart(hsr, outfile, overwrite = TRUE, nl = 84) #nl = n_veg
  for (j in 1:tr$n){
    v <- getValuesBlock(hsr, row = tr$row[j], nrows = tr$nrows[j])
    v <- as.matrix(vegindex(v, index = vegindex_names))
    res <- writeValues(res, v, tr$row[j])
  }
  res <- writeStop(res)
  return(res)
})

stopCluster(cl)

