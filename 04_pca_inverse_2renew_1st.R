library(hsdar)
library(foreach)
library(doParallel)
library(RStoolbox)
library(dplyr)
library(XML)

# faster
cl <- makeCluster(13)
registerDoParallel(cl)

setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/1st/norm_441/re_norm")
filepath_raster <- "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/1st/norm_441/re_norm"


# list files
fls_hps <- list.files(paste0(filepath_raster), full.names = FALSE,
                      recursive = FALSE, pattern = "*.tif")

# stack and raster files
fls_hps_stck <- foreach(i = seq(fls_hps), .packages = "raster") %dopar% {
  stck <- stack(fls_hps[i])
}


######
## cof1
#pca
cof1_pca <- rasterPCA(fls_hps_stck[[1]][[-c(1:2,10)]], nComp = 157)
#pci
cof1_pci = cof1_pca$map@data@values[,1:54] %*% t(cof1_pca$model$loadings[,1:54])
#scale pci
cof1 = as.data.frame(as.matrix(fls_hps_stck[[1]][[-c(1:2,10)]]))
mu = colMeans(cof1, na.rm = TRUE)
cof1_pci_scl = scale(cof1_pci, center = -mu, scale = FALSE)

# get raster stack
cof1_pci_rst <- foreach(i = seq(ncol(cof1_pci_scl)), .packages = "raster") %dopar% {
  tst <- cof1_pci_scl[,i]
  dim(tst) <- c(201, 201)
  cof1_rst <- raster(tst,
                     xmn = 305617.5,
                     xmx = 305718,
                     ymn = 9641336.5,
                     ymx = 9641437,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

cof1_pci_stck <- stack(cof1_pci_rst)
writeRaster(cof1_pci_stck, 
            paste0("pca/pca_inverse/", "cof1_441_re_pci"), format = "GTiff")


## cof2
cof2_pca <- rasterPCA(fls_hps_stck[[2]][[-c(1:2,10)]], nComp = 157)
cof2_pci = cof2_pca$map@data@values[,1:38] %*% t(cof2_pca$model$loadings[,1:38])
#scale pci
cof2 = as.data.frame(as.matrix(fls_hps_stck[[2]][[-c(1:2,10)]]))
mu = colMeans(cof2, na.rm = TRUE)
cof2_pci_scl = scale(cof2_pci, center = -mu, scale = FALSE)

# get raster stack
cof2_pci_rst <- foreach(i = seq(ncol(cof2_pci_scl)), .packages = "raster") %dopar% {
  tst <- cof2_pci_scl[,i]
  dim(tst) <- c(201, 201)
  cof1_rst <- raster(tst,
                     xmn = 307636.5,
                     xmx = 307737,
                     ymn = 9642424.5,
                     ymx = 9642525,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

cof2_pci_stck <- stack(cof2_pci_rst)
writeRaster(cof2_pci_stck, 
            paste0("pca/pca_inverse/", "cof2_441_re_pci"), format = "GTiff")


## cof3
cof3_pca <- rasterPCA(fls_hps_stck[[3]][[-c(1:2,10)]], nComp = 157)
cof3_pci = cof3_pca$map@data@values[,1:44] %*% t(cof3_pca$model$loadings[,1:44])
#scale pci
cof3 = as.data.frame(as.matrix(fls_hps_stck[[3]][[-c(1:2,10)]]))
mu = colMeans(cof3, na.rm = TRUE)
cof3_pci_scl = scale(cof3_pci, center = -mu, scale = FALSE)

# get raster stack
cof3_pci_rst <- foreach(i = seq(ncol(cof3_pci_scl)), .packages = "raster") %dopar% {
  tst <- cof3_pci_scl[,i]
  dim(tst) <- c(201, 201)
  cof3_rst <- raster(tst,
                     xmn = 313313.5,
                     xmx = 313414,
                     ymn = 9640810,
                     ymx = 9640910.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

cof3_pci_stck <- stack(cof3_pci_rst)
writeRaster(cof3_pci_stck, 
            paste0("pca/pca_inverse/", "cof3_441_re_pci"), format = "GTiff")


## cof4
cof4_pca <- rasterPCA(fls_hps_stck[[4]][[-c(1:2,10)]], nComp = 157)
cof4_pci = cof4_pca$map@data@values[,1:25] %*% t(cof4_pca$model$loadings[,1:25])
#scale pci
cof4 = as.data.frame(as.matrix(fls_hps_stck[[4]][[-c(1:2,10)]]))
mu = colMeans(cof4, na.rm = TRUE)
cof4_pci_scl = scale(cof4_pci, center = -mu, scale = FALSE)

# get raster stack
cof4_pci_rst <- foreach(i = seq(ncol(cof4_pci_scl)), .packages = "raster") %dopar% {
  tst <- cof4_pci_scl[,i]
  dim(tst) <- c(201, 201)
  cof4_rst <- raster(tst,
                     xmn = 313550.5,
                     xmx = 313651,
                     ymn = 9637142,
                     ymx = 9637242.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

cof4_pci_stck <- stack(cof4_pci_rst)
writeRaster(cof4_pci_stck, 
            paste0("pca/pca_inverse/", "cof4_441_re_pci"), format = "GTiff")

## cof5
cof5_pca <- rasterPCA(fls_hps_stck[[5]][[-c(1:2,10)]], nComp = 157)
cof5_pci = cof5_pca$map@data@values[,1:44] %*% t(cof5_pca$model$loadings[,1:44])
#scale pci
cof5 = as.data.frame(as.matrix(fls_hps_stck[[5]][[-c(1:2,10)]]))
mu = colMeans(cof5, na.rm = TRUE)
cof5_pci_scl = scale(cof5_pci, center = -mu, scale = FALSE)

# get raster stack
cof5_pci_rst <- foreach(i = seq(ncol(cof5_pci_scl)), .packages = "raster") %dopar% {
  tst <- cof5_pci_scl[,i]
  dim(tst) <- c(201, 201)
  cof5_rst <- raster(tst,
                     xmn = 329997.5,
                     xmx = 330098,
                     ymn = 9637820,
                     ymx = 9637920.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

cof5_pci_stck <- stack(cof5_pci_rst)
writeRaster(cof5_pci_stck, 
            paste0("pca/pca_inverse/", "cof5_441_re_pci"), format = "GTiff")


## cof6
#cof6_pca <- rasterPCA(fls_hps_stck[[6]][[-c(1:2,10)]], nComp = 157)
#cof6_pci = cof6_pca$map@data@values[,1:44] %*% t(cof6_pca$model$loadings[,1:44])
#scale pci
#cof6 = as.data.frame(as.matrix(fls_hps_stck[[6]][[-c(1:2,10)]]))
#mu = colMeans(cof6, na.rm = TRUE)
#cof6_pci_scl = scale(cof6_pci, center = -mu, scale = FALSE)

# get raster stack
#cof6_pci_rst <- foreach(i = seq(ncol(cof6_pci_scl)), .packages = "raster") %dopar% {
#  tst <- cof6_pci_scl[,i]
#  dim(tst) <- c(201, 201)
#  cof6_rst <- raster(tst,
#                     xmn = 315639.5,
#                     xmx = 315740,
#                     ymn = 9637820,
#                     ymx = 9642618.5,
#                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#}

#cof6_pci_stck <- stack(cof6_pci_rst)
#writeRaster(cof6_pci_stck, 
#            paste0("pca/pca_inverse/", "cof6_441_re_pci"), format = "GTiff")


## flm1
flm1_pca <- rasterPCA(fls_hps_stck[[7]][[-c(1:2,10)]], nComp = 157)
flm1_pci = flm1_pca$map@data@values[,1:53] %*% t(flm1_pca$model$loadings[,1:53])
#scale pci
flm1 = as.data.frame(as.matrix(fls_hps_stck[[7]][[-c(1:2,10)]]))
mu = colMeans(flm1, na.rm = TRUE)
flm1_pci_scl = scale(flm1_pci, center = -mu, scale = FALSE)

# get raster stack
flm1_pci_rst <- foreach(i = seq(ncol(flm1_pci_scl)), .packages = "raster") %dopar% {
  tst <- flm1_pci_scl[,i]
  dim(tst) <- c(201, 201)
  flm1_rst <- raster(tst,
                     xmn = 303959.5,
                     xmx = 304060,
                     ymn = 9649680,
                     ymx = 9649780.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

flm1_pci_stck <- stack(flm1_pci_rst)
writeRaster(flm1_pci_stck, 
            paste0("pca/pca_inverse/", "flm1_441_re_pci"), format = "GTiff")


## flm2
flm2_pca <- rasterPCA(fls_hps_stck[[8]][[-c(1:2,10)]], nComp = 157)
flm2_pci = flm2_pca$map@data@values[,1:50] %*% t(flm2_pca$model$loadings[,1:50])
#scale pci
flm2 = as.data.frame(as.matrix(fls_hps_stck[[8]][[-c(1:2,10)]]))
mu = colMeans(flm2, na.rm = TRUE)
flm2_pci_scl = scale(flm2_pci, center = -mu, scale = FALSE)

# get raster stack
flm2_pci_rst <- foreach(i = seq(ncol(flm2_pci_scl)), .packages = "raster") %dopar% {
  tst <- flm2_pci_scl[,i]
  dim(tst) <- c(201, 201)
  flm2_rst <- raster(tst,
                     xmn = 315871.5,
                     xmx = 315972,
                     ymn = 9644803,
                     ymx = 9644903.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

flm2_pci_stck <- stack(flm2_pci_rst)
writeRaster(flm2_pci_stck, 
            paste0("pca/pca_inverse/", "flm2_441_re_pci"), format = "GTiff")

## flm3
flm3_pca <- rasterPCA(fls_hps_stck[[9]][[-c(1:2,10)]], nComp = 157)
flm3_pci = flm3_pca$map@data@values[,1:53] %*% t(flm3_pca$model$loadings[,1:53])
#scale pci
flm3 = as.data.frame(as.matrix(fls_hps_stck[[9]][[-c(1:2,10)]]))
mu = colMeans(flm3, na.rm = TRUE)
flm3_pci_scl = scale(flm3_pci, center = -mu, scale = FALSE)

# get raster stack
flm3_pci_rst <- foreach(i = seq(ncol(flm3_pci_scl)), .packages = "raster") %dopar% {
  tst <- flm3_pci_scl[,i]
  dim(tst) <- c(201, 201)
  flm3_rst <- raster(tst,
                     xmn = 324149.5,
                     xmx = 324250,
                     ymn = 9639450,
                     ymx = 9639550.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

flm3_pci_stck <- stack(flm3_pci_rst)
writeRaster(flm3_pci_stck, 
            paste0("pca/pca_inverse/", "flm3_441_re_pci"), format = "GTiff")

## flm4
flm4_pca <- rasterPCA(fls_hps_stck[[10]][[-c(1:2,10)]], nComp = 157)
flm4_pci = flm4_pca$map@data@values[,1:72] %*% t(flm4_pca$model$loadings[,1:72])
#scale pci
flm4 = as.data.frame(as.matrix(fls_hps_stck[[10]][[-c(1:2,10)]]))
mu = colMeans(flm4, na.rm = TRUE)
flm4_pci_scl = scale(flm4_pci, center = -mu, scale = FALSE)

# get raster stack
flm4_pci_rst <- foreach(i = seq(ncol(flm4_pci_scl)), .packages = "raster") %dopar% {
  tst <- flm4_pci_scl[,i]
  dim(tst) <- c(201, 201)
  flm4_rst <- raster(tst,
                     xmn = 324425.5,
                     xmx = 324526,
                     ymn = 9639741,
                     ymx = 9639841.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

flm4_pci_stck <- stack(flm4_pci_rst)
writeRaster(flm4_pci_stck, 
            paste0("pca/pca_inverse/", "flm4_441_re_pci"), format = "GTiff")

## flm6
flm6_pca <- rasterPCA(fls_hps_stck[[11]][[-c(1:2,10)]], nComp = 157)
flm6_pci = flm6_pca$map@data@values[,1:78] %*% t(flm6_pca$model$loadings[,1:78])
#scale pci
flm6 = as.data.frame(as.matrix(fls_hps_stck[[11]][[-c(1:2,10)]]))
mu = colMeans(flm6, na.rm = TRUE)
flm6_pci_scl = scale(flm6_pci, center = -mu, scale = FALSE)

# get raster stack
flm6_pci_rst <- foreach(i = seq(ncol(flm6_pci_scl)), .packages = "raster") %dopar% {
  tst <- flm6_pci_scl[,i]
  dim(tst) <- c(201, 201)
  flm6_rst <- raster(tst,
                     xmn = 335475.5,
                     xmx = 335576,
                     ymn = 9642999.5,
                     ymx = 9643100,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

flm6_pci_stck <- stack(flm6_pci_rst)
writeRaster(flm6_pci_stck, 
            paste0("pca/pca_inverse/", "flm6_441_re_pci"), format = "GTiff")

## foc1
foc1_pca <- rasterPCA(fls_hps_stck[[12]][[-c(1:2,10)]], nComp = 157)
foc1_pci = foc1_pca$map@data@values[,1:71] %*% t(foc1_pca$model$loadings[,1:71])
#scale pci
foc1 = as.data.frame(as.matrix(fls_hps_stck[[12]][[-c(1:2,10)]]))
mu = colMeans(foc1, na.rm = TRUE)
foc1_pci_scl = scale(foc1_pci, center = -mu, scale = FALSE)

# get raster stack
foc1_pci_rst <- foreach(i = seq(ncol(foc1_pci_scl)), .packages = "raster") %dopar% {
  tst <- foc1_pci_scl[,i]
  dim(tst) <- c(201, 201)
  foc1_rst <- raster(tst,
                     xmn = 304718,
                     xmx = 304818.5,
                     ymn = 9652187,
                     ymx = 9652287.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

foc1_pci_stck <- stack(foc1_pci_rst)
writeRaster(foc1_pci_stck, 
            paste0("pca/pca_inverse/", "foc1_441_re_pci"), format = "GTiff")

## foc6
foc6_pca <- rasterPCA(fls_hps_stck[[13]][[-c(1:2,10)]], nComp = 157)
foc6_pci = foc6_pca$map@data@values[,1:13] %*% t(foc6_pca$model$loadings[,1:13])
#scale pci
foc6 = as.data.frame(as.matrix(fls_hps_stck[[13]][[-c(1:2,10)]]))
mu = colMeans(foc6, na.rm = TRUE)
foc6_pci_scl = scale(foc6_pci, center = -mu, scale = FALSE)

# get raster stack
foc6_pci_rst <- foreach(i = seq(ncol(foc6_pci_scl)), .packages = "raster") %dopar% {
  tst <- foc6_pci_scl[,i]
  dim(tst) <- c(201, 201)
  foc6_rst <- raster(tst,
                     xmn = 304911,
                     xmx = 305011.5,
                     ymn = 9652203,
                     ymx = 9652303.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

foc6_pci_stck <- stack(foc6_pci_rst)
writeRaster(foc6_pci_stck, 
            paste0("pca/pca_inverse/", "foc6_441_re_pci"), format = "GTiff")

## fod5
fod5_pca <- rasterPCA(fls_hps_stck[[14]][[-c(1:2,10)]], nComp = 157)
fod5_pci = fod5_pca$map@data@values[,1:49] %*% t(fod5_pca$model$loadings[,1:49])
#scale pci
fod5 = as.data.frame(as.matrix(fls_hps_stck[[14]][[-c(1:2,10)]]))
mu = colMeans(fod5, na.rm = TRUE)
fod5_pci_scl = scale(fod5_pci, center = -mu, scale = FALSE)

# get raster stack
fod5_pci_rst <- foreach(i = seq(ncol(fod5_pci_scl)), .packages = "raster") %dopar% {
  tst <- fod5_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fod5_rst <- raster(tst,
                     xmn = 335040.5,
                     xmx = 335141,
                     ymn = 9645929,
                     ymx = 9646029.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fod5_pci_stck <- stack(fod5_pci_rst)
writeRaster(fod5_pci_stck, 
            paste0("pca/pca_inverse/", "fod5_441_re_pci"), format = "GTiff")

## gra1
gra1_pca <- rasterPCA(fls_hps_stck[[15]][[-c(1:2,10)]], nComp = 157)
gra1_pci = gra1_pca$map@data@values[,1:29] %*% t(gra1_pca$model$loadings[,1:29])
#scale pci
gra1 = as.data.frame(as.matrix(fls_hps_stck[[15]][[-c(1:2,10)]]))
mu = colMeans(gra1, na.rm = TRUE)
gra1_pci_scl = scale(gra1_pci, center = -mu, scale = FALSE)

# get raster stack
gra1_pci_rst <- foreach(i = seq(ncol(gra1_pci_scl)), .packages = "raster") %dopar% {
  tst <- gra1_pci_scl[,i]
  dim(tst) <- c(201, 201)
  gra1_rst <- raster(tst,
                     xmn = 305269,
                     xmx = 305369.5,
                     ymn = 9648277,
                     ymx = 9648377.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

gra1_pci_stck <- stack(gra1_pci_rst)
writeRaster(gra1_pci_stck, 
            paste0("pca/pca_inverse/", "gra1_441_re_pci"), format = "GTiff")

## gra2
gra2_pca <- rasterPCA(fls_hps_stck[[16]][[-c(1:2,10)]], nComp = 157)
gra2_pci = gra2_pca$map@data@values[,1:20] %*% t(gra2_pca$model$loadings[,1:20])
#scale pci
gra2 = as.data.frame(as.matrix(fls_hps_stck[[16]][[-c(1:2,10)]]))
mu = colMeans(gra2, na.rm = TRUE)
gra2_pci_scl = scale(gra2_pci, center = -mu, scale = FALSE)

# get raster stack
gra2_pci_rst <- foreach(i = seq(ncol(gra2_pci_scl)), .packages = "raster") %dopar% {
  tst <- gra2_pci_scl[,i]
  dim(tst) <- c(201, 201)
  gra2_rst <- raster(tst,
                     xmn = 328352.5,
                     xmx = 328453,
                     ymn = 9636901,
                     ymx = 9637001.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

gra2_pci_stck <- stack(gra2_pci_rst)
writeRaster(gra2_pci_stck, 
            paste0("pca/pca_inverse/", "gra2_441_re_pci"), format = "GTiff")

## gra3
gra3_pca <- rasterPCA(fls_hps_stck[[17]][[-c(1:2,10)]], nComp = 157)
gra3_pci = gra3_pca$map@data@values[,1:32] %*% t(gra3_pca$model$loadings[,1:32])
#scale pci
gra3 = as.data.frame(as.matrix(fls_hps_stck[[17]][[-c(1:2,10)]]))
mu = colMeans(gra3, na.rm = TRUE)
gra3_pci_scl = scale(gra3_pci, center = -mu, scale = FALSE)

# get raster stack
gra3_pci_rst <- foreach(i = seq(ncol(gra3_pci_scl)), .packages = "raster") %dopar% {
  tst <- gra3_pci_scl[,i]
  dim(tst) <- c(201, 201)
  gra3_rst <- raster(tst,
                     xmn = 333429,
                     xmx = 333529.5,
                     ymn = 9634616,
                     ymx = 9634716.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

gra3_pci_stck <- stack(gra3_pci_rst)
writeRaster(gra3_pci_stck, 
            paste0("pca/pca_inverse/", "gra3_441_re_pci"), format = "GTiff")

## gra4
gra4_pca <- rasterPCA(fls_hps_stck[[18]][[-c(1:2,10)]], nComp = 157)
gra4_pci = gra4_pca$map@data@values[,1:14] %*% t(gra4_pca$model$loadings[,1:14])
#scale pci
gra4 = as.data.frame(as.matrix(fls_hps_stck[[18]][[-c(1:2,10)]]))
mu = colMeans(gra4, na.rm = TRUE)
gra4_pci_scl = scale(gra4_pci, center = -mu, scale = FALSE)

# get raster stack
gra4_pci_rst <- foreach(i = seq(ncol(gra4_pci_scl)), .packages = "raster") %dopar% {
  tst <- gra4_pci_scl[,i]
  dim(tst) <- c(201, 201)
  gra4_rst <- raster(tst,
                     xmn = 333782.5,
                     xmx = 333883,
                     ymn = 9631496,
                     ymx = 9631596.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

gra4_pci_stck <- stack(gra4_pci_rst)
writeRaster(gra4_pci_stck, 
            paste0("pca/pca_inverse/", "gra4_441_re_pci"), format = "GTiff")

## gra5
gra5_pca <- rasterPCA(fls_hps_stck[[19]][[-c(1:2,10)]], nComp = 157)
gra5_pci = gra5_pca$map@data@values[,1:29] %*% t(gra5_pca$model$loadings[,1:29])
#scale pci
gra5 = as.data.frame(as.matrix(fls_hps_stck[[19]][[-c(1:2,10)]]))
mu = colMeans(gra5, na.rm = TRUE)
gra5_pci_scl = scale(gra5_pci, center = -mu, scale = FALSE)

# get raster stack
gra5_pci_rst <- foreach(i = seq(ncol(gra5_pci_scl)), .packages = "raster") %dopar% {
  tst <- gra5_pci_scl[,i]
  dim(tst) <- c(201, 201)
  gra5_rst <- raster(tst,
                     xmn = 346209.5,
                     xmx = 346310,
                     ymn = 9635174,
                     ymx = 9635274.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

gra5_pci_stck <- stack(gra5_pci_rst)
writeRaster(gra5_pci_stck, 
            paste0("pca/pca_inverse/", "gra5_441_re_pci"), format = "GTiff")

## gra6
gra6_pca <- rasterPCA(fls_hps_stck[[20]][[-c(1:2,10)]], nComp = 157)
gra6_pci = gra6_pca$map@data@values[,1:25] %*% t(gra6_pca$model$loadings[,1:25])
#scale pci
gra6 = as.data.frame(as.matrix(fls_hps_stck[[20]][[-c(1:2,10)]]))
mu = colMeans(gra6, na.rm = TRUE)
gra6_pci_scl = scale(gra6_pci, center = -mu, scale = FALSE)

# get raster stack
gra6_pci_rst <- foreach(i = seq(ncol(gra6_pci_scl)), .packages = "raster") %dopar% {
  tst <- gra6_pci_scl[,i]
  dim(tst) <- c(201, 201)
  gra6_rst <- raster(tst,
                     xmn = 333412,
                     xmx = 333512.5,
                     ymn = 9631026,
                     ymx = 9631126.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

gra6_pci_stck <- stack(gra6_pci_rst)
writeRaster(gra6_pci_stck, 
            paste0("pca/pca_inverse/", "gra6_441_re_pci"), format = "GTiff")

## hom1
hom1_pca <- rasterPCA(fls_hps_stck[[21]][[-c(1:2,10)]], nComp = 157)
hom1_pci = hom1_pca$map@data@values[,1:37] %*% t(hom1_pca$model$loadings[,1:37])
#scale pci
hom1 = as.data.frame(as.matrix(fls_hps_stck[[21]][[-c(1:2,10)]]))
mu = colMeans(hom1, na.rm = TRUE)
hom1_pci_scl = scale(hom1_pci, center = -mu, scale = FALSE)

# get raster stack
hom1_pci_rst <- foreach(i = seq(ncol(hom1_pci_scl)), .packages = "raster") %dopar% {
  tst <- hom1_pci_scl[,i]
  dim(tst) <- c(201, 201)
  hom1_rst <- raster(tst,
                     xmn = 306045,
                     xmx = 306145.5,
                     ymn = 9647633,
                     ymx = 9647733.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

hom1_pci_stck <- stack(hom1_pci_rst)
writeRaster(hom1_pci_stck, 
            paste0("pca/pca_inverse/", "hom1_441_re_pci"), format = "GTiff")

## hom2
hom2_pca <- rasterPCA(fls_hps_stck[[22]][[-c(1:2,10)]], nComp = 157)
hom2_pci = hom2_pca$map@data@values[,1:31] %*% t(hom2_pca$model$loadings[,1:31])
#scale pci
hom2 = as.data.frame(as.matrix(fls_hps_stck[[22]][[-c(1:2,10)]]))
mu = colMeans(hom2, na.rm = TRUE)
hom2_pci_scl = scale(hom2_pci, center = -mu, scale = FALSE)

# get raster stack
hom2_pci_rst <- foreach(i = seq(ncol(hom2_pci_scl)), .packages = "raster") %dopar% {
  tst <- hom2_pci_scl[,i]
  dim(tst) <- c(201, 201)
  hom2_rst <- raster(tst,
                     xmn = 311946.5,
                     xmx = 312047,
                     ymn = 9638508,
                     ymx = 9638608.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

hom2_pci_stck <- stack(hom2_pci_rst)
writeRaster(hom2_pci_stck, 
            paste0("pca/pca_inverse/", "hom2_441_re_pci"), format = "GTiff")

## hom3
hom3_pca <- rasterPCA(fls_hps_stck[[23]][[-c(1:2,10)]], nComp = 157)
hom3_pci = hom3_pca$map@data@values[,1:47] %*% t(hom3_pca$model$loadings[,1:47])
#scale pci
hom3 = as.data.frame(as.matrix(fls_hps_stck[[23]][[-c(1:2,10)]]))
mu = colMeans(hom3, na.rm = TRUE)
hom3_pci_scl = scale(hom3_pci, center = -mu, scale = FALSE)

# get raster stack
hom3_pci_rst <- foreach(i = seq(ncol(hom3_pci_scl)), .packages = "raster") %dopar% {
  tst <- hom3_pci_scl[,i]
  dim(tst) <- c(201, 201)
  hom3_rst <- raster(tst,
                     xmn = 324382,
                     xmx = 324482.5,
                     ymn = 9638425,
                     ymx = 9638525.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

hom3_pci_stck <- stack(hom3_pci_rst)
writeRaster(hom3_pci_stck, 
            paste0("pca/pca_inverse/", "hom3_441_re_pci"), format = "GTiff")

## hom4
hom4_pca <- rasterPCA(fls_hps_stck[[24]][[-c(1:2,10)]], nComp = 157)
hom4_pci = hom4_pca$map@data@values[,1:42] %*% t(hom4_pca$model$loadings[,1:42])
#scale pci
hom4 = as.data.frame(as.matrix(fls_hps_stck[[24]][[-c(1:2,10)]]))
mu = colMeans(hom4, na.rm = TRUE)
hom4_pci_scl = scale(hom4_pci, center = -mu, scale = FALSE)

# get raster stack
hom4_pci_rst <- foreach(i = seq(ncol(hom4_pci_scl)), .packages = "raster") %dopar% {
  tst <- hom4_pci_scl[,i]
  dim(tst) <- c(201, 201)
  hom4_rst <- raster(tst,
                     xmn = 332526.5,
                     xmx = 332627,
                     ymn = 9630929,
                     ymx = 9631029.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

hom4_pci_stck <- stack(hom4_pci_rst)
writeRaster(hom4_pci_stck, 
            paste0("pca/pca_inverse/", "hom4_441_re_pci"), format = "GTiff")

## hom5
hom5_pca <- rasterPCA(fls_hps_stck[[25]][[-c(1:2,10)]], nComp = 157)
hom5_pci = hom5_pca$map@data@values[,1:31] %*% t(hom5_pca$model$loadings[,1:31])
#scale pci
hom5 = as.data.frame(as.matrix(fls_hps_stck[[25]][[-c(1:2,10)]]))
mu = colMeans(hom5, na.rm = TRUE)
hom5_pci_scl = scale(hom5_pci, center = -mu, scale = FALSE)

# get raster stack
hom5_pci_rst <- foreach(i = seq(ncol(hom5_pci_scl)), .packages = "raster") %dopar% {
  tst <- hom5_pci_scl[,i]
  dim(tst) <- c(201, 201)
  hom5_rst <- raster(tst,
                     xmn = 344888.5,
                     xmx = 344989,
                     ymn = 9638024,
                     ymx = 9638124.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

hom5_pci_stck <- stack(hom5_pci_rst)
writeRaster(hom5_pci_stck, 
            paste0("pca/pca_inverse/", "hom5_441_re_pci"), format = "GTiff")

## mai1
mai1_pca <- rasterPCA(fls_hps_stck[[26]][[-c(1:2,10)]], nComp = 157)
mai1_pci = mai1_pca$map@data@values[,1:27] %*% t(mai1_pca$model$loadings[,1:27])
#scale pci
mai1 = as.data.frame(as.matrix(fls_hps_stck[[26]][[-c(1:2,10)]]))
mu = colMeans(mai1, na.rm = TRUE)
mai1_pci_scl = scale(mai1_pci, center = -mu, scale = FALSE)

# get raster stack
mai1_pci_rst <- foreach(i = seq(ncol(mai1_pci_scl)), .packages = "raster") %dopar% {
  tst <- mai1_pci_scl[,i]
  dim(tst) <- c(201, 201)
  mai1_rst <- raster(tst,
                     xmn = 304865.5,
                     xmx = 304966,
                     ymn = 9633865,
                     ymx = 9633965.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

mai1_pci_stck <- stack(mai1_pci_rst)
writeRaster(mai1_pci_stck, 
            paste0("pca/pca_inverse/", "mai1_441_re_pci"), format = "GTiff")

## mai2
mai2_pca <- rasterPCA(fls_hps_stck[[27]][[-c(1:2,10)]], nComp = 157)
mai2_pci = mai2_pca$map@data@values[,1:27] %*% t(mai2_pca$model$loadings[,1:27])
#scale pci
mai2 = as.data.frame(as.matrix(fls_hps_stck[[27]][[-c(1:2,10)]]))
mu = colMeans(mai2, na.rm = TRUE)
mai2_pci_scl = scale(mai2_pci, center = -mu, scale = FALSE)

# get raster stack
mai2_pci_rst <- foreach(i = seq(ncol(mai2_pci_scl)), .packages = "raster") %dopar% {
  tst <- mai2_pci_scl[,i]
  dim(tst) <- c(201, 201)
  mai2_rst <- raster(tst,
                     xmn = 327278.5,
                     xmx = 327379,
                     ymn = 9626376,
                     ymx = 9626476.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

mai2_pci_stck <- stack(mai2_pci_rst)
writeRaster(mai2_pci_stck, 
            paste0("pca/pca_inverse/", "mai2_441_re_pci"), format = "GTiff")

## mai3
mai3_pca <- rasterPCA(fls_hps_stck[[28]][[-c(1:2,10)]], nComp = 157)
mai3_pci = mai3_pca$map@data@values[,1:22] %*% t(mai3_pca$model$loadings[,1:22])
#scale pci
mai3 = as.data.frame(as.matrix(fls_hps_stck[[28]][[-c(1:2,10)]]))
mu = colMeans(mai3, na.rm = TRUE)
mai3_pci_scl = scale(mai3_pci, center = -mu, scale = FALSE)

# get raster stack
mai3_pci_rst <- foreach(i = seq(ncol(mai3_pci_scl)), .packages = "raster") %dopar% {
  tst <- mai3_pci_scl[,i]
  dim(tst) <- c(201, 201)
  mai3_rst <- raster(tst,
                     xmn = 333765.5,
                     xmx = 333866,
                     ymn = 9625245,
                     ymx = 9625345.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

mai3_pci_stck <- stack(mai3_pci_rst)
writeRaster(mai3_pci_stck, 
            paste0("pca/pca_inverse/", "mai3_441_re_pci"), format = "GTiff")

## mai4
mai4_pca <- rasterPCA(fls_hps_stck[[29]][[-c(1:2,10)]], nComp = 157)
mai4_pci = mai4_pca$map@data@values[,1:20] %*% t(mai4_pca$model$loadings[,1:20])
#scale pci
mai4 = as.data.frame(as.matrix(fls_hps_stck[[29]][[-c(1:2,10)]]))
mu = colMeans(mai4, na.rm = TRUE)
mai4_pci_scl = scale(mai4_pci, center = -mu, scale = FALSE)

# get raster stack
mai4_pci_rst <- foreach(i = seq(ncol(mai4_pci_scl)), .packages = "raster") %dopar% {
  tst <- mai4_pci_scl[,i]
  dim(tst) <- c(201, 201)
  mai4_rst <- raster(tst,
                     xmn = 345844,
                     xmx = 345944.5,
                     ymn = 9628292,
                     ymx = 9628392.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

mai4_pci_stck <- stack(mai4_pci_rst)
writeRaster(mai4_pci_stck, 
            paste0("pca/pca_inverse/", "mai4_441_re_pci"), format = "GTiff")

## mai5
mai5_pca <- rasterPCA(fls_hps_stck[[30]][[-c(1:2,10)]], nComp = 157)
mai5_pci = mai5_pca$map@data@values[,1:26] %*% t(mai5_pca$model$loadings[,1:26])
#scale pci
mai5 = as.data.frame(as.matrix(fls_hps_stck[[30]][[-c(1:2,10)]]))
mu = colMeans(mai5, na.rm = TRUE)
mai5_pci_scl = scale(mai5_pci, center = -mu, scale = FALSE)

# get raster stack
mai5_pci_rst <- foreach(i = seq(ncol(mai5_pci_scl)), .packages = "raster") %dopar% {
  tst <- mai5_pci_scl[,i]
  dim(tst) <- c(201, 201)
  mai5_rst <- raster(tst,
                     xmn = 352719,
                     xmx = 352819.5,
                     ymn = 9632442,
                     ymx = 9632542.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

mai5_pci_stck <- stack(mai5_pci_rst)
writeRaster(mai5_pci_stck, 
            paste0("pca/pca_inverse/", "mai5_441_re_pci"), format = "GTiff")

## sav1
sav1_pca <- rasterPCA(fls_hps_stck[[31]][[-c(1:2,10)]], nComp = 157)
sav1_pci = sav1_pca$map@data@values[,1:20] %*% t(sav1_pca$model$loadings[,1:20])
#scale pci
sav1 = as.data.frame(as.matrix(fls_hps_stck[[31]][[-c(1:2,10)]]))
mu = colMeans(sav1, na.rm = TRUE)
sav1_pci_scl = scale(sav1_pci, center = -mu, scale = FALSE)

# get raster stack
sav1_pci_rst <- foreach(i = seq(ncol(sav1_pci_scl)), .packages = "raster") %dopar% {
  tst <- sav1_pci_scl[,i]
  dim(tst) <- c(201, 201)
  sav1_rst <- raster(tst,
                     xmn = 328294,
                     xmx = 328394.5,
                     ymn = 9626800,
                     ymx = 9626900.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

sav1_pci_stck <- stack(sav1_pci_rst)
writeRaster(sav1_pci_stck, 
            paste0("pca/pca_inverse/", "sav1_441_re_pci"), format = "GTiff")

## sav2
sav2_pca <- rasterPCA(fls_hps_stck[[32]][[-c(1:2,10)]], nComp = 157)
sav2_pci = sav2_pca$map@data@values[,1:19] %*% t(sav2_pca$model$loadings[,1:19])
#scale pci
sav2 = as.data.frame(as.matrix(fls_hps_stck[[32]][[-c(1:2,10)]]))
mu = colMeans(sav2, na.rm = TRUE)
sav2_pci_scl = scale(sav2_pci, center = -mu, scale = FALSE)

# get raster stack
sav2_pci_rst <- foreach(i = seq(ncol(sav2_pci_scl)), .packages = "raster") %dopar% {
  tst <- sav2_pci_scl[,i]
  dim(tst) <- c(201, 201)
  sav2_rst <- raster(tst,
                     xmn = 332905,
                     xmx = 333005.5,
                     ymn = 9626148,
                     ymx = 9626248.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

sav2_pci_stck <- stack(sav2_pci_rst)
writeRaster(sav2_pci_stck, 
            paste0("pca/pca_inverse/", "sav2_441_re_pci"), format = "GTiff")

## sav3
sav3_pca <- rasterPCA(fls_hps_stck[[33]][[-c(1:2,10)]], nComp = 157)
sav3_pci = sav3_pca$map@data@values[,1:19] %*% t(sav3_pca$model$loadings[,1:19])
#scale pci
sav3 = as.data.frame(as.matrix(fls_hps_stck[[33]][[-c(1:2,10)]]))
mu = colMeans(sav3, na.rm = TRUE)
sav3_pci_scl = scale(sav3_pci, center = -mu, scale = FALSE)

# get raster stack
sav3_pci_rst <- foreach(i = seq(ncol(sav3_pci_scl)), .packages = "raster") %dopar% {
  tst <- sav3_pci_scl[,i]
  dim(tst) <- c(201, 201)
  sav3_rst <- raster(tst,
                     xmn = 349202,
                     xmx = 349302.5,
                     ymn = 9631744,
                     ymx = 9631844.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

sav3_pci_stck <- stack(sav3_pci_rst)
writeRaster(sav3_pci_stck, 
            paste0("pca/pca_inverse/", "sav3_441_re_pci"), format = "GTiff")

## sav4
sav4_pca <- rasterPCA(fls_hps_stck[[34]][[-c(1:2,10)]], nComp = 157)
sav4_pci = sav4_pca$map@data@values[,1:22] %*% t(sav4_pca$model$loadings[,1:22])
#scale pci
sav4 = as.data.frame(as.matrix(fls_hps_stck[[34]][[-c(1:2,10)]]))
mu = colMeans(sav4, na.rm = TRUE)
sav4_pci_scl = scale(sav4_pci, center = -mu, scale = FALSE)

# get raster stack
sav4_pci_rst <- foreach(i = seq(ncol(sav4_pci_scl)), .packages = "raster") %dopar% {
  tst <- sav4_pci_scl[,i]
  dim(tst) <- c(201, 201)
  sav4_rst <- raster(tst,
                     xmn = 353619,
                     xmx = 353719.5,
                     ymn = 9633021,
                     ymx = 9633121.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

sav4_pci_stck <- stack(sav4_pci_rst)
writeRaster(sav4_pci_stck, 
            paste0("pca/pca_inverse/", "sav4_441_re_pci"), format = "GTiff")

## sav5
sav5_pca <- rasterPCA(fls_hps_stck[[35]][[-c(1:2,10)]], nComp = 157)
sav5_pci = sav5_pca$map@data@values[,1:18] %*% t(sav5_pca$model$loadings[,1:18])
#scale pci
sav5 = as.data.frame(as.matrix(fls_hps_stck[[35]][[-c(1:2,10)]]))
mu = colMeans(sav5, na.rm = TRUE)
sav5_pci_scl = scale(sav5_pci, center = -mu, scale = FALSE)

# get raster stack
sav5_pci_rst <- foreach(i = seq(ncol(sav5_pci_scl)), .packages = "raster") %dopar% {
  tst <- sav5_pci_scl[,i]
  dim(tst) <- c(201, 201)
  sav5_rst <- raster(tst,
                     xmn = 353749.5,
                     xmx = 353850,
                     ymn = 9634257,
                     ymx = 9634357.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

sav5_pci_stck <- stack(sav5_pci_rst)
writeRaster(sav5_pci_stck, 
            paste0("pca/pca_inverse/", "sav5_441_re_pci"), format = "GTiff")
