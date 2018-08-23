library(hsdar)
library(foreach)
library(doParallel)
library(RStoolbox)
library(dplyr)
library(XML)

# faster
cl <- makeCluster(15)
registerDoParallel(cl)

setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd/norm_441/re_norm")
filepath_raster <- "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd/norm_441/re_norm"

# list pca files
fls_hps <- list.files(paste0(filepath_raster), full.names = FALSE,
                      recursive = FALSE, pattern = "*.tif")

# stack and raster pca files
fls_hps_stck <- foreach(i = seq(fls_hps)) %do% {
  stck <- stack(fls_hps[i])
}


######
## fed1
#pca
fed1_pca <- rasterPCA(fls_hps_stck[[1]][[-c(8,159:160)]], nComp = 157)
#pci
fed1_pci = fed1_pca$map@data@values[,1:95] %*% t(fed1_pca$model$loadings[,1:95])
#scale pci
fed1 = as.data.frame(as.matrix(fls_hps_stck[[1]][[-c(8,159:160)]]))
mu = colMeans(fed1, na.rm = TRUE)
fed1_pci_scl = scale(fed1_pci, center = -mu, scale = FALSE)

# get raster stack
fed1_pci_rst <- foreach(i = seq(ncol(fed1_pci_scl)), .packages = "raster") %dopar% {
  tst <- fed1_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fed1_rst <- raster(tst,
                     xmn = 308740.5,
                     xmx = 308841,
                     ymn = 9658767,
                     ymx = 9658867.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fed1_pci_stck <- stack(fed1_pci_rst)
writeRaster(fed1_pci_stck, 
            paste0("pca/pca_inverse/", "fed1_441_re_pci"), format = "GTiff")


## fed2
#pca
fed2_pca <- rasterPCA(fls_hps_stck[[2]][[-c(8,159:160)]], nComp = 157)
#pci
fed2_pci = fed2_pca$map@data@values[,1:98] %*% t(fed2_pca$model$loadings[,1:98])
#scale pci
fed2 = as.data.frame(as.matrix(fls_hps_stck[[2]][[-c(8,159:160)]]))
mu = colMeans(fed2, na.rm = TRUE)
fed2_pci_scl = scale(fed2_pci, center = -mu, scale = FALSE)

# get raster stack
fed2_pci_rst <- foreach(i = seq(ncol(fed2_pci_scl)), .packages = "raster") %dopar% {
  tst <- fed2_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fed2_rst <- raster(tst,
                     xmn = 318843,
                     xmx = 318943.5,
                     ymn = 9653419,
                     ymx = 9653519.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fed2_pci_stck <- stack(fed2_pci_rst)
writeRaster(fed2_pci_stck, 
            paste0("pca/pca_inverse/", "fed2_441_re_pci"), format = "GTiff")


## fed3
#pca
fed3_pca <- rasterPCA(fls_hps_stck[[3]][[-c(8,159:160)]], nComp = 157)
#pci
fed3_pci = fed3_pca$map@data@values[,1:35] %*% t(fed3_pca$model$loadings[,1:35])
#scale pci
fed3 = as.data.frame(as.matrix(fls_hps_stck[[3]][[-c(8,159:160)]]))
mu = colMeans(fed3, na.rm = TRUE)
fed3_pci_scl = scale(fed3_pci, center = -mu, scale = FALSE)

# get raster stack
fed3_pci_rst <- foreach(i = seq(ncol(fed3_pci_scl)), .packages = "raster") %dopar% {
  tst <- fed3_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fed3_rst <- raster(tst,
                     xmn = 326066,
                     xmx = 326166.5,
                     ymn = 9653997,
                     ymx = 9654097.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fed3_pci_stck <- stack(fed3_pci_rst)
writeRaster(fed3_pci_stck, 
            paste0("pca/pca_inverse/", "fed3_441_re_pci"), format = "GTiff")


## fed4
#pca
fed4_pca <- rasterPCA(fls_hps_stck[[4]][[-c(8,159:160)]], nComp = 157)
#pci
fed4_pci = fed4_pca$map@data@values[,1:38] %*% t(fed4_pca$model$loadings[,1:38])
#scale pci
fed4 = as.data.frame(as.matrix(fls_hps_stck[[4]][[-c(8,159:160)]]))
mu = colMeans(fed4, na.rm = TRUE)
fed4_pci_scl = scale(fed4_pci, center = -mu, scale = FALSE)

# get raster stack
fed4_pci_rst <- foreach(i = seq(ncol(fed4_pci_scl)), .packages = "raster") %dopar% {
  tst <- fed4_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fed4_rst <- raster(tst,
                     xmn = 325834.5,
                     xmx = 325935,
                     ymn = 9652482,
                     ymx = 9652582.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fed4_pci_stck <- stack(fed4_pci_rst)
writeRaster(fed4_pci_stck, 
            paste0("pca/pca_inverse/", "fed4_441_re_pci"), format = "GTiff")


## fed5
#pca
fed5_pca <- rasterPCA(fls_hps_stck[[5]][[-c(8,159:160)]], nComp = 157)
#pci
fed5_pci = fed5_pca$map@data@values[,1:37] %*% t(fed5_pca$model$loadings[,1:37])
#scale pci
fed5 = as.data.frame(as.matrix(fls_hps_stck[[5]][[-c(8,159:160)]]))
mu = colMeans(fed5, na.rm = TRUE)
fed5_pci_scl = scale(fed5_pci, center = -mu, scale = FALSE)

# get raster stack
fed5_pci_rst <- foreach(i = seq(ncol(fed5_pci_scl)), .packages = "raster") %dopar% {
  tst <- fed5_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fed5_rst <- raster(tst,
                     xmn = 325817.5,
                     xmx = 325918,
                     ymn = 9653139,
                     ymx = 9653239.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fed5_pci_stck <- stack(fed5_pci_rst)
writeRaster(fed5_pci_stck, 
            paste0("pca/pca_inverse/", "fed5_441_re_pci"), format = "GTiff")


## fer0
#pca
fer0_pca <- rasterPCA(fls_hps_stck[[6]][[-c(8,159:160)]], nComp = 157)
#pci
fer0_pci = fer0_pca$map@data@values[,1:86] %*% t(fer0_pca$model$loadings[,1:86])
#scale pci
fer0 = as.data.frame(as.matrix(fls_hps_stck[[6]][[-c(8,159:160)]]))
mu = colMeans(fer0, na.rm = TRUE)
fer0_pci_scl = scale(fer0_pci, center = -mu, scale = FALSE)

# get raster stack
fer0_pci_rst <- foreach(i = seq(ncol(fer0_pci_scl)), .packages = "raster") %dopar% {
  tst <- fer0_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fer0_rst <- raster(tst,
                     xmn = 310210.5,
                     xmx = 310311,
                     ymn = 9659288,
                     ymx = 9659388.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fer0_pci_stck <- stack(fer0_pci_rst)
writeRaster(fer0_pci_stck, 
            paste0("pca/pca_inverse/", "fer0_441_re_pci"), format = "GTiff")

## fer2
#pca
fer2_pca <- rasterPCA(fls_hps_stck[[7]][[-c(8,159:160)]], nComp = 157)
#pci
fer2_pci = fer2_pca$map@data@values[,1:107] %*% t(fer2_pca$model$loadings[,1:107])
#scale pci
fer2 = as.data.frame(as.matrix(fls_hps_stck[[7]][[-c(8,159:160)]]))
mu = colMeans(fer2, na.rm = TRUE)
fer2_pci_scl = scale(fer2_pci, center = -mu, scale = FALSE)

# get raster stack
fer2_pci_rst <- foreach(i = seq(ncol(fer2_pci_scl)), .packages = "raster") %dopar% {
  tst <- fer2_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fer2_rst <- raster(tst,
                     xmn = 313072,
                     xmx = 313172.5,
                     ymn = 9656342,
                     ymx = 9656442.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fer2_pci_stck <- stack(fer2_pci_rst)
writeRaster(fer2_pci_stck, 
            paste0("pca/pca_inverse/", "fer2_441_re_pci"), format = "GTiff")

## fer3
#pca
fer3_pca <- rasterPCA(fls_hps_stck[[8]][[-c(8,159:160)]], nComp = 157)
#pci
fer3_pci = fer3_pca$map@data@values[,1:51] %*% t(fer3_pca$model$loadings[,1:51])
#scale pci
fer3 = as.data.frame(as.matrix(fls_hps_stck[[8]][[-c(8,159:160)]]))
mu = colMeans(fer3, na.rm = TRUE)
fer3_pci_scl = scale(fer3_pci, center = -mu, scale = FALSE)

# get raster stack
fer3_pci_rst <- foreach(i = seq(ncol(fer3_pci_scl)), .packages = "raster") %dopar% {
  tst <- fer3_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fer3_rst <- raster(tst,
                     xmn = 313487.5,
                     xmx = 313588,
                     ymn = 9656879,
                     ymx = 9656979.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fer3_pci_stck <- stack(fer3_pci_rst)
writeRaster(fer3_pci_stck, 
            paste0("pca/pca_inverse/", "fer3_441_re_pci"), format = "GTiff")

## fer4
#pca
fer4_pca <- rasterPCA(fls_hps_stck[[9]][[-c(8,159:160)]], nComp = 157)
#pci
fer4_pci = fer4_pca$map@data@values[,1:65] %*% t(fer4_pca$model$loadings[,1:65])
#scale pci
fer4 = as.data.frame(as.matrix(fls_hps_stck[[9]][[-c(8,159:160)]]))
mu = colMeans(fer4, na.rm = TRUE)
fer4_pci_scl = scale(fer4_pci, center = -mu, scale = FALSE)

# get raster stack
fer4_pci_rst <- foreach(i = seq(ncol(fer4_pci_scl)), .packages = "raster") %dopar% {
  tst <- fer4_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fer4_rst <- raster(tst,
                     xmn = 313062.5,
                     xmx = 313163,
                     ymn = 9655890,
                     ymx = 9655990.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fer4_pci_stck <- stack(fer4_pci_rst)
writeRaster(fer4_pci_stck, 
            paste0("pca/pca_inverse/", "fer4_441_re_pci"), format = "GTiff")

## foc0
#pca
foc0_pca <- rasterPCA(fls_hps_stck[[10]][[-c(8,159:160)]], nComp = 157)
#pci
foc0_pci = foc0_pca$map@data@values[,1:78] %*% t(foc0_pca$model$loadings[,1:78])
#scale pci
foc0 = as.data.frame(as.matrix(fls_hps_stck[[10]][[-c(8,159:160)]]))
mu = colMeans(foc0, na.rm = TRUE)
foc0_pci_scl = scale(foc0_pci, center = -mu, scale = FALSE)

# get raster stack
foc0_pci_rst <- foreach(i = seq(ncol(foc0_pci_scl)), .packages = "raster") %dopar% {
  tst <- foc0_pci_scl[,i]
  dim(tst) <- c(201, 201)
  foc0_rst <- raster(tst,
                     xmn = 305524,
                     xmx = 305624.5,
                     ymn = 9654764,
                     ymx = 9654864.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

foc0_pci_stck <- stack(foc0_pci_rst)
writeRaster(foc0_pci_stck, 
            paste0("pca/pca_inverse/", "foc0_441_re_pci"), format = "GTiff")

## foc1
#pca
foc1_pca <- rasterPCA(fls_hps_stck[[11]][[-c(8,159:160)]], nComp = 157)
#pci
foc1_pci = foc1_pca$map@data@values[,1:74] %*% t(foc1_pca$model$loadings[,1:74])
#scale pci
foc1 = as.data.frame(as.matrix(fls_hps_stck[[11]][[-c(8,159:160)]]))
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


## foc2
#pca
foc2_pca <- rasterPCA(fls_hps_stck[[12]][[-c(8,159:160)]], nComp = 157)
#pci
foc2_pci = foc2_pca$map@data@values[,1:73] %*% t(foc2_pca$model$loadings[,1:73])
#scale pci
foc2 = as.data.frame(as.matrix(fls_hps_stck[[12]][[-c(8,159:160)]]))
mu = colMeans(foc2, na.rm = TRUE)
foc2_pci_scl = scale(foc2_pci, center = -mu, scale = FALSE)

# get raster stack
foc2_pci_rst <- foreach(i = seq(ncol(foc2_pci_scl)), .packages = "raster") %dopar% {
  tst <- foc2_pci_scl[,i]
  dim(tst) <- c(201, 201)
  foc2_rst <- raster(tst,
                     xmn = 304976.5,
                     xmx = 305077,
                     ymn = 9653075,
                     ymx = 9653175.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

foc2_pci_stck <- stack(foc2_pci_rst)
writeRaster(foc2_pci_stck, 
            paste0("pca/pca_inverse/", "foc2_441_re_pci"), format = "GTiff")


## foc3
#pca
foc3_pca <- rasterPCA(fls_hps_stck[[13]][[-c(8,159:160)]], nComp = 157)
#pci
foc3_pci = foc3_pca$map@data@values[,1:77] %*% t(foc3_pca$model$loadings[,1:77])
#scale pci
foc3 = as.data.frame(as.matrix(fls_hps_stck[[13]][[-c(8,159:160)]]))
mu = colMeans(foc3, na.rm = TRUE)
foc3_pci_scl = scale(foc3_pci, center = -mu, scale = FALSE)

# get raster stack
foc3_pci_rst <- foreach(i = seq(ncol(foc3_pci_scl)), .packages = "raster") %dopar% {
  tst <- foc3_pci_scl[,i]
  dim(tst) <- c(201, 201)
  foc3_rst <- raster(tst,
                     xmn = 311381.5,
                     xmx = 311482,
                     ymn = 9652741,
                     ymx = 9652841.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

foc3_pci_stck <- stack(foc3_pci_rst)
writeRaster(foc3_pci_stck, 
            paste0("pca/pca_inverse/", "foc3_441_re_pci"), format = "GTiff")


## foc4
#pca
foc4_pca <- rasterPCA(fls_hps_stck[[14]][[-c(8,159:160)]], nComp = 157)
#pci
foc4_pci = foc4_pca$map@data@values[,1:74] %*% t(foc4_pca$model$loadings[,1:74])
#scale pci
foc4 = as.data.frame(as.matrix(fls_hps_stck[[14]][[-c(8,159:160)]]))
mu = colMeans(foc4, na.rm = TRUE)
foc4_pci_scl = scale(foc4_pci, center = -mu, scale = FALSE)

# get raster stack
foc4_pci_rst <- foreach(i = seq(ncol(foc4_pci_scl)), .packages = "raster") %dopar% {
  tst <- foc4_pci_scl[,i]
  dim(tst) <- c(201, 201)
  foc4_rst <- raster(tst,
                     xmn = 311618,
                     xmx = 311718.5,
                     ymn = 9652895,
                     ymx = 9652995.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

foc4_pci_stck <- stack(foc4_pci_rst)
writeRaster(foc4_pci_stck, 
            paste0("pca/pca_inverse/", "foc4_441_re_pci"), format = "GTiff")


## foc5
#pca
foc5_pca <- rasterPCA(fls_hps_stck[[15]][[-c(8,159:160)]], nComp = 157)
#pci
foc5_pci = foc5_pca$map@data@values[,1:77] %*% t(foc5_pca$model$loadings[,1:77])
#scale pci
foc5 = as.data.frame(as.matrix(fls_hps_stck[[15]][[-c(8,159:160)]]))
mu = colMeans(foc5, na.rm = TRUE)
foc5_pci_scl = scale(foc5_pci, center = -mu, scale = FALSE)

# get raster stack
foc5_pci_rst <- foreach(i = seq(ncol(foc5_pci_scl)), .packages = "raster") %dopar% {
  tst <- foc5_pci_scl[,i]
  dim(tst) <- c(201, 201)
  foc5_rst <- raster(tst,
                     xmn = 317526,
                     xmx = 317626.5,
                     ymn = 9649173,
                     ymx = 9649273.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

foc5_pci_stck <- stack(foc5_pci_rst)
writeRaster(foc5_pci_stck, 
            paste0("pca/pca_inverse/", "foc5_441_re_pci"), format = "GTiff")


## foc6
#pca
foc6_pca <- rasterPCA(fls_hps_stck[[16]][[-c(8,159:160)]], nComp = 157)
#pci
foc6_pci = foc6_pca$map@data@values[,1:70] %*% t(foc6_pca$model$loadings[,1:70])
#scale pci
foc6 = as.data.frame(as.matrix(fls_hps_stck[[16]][[-c(8,159:160)]]))
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


## fod1
#pca
fod1_pca <- rasterPCA(fls_hps_stck[[17]][[-c(8,159:160)]], nComp = 157)
#pci
fod1_pci = fod1_pca$map@data@values[,1:70] %*% t(fod1_pca$model$loadings[,1:70])
#scale pci
fod1 = as.data.frame(as.matrix(fls_hps_stck[[17]][[-c(8,159:160)]]))
mu = colMeans(fod1, na.rm = TRUE)
fod1_pci_scl = scale(fod1_pci, center = -mu, scale = FALSE)

# get raster stack
fod1_pci_rst <- foreach(i = seq(ncol(fod1_pci_scl)), .packages = "raster") %dopar% {
  tst <- fod1_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fod1_rst <- raster(tst,
                     xmn = 309917,
                     xmx = 310017.5,
                     ymn = 9651503,
                     ymx = 9651603.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fod1_pci_stck <- stack(fod1_pci_rst)
writeRaster(fod1_pci_stck, 
            paste0("pca/pca_inverse/", "fod1_441_re_pci"), format = "GTiff")


## fod2
#pca
fod2_pca <- rasterPCA(fls_hps_stck[[18]][[-c(8,159:160)]], nComp = 157)
#pci
fod2_pci = fod2_pca$map@data@values[,1:72] %*% t(fod2_pca$model$loadings[,1:72])
#scale pci
fod2 = as.data.frame(as.matrix(fls_hps_stck[[18]][[-c(8,159:160)]]))
mu = colMeans(fod2, na.rm = TRUE)
fod2_pci_scl = scale(fod2_pci, center = -mu, scale = FALSE)

# get raster stack
fod2_pci_rst <- foreach(i = seq(ncol(fod2_pci_scl)), .packages = "raster") %dopar% {
  tst <- fod2_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fod2_rst <- raster(tst,
                     xmn = 317530.5,
                     xmx = 317631,
                     ymn = 9647910,
                     ymx = 9648010.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fod2_pci_stck <- stack(fod2_pci_rst)
writeRaster(fod2_pci_stck, 
            paste0("pca/pca_inverse/", "fod2_441_re_pci"), format = "GTiff")


## fod3
#pca
fod3_pca <- rasterPCA(fls_hps_stck[[19]][[-c(8,159:160)]], nComp = 157)
#pci
fod3_pci = fod3_pca$map@data@values[,1:50] %*% t(fod3_pca$model$loadings[,1:50])
#scale pci
fod3 = as.data.frame(as.matrix(fls_hps_stck[[19]][[-c(8,159:160)]]))
mu = colMeans(fod3, na.rm = TRUE)
fod3_pci_scl = scale(fod3_pci, center = -mu, scale = FALSE)

# get raster stack
fod3_pci_rst <- foreach(i = seq(ncol(fod3_pci_scl)), .packages = "raster") %dopar% {
  tst <- fod3_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fod3_rst <- raster(tst,
                     xmn = 328248,
                     xmx = 328348.5,
                     ymn = 9643695,
                     ymx = 9643795.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fod3_pci_stck <- stack(fod3_pci_rst)
writeRaster(fod3_pci_stck, 
            paste0("pca/pca_inverse/", "fod3_441_re_pci"), format = "GTiff")


## fpd0
#pca
fpd0_pca <- rasterPCA(fls_hps_stck[[20]][[-c(8,159:160)]], nComp = 157)
#pci
fpd0_pci = fpd0_pca$map@data@values[,1:46] %*% t(fpd0_pca$model$loadings[,1:46])
#scale pci
fpd0 = as.data.frame(as.matrix(fls_hps_stck[[20]][[-c(8,159:160)]]))
mu = colMeans(fpd0, na.rm = TRUE)
fpd0_pci_scl = scale(fpd0_pci, center = -mu, scale = FALSE)

# get raster stack
fpd0_pci_rst <- foreach(i = seq(ncol(fpd0_pci_scl)), .packages = "raster") %dopar% {
  tst <- fpd0_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fpd0_rst <- raster(tst,
                     xmn = 318628,
                     xmx = 318728.5,
                     ymn = 9650136,
                     ymx = 9650236.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fpd0_pci_stck <- stack(fpd0_pci_rst)
writeRaster(fpd0_pci_stck, 
            paste0("pca/pca_inverse/", "fpd0_441_re_pci"), format = "GTiff")


## fpd2
#pca
fpd2_pca <- rasterPCA(fls_hps_stck[[21]][[-c(8,159:160)]], nComp = 157)
#pci
fpd2_pci = fpd2_pca$map@data@values[,1:45] %*% t(fpd2_pca$model$loadings[,1:45])
#scale pci
fpd2 = as.data.frame(as.matrix(fls_hps_stck[[21]][[-c(8,159:160)]]))
mu = colMeans(fpd2, na.rm = TRUE)
fpd2_pci_scl = scale(fpd2_pci, center = -mu, scale = FALSE)

# get raster stack
fpd2_pci_rst <- foreach(i = seq(ncol(fpd2_pci_scl)), .packages = "raster") %dopar% {
  tst <- fpd2_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fpd2_rst <- raster(tst,
                     xmn = 318549,
                     xmx = 318649.5,
                     ymn = 9650085,
                     ymx = 9650185.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fpd2_pci_stck <- stack(fpd2_pci_rst)
writeRaster(fpd2_pci_stck, 
            paste0("pca/pca_inverse/", "fpd2_441_re_pci"), format = "GTiff")


## fpd3
#pca
fpd3_pca <- rasterPCA(fls_hps_stck[[22]][[-c(8,159:160)]], nComp = 157)
#pci
fpd3_pci = fpd3_pca$map@data@values[,1:50] %*% t(fpd3_pca$model$loadings[,1:50])
#scale pci
fpd3 = as.data.frame(as.matrix(fls_hps_stck[[22]][[-c(8,159:160)]]))
mu = colMeans(fpd3, na.rm = TRUE)
fpd3_pci_scl = scale(fpd3_pci, center = -mu, scale = FALSE)

# get raster stack
fpd3_pci_rst <- foreach(i = seq(ncol(fpd3_pci_scl)), .packages = "raster") %dopar% {
  tst <- fpd3_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fpd3_rst <- raster(tst,
                     xmn = 326497.5,
                     xmx = 326598,
                     ymn = 9647246,
                     ymx = 9647346.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fpd3_pci_stck <- stack(fpd3_pci_rst)
writeRaster(fpd3_pci_stck, 
            paste0("pca/pca_inverse/", "fpd3_441_re_pci"), format = "GTiff")


## fpd4
#pca
fpd4_pca <- rasterPCA(fls_hps_stck[[23]][[-c(8,159:160)]], nComp = 157)
#pci
fpd4_pci = fpd4_pca$map@data@values[,1:48] %*% t(fpd4_pca$model$loadings[,1:48])
#scale pci
fpd4 = as.data.frame(as.matrix(fls_hps_stck[[23]][[-c(8,159:160)]]))
mu = colMeans(fpd4, na.rm = TRUE)
fpd4_pci_scl = scale(fpd4_pci, center = -mu, scale = FALSE)

# get raster stack
fpd4_pci_rst <- foreach(i = seq(ncol(fpd4_pci_scl)), .packages = "raster") %dopar% {
  tst <- fpd4_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fpd4_rst <- raster(tst,
                     xmn = 326642.5,
                     xmx = 326743,
                     ymn = 9647625,
                     ymx = 9647725.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fpd4_pci_stck <- stack(fpd4_pci_rst)
writeRaster(fpd4_pci_stck, 
            paste0("pca/pca_inverse/", "fpd4_441_re_pci"), format = "GTiff")


## fpd5
#pca
fpd5_pca <- rasterPCA(fls_hps_stck[[24]][[-c(8,159:160)]], nComp = 157)
#pci
fpd5_pci = fpd5_pca$map@data@values[,1:106] %*% t(fpd5_pca$model$loadings[,1:106])
#scale pci
fpd5 = as.data.frame(as.matrix(fls_hps_stck[[24]][[-c(8,159:160)]]))
mu = colMeans(fpd5, na.rm = TRUE)
fpd5_pci_scl = scale(fpd5_pci, center = -mu, scale = FALSE)

# get raster stack
fpd5_pci_rst <- foreach(i = seq(ncol(fpd5_pci_scl)), .packages = "raster") %dopar% {
  tst <- fpd5_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fpd5_rst <- raster(tst,
                     xmn = 335403.5,
                     xmx = 335504,
                     ymn = 9648806,
                     ymx = 9648906.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fpd5_pci_stck <- stack(fpd5_pci_rst)
writeRaster(fpd5_pci_stck, 
            paste0("pca/pca_inverse/", "fpd5_441_re_pci"), format = "GTiff")


## fpo0
#pca
fpo0_pca <- rasterPCA(fls_hps_stck[[25]][[-c(8,159:160)]], nComp = 157)
#pci
fpo0_pci = fpo0_pca$map@data@values[,1:62] %*% t(fpo0_pca$model$loadings[,1:62])
#scale pci
fpo0 = as.data.frame(as.matrix(fls_hps_stck[[25]][[-c(8,159:160)]]))
mu = colMeans(fpo0, na.rm = TRUE)
fpo0_pci_scl = scale(fpo0_pci, center = -mu, scale = FALSE)

# get raster stack
fpo0_pci_rst <- foreach(i = seq(ncol(fpo0_pci_scl)), .packages = "raster") %dopar% {
  tst <- fpo0_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fpo0_rst <- raster(tst,
                     xmn = 306511,
                     xmx = 306611.5,
                     ymn = 9656710,
                     ymx = 9656810.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fpo0_pci_stck <- stack(fpo0_pci_rst)
writeRaster(fpo0_pci_stck, 
            paste0("pca/pca_inverse/", "fpo0_441_re_pci"), format = "GTiff")


## fpo1
#pca
fpo1_pca <- rasterPCA(fls_hps_stck[[26]][[-c(8,159:160)]], nComp = 157)
#pci
fpo1_pci = fpo1_pca$map@data@values[,1:60] %*% t(fpo1_pca$model$loadings[,1:60])
#scale pci
fpo1 = as.data.frame(as.matrix(fls_hps_stck[[26]][[-c(8,159:160)]]))
mu = colMeans(fpo1, na.rm = TRUE)
fpo1_pci_scl = scale(fpo1_pci, center = -mu, scale = FALSE)

# get raster stack
fpo1_pci_rst <- foreach(i = seq(ncol(fpo1_pci_scl)), .packages = "raster") %dopar% {
  tst <- fpo1_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fpo1_rst <- raster(tst,
                     xmn = 306629.5,
                     xmx = 306730,
                     ymn = 9656756,
                     ymx = 9656856.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fpo1_pci_stck <- stack(fpo1_pci_rst)
writeRaster(fpo1_pci_stck, 
            paste0("pca/pca_inverse/", "fpo1_441_re_pci"), format = "GTiff")


## fpo2
#pca
fpo2_pca <- rasterPCA(fls_hps_stck[[27]][[-c(8,159:160)]], nComp = 157)
#pci
fpo2_pci = fpo2_pca$map@data@values[,1:62] %*% t(fpo2_pca$model$loadings[,1:62])
#scale pci
fpo2 = as.data.frame(as.matrix(fls_hps_stck[[27]][[-c(8,159:160)]]))
mu = colMeans(fpo2, na.rm = TRUE)
fpo2_pci_scl = scale(fpo2_pci, center = -mu, scale = FALSE)

# get raster stack
fpo2_pci_rst <- foreach(i = seq(ncol(fpo2_pci_scl)), .packages = "raster") %dopar% {
  tst <- fpo2_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fpo2_rst <- raster(tst,
                     xmn = 318005,
                     xmx = 318105.5,
                     ymn = 9649880,
                     ymx = 9649980.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fpo2_pci_stck <- stack(fpo2_pci_rst)
writeRaster(fpo2_pci_stck, 
            paste0("pca/pca_inverse/", "fpo2_441_re_pci"), format = "GTiff")


## fpo3
#pca
fpo3_pca <- rasterPCA(fls_hps_stck[[28]][[-c(8,159:160)]], nComp = 157)
#pci
fpo3_pci = fpo3_pca$map@data@values[,1:47] %*% t(fpo3_pca$model$loadings[,1:47])
#scale pci
fpo3 = as.data.frame(as.matrix(fls_hps_stck[[28]][[-c(8,159:160)]]))
mu = colMeans(fpo3, na.rm = TRUE)
fpo3_pci_scl = scale(fpo3_pci, center = -mu, scale = FALSE)

# get raster stack
fpo3_pci_rst <- foreach(i = seq(ncol(fpo3_pci_scl)), .packages = "raster") %dopar% {
  tst <- fpo3_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fpo3_rst <- raster(tst,
                     xmn = 318062,
                     xmx = 318162.5,
                     ymn = 9650350,
                     ymx = 9650450.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fpo3_pci_stck <- stack(fpo3_pci_rst)
writeRaster(fpo3_pci_stck, 
            paste0("pca/pca_inverse/", "fpo3_441_re_pci"), format = "GTiff")


## fpo4
#pca
fpo4_pca <- rasterPCA(fls_hps_stck[[29]][[-c(8,159:160)]], nComp = 157)
#pci
fpo4_pci = fpo4_pca$map@data@values[,1:50] %*% t(fpo4_pca$model$loadings[,1:50])
#scale pci
fpo4 = as.data.frame(as.matrix(fls_hps_stck[[29]][[-c(8,159:160)]]))
mu = colMeans(fpo4, na.rm = TRUE)
fpo4_pci_scl = scale(fpo4_pci, center = -mu, scale = FALSE)

# get raster stack
fpo4_pci_rst <- foreach(i = seq(ncol(fpo4_pci_scl)), .packages = "raster") %dopar% {
  tst <- fpo4_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fpo4_rst <- raster(tst,
                     xmn = 326839.5,
                     xmx = 326940,
                     ymn = 9646831,
                     ymx = 9646931.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fpo4_pci_stck <- stack(fpo4_pci_rst)
writeRaster(fpo4_pci_stck, 
            paste0("pca/pca_inverse/", "fpo4_441_re_pci"), format = "GTiff")


## fpo5
#pca
fpo5_pca <- rasterPCA(fls_hps_stck[[30]][[-c(8,159:160)]], nComp = 157)
#pci
fpo5_pci = fpo5_pca$map@data@values[,1:71] %*% t(fpo5_pca$model$loadings[,1:71])
#scale pci
fpo5 = as.data.frame(as.matrix(fls_hps_stck[[30]][[-c(8,159:160)]]))
mu = colMeans(fpo5, na.rm = TRUE)
fpo5_pci_scl = scale(fpo5_pci, center = -mu, scale = FALSE)

# get raster stack
fpo5_pci_rst <- foreach(i = seq(ncol(fpo5_pci_scl)), .packages = "raster") %dopar% {
  tst <- fpo5_pci_scl[,i]
  dim(tst) <- c(201, 201)
  fpo5_rst <- raster(tst,
                     xmn = 334684,
                     xmx = 334784.5,
                     ymn = 9648464,
                     ymx = 9648565,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

fpo5_pci_stck <- stack(fpo5_pci_rst)
writeRaster(fpo5_pci_stck, 
            paste0("pca/pca_inverse/", "fpo5_441_re_pci"), format = "GTiff")


## hel1
#pca
hel1_pca <- rasterPCA(fls_hps_stck[[31]][[-c(8,159:160)]], nComp = 157)
#pci
hel1_pci = hel1_pca$map@data@values[,1:53] %*% t(hel1_pca$model$loadings[,1:53])
#scale pci
hel1 = as.data.frame(as.matrix(fls_hps_stck[[31]][[-c(8,159:160)]]))
mu = colMeans(hel1, na.rm = TRUE)
hel1_pci_scl = scale(hel1_pci, center = -mu, scale = FALSE)

# get raster stack
hel1_pci_rst <- foreach(i = seq(ncol(hel1_pci_scl)), .packages = "raster") %dopar% {
  tst <- hel1_pci_scl[,i]
  dim(tst) <- c(201, 201)
  hel1_rst <- raster(tst,
                     xmn = 308234.5,
                     xmx = 308335,
                     ymn = 9662357,
                     ymx = 9662457.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

hel1_pci_stck <- stack(hel1_pci_rst)
writeRaster(hel1_pci_stck, 
            paste0("pca/pca_inverse/", "hel1_441_re_pci"), format = "GTiff")


## hel2
#pca
hel2_pca <- rasterPCA(fls_hps_stck[[32]][[-c(8,159:160)]], nComp = 157)
#pci
hel2_pci = hel2_pca$map@data@values[,1:29] %*% t(hel2_pca$model$loadings[,1:29])
#scale pci
hel2 = as.data.frame(as.matrix(fls_hps_stck[[32]][[-c(8,159:160)]]))
mu = colMeans(hel2, na.rm = TRUE)
hel2_pci_scl = scale(hel2_pci, center = -mu, scale = FALSE)

# get raster stack
hel2_pci_rst <- foreach(i = seq(ncol(hel2_pci_scl)), .packages = "raster") %dopar% {
  tst <- hel2_pci_scl[,i]
  dim(tst) <- c(201, 201)
  hel2_rst <- raster(tst,
                     xmn = 313746,
                     xmx = 313846.5,
                     ymn = 9658081,
                     ymx = 9658181.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

hel2_pci_stck <- stack(hel2_pci_rst)
writeRaster(hel2_pci_stck, 
            paste0("pca/pca_inverse/", "hel2_441_re_pci"), format = "GTiff")


## hel3
#pca
hel3_pca <- rasterPCA(fls_hps_stck[[33]][[-c(8,159:160)]], nComp = 157)
#pci
hel3_pci = hel3_pca$map@data@values[,1:36] %*% t(hel3_pca$model$loadings[,1:36])
#scale pci
hel3 = as.data.frame(as.matrix(fls_hps_stck[[33]][[-c(8,159:160)]]))
mu = colMeans(hel3, na.rm = TRUE)
hel3_pci_scl = scale(hel3_pci, center = -mu, scale = FALSE)

# get raster stack
hel3_pci_rst <- foreach(i = seq(ncol(hel3_pci_scl)), .packages = "raster") %dopar% {
  tst <- hel3_pci_scl[,i]
  dim(tst) <- c(201, 201)
  hel3_rst <- raster(tst,
                     xmn = 319530,
                     xmx = 319630.5,
                     ymn = 9655420,
                     ymx = 9655520.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

hel3_pci_stck <- stack(hel3_pci_rst)
writeRaster(hel3_pci_stck, 
            paste0("pca/pca_inverse/", "hel3_441_re_pci"), format = "GTiff")


## hel4
#pca
hel4_pca <- rasterPCA(fls_hps_stck[[34]][[-c(8,159:160)]], nComp = 157)
#pci
hel4_pci = hel4_pca$map@data@values[,1:36] %*% t(hel4_pca$model$loadings[,1:36])
#scale pci
hel4 = as.data.frame(as.matrix(fls_hps_stck[[34]][[-c(8,159:160)]]))
mu = colMeans(hel4, na.rm = TRUE)
hel4_pci_scl = scale(hel4_pci, center = -mu, scale = FALSE)

# get raster stack
hel4_pci_rst <- foreach(i = seq(ncol(hel4_pci_scl)), .packages = "raster") %dopar% {
  tst <- hel4_pci_scl[,i]
  dim(tst) <- c(201, 201)
  hel4_rst <- raster(tst,
                     xmn = 326769.5,
                     xmx = 326870,
                     ymn = 9656784,
                     ymx = 9656884.5,
                     crs = CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

hel4_pci_stck <- stack(hel4_pci_rst)
writeRaster(hel4_pci_stck, 
            paste0("pca/pca_inverse/", "hel4_441_re_pci"), format = "GTiff")



