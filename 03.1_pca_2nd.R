library(hsdar)
library(foreach)
library(doParallel)
library(RStoolbox)
library(dplyr)
library(XML)

# faster
cl <- makeCluster(6)
registerDoParallel(cl)

setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd/norm_441/re_norm")
filepath_raster <- "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd/norm_441/re_norm"

# meta data
band_meta <- read.csv(paste0("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd/", 
                             "band_meta.csv"))
band_meta_1 <- band_meta[-c(8,159:160),]

# list files
fls_hps <- list.files(paste0(filepath_raster), full.names = FALSE,
                      recursive = FALSE, pattern = "*.tif")

# stack and raster files
fls_hps_stck <- foreach(i = seq(fls_hps)) %do% {
  stck <- stack(fls_hps[i])
}

######
## fed1
# pca
fed1_pca <- rasterPCA(fls_hps_stck[[1]][[-c(8,159:160)]], 
                      nComp = 95)
fed1_pca_explainVar95 <- fed1_pca$map
writeRaster(fed1_pca_explainVar95, 
            paste0("pca/", "fed1_441_pca_exVar95"), format = "GTiff")


## fed2
# pca
fed2_pca <- rasterPCA(fls_hps_stck[[2]][[-c(8,159:160)]], 
                      nComp = 98)
fed2_pca_explainVar95 <- fed2_pca$map
writeRaster(fed2_pca_explainVar95, 
            paste0("pca/", "fed2_441_pca_exVar95"), format = "GTiff")


## fed3
# pca
fed3_pca <- rasterPCA(fls_hps_stck[[3]][[-c(8,159:160)]], 
                      nComp = 35)
fed3_pca_explainVar95 <- fed3_pca$map
writeRaster(fed3_pca_explainVar95, 
            paste0("pca/", "fed3_441_pca_exVar95"), format = "GTiff")


## fed4
# pca
fed4_pca <- rasterPCA(fls_hps_stck[[4]][[-c(8,159:160)]], 
                      nComp = 38)
fed4_pca_explainVar95 <- fed4_pca$map
writeRaster(fed4_pca_explainVar95, 
            paste0("pca/", "fed4_441_pca_exVar95"), format = "GTiff")


## fed5
# pca
fed5_pca <- rasterPCA(fls_hps_stck[[5]][[-c(8,159:160)]], 
                      nComp = 37)
fed5_pca_explainVar95 <- fed5_pca$map
writeRaster(fed5_pca_explainVar95, 
            paste0("pca/", "fed5_441_pca_exVar95"), format = "GTiff")


## fer0
# pca
fer0_pca <- rasterPCA(fls_hps_stck[[6]][[-c(8,159:160)]], 
                      nComp = 86)
fer0_pca_explainVar95 <- fer0_pca$map
writeRaster(fer0_pca_explainVar95, 
            paste0("pca/", "fer0_441_pca_exVar95"), format = "GTiff")


## fer2
# pca
fer2_pca <- rasterPCA(fls_hps_stck[[7]][[-c(8,159:160)]], 
                      nComp = 107)
fer2_pca_explainVar95 <- fer2_pca$map
writeRaster(fer2_pca_explainVar95, 
            paste0("pca/", "fer2_441_pca_exVar95"), format = "GTiff")

## fer3
# pca
fer3_pca <- rasterPCA(fls_hps_stck[[8]][[-c(8,159:160)]], 
                      nComp = 51)
fer3_pca_explainVar95 <- fer3_pca$map
writeRaster(fer3_pca_explainVar95, 
            paste0("pca/", "fer3_441_pca_exVar95"), format = "GTiff")

## fer4
# pca
fer4_pca <- rasterPCA(fls_hps_stck[[9]][[-c(8,159:160)]], 
                      nComp = 65)
fer4_pca_explainVar95 <- fer4_pca$map
writeRaster(fer4_pca_explainVar95, 
            paste0("pca/", "fer4_441_pca_exVar95"), format = "GTiff")

## foc0
# pca
foc0_pca <- rasterPCA(fls_hps_stck[[10]][[-c(8,159:160)]], 
                      nComp = 78)
foc0_pca_explainVar95 <- foc0_pca$map
writeRaster(foc0_pca_explainVar95, 
            paste0("pca/", "foc0_441_pca_exVar95"), format = "GTiff")

## foc1
# pca
foc1_pca <- rasterPCA(fls_hps_stck[[11]][[-c(8,159:160)]], 
                      nComp = 74)
foc1_pca_explainVar95 <- foc1_pca$map
writeRaster(foc1_pca_explainVar95, 
            paste0("pca/", "foc1_441_pca_exVar95"), format = "GTiff")

## foc2
# pca
foc2_pca <- rasterPCA(fls_hps_stck[[12]][[-c(8,159:160)]], 
                      nComp = 73)
foc2_pca_explainVar95 <- foc2_pca$map
writeRaster(foc2_pca_explainVar95, 
            paste0("pca/", "foc2_441_pca_exVar95"), format = "GTiff")

## foc3
# pca
foc3_pca <- rasterPCA(fls_hps_stck[[13]][[-c(8,159:160)]], 
                      nComp = 77)
foc3_pca_explainVar95 <- foc3_pca$map
writeRaster(foc6_pca_explainVar95, 
            paste0("pca/", "foc3_441_pca_exVar95"), format = "GTiff")

## foc4
# pca
foc4_pca <- rasterPCA(fls_hps_stck[[14]][[-c(8,159:160)]], 
                      nComp = 74)
foc4_pca_explainVar95 <- foc4_pca$map
writeRaster(foc4_pca_explainVar95, 
            paste0("pca/", "foc4_441_pca_exVar95"), format = "GTiff")

## foc5
# pca
foc5_pca <- rasterPCA(fls_hps_stck[[15]][[-c(8,159:160)]], 
                      nComp = 77)
foc5_pca_explainVar95 <- foc5_pca$map
writeRaster(foc5_pca_explainVar95, 
            paste0("pca/", "foc5_441_pca_exVar95"), format = "GTiff")

## foc6
# pca
foc6_pca <- rasterPCA(fls_hps_stck[[16]][[-c(8,159:160)]], 
                      nComp = 70)
foc6_pca_explainVar95 <- foc6_pca$map
writeRaster(foc6_pca_explainVar95, 
            paste0("pca/", "foc6_441_pca_exVar95"), format = "GTiff")

## fod1
# pca
fod1_pca <- rasterPCA(fls_hps_stck[[17]][[-c(8,159:160)]], 
                      nComp = 70)
fod1_pca_explainVar95 <- fod1_pca$map
writeRaster(fod1_pca_explainVar95, 
            paste0("pca/", "fod1_441_pca_exVar95"), format = "GTiff")

## fod2
# pca
fod2_pca <- rasterPCA(fls_hps_stck[[18]][[-c(8,159:160)]], 
                      nComp = 72)
fod2_pca_explainVar95 <- fod2_pca$map
writeRaster(fod2_pca_explainVar95, 
            paste0("pca/", "fod2_441_pca_exVar95"), format = "GTiff")

## fod3
# pca
fod3_pca <- rasterPCA(fls_hps_stck[[19]][[-c(8,159:160)]], 
                      nComp = 50)
fod3_pca_explainVar95 <- fod3_pca$map
writeRaster(fod3_pca_explainVar95, 
            paste0("pca/", "fod3_441_pca_exVar95"), format = "GTiff")

## fpd0
# pca
fpd0_pca <- rasterPCA(fls_hps_stck[[20]][[-c(8,159:160)]], 
                      nComp = 46)
fpd0_pca_explainVar95 <- fpd0_pca$map
writeRaster(fpd0_pca_explainVar95, 
            paste0("pca/", "fpd0_441_pca_exVar95"), format = "GTiff")

## fpd2
# pca
fpd2_pca <- rasterPCA(fls_hps_stck[[21]][[-c(8,159:160)]], 
                      nComp = 45)
fpd2_pca_explainVar95 <- fpd2_pca$map
writeRaster(fpd2_pca_explainVar95, 
            paste0("pca/", "fpd2_441_pca_exVar95"), format = "GTiff")

## fpd3
# pca
fpd3_pca <- rasterPCA(fls_hps_stck[[22]][[-c(8,159:160)]], 
                      nComp = 50)
fpd3_pca_explainVar95 <- fpd3_pca$map
writeRaster(fpd3_pca_explainVar95, 
            paste0("pca/", "fpd3_441_pca_exVar95"), format = "GTiff")

## fpd4
# pca
fpd4_pca <- rasterPCA(fls_hps_stck[[23]][[-c(8,159:160)]], 
                      nComp = 48)
fpd4_pca_explainVar95 <- fpd4_pca$map
writeRaster(fpd4_pca_explainVar95, 
            paste0("pca/", "fpd4_441_pca_exVar95"), format = "GTiff")

## fpd5
# pca
fpd5_pca <- rasterPCA(fls_hps_stck[[24]][[-c(8,159:160)]], 
                      nComp = 106)
fpd5_pca_explainVar95 <- fpd5_pca$map
writeRaster(fpd5_pca_explainVar95, 
            paste0("pca/", "fpd5_441_pca_exVar95"), format = "GTiff")

## fpo0
# pca
fpo0_pca <- rasterPCA(fls_hps_stck[[25]][[-c(8,159:160)]], 
                      nComp = 62)
fpo0_pca_explainVar95 <- fpo0_pca$map
writeRaster(fpo0_pca_explainVar95, 
            paste0("pca/", "fpo0_441_pca_exVar95"), format = "GTiff")

## fpo1
# pca
fpo1_pca <- rasterPCA(fls_hps_stck[[26]][[-c(8,159:160)]], 
                      nComp = 60)
fpo1_pca_explainVar95 <- fpo1_pca$map
writeRaster(fpo1_pca_explainVar95, 
            paste0("pca/", "fpo1_441_pca_exVar95"), format = "GTiff")

## fpo2
# pca
fpo2_pca <- rasterPCA(fls_hps_stck[[27]][[-c(8,159:160)]], 
                      nComp = 62)
fpo2_pca_explainVar95 <- fpo2_pca$map
writeRaster(fpo2_pca_explainVar95, 
            paste0("pca/", "fpo2_441_pca_exVar95"), format = "GTiff")

## fpo3
# pca
fpo3_pca <- rasterPCA(fls_hps_stck[[28]][[-c(8,159:160)]], 
                      nComp = 47)
fpo3_pca_explainVar95 <- fpo3_pca$map
writeRaster(fpo3_pca_explainVar95, 
            paste0("pca/", "fpo3_441_pca_exVar95"), format = "GTiff")

## fpo4
# pca
fpo4_pca <- rasterPCA(fls_hps_stck[[29]][[-c(8,159:160)]], 
                      nComp = 50)
fpo4_pca_explainVar95 <- fpo4_pca$map
writeRaster(fpo4_pca_explainVar95, 
            paste0("pca/", "fpo4_441_pca_exVar95"), format = "GTiff")

## fpo5
# pca
fpo5_pca <- rasterPCA(fls_hps_stck[[30]][[-c(8,159:160)]], 
                      nComp = 71)
fpo5_pca_explainVar95 <- fpo5_pca$map
writeRaster(fpo5_pca_explainVar95, 
            paste0("pca/", "fpo5_441_pca_exVar95"), format = "GTiff")

## hel1
# pca
hel1_pca <- rasterPCA(fls_hps_stck[[31]][[-c(8,159:160)]], 
                      nComp = 53)
hel1_pca_explainVar95 <- hel1_pca$map
writeRaster(hel1_pca_explainVar95, 
            paste0("pca/", "hel1_441_pca_exVar95"), format = "GTiff")

## hel2
# pca
hel2_pca <- rasterPCA(fls_hps_stck[[32]][[-c(8,159:160)]], 
                      nComp = 29)
hel2_pca_explainVar95 <- hel2_pca$map
writeRaster(hel2_pca_explainVar95, 
            paste0("pca/", "hel2_441_pca_exVar95"), format = "GTiff")

## hel3
# pca
hel3_pca <- rasterPCA(fls_hps_stck[[33]][[-c(8,159:160)]], 
                      nComp = 36)
hel3_pca_explainVar95 <- hel3_pca$map
writeRaster(hel3_pca_explainVar95, 
            paste0("pca/", "hel3_441_pca_exVar95"), format = "GTiff")

## hel4
# pca
hel4_pca <- rasterPCA(fls_hps_stck[[34]][[-c(8,159:160)]], 
                      nComp = 36)
hel4_pca_explainVar95 <- hel4_pca$map
writeRaster(hel4_pca_explainVar95, 
            paste0("pca/", "hel4_441_pca_exVar95"), format = "GTiff")



