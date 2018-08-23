library(hsdar)
library(foreach)
library(doParallel)
library(RStoolbox)
library(dplyr)
library(XML)

# faster
cl <- makeCluster(6)
registerDoParallel(cl)

setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/1st/norm_441/re_norm")
filepath_raster <- "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/1st/norm_441/re_norm"

# meta data
band_meta <- read.csv(paste0("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/1st/", 
                             "band_meta.csv"))
band_meta_1 <- band_meta[-c(1:2,10),]

# list files
fls_hps <- list.files(paste0(filepath_raster), full.names = FALSE,
                      recursive = FALSE, pattern = "*.tif")

# stack and raster files
fls_hps_stck <- foreach(i = seq(fls_hps)) %do% {
  stck <- stack(fls_hps[i])
}

######
#pca
#fls_stck_pca <- foreach(i = seq(fls_hps_stck), .packages = "RStoolbox") %dopar% {
#  rasterPCA(fls_hps_stck[[i]][[-c(1:2,10)]], nComp = 157)
#}
r <- stack(fls_hps[1]) 
r <- r[[-c(1:2, 10)]]
r.val <- getValues(r)
idx <- which(!is.na(r.val)) 

# pca
pca <- princomp(na.omit(r.val, cor = TRUE))
###
## cof1
cof1 <- fls_hps_stck[[1]][[-c(1:2,10)]]
cof1.val <- getValues(cof1)
pca.cof1 <- prcomp(na.omit(cof1.val), scale = TRUE)
#pca.cof1 <- rasterPCA(cof1, spca = TRUE)
pca.rst.cof1 <- rasterPCA(cof1, spca = TRUE, nComp = 54)
tst <- getValues(pca.rst.cof1)
pci.cof1 <- predict(cof1, pca.rst.cof1$model, index = 1:157)
str(pca.rst.cof1$map)
#pca.cof1$rotation[,1:54]

ndvi <- (pci.cof1[[106]] - pci.cof1[[73]])/(pci.cof1[[106]] + pci.cof1[[73]])
ndvi.org <- (cof1[[106]] - cof1[[73]]) / (cof1[[106]] + cof1[[73]])
cof1min <- getValues(cof1[[73]])


#cof1 <- rasterPCA(fls_hps_stck[[1]][[-c(1:2,10)]], nComp = 157)
#cof1_explainVar95 <- summary(cof1$model)
cof1_pca <- rasterPCA(fls_hps_stck[[1]][[-c(1:2,10)]], 
                                   nComp = 54)
cof1_pca_explainVar95 <- cof1_pca$map
writeRaster(cof1_pca_explainVar95, 
            paste0("pca/", "cof1_441_pca_exVar95"), format = "GTiff")

## cof2
#cof2 <- rasterPCA(fls_hps_stck[[2]][[-c(1:2,10)]], nComp = 157)
#cof2_explainVar95 <- summary(cof2$model)
cof2_pca <- rasterPCA(fls_hps_stck[[2]][[-c(1:2,10)]], 
                      nComp = 38)
cof2_pca_explainVar95 <- cof2_pca$map
writeRaster(cof2_pca_explainVar95, 
            paste0("pca/", "cof2_441_pca_exVar95"), format = "GTiff")

## cof3
#cof3 <- rasterPCA(fls_hps_stck[[3]][[-c(1:2,10)]], nComp = 157)
#cof3_explainVar95 <- summary(cof3$model)
cof3_pca <- rasterPCA(fls_hps_stck[[3]][[-c(1:2,10)]], 
                      nComp = 44)
cof3_pca_explainVar95 <- cof3_pca$map
writeRaster(cof3_pca_explainVar95, 
            paste0("pca/", "cof3_441_pca_exVar95"), format = "GTiff")

## cof4
#cof4 <- rasterPCA(fls_hps_stck[[4]][[-c(1:2,10)]], nComp = 157)
#cof4_explainVar95 <- summary(cof4$model)
cof4_pca <- rasterPCA(fls_hps_stck[[4]][[-c(1:2,10)]], 
                      nComp = 25)
cof4_pca_explainVar95 <- cof4_pca$map
writeRaster(cof4_pca_explainVar95, 
            paste0("pca/", "cof4_441_pca_exVar95"), format = "GTiff")

## cof5
#cof5 <- rasterPCA(fls_hps_stck[[5]][[-c(1:2,10)]], nComp = 157)
#cof5_explainVar95 <- summary(cof5$model)
cof5_pca <- rasterPCA(fls_hps_stck[[5]][[-c(1:2,10)]], 
                      nComp = 44)
cof5_pca_explainVar95 <- cof5_pca$map
writeRaster(cof5_pca_explainVar95, 
            paste0("pca/", "cof5_441_pca_exVar95"), format = "GTiff")

## cof6
#cof6 <- rasterPCA(fls_hps_stck[[6]][[-c(1:2,10)]], nComp = 157)
#cof6_explainVar95 <- summary(cof6$model)
cof6_pca <- rasterPCA(fls_hps_stck[[6]][[-c(1:2,10)]], 
                      nComp = 44)
cof6_pca_explainVar95 <- cof6_pca$map
writeRaster(cof6_pca_explainVar95, 
            paste0("pca/", "cof6_441_pca_exVar95"), format = "GTiff")

## flm1
#flm1 <- rasterPCA(fls_hps_stck[[7]][[-c(1:2,10)]], nComp = 157)
#flm1_explainVar95 <- summary(flm1$model)
flm1_pca <- rasterPCA(fls_hps_stck[[7]][[-c(1:2,10)]], 
                      nComp = 53)
flm1_pca_explainVar95 <- flm1_pca$map
writeRaster(flm1_pca_explainVar95, 
            paste0("pca/", "flm1_441_pca_exVar95"), format = "GTiff")

## flm2
#flm2 <- rasterPCA(fls_hps_stck[[8]][[-c(1:2,10)]], nComp = 157)
#flm2_explainVar95 <- summary(flm2$model)
flm2_pca <- rasterPCA(fls_hps_stck[[8]][[-c(1:2,10)]], 
                      nComp = 50)
flm2_pca_explainVar95 <- flm1_pca$map
writeRaster(flm1_pca_explainVar95, 
            paste0("pca/", "flm2_441_pca_exVar95"), format = "GTiff")

## flm3
#flm3 <- rasterPCA(fls_hps_stck[[9]][[-c(1:2,10)]], nComp = 157)
#flm3_explainVar95 <- summary(flm3$model)
flm3_pca <- rasterPCA(fls_hps_stck[[9]][[-c(1:2,10)]], 
                      nComp = 53)
flm3_pca_explainVar95 <- flm3_pca$map
writeRaster(flm3_pca_explainVar95, 
            paste0("pca/", "flm3_441_pca_exVar95"), format = "GTiff")

## flm4
#flm4 <- rasterPCA(fls_hps_stck[[10]][[-c(1:2,10)]], nComp = 157)
#flm4_explainVar95 <- summary(flm4$model)
flm4_pca <- rasterPCA(fls_hps_stck[[10]][[-c(1:2,10)]], 
                      nComp = 72)
flm4_pca_explainVar95 <- flm4_pca$map
writeRaster(flm4_pca_explainVar95, 
            paste0("pca/", "flm4_441_pca_exVar95"), format = "GTiff")

## flm6
#flm6 <- rasterPCA(fls_hps_stck[[11]][[-c(1:2,10)]], nComp = 157)
#flm6_explainVar95 <- summary(flm6$model)
flm6_pca <- rasterPCA(fls_hps_stck[[11]][[-c(1:2,10)]], 
                      nComp = 78)
flm6_pca_explainVar95 <- flm6_pca$map
writeRaster(flm6_pca_explainVar95, 
            paste0("pca/", "flm6_441_pca_exVar95"), format = "GTiff")

## foc1
#foc1 <- rasterPCA(fls_hps_stck[[12]][[-c(1:2,10)]], nComp = 157)
#foc1_explainVar95 <- summary(foc1$model)
foc1_pca <- rasterPCA(fls_hps_stck[[12]][[-c(1:2,10)]], 
                      nComp = 71)
foc1_pca_explainVar95 <- foc1_pca$map
writeRaster(foc1_pca_explainVar95, 
            paste0("pca/", "foc1_441_pca_exVar95"), format = "GTiff")

## foc6
#foc6 <- rasterPCA(fls_hps_stck[[13]][[-c(1:2,10)]], nComp = 157)
#foc6_explainVar95 <- summary(foc6$model)
foc6_pca <- rasterPCA(fls_hps_stck[[13]][[-c(1:2,10)]], 
                      nComp = )
foc6_pca_explainVar95 <- foc6_pca$map
writeRaster(foc6_pca_explainVar95, 
            paste0("pca/", "foc6_441_pca_exVar95"), format = "GTiff")

## fod5
#fod5 <- rasterPCA(fls_hps_stck[[14]][[-c(1:2,10)]], nComp = 157)
#fod5_explainVar95 <- summary(fod5$model)
fod5_pca <- rasterPCA(fls_hps_stck[[14]][[-c(1:2,10)]], 
                      nComp = 49)
fod5_pca_explainVar95 <- fod5_pca$map
writeRaster(fod5_pca_explainVar95, 
            paste0("pca/", "fod5_441_pca_exVar95"), format = "GTiff")

## gra1
#gra1 <- rasterPCA(fls_hps_stck[[15]][[-c(1:2,10)]], nComp = 157)
#gra1_explainVar95 <- summary(gra1$model)
gra1_pca <- rasterPCA(fls_hps_stck[[15]][[-c(1:2,10)]], 
                      nComp = 29)
gra1_pca_explainVar95 <- gra1_pca$map
writeRaster(gra1_pca_explainVar95, 
            paste0("pca/", "gra1_441_pca_exVar95"), format = "GTiff")

## gra2
#gra2 <- rasterPCA(fls_hps_stck[[16]][[-c(1:2,10)]], nComp = 157)
#gra2_explainVar95 <- summary(gra2$model)
gra2_pca <- rasterPCA(fls_hps_stck[[16]][[-c(1:2,10)]], 
                      nComp = 20)
gra2_pca_explainVar95 <- gra2_pca$map
writeRaster(gra2_pca_explainVar95, 
            paste0("pca/", "gra2_441_pca_exVar95"), format = "GTiff")

## gra3
#gra3 <- rasterPCA(fls_hps_stck[[17]][[-c(1:2,10)]], nComp = 157)
#gra3_explainVar95 <- summary(gra3$model)
gra3_pca <- rasterPCA(fls_hps_stck[[17]][[-c(1:2,10)]], 
                      nComp = 32)
gra3_pca_explainVar95 <- gra3_pca$map
writeRaster(gra3_pca_explainVar95, 
            paste0("pca/", "gra3_441_pca_exVar95"), format = "GTiff")

## gra4
#gra4 <- rasterPCA(fls_hps_stck[[18]][[-c(1:2,10)]], nComp = 157)
#gra4_explainVar95 <- summary(gra4$model)
gra4_pca <- rasterPCA(fls_hps_stck[[18]][[-c(1:2,10)]], 
                      nComp = 14)
gra4_pca_explainVar95 <- gra4_pca$map
writeRaster(gra4_pca_explainVar95, 
            paste0("pca/", "gra4_441_pca_exVar95"), format = "GTiff")

## gra5
#gra5 <- rasterPCA(fls_hps_stck[[19]][[-c(1:2,10)]], nComp = 157)
#gra5_explainVar95 <- summary(gra5$model)
gra5_pca <- rasterPCA(fls_hps_stck[[19]][[-c(1:2,10)]], 
                      nComp = 29)
gra5_pca_explainVar95 <- gra5_pca$map
writeRaster(gra5_pca_explainVar95, 
            paste0("pca/", "gra5_441_pca_exVar95"), format = "GTiff")

## gra6
#gra6 <- rasterPCA(fls_hps_stck[[20]][[-c(1:2,10)]], nComp = 157)
#gra6_explainVar95 <- summary(gra6$model)
gra6_pca <- rasterPCA(fls_hps_stck[[20]][[-c(1:2,10)]], 
                      nComp = 25)
gra6_pca_explainVar95 <- gra6_pca$map
writeRaster(gra6_pca_explainVar95, 
            paste0("pca/", "gra6_441_pca_exVar95"), format = "GTiff")

## hom1
#hom1 <- rasterPCA(fls_hps_stck[[21]][[-c(1:2,10)]], nComp = 157)
#hom1_explainVar95 <- summary(hom1$model)
hom1_pca <- rasterPCA(fls_hps_stck[[21]][[-c(1:2,10)]], 
                      nComp = 37)
hom1_pca_explainVar95 <- hom1_pca$map
writeRaster(hom1_pca_explainVar95, 
            paste0("pca/", "hom1_441_pca_exVar95"), format = "GTiff")

## hom2
#hom2 <- rasterPCA(fls_hps_stck[[22]][[-c(1:2,10)]], nComp = 157)
#hom2_explainVar95 <- summary(hom2$model)
hom2_pca <- rasterPCA(fls_hps_stck[[22]][[-c(1:2,10)]], 
                      nComp = 31)
hom2_pca_explainVar95 <- hom2_pca$map
writeRaster(hom2_pca_explainVar95, 
            paste0("pca/", "hom2_441_pca_exVar95"), format = "GTiff")

## hom3
#hom3 <- rasterPCA(fls_hps_stck[[23]][[-c(1:2,10)]], nComp = 157)
#hom3_explainVar95 <- summary(hom3$model)
hom3_pca <- rasterPCA(fls_hps_stck[[23]][[-c(1:2,10)]], 
                      nComp = 47)
hom3_pca_explainVar95 <- hom3_pca$map
writeRaster(hom3_pca_explainVar95, 
            paste0("pca/", "hom3_441_pca_exVar95"), format = "GTiff")

## hom4
#hom4 <- rasterPCA(fls_hps_stck[[24]][[-c(1:2,10)]], nComp = 157)
#hom4_explainVar95 <- summary(hom4$model)
hom4_pca <- rasterPCA(fls_hps_stck[[24]][[-c(1:2,10)]], 
                      nComp = 42)
hom4_pca_explainVar95 <- hom4_pca$map
writeRaster(hom4_pca_explainVar95, 
            paste0("pca/", "hom4_441_pca_exVar95"), format = "GTiff")

## hom5
#hom5 <- rasterPCA(fls_hps_stck[[25]][[-c(1:2,10)]], nComp = 157)
#hom5_explainVar95 <- summary(hom5$model)
hom5_pca <- rasterPCA(fls_hps_stck[[25]][[-c(1:2,10)]], 
                      nComp = 31)
hom5_pca_explainVar95 <- hom5_pca$map
writeRaster(hom5_pca_explainVar95, 
            paste0("pca/", "hom5_441_pca_exVar95"), format = "GTiff")

## mai1
#mai1 <- rasterPCA(fls_hps_stck[[26]][[-c(1:2,10)]], nComp = 157)
#mai1_explainVar95 <- summary(mai1$model)
mai1_pca <- rasterPCA(fls_hps_stck[[26]][[-c(1:2,10)]], 
                      nComp = 27)
mai1_pca_explainVar95 <- mai1_pca$map
writeRaster(mai1_pca_explainVar95, 
            paste0("pca/", "mai1_441_pca_exVar95"), format = "GTiff")

## mai2
#mai2 <- rasterPCA(fls_hps_stck[[27]][[-c(1:2,10)]], nComp = 157)
#mai2_explainVar95 <- summary(mai2$model)
mai2_pca <- rasterPCA(fls_hps_stck[[27]][[-c(1:2,10)]], 
                      nComp = 27)
mai2_pca_explainVar95 <- mai2_pca$map
writeRaster(mai2_pca_explainVar95, 
            paste0("pca/", "mai2_441_pca_exVar95"), format = "GTiff")

## mai3
#mai3 <- rasterPCA(fls_hps_stck[[28]][[-c(1:2,10)]], nComp = 157)
#mai3_explainVar95 <- summary(mai3$model)
mai3_pca <- rasterPCA(fls_hps_stck[[28]][[-c(1:2,10)]], 
                      nComp = 22)
mai3_pca_explainVar95 <- mai3_pca$map
writeRaster(mai3_pca_explainVar95, 
            paste0("pca/", "mai3_441_pca_exVar95"), format = "GTiff")

## mai4
#mai4 <- rasterPCA(fls_hps_stck[[29]][[-c(1:2,10)]], nComp = 157)
#mai4_explainVar95 <- summary(mai4$model)
mai4_pca <- rasterPCA(fls_hps_stck[[29]][[-c(1:2,10)]], 
                      nComp = 20)
mai4_pca_explainVar95 <- mai4_pca$map
writeRaster(mai4_pca_explainVar95, 
            paste0("pca/", "mai4_441_pca_exVar95"), format = "GTiff")

## mai5
#mai5 <- rasterPCA(fls_hps_stck[[30]][[-c(1:2,10)]], nComp = 157)
#mai5_explainVar95 <- summary(mai5$model)
mai5_pca <- rasterPCA(fls_hps_stck[[30]][[-c(1:2,10)]], 
                      nComp = 26)
mai5_pca_explainVar95 <- mai5_pca$map
writeRaster(mai5_pca_explainVar95, 
            paste0("pca/", "mai5_441_pca_exVar95"), format = "GTiff")

## sav1
#sav1 <- rasterPCA(fls_hps_stck[[31]][[-c(1:2,10)]], nComp = 157)
#sav1_explainVar95 <- summary(sav1$model)
sav1_pca <- rasterPCA(fls_hps_stck[[31]][[-c(1:2,10)]], 
                      nComp = 20)
sav1_pca_explainVar95 <- sav1_pca$map
writeRaster(sav1_pca_explainVar95, 
            paste0("pca/", "sav1_441_pca_exVar95"), format = "GTiff")

## sav2
#sav2 <- rasterPCA(fls_hps_stck[[32]][[-c(1:2,10)]], nComp = 157)
#sav2_explainVar95 <- summary(sav2$model)
sav2_pca <- rasterPCA(fls_hps_stck[[32]][[-c(1:2,10)]], 
                      nComp = 19)
sav2_pca_explainVar95 <- sav2_pca$map
writeRaster(sav2_pca_explainVar95, 
            paste0("pca/", "sav2_441_pca_exVar95"), format = "GTiff")

## sav3
#sav3 <- rasterPCA(fls_hps_stck[[33]][[-c(1:2,10)]], nComp = 157)
#sav3_explainVar95 <- summary(sav3$model)
sav3_pca <- rasterPCA(fls_hps_stck[[33]][[-c(1:2,10)]], 
                      nComp = 19)
sav3_pca_explainVar95 <- sav3_pca$map
writeRaster(sav3_pca_explainVar95, 
            paste0("pca/", "sav3_441_pca_exVar95"), format = "GTiff")

## sav4
#sav4 <- rasterPCA(fls_hps_stck[[34]][[-c(1:2,10)]], nComp = 157)
#sav4_explainVar95 <- summary(sav4$model)
sav4_pca <- rasterPCA(fls_hps_stck[[34]][[-c(1:2,10)]], 
                      nComp = 22)
sav4_pca_explainVar95 <- sav4_pca$map
writeRaster(sav4_pca_explainVar95, 
            paste0("pca/", "sav4_441_pca_exVar95"), format = "GTiff")

## sav5
#sav5 <- rasterPCA(fls_hps_stck[[35]][[-c(1:2,10)]], nComp = 157)
#sav5_explainVar95 <- summary(sav5$model)
sav5_pca <- rasterPCA(fls_hps_stck[[35]][[-c(1:2,10)]], 
                      nComp = 18)
sav5_pca_explainVar95 <- sav5_pca$map
writeRaster(sav5_pca_explainVar95, 
            paste0("pca/", "sav5_441_pca_exVar95"), format = "GTiff")



