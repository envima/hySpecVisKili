library(hsdar)
library(foreach)
library(doParallel)
library(RStoolbox)
library(dplyr)
library(XML)

# faster
cl <- makeCluster(6)
registerDoParallel(cl)

setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/1st/norm_441/re_norm/pca")
filepath_pca_raster <- "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/1st/norm_441/re_norm/pca"
filepath_org_raster <- "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/1st/norm_441/re_norm"

# meta data
band_meta <- read.csv(paste0("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/1st/", 
                             "band_meta.csv"))
band_meta_1 <- band_meta[-c(1:2,10),]

# list pca files
fls_pca <- list.files(paste0(filepath_pca_raster), full.names = FALSE,
                      recursive = FALSE, pattern = "*.tif")

# stack and raster pca files
fls_pca_stck <- foreach(i = seq(fls_pca)) %do% {
  stck <- stack(fls_pca[i])
}

####
# original data
# list pca files
fls_org <- list.files(paste0(filepath_org_raster), full.names = FALSE,
                      recursive = FALSE, pattern = "*.tif")

# stack and raster pca files
fls_org_stck <- foreach(i = seq(fls_org)) %do% {
  stck <- stack(fls_org[i])
}

######
## cof1
pca <- fls_pca_stck[[1]]
org <- fls_org_stck[[1]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "cof1_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "cof1_441_pca_inverse"), format = "GTiff")


## cof2
pca <- fls_pca_stck[[2]]
org <- fls_org_stck[[2]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "cof2_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "cof2_441_pca_inverse"), format = "GTiff")


## cof3
pca <- fls_pca_stck[[3]]
org <- fls_org_stck[[3]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "cof3_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "cof3_441_pca_inverse"), format = "GTiff")


## cof4
pca <- fls_pca_stck[[4]]
org <- fls_org_stck[[4]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "cof4_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "cof4_441_pca_inverse"), format = "GTiff")


## cof5
pca <- fls_pca_stck[[5]]
org <- fls_org_stck[[5]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "cof5_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "cof5_441_pca_inverse"), format = "GTiff")


## flm1
pca <- fls_pca_stck[[6]]
org <- fls_org_stck[[7]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "flm1_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "flm1_441_pca_inverse"), format = "GTiff")

## flm2
pca <- fls_pca_stck[[7]]
org <- fls_org_stck[[8]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "flm2_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "flm2_441_pca_inverse"), format = "GTiff")

## flm3
pca <- fls_pca_stck[[8]]
org <- fls_org_stck[[9]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "flm3_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "flm3_441_pca_inverse"), format = "GTiff")

## flm4
pca <- fls_pca_stck[[9]]
org <- fls_org_stck[[10]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "flm4_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "flm4_441_pca_inverse"), format = "GTiff")

## flm6
pca <- fls_pca_stck[[10]]
org <- fls_org_stck[[11]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "flm6_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "flm6_441_pca_inverse"), format = "GTiff")

## foc1
pca <- fls_pca_stck[[11]]
org <- fls_org_stck[[12]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "foc1_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "foc1_441_pca_inverse"), format = "GTiff")

## foc6
pca <- fls_pca_stck[[12]]
org <- fls_org_stck[[13]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "foc6_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "foc6_441_pca_inverse"), format = "GTiff")


## fod5
# pca
pca <- fls_pca_stck[[13]]
org <- fls_org_stck[[14]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "fod5_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fod5_441_pca_inverse"), format = "GTiff")

## gra1
# pca
pca <- fls_pca_stck[[14]]
org <- fls_org_stck[[15]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "gra1_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "gra1_441_pca_inverse"), format = "GTiff")

## gra2
# pca
pca <- fls_pca_stck[[15]]
org <- fls_org_stck[[16]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "gra2_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "gra2_441_pca_inverse"), format = "GTiff")

## gra3
# pca
pca <- fls_pca_stck[[16]]
org <- fls_org_stck[[17]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "gra3_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "gra3_441_pca_inverse"), format = "GTiff")

## gra4
# pca
pca <- fls_pca_stck[[17]]
org <- fls_org_stck[[18]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "gra4_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "gra4_441_pca_inverse"), format = "GTiff")

## gra5
# pca
pca <- fls_pca_stck[[18]]
org <- fls_org_stck[[19]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "gra5_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "gra5_441_pca_inverse"), format = "GTiff")

## gra6
# pca
pca <- fls_pca_stck[[19]]
org <- fls_org_stck[[20]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "gra6_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "gra6_441_pca_inverse"), format = "GTiff")

## hom1
# pca
pca <- fls_pca_stck[[20]]
org <- fls_org_stck[[21]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "hom1_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "hom1_441_pca_inverse"), format = "GTiff")

## hom2
# pca
pca <- fls_pca_stck[[21]]
org <- fls_org_stck[[22]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "hom2_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "hom2_441_pca_inverse"), format = "GTiff")

## hom3
# pca
pca <- fls_pca_stck[[22]]
org <- fls_org_stck[[23]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "hom3_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "hom3_441_pca_inverse"), format = "GTiff")

## hom4
# pca
pca <- fls_pca_stck[[23]]
org <- fls_org_stck[[24]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "hom4_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "hom4_441_pca_inverse"), format = "GTiff")

## hom5
# pca
pca <- fls_pca_stck[[24]]
org <- fls_org_stck[[25]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "hom5_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "hom5_441_pca_inverse"), format = "GTiff")

## mai1
# pca
pca <- fls_pca_stck[[25]]
org <- fls_org_stck[[26]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "mai1_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "mai1_441_pca_inverse"), format = "GTiff")

## mai2
# pca
pca <- fls_pca_stck[[26]]
org <- fls_org_stck[[27]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "mai2_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "mai2_441_pca_inverse"), format = "GTiff")

## mai3
# pca
pca <- fls_pca_stck[[27]]
org <- fls_org_stck[[28]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "mai3_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "mai3_441_pca_inverse"), format = "GTiff")

## mai4
# pca
pca <- fls_pca_stck[[28]]
org <- fls_org_stck[[29]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "mai4_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "mai4_441_pca_inverse"), format = "GTiff")

## mai5
# pca
pca <- fls_pca_stck[[29]]
org <- fls_org_stck[[30]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "mai5_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "mai5_441_pca_inverse"), format = "GTiff")

## sav1
# pca
pca <- fls_pca_stck[[30]]
org <- fls_org_stck[[31]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "sav1_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "sav1_441_pca_inverse"), format = "GTiff")
## sav2
# pca
pca <- fls_pca_stck[[31]]
org <- fls_org_stck[[32]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "sav2_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "sav2_441_pca_inverse"), format = "GTiff")

## sav3
# pca
pca <- fls_pca_stck[[32]]
org <- fls_org_stck[[33]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "sav3_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "sav3_441_pca_inverse"), format = "GTiff")

## sav4
# pca
pca <- fls_pca_stck[[33]]
org <- fls_org_stck[[34]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "sav4_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "sav4_441_pca_inverse"), format = "GTiff")

## sav5
# pca
pca <- fls_pca_stck[[34]]
org <- fls_org_stck[[35]]
org <- org[[-c(1:2,10)]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "sav5_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "sav5_441_pca_inverse"), format = "GTiff")

