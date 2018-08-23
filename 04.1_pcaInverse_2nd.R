library(hsdar)
library(foreach)
library(doParallel)
library(RStoolbox)
library(dplyr)
library(XML)

# faster
cl <- makeCluster(6)
registerDoParallel(cl)

setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd/norm_441/re_norm/pca")
filepath_pca_raster <- "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd/norm_441/re_norm/pca"
filepath_org_raster <- "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd/norm_441/re_norm"

# meta data
band_meta <- read.csv(paste0("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd/", 
                             "band_meta.csv"))
band_meta_1 <- band_meta[-c(8,159:160),]

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
## fed1
pca <- fls_pca_stck[[1]]
org <- fls_org_stck[[1]]
org <- org[[-8]]

# calc inverse pca
pca_inv_rst <- foreach(i = 1:nlayers(org)) %do% {
  lapply(1:nlayers(org[[i]]), function(j){
    org[[i]] * pca[[j]]
  })
}

# unlist and stack
pca_inv_rst_ul <- unlist(pca_inv_rst)
pca_inv_stck <- stack(pca_inv_rst_ul)
writeRaster(pca_inv_stck, 
            paste0("pca_inverse/", "fed1_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fed1_441_pca_inverse"), format = "GTiff")


## fed2
pca <- fls_pca_stck[[2]]
org <- fls_org_stck[[2]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "fed2_441_pca_inverse_unscaled"), 
            format = "GTiff", overwrite = TRUE)

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fed2_441_pca_inverse"), 
            format = "GTiff", overwrite = TRUE)


## fed3
pca <- fls_pca_stck[[3]]
org <- fls_org_stck[[3]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "fed3_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fed3_441_pca_inverse"), format = "GTiff")


## fed4
pca <- fls_pca_stck[[4]]
org <- fls_org_stck[[4]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "fed4_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fed4_441_pca_inverse"), format = "GTiff")


## fed5
pca <- fls_pca_stck[[5]]
org <- fls_org_stck[[5]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "fed5_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fed5_441_pca_inverse"), format = "GTiff")


## fer0
pca <- fls_pca_stck[[6]]
org <- fls_org_stck[[6]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "fer0_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fer0_441_pca_inverse"), format = "GTiff")

## fer2
pca <- fls_pca_stck[[7]]
org <- fls_org_stck[[7]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "fer2_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fer2_441_pca_inverse"), format = "GTiff")

## fer3
pca <- fls_pca_stck[[8]]
org <- fls_org_stck[[8]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "fer3_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fer3_441_pca_inverse"), format = "GTiff")

## fer4
pca <- fls_pca_stck[[9]]
org <- fls_org_stck[[9]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "fer4_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fer4_441_pca_inverse"), format = "GTiff")

## foc0
pca <- fls_pca_stck[[10]]
org <- fls_org_stck[[10]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "foc0_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "foc0_441_pca_inverse"), format = "GTiff")

## foc1
pca <- fls_pca_stck[[11]]
org <- fls_org_stck[[11]]
org <- org[[-8]]

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


## foc2
pca <- fls_pca_stck[[12]]
org <- fls_org_stck[[12]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "foc2_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "foc2_441_pca_inverse"), format = "GTiff")


## foc3
#pca <- fls_pca_stck[[13]]
#org <- fls_org_stck[[13]]
#org <- org[[-c(8,159:160)]]

# calc inverse pca
#pca_inv_rst <- foreach(i = 1:nlayers(org), .packages = "raster") %dopar% {
#  lapply(1:nlayers(org[[i]]), function(j){
#    org[[i]] * pca[[j]]
#  })
#}

# unlist and stack
#pca_inv_rst_ul <- unlist(pca_inv_rst)
#pca_inv_stck <- stack(pca_inv_rst_ul)
#writeRaster(pca_inv_stck, 
#            paste0("pca_inverse/", "foc3_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
#org.val <- getValues(org)
#pca <- princomp(na.omit(org.val, cor = TRUE))

#pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
#  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
#})
#writeRaster(pca_inv_stck_scl, 
#            paste0("pca_inverse/", "foc3_441_pca_inverse"), format = "GTiff")


## foc4
pca <- fls_pca_stck[[13]]
org <- fls_org_stck[[14]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "foc4_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "foc4_441_pca_inverse"), format = "GTiff")


## foc5
pca <- fls_pca_stck[[14]]
org <- fls_org_stck[[15]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "foc5_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "foc5_441_pca_inverse"), format = "GTiff")


## foc6
pca <- fls_pca_stck[[15]]
org <- fls_org_stck[[16]]
org <- org[[-8]]

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


## fod1
pca <- fls_pca_stck[[16]]
org <- fls_org_stck[[17]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "fod1_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fod1_441_pca_inverse"), format = "GTiff")


## fod2
pca <- fls_pca_stck[[17]]
org <- fls_org_stck[[18]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "fod2_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fod2_441_pca_inverse"), format = "GTiff")


## fod3
pca <- fls_pca_stck[[18]]
org <- fls_org_stck[[19]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "fod3_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fod3_441_pca_inverse"), format = "GTiff")


## fpd0
pca <- fls_pca_stck[[19]]
org <- fls_org_stck[[20]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "fpd0_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fpd0_441_pca_inverse"), format = "GTiff")


## fpd2
pca <- fls_pca_stck[[20]]
org <- fls_org_stck[[21]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "fpd2_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fpd2_441_pca_inverse"), format = "GTiff")


## fpd3
pca <- fls_pca_stck[[21]]
org <- fls_org_stck[[22]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "fpd3_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fpd3_441_pca_inverse"), format = "GTiff")


## fpd4
pca <- fls_pca_stck[[22]]
org <- fls_org_stck[[23]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "fpd4_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fpd4_441_pca_inverse"), format = "GTiff")


## fpd5
pca <- fls_pca_stck[[23]]
org <- fls_org_stck[[24]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "fpd5_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fpd5_441_pca_inverse"), format = "GTiff")


## fpo0
pca <- fls_pca_stck[[24]]
org <- fls_org_stck[[25]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "fpo0_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fpo0_441_pca_inverse"), format = "GTiff")


## fpo1
pca <- fls_pca_stck[[25]]
org <- fls_org_stck[[26]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "fpo1_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fpo1_441_pca_inverse"), format = "GTiff")


## fpo2
pca <- fls_pca_stck[[26]]
org <- fls_org_stck[[27]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "fpo2_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fpo2_441_pca_inverse"), format = "GTiff")


## fpo3
pca <- fls_pca_stck[[27]]
org <- fls_org_stck[[28]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "fpo3_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fpo3_441_pca_inverse"), format = "GTiff")


## fpo4
pca <- fls_pca_stck[[28]]
org <- fls_org_stck[[29]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "fpo4_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fpo4_441_pca_inverse"), format = "GTiff")


## fpo5
pca <- fls_pca_stck[[29]]
org <- fls_org_stck[[30]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "fpo5_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "fpo5_441_pca_inverse"), format = "GTiff")


## hel1
pca <- fls_pca_stck[[30]]
org <- fls_org_stck[[31]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "hel1_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "hel1_441_pca_inverse"), format = "GTiff")


## hel2
pca <- fls_pca_stck[[31]]
org <- fls_org_stck[[32]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "hel2_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "hel2_441_pca_inverse"), format = "GTiff")


## hel3
pca <- fls_pca_stck[[32]]
org <- fls_org_stck[[33]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "hel3_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "hel3_441_pca_inverse"), format = "GTiff")


## hel4
pca <- fls_pca_stck[[33]]
org <- fls_org_stck[[34]]
org <- org[[-8]]

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
            paste0("pca_inverse/", "hel4_441_pca_inverse_unscaled"), format = "GTiff")

# center pca inverse
org.val <- getValues(org)
pca <- princomp(na.omit(org.val, cor = TRUE))

pca_inv_stck_scl <- stack(foreach(i = 1:nlayers(pca_inv_stck), .packages = "raster") %dopar% {
  scale(pca_inv_stck[[i]], center = -(pca$center[[i]]), scale = FALSE) 
})
writeRaster(pca_inv_stck_scl, 
            paste0("pca_inverse/", "hel4_441_pca_inverse"), format = "GTiff")



