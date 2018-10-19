# Compute texture metrics on mean distance from centroid datasets.

source("C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R")

dir.create(paste0(path_hyp_glcm), showWarnings = FALSE)

hd_files = c(list.files(path_hyp_kmdc, recursive = FALSE, full.names = TRUE))

h_meta = readRDS(paste0(path_meta, "hyp_meta.rds"))


windows = c(3, 11, 31)
n_grey = c(32)

foreach (i = seq(length(hd_files))) %do% {
  filename = basename(hd_files[i])
  productid = substr(filename, 1, nchar(filename)-4)
  
  r = readRDS(hd_files[[i]])
  
  txtr_wg = lapply(windows, function(w){
    txtr_g = lapply(n_grey, function(g){
      txtr = glcmTextures(r, kernel_size = w,
                          stats = c("entropy", "homogeneity", "second_moment"),
                          n_grey = g, parallel = FALSE)
      names(txtr[[1]]) = paste0(productid, "_", names(txtr[[1]]), "_w", sprintf("%02d", w), "_g", sprintf("%02d", g))
      return(txtr)
    })
  })
  txtr_wg = stack(unlist(txtr_wg, recursive = TRUE))

  saveRDS(txtr_wg, file = paste0(path_hyp_glcm, productid, "_glcm.rds"))
} 

# Visually check data
visCheck(datapath = path_hyp_glcm, polygonfile = paste0(path_plots, "BPolygon.shp"), band = 1)

