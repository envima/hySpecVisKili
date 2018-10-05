# Compute noise removal on a per plot basis.

source("C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R")

hd_files = list.files(path_hyp_aio, recursive = FALSE, full.names = TRUE)
h_meta = readRDS(paste0(path_meta, "hyp_meta.rds"))

pb = shapefile(paste0(path_plots, "BPolygon.shp"))

dir.create(paste0(path_hyp_nrm), showWarnings = FALSE)

for(f in hd_files){
 r = readRDS(f)
 
 m = mnf(as(r, "SpatialGridDataFrame"), use = "complete.obs")
 
 thv = 1-m$values
 use = which(thv > 0.1)
 spplot(m$x[160])
 
 for(i in use){
   m$x@data[,i] = 0 #mean(m$x@data[,i], na.rm = TRUE)
 }
 # focal(r, w=matrix(1/9,nrow=3,ncol=3)) 
 
 # plot(m$values)
 # mi = as.matrix(m$x@data) %*% solve(m$rotation)
 mi = as.matrix(m$x@data)[, use] %*% solve(m$rotation)[use, ]
 # roti = solve(e$rotation)
 # xi = e$x[, -1] %*% roti[-1, ]
 # xi = e$x %*% roti
 tmp = r[[1]]
 mir = stack(lapply(seq(ncol(mi)), function(i){
   setValues(tmp, mi[, i])
 }))
 plot(mir)
 plot(r)
 
 summary(mir[[108]] / r[[108]])
 
 mir[[108]][4002]
 r[[108]][4002]
 
 xr = as(m$x, "RasterStack")
 writeRaster(xr, paste0(path_temp, "m.tif"), "GTiff", overwrite = TRUE)
 writeRaster(r, paste0(path_temp, "r.tif"), "GTiff", overwrite = TRUE)
}






pca = rasterPCA(r)
v = pca$model$sdev**2
thv = v-1
use = which(thv > 2)
pcai = t(t(as.matrix(pca$map)[, use] %*% t(pca$model$loadings)[use, ]) + pca$model$center)

tmp = r[[1]]
pcair = stack(lapply(seq(ncol(pcai)), function(i){
  setValues(tmp, pcai[, i])
}))
plot(pcair[[108]])
plot(r[[108]])
