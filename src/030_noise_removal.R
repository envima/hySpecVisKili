# Compute noise removal on a per plot basis

source("C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R")

hd_files = list.files(path_hyp_aio, recursive = FALSE, full.names = TRUE)
h_meta = readRDS(paste0(path_meta, "hyp_meta.rds"))

pb = shapefile(paste0(path_plots, "BPolygon.shp"))

dir.create(paste0(path_hyp_nrm), showWarnings = FALSE)

# for(f in hd_files){
#  r = readRDS(f)
#  
#  m = mnf(as(r, "SpatialGridDataFrame"), use = "complete.obs")
#  
#  thv = 1-m$values
#  set_mean = which(thv < -0.10)
#  
#  for(i in use){
#    m$x@data[,i] = mean(m$x@data[,i], na.rm = TRUE)
#  }
#  mi = as.matrix(m$x@data) %*% solve(m$rotation)
#  tmp = r[[1]]
#  mir = stack(lapply(seq(ncol(mi)), function(i){
#    setValues(tmp, mi[, i])
#  }))
# 
#  saveRDS(mir, file = paste0(path_hyp_nrm, substr(basename(f), 1, 4), "_mnfi.rds"))
# }

log = list()
log_entry = 0

for(f in hd_files){
  r = readRDS(f)
  
  all_na = grep(ncell(r), summary(r)[6,])
  if(length(all_na) > 0){
    log_entry = log_entry + 1
    log[[log_entry]] = list(file = basename(f), all_na = all_na)
    r = r[[-all_na]]
  }
  
  pca = rasterPCA(r)
  v = pca$model$sdev**2
  
  # Continuous Significant Dimensionality 
  csd = round(sum(sapply(v, function(x){min(x,1)})), 0)
  use = seq(csd)
  pcai = t(t(as.matrix(pca$map)[, use] %*% t(pca$model$loadings)[use, ]) + pca$model$center)
  
  tmp = r[[1]]
  pcair = stack(lapply(seq(ncol(pcai)), function(i){
    setValues(tmp, pcai[, i])
  }))

  saveRDS(pcair, file = paste0(path_hyp_nrm, substr(basename(f), 1, 4), "_pcai.rds"))
}

saveRDS(log, file = paste0(path_meta, "030_noise_removal_log.rds"))
