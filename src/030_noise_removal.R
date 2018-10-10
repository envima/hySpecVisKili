# Compute noise removal on a per plot basis

source("C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R")
if(length(showConnections()) == 0){
  cores = 3
  cl = parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
}

hd_files = list.files(path_hyp_aio, recursive = FALSE, full.names = TRUE)
h_meta = readRDS(paste0(path_meta, "hyp_meta.rds"))

pb = shapefile(paste0(path_plots, "BPolygon.shp"))

dir.create(paste0(path_hyp_nrm), showWarnings = FALSE)

# for(f in hd_files){
#  r = readRDS(f)
# 
#  m = mnf(as(r, "SpatialGridDataFrame"), use = "complete.obs")
# 
#  # thv = 1-m$values
#  # set_mean = which(thv < -0.10)
#  use = seq(2, length(m$values))
#  mi = as.matrix(m$x@data[, use]) %*% solve(m$rotation)[use, ]
#  
#  tmp = r[[1]]
#  mir = stack(lapply(seq(ncol(mi)), function(i){
#    setValues(tmp, mi[, i])
#  }))
# 
#  saveRDS(mir, file = paste0(path_hyp_nrm, substr(basename(f), 1, 4), "_mnfi.rds"))
# }


log = foreach (i = seq(length(hd_files)), .packages = c("raster", "RStoolbox")) %dopar% {
  f = hd_files[i]
  plotid = substr(basename(f), 1, 4)
  r = readRDS(f)
  nl = nlayers(r)
  
  all_na = grep(ncell(r), summary(r)[6,])
  if(length(all_na) > 0){
    r = r[[-all_na]]
  } else {
    all_na = -1
  }
  
  pca = rasterPCA(r)
  v = pca$model$sdev**2
  
  # Continuous Significant Dimensionality 
  csd = round(sum(sapply(v, function(x){min(x,1)})), 0)
  use = seq(csd)

  log = list(file = basename(f), all_na = all_na, csd = csd)
  
  pcai = t(t(as.matrix(pca$map)[, use] %*% t(pca$model$loadings)[use, ]) + pca$model$center)
  
  tmp = r[[1]]
  pcair = stack(lapply(seq(ncol(pcai)), function(i){
    setValues(tmp, pcai[, i])
  }))
  
  if(all_na == 1){
    pcair = stack(setValues(tmp, rep(NA, ncell(tmp))), pcair)
  } else if(all_na > 1){
    pcair = stack(pcair[[1:(all_na-1)]],
                  setValues(tmp, rep(NA, ncell(tmp))),
                  pcair[[(all_na):nlayers(pcair)]])
  }
  names(pcair) = paste0(plotid, "_pcai_", seq(nl))

  saveRDS(pcair, file = paste0(path_hyp_nrm, plotid, "_pcai.rds"))
  
  return(log)
}

saveRDS(log, file = paste0(path_meta, "030_noise_removal_log.rds"))

stopCluster(cl)


# Cross check
log = readRDS(file = paste0(path_meta, "030_noise_removal_log.rds"))

csd_stat = sapply(log, function(l){l$csd})
summary(csd_stat)

all_na_stat = sapply(log, function(l){l$all_na})
summary(all_na_stat)

  