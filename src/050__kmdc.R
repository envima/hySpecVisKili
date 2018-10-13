# Compute mean distance from centroid on original band stack and 
# scaled vegetation inidces stack

source("C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R")
if(length(showConnections()) == 0){
  cores = 3
  cl = parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
}

dir.create(paste0(path_hyp_kmdc), showWarnings = FALSE)


hd_files = c(list.files(path_hyp_nrm, recursive = FALSE, full.names = TRUE), 
             list.files(path_hyp_vegidcs, recursive = FALSE, full.names = TRUE))

h_meta = readRDS(paste0(path_meta, "hyp_meta.rds"))

foreach (i = seq(length(hd_files)), .packages = c("raster")) %dopar% {
  filename = basename(hd_files[i])
  productid = paste0(substr(filename, 1, nchar(filename)-4), "_kmdc")

  r = readRDS(hd_files[[i]])
  rds = getValues(r)
  
  all_na = grep(nrow(rds), colSums(is.na(rds)))
  if(length(all_na) > 0){
    rds = rds[,-all_na]
  } else {
    all_na = -1
  }
  
  cc = which(complete.cases(rds))
  rds_cc = rds[cc, ]
  
  # Scale vegetation indicies
  if(grepl("vegidcs", filename)){
    rds_cc = scale(rds_cc, center = TRUE, scale = TRUE)
  }
  
  km = kmeans(rds_cc, center = 1)
  kmd = sqrt(rowSums(rds_cc - fitted(km))**2)
  
  rds_kmd = rds[, 1]
  rds_kmd[cc] = kmd
  rds_kmd = setValues(r[[1]], rds_kmd)
  
  names(rds_kmd) = productid
  
  saveRDS(rds_kmd, file=paste0(path_hyp_kmdc, productid, ".rds"))
}  
  
stopCluster(cl)


# Visually check data
visCheck(datapath = path_hyp_kmdc, polygonfile = paste0(path_plots, "BPolygon.shp"), band = 1)

