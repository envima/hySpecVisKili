# Compute spectral diversity indicies

source("C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R")
if(length(showConnections()) == 0){
  cores = 3
  cl = parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
}

hd_files = list.files(path_hyp_aio, recursive = FALSE, full.names = TRUE)
h_meta = readRDS(paste0(path_meta, "hyp_meta.rds"))

dir.create(paste0(path_hyp_dividcs), showWarnings = FALSE)

foreach(i = seq(length(hd_files))) %dopar% {
  plotid = substr(basename(hd_files[[i]]), 1, 4)
  r = readRDS(hd_files[[i]])
  # ra = aggregate(r, fact=2, fun=mean)
  Sys.time()
  raomatrix <- spectralrao(as.list(r), 
                           mode="multidimension", 
                           distance_m="euclidean", 
                           window=3, 
                           shannon=FALSE, 
                           debugging=TRUE, 
                           simplify=3)
  raor = setValues(r[[1]], raomatrix[[1]])
  names(raor) = plotid
  saveRDS(raor, file = paste0(path_hyp_dividcs, 
                            substr(basename(hd_files[[i]]), 1, 4), 
                            "_dividcs.rds"))
} 

stopCluster(cl)
