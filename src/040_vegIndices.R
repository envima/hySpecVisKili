# Compute vegetation indicies

source("C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R")
if(length(showConnections()) == 0){
  cores = 3
  cl = parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
}

hd_files = list.files(path_hyp_aio, recursive = FALSE, full.names = TRUE)
h_meta = readRDS(paste0(path_meta, "hyp_meta.rds"))

dir.create(paste0(path_hyp_vegidcs), showWarnings = FALSE)

vis =  c("CARI",
         "Carter", "Carter2", "Carter3", "Carter4", "Carter5", "Carter6", 
         "CI", "CI2", "ClAInt", 
         "CRI1", "CRI2", "CRI3", "CRI4", 
         "Datt", "Datt2", "Datt4", "Datt5", "Datt6", 
         "DD", "DDn", "DWSI4", 
         "EVI", "GDVI_2", "GDVI_3", "GDVI_4", "GI", "Gitelson", "Gitelson2", 
         "GMI1", "GMI2", "GreenNDVI", "Maccioni", 
         "MCARI", "MCARI/OSAVI", "MCARI2", "MCARI2/OSAVI2", 
         "mND705", "mNDVI", "MPRI", "MSAVI", "mSR", "mSR2", "mSR705", 
         "MTCI", "MTVI", "NDVI", "NDVI2", "NDVI3", "NPCI", 
         "OSAVI", "OSAVI2", "PARS", "PRI", "PRI*CI2", "PRI_norm", "PSND", 
         "PSRI", "PSSR", "PWI", "RDVI", "REP_Li", "SAVI", "SIPI", "SPVI", 
         "SR", "SR1", "SR2", "SR3", "SR4", "SR5", "SR6", "SR7", "SR8", 
         "SRPI", "TCARI", "TCARI/OSAVI", "TCARI2", "TCARI2/OSAVI2", 
         "TGI", "TVI", "Vogelmann", "Vogelmann2", "Vogelmann4")


foreach(i = seq(length(hd_files))) %dopar% {
  plotid = substr(basename(hd_files[[i]]), 1, 4)
  m = h_meta[[2]][[h_meta[[1]]$list[grep(plotid, h_meta[[1]]$plotID)]]]
  r = speclib(readRDS(hd_files[[i]]),
              wavelength = m$wavelength,
              fwhm = m$fwhm, 
              continuousdata = "auto")
  v = vegindex(r, index = vis)          
  vr = v@spectra@spectra_ra
  names(vr) = vis
  saveRDS(vr, file = paste0(path_hyp_vegidcs, plotid, "_vegidcs.rds"))
} 

stopCluster(cl)
