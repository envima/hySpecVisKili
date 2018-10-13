# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  filepath_base = "C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/"
} else {
  filepath_base = "/media/permanent/active/KI-Hyperspec/"
}

filepath_source = paste0(filepath_base, "HySpec_KiLi/src/001_functions.R")
path_data = paste0(filepath_base, "/data/")
path_biodiv = paste0(path_data, "/biodiv/")

path_hyp_org = paste0(path_data, "/020_hypspec_org/")
path_hyp_aio = paste0(path_data, "/025_hypspec_aio/")
path_hyp_nrm = paste0(path_data, "/030_hypspec_nrm/")
path_hyp_vegidcs = paste0(path_data, "/040_hypspec_vegidcs/")
path_hyp_kmdc = paste0(path_data, "/050_hypspec_kmdc/")
path_hyp_raoq = paste0(path_data, "/060_hypspec_raoq/")
path_hyp_pred = paste0(path_data, "/070_hypspec_pred/")
path_comb_gpm = paste0(path_data, "/075_comb_gpm/")
path_model_gpm = paste0(path_data, "/080_model_gpm/")

path_plots = paste0(path_data, "/plots/")
path_rdata = paste0(path_data, "/rdata/")
path_meta = paste0(path_data, "/meta/")
path_temp = paste0(path_data, "/temp/")
path_output = paste0(path_data, "/output/")
path_vis = paste0(path_data, "/vis/")


# Set libraries ----------------------------------------------------------------
library(biodivTools) # devtools::install_github("environmentalinformatics-marburg/biodivTools")
library(CAST)
library(corrplot)
library(doParallel)
library(grid)
library(gridExtra)
library(gpm)  # devtools::install_github("environmentalinformatics-marburg/gpm")
library(hsdar)
# library(lavaan)
# library(rPointDB)
library(rgeos)
library(ggplot2)
library(mapview)
# library(metTools)  # devtools::install_github("environmentalinformatics-marburg/metTools")
library(raster)
library(RStoolbox)
library(reshape2)
library(rgdal)
# library(satellite)
library(satelliteTools)  # devtools::install_github("environmentalinformatics-marburg/satelliteTools")
# library(semPlot)
library(sp)
library(spacetime)
# library(vegan)
# library(yaml)

# Other settings ---------------------------------------------------------------
source(filepath_source)

rasterOptions(tmpdir = path_temp)

saga_cmd = "C:/OSGeo4W64/apps/saga-ltr/saga_cmd.exe"
# initOTB("C:/OSGeo4W64/bin/")
initOTB("C:/OSGeo4W64/OTB-6.2.0-Win64/bin/")


