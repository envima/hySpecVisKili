# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  filepath_base <- "C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/"
} else {
  filepath_base <- "/media/permanent/active/KI-Hyperspec/"
}

path_data <- paste0(filepath_base, "/data/")
path_biodiv <- paste0(path_data, "/biodiv/")
path_org <- paste0(path_data, "/hypspec_org/")
path_plots <- paste0(path_data, "/plots/")
path_rdata <- paste0(path_data, "/rdata/")
path_temp <- paste0(path_data, "/temp/")
path_output <- paste0(path_data, "/output/")
path_vis <- paste0(path_data, "/vis/")


# Set libraries ----------------------------------------------------------------
library(biodivTools) # devtools::install_github("environmentalinformatics-marburg/biodivTools")
library(doParallel)
library(CAST)
library(grid)
library(gridExtra)
library(gpm)
library(lavaan)
library(rgeos)
library(ggplot2)
library(mapview)
library(metTools)  # devtools::install_github("environmentalinformatics-marburg/metTools")
library(raster)
library(reshape2)
library(rgdal)
library(satellite)
library(satelliteTools)  # devtools::install_github("environmentalinformatics-marburg/satelliteTools")
library(semPlot)
library(sp)
library(vegan)
library(yaml)

# Other settings ---------------------------------------------------------------
rasterOptions(tmpdir = path_temp)

saga_cmd <- "C:/OSGeo4W64/apps/saga/saga_cmd.exe "
# initOTB("C:/OSGeo4W64/bin/")
initOTB("C:/OSGeo4W64/OTB-5.8.0-win64/OTB-5.8.0-win64/bin/")


