# Extract hyperspectral data from database using the extent of the plots as 
# defined by polygons B

source("C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R")

# Set account
userpwd <- "thomas.nauss:cd7dLfgm" # use this account (if not loaded from file)

# Open remote sensing database
remotesensing <- RemoteSensing$new("http://137.248.191.215:8081", userpwd) # remote server

# Get rasterdb
dbs = c("kili_campaign1_hyperspectral_lvl3_2015", "kili_campaign2_hyperspectral_lvl3_2016")
for(db in dbs){
  dir.create(paste0(path_org, db), 
             showWarnings = FALSE)
  rasterdb <- remotesensing$rasterdb(db)
  bands = rasterdb$bands
  saveRDS(bands,
          file = paste0(path_org, db, "/bands_", db, ".rds"))
  
  # Get data
  rg = "kili_poi_plots"
  pois = remotesensing$poi_group(rg)
  for(n in pois$name){
    poi <- remotesensing$poi(group_name=rg, poi_name=n)
    ext <- extent_diameter(poi$x, poi$y, 100)
    r <- rasterdb$raster(ext)
    saveRDS(r, file = paste0(path_org, db, "/", n, ".rds"))
  }
}


# # Get data
# rg = "kili_roi_plot_poles_b"
# rois = remotesensing$roi_group(rg)
# for(n in rois$name){
#   roi <- remotesensing$roi(group_name=rg, roi_name=n)
#   ext <- extent(min(roi$polygon[[1]][,1]), max(roi$polygon[[1]][,1]),
#                 min(roi$polygon[[1]][,2]), max(roi$polygon[[1]][,2]))
#   r <- rasterdb$raster(ext)
#   saveRDS(r, file =  paste0(path_org, n, ".rds"))
# }


# Check data
ds = list.files(path_org, full.names = TRUE)
pb = shapefile(paste0(path_plots, "BPolygon.shp"))

temp = readRDS(ds[[1]])
pb = spTransform(pb, projection(temp))
for(d in ds){
  r = readRDS(d)
  plot(r[[109]], main = substr(basename(d), 1, 4))
  plot(pb[grep(substr(basename(d), 1, 4), pb$PlotID),], add = TRUE)
  mapview(r[[109]]) + pb[grep(substr(basename(d), 1, 4), pb$PlotID),]
}
