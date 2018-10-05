# Extract plot area (50 x 50 m) from hyperspectral data ans mask all NAs

source("C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R")

hd_files = list.files(path_hyp_org, recursive = FALSE, full.names = TRUE)
hd_files = hd_files[-grep("kili_campaign", hd_files)]
pb = shapefile(paste0(path_plots, "BPolygon.shp"))

dir.create(paste0(path_hyp_aio), showWarnings = FALSE)

reproj = TRUE
for(f in hd_files){
 r = readRDS(f)
 if(reproj){
   pb = spTransform(pb, projection(r))
   reproj = FALSE
 }
 pid = substr(basename(f), 1, 4)
 aoi = pb[grep(pid, pb$PlotID),]
 r = raster::mask(crop(r, aoi), aoi)
 # r = raster::mask(r, calc(r, fun=sum)) # We will do that later in the workflow
 names(r) = paste0(pid, "_", names(r))
 saveRDS(r, file = paste0(path_hyp_aio, pid, ".rds"))
}


# Visually check data
visCheck(datapath = path_hyp_aio, polygonfile = paste0(path_plots, "BPolygon.shp"))
