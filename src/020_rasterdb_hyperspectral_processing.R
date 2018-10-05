# Extract hyperspectral data from database using a buffer of 100 m in diameter
# arround the center of each observation plot.

source("C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R")

# Set account
userpwd <- "thomas.nauss:cd7dLfgm" # use this account (if not loaded from file)

# Open remote sensing database
remotesensing <- RemoteSensing$new("http://137.248.191.215:8081", userpwd) # remote server

# Get rasterdb
dbs = c("kili_campaign1_hyperspectral_lvl3_2015", "kili_campaign2_hyperspectral_lvl3_2016")
for(db in dbs){
  dir.create(paste0(path_hyp_org, db), 
             showWarnings = FALSE)
  rasterdb <- remotesensing$rasterdb(db)
  bands = rasterdb$bands
  saveRDS(bands,
          file = paste0(path_hyp_org, db, "/bands_", db, ".rds"))
  
  # Get data
  rg = "kili_poi_plots"
  pois = remotesensing$poi_group(rg)
  for(n in pois$name){
    radius = 100
    if(n == "fer3"){
      radius = 150
    }
    poi <- remotesensing$poi(group_name=rg, poi_name=n)
    ext <- extent_diameter(poi$x, poi$y, radius)
    r <- rasterdb$raster(ext, product="gap_filling(full_spectrum)")
    saveRDS(r, file = paste0(path_hyp_org, db, "/", n, ".rds"))
  }
}

# Combine from first and second flight based on filesize 
# (non-observed areas have considerably smaller file sizes)
# Use foc1 and foc6 from second flight.
bd_plots = readRDS(paste0(path_biodiv, "biodiv_plots.rds"))
hd_files = list.files(path_hyp_org, recursive = TRUE, full.names = TRUE)
hd_files = hd_files[nchar(basename(hd_files)) == 8]
hd_size = lapply(hd_files, function(f){
  c = if(grepl(dbs[[1]], f)){
    c = dbs[[1]]
  } else {
    c = dbs[[2]]
  }
  data.frame(f = f,
             s = file.size(f),
             c = c,
             plotID = substr(basename(f), 1, 4))
})
hd_size = do.call("rbind", hd_size)
hd_size_valid = hd_size[hd_size$s > 300000, ]
hd_size_valid$f = as.character(hd_size_valid$f)

hd_size_valid = hd_size_valid[-grep(paste0(dbs[[1]], "/foc1.rds"), hd_size_valid$f),]
hd_size_valid = hd_size_valid[-grep(paste0(dbs[[1]], "/foc6.rds"), hd_size_valid$f),]

hd_size_valid = hd_size_valid[substr(basename(hd_size_valid$f), 1, 4) %in% bd_plots,]

for(f in hd_size_valid$f){
  file.copy(f, paste0(path_hyp_org, "/", basename(f)))
}


# Save metadata
# Combine metadata
meta = list(data.frame(hd_size_valid[, c("c", "plotID")], list = 1))
meta[[1]]$list[meta[[1]]$c == dbs[[2]]] = 2
meta[[2]] = list(meta_01 = readRDS(paste0(path_hyp_org, dbs[[1]], "/bands_", dbs[[1]], ".rds")),
            meta_02 = readRDS(paste0(path_hyp_org, dbs[[2]], "/bands_", dbs[[2]], ".rds")))
dir.create(path_meta, showWarnings = FALSE)
saveRDS(meta, file = paste0(path_meta, "hyp_meta.rds"))


# Visually check data
visCheck(datapath = path_hyp_org, polygonfile = paste0(path_plots, "BPolygon.shp"))
