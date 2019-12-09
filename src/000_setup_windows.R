# Set environment for environmental information systems analysis

root_folder = path.expand("~/analysis/globalTreeWater/")
fcts_folder = file.path(root_folder, "EI-GlobalTreeWater/src/functions/")

project_folders = c("data/",
                    "data/biomass_2010_gsv/",
                    "data/ecoregions/",
                    "data/gee_landcover_rainfall/",
                    "data/graphics/",
                    "data/maped_datasets/",
                    "data/rds_data/",
                    "data/tree_water_content/",
                    "data/tmp/")

libs = c("colorspace", "gdalUtils", "ggplot2", "maptools", "mapview", "raster", "RColorBrewer", "rgdal", "sp", "sf", "tmap")

envrmt = createEnvi(root_folder = root_folder,
                    fcts_folder = fcts_folder,
                    folders = project_folders, 
                    path_prefix = "path_", libs = libs,
                    alt_env_id = "COMPUTERNAME", alt_env_value = "PCRZP",
                    alt_env_root_folder = "F:\\BEN\\edu")

# More settings
rasterOptions(tmpdir = envrmt$path_tmp)
mapviewOptions(basemaps = mapviewGetOption("basemaps")[c(3, 1:2, 4:5)])

