# Set environment for environmental information systems analysis

root_folder = path.expand("~/analysis/global_forest_cover/")

project_folders = c("data/",
                    "data/biomass_1950_2010/",
                    "data/biomass_2010_gsv/",
                    "data/maped_datasets/",
                    "data/gee_landcover_rainfall/",
                    "data/tree_water_content/",
                    "data/tmp/", 
                    "EI-GlobalForestAnalysis/src/")

libs = c("gdalUtils", "mapview", "raster", "rgdal", "sp")

envrmt = createEnvi(root_folder = root_folder, folders = project_folders, 
                    path_prefix = "path_", libs = libs,
                    alt_env_id = "COMPUTERNAME", alt_env_value = "PCRZP",
                    alt_env_root_folder = "F:\\BEN\\edu")

# More settings
rasterOptions(tmpdir = envrmt$path_tmp)
mapviewOptions(basemaps = mapviewGetOption("basemaps")[c(3, 1:2, 4:5)])

