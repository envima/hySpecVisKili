# Set environment for environmental information systems analysis
require(envimaR)

root_folder = path.expand("~/plygrnd/hySpecVisKili/")
fcts_folder = file.path(root_folder, "hySpecVisKili/src/functions/")

project_folders = c("data/",
                    "data/biodiv",
                    "data/020_hypspec_org/",
                    "data/025_hypspec_aio/",
                    "data/030_hypspec_nrm/",
                    "data/040_hypspec_vegidcs/",
                    "data/050_hypspec_kmdc/",
                    "data/060_hypspec_raoq/",
                    "data/070_hypspec_glcm/",
                    "data/090_hypspec_pred/",
                    "data/100_comb_gpm_sr/",
                    "data/110_model_gpm_sr/",
                    "data/120_compile_analysis_sr/",
                    "data/200_comb_gpm_sr_res/",
                    "data/210_model_gpm_sr_res/",
                    "data/220_compile_analysis_sr_elev_res/",
                    "data/300_model_gpm_sr_indp/",
                    "data/310_comb_gpm_sr_elev_res_indp/",
                    "data/320_model_gpm_sr_elev_res_indp/",
                    "data/500_analysis_sr/",
                    "data/510_analysis_sr_elev_res/",
                    "data/plots/",
                    "data/rdata/",
                    "data/meta/",
                    "data/output/",
                    "data/vis/",
                    "data/temp/")

libs = c("biodivTools", "CAST", "corrplot", "doParallel", "grid", "gridExtra", 
         "gpm", "ggplot2", "hsdar", "rgeos", "ggplot2", "mapview", 
         "raster", "RStoolbox", "reshape2", "rgdal", "satelliteTools", "sp", 
         "spacetime", "vegan")

envrmt = createEnvi(root_folder = root_folder,
                    fcts_folder = fcts_folder,
                    folders = project_folders, 
                    path_prefix = "path_", libs = libs,
                    alt_env_id = "COMPUTERNAME", alt_env_value = "PCRZP",
                    alt_env_root_folder = "F:\\BEN\\edu")

# More settings
rasterOptions(tmpdir = envrmt$path_temp)
mapviewOptions(basemaps = mapviewGetOption("basemaps")[c(3, 1:2, 4:5)])
saga_cmd = "C:/OSGeo4W64/apps/saga-ltr/saga_cmd.exe"
# initOTB("C:/OSGeo4W64/bin/")
initOTB("C:/OSGeo4W64/OTB-6.2.0-Win64/bin/")


