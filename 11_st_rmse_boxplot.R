library(gpm)
library(Metrics)
  
setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/gpm_re_norm_pci_hara_species_turnover")

## Read files and build GPM object
filepath_gpm = "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/gpm_re_norm_pci_hara_species_turnover/"
filepath = paste(filepath_gpm, "residuen", sep = "")

outpath <- filepath


mrg_tbl_res_gpm_model <- readRDS(paste0(filepath_gpm, "gpm_pci_species_turnover_res_model_pls_2018-03-26.rds"))
mrg_tbl_gpm_model <- readRDS(paste0(filepath_gpm, "gpm_pci_species_turnover_model_pls_2018-03-26.rds"))
  
  
# residuen
tstat_res <- compRegrTests(mrg_tbl_res_gpm_model@model[[1]])
overview_res = aggregate(tstat_res$r_squared, by = list(tstat_res$model_response), mean)
colnames(overview_res) = c("Species Richness", "r.sq_residuen")
overview_res[order(overview_res$r.sq_residuen),]

RMSE_mdl_res_gpm <- ddply(tstat_res, .(model_response, model_selector), 
                            summarise, RMSE = rmse(testing_response, testing_predicted))
  
  
levels(RMSE_mdl_res_gpm$model_response) <- c("aculeate NMDS I (resid)", "aculeate NMDS II (resid)",
                                             "ants NMDS I (resid)", "ants NMDS II (resid)", "asterids NMDS I (resid)",   
                                             "asterids NMDS II (resid)", "bats NMDS I (resid)", "bats NMDS II (resid)",
                                             "bees NMDS I (resid)", "bees NMDS II (resid)", "beetles NMDS I (resid)",
                                             "beetles NMDS II (resid)", "birds NMDS I (resid)", "bats NMDS II (resid)",
                                             "conifers NMDS I (resid)", "conifers NMDS II (resid)", 
                                             "dungbeetles NMDS I (resid)", "dungbeetles NMDS II (resid)",
                                             "eudicots NMDS I (resid)", "eudicots NMDS II (resid)", "ferns NMDS I (resid)",
                                             "ferns NMDS II (resid)", "gastropods NMDS I (resid)", "gastropods NMDS II (resid)",
                                             "magnoliids NMDS I (resid)", "magnoliids NMDS II (resid)", "mammals NMDS I (resid)",
                                             "mammals NMDS II (resid)", "millipedes NMDS I (resid)", "millipedes NMDS II (resid)",   
                                             "monocots NMDS I (resid)", "monocots NMDS II (resid)", "mosses NMDS I (resid)",
                                             "mosses NMDS II (resid)", "moths NMDS I (resid)", "moths NMDS II (resid)",      
                                             "orthoptera NMDS I (resid)", "orthoptera NMDS II (resid)", "parasitoids NMDS I(resid)",
                                             "parasitoids NMDS II (resid)", "rosids NMDS I (resid)", "rosids NMDS II (resid)",
                                             "syrphid NMDS I (resid)", "syrphid NMDS II (resid)")
  
# nicht residuen
tstat <- compRegrTests(mrg_tbl_gpm_model@model[[1]])
overview = aggregate(tstat$r_squared, by = list(tstat$model_response), mean)
colnames(overview) = c("Species Richness", "r.sq")
overview[order(overview$r.sq),]

RMSE_mdl_gpm <- ddply(tstat, .(model_response, model_selector), 
                   summarise, RMSE = rmse(testing_response, testing_predicted))

levels(RMSE_mdl_gpm$model_response) <- c("aculeate NMDS I", "aculeate NMDS II",
                                         "ants NMDS I", "ants NMDS II", "asterids NMDS I",   
                                         "asterids NMDS II", "bats NMDS I", "bats NMDS II",
                                         "bees NMDS I", "bees NMDS II", "beetles NMDS I",
                                         "beetles NMDS II", "birds NMDS I", "bats NMDS II",
                                         "conifers NMDS I", "conifers NMDS II", 
                                         "dungbeetles NMDS I", "dungbeetles NMDS II",
                                         "eudicots NMDS I", "eudicots NMDS II", "ferns NMDS I",
                                         "ferns NMDS II", "gastropods NMDS I", "gastropods NMDS II",
                                         "magnoliids NMDS I", "magnoliids NMDS II", "mammals NMDS I",
                                         "mammals NMDS II", "millipedes NMDS I", "millipedes NMDS II",   
                                         "monocots NMDS I", "monocots NMDS II", "mosses NMDS I",
                                         "mosses NMDS II", "moths NMDS I", "moths NMDS II",      
                                         "orthoptera NMDS I", "orthoptera NMDS II", "parasitoids NMDS I",
                                         "parasitoids NMDS II", "rosids NMDS I", "rosids NMDS II",
                                         "syrphid NMDS I", "syrphid NMDS II")

  
plot_resp <- function(df, var, names, resp_title, path = outpath, comm){
  df$resp = reorder(df$model_response, df[[var]], median)
  p <- ggplot(aes_string(x = "resp", y = var), data = df) + 
    geom_boxplot() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    xlab(resp_title) + 
    ylab(var)
  print(p)
  ggsave(filename = paste0(path, "plot_", var, "_", comm, ".pdf"), plot = p, width = 25,
         height = 25, units = "cm")
  
  dev.off()
}
  
plot_resp(df = RMSE_mdl_res_gpm, var = "RMSE", resp_title = "Taxa (Residuals)", comm = "res")
plot_resp(df = RMSE_mdl_gpm, var = "RMSE", resp_title = "Taxa", comm = "")
  
RMSE_mdl_res_gpm_sb = subset(RMSE_mdl_res_gpm, RMSE_mdl_res_gpm$model_response != "lui" &
                               RMSE_mdl_res_gpm$model_response != "SRallplants" &
                               RMSE_mdl_res_gpm$model_response != "SRanimals")  
  
RMSE_mdl_gpm_sb = subset(RMSE_mdl_gpm, RMSE_mdl_gpm$model_response != "lui" &
                               RMSE_mdl_gpm$model_response != "all plants" &
                               RMSE_mdl_gpm$model_response != "all animals")

  
