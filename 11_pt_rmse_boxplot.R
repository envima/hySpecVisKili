library(gpm)
library(Metrics)
  
setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/gpm_re_norm_pci_hara_turnover_plotdiv")

## Read files and build GPM object
filepath_gpm = "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/gpm_re_norm_pci_hara_turnover_plotdiv/"
filepath = paste(filepath_gpm, "residuen", sep = "")

outpath <- filepath


mrg_tbl_res_gpm_model <- readRDS(paste0(filepath_gpm, "gpm_pci_plotdiv_turnover_res_model_pls_2018-03-26.rds"))
mrg_tbl_gpm_model <- readRDS(paste0(filepath_gpm, "gpm_pci_plotdiv_turnover_model_pls_2018-03-26.rds"))
  
  
# residuen
tstat_res <- compRegrTests(mrg_tbl_res_gpm_model@model[[1]])
overview_res = aggregate(tstat_res$r_squared, by = list(tstat_res$model_response), mean)
colnames(overview_res) = c("Species Richness", "r.sq_residuen")
overview_res[order(overview_res$r.sq_residuen),]

RMSE_mdl_res_gpm <- ddply(tstat_res, .(model_response, model_selector), 
                            summarise, RMSE = rmse(testing_response, testing_predicted))
  
  
levels(RMSE_mdl_res_gpm$model_response) <- c("animals NMDS I (resid)", "plants NMDS I (resid)", 
                                             "animals NMDS II (resid)", "plants NMDS II (resid)")
  
# nicht residuen
tstat <- compRegrTests(mrg_tbl_gpm_model@model[[1]])
overview = aggregate(tstat$r_squared, by = list(tstat$model_response), mean)
colnames(overview) = c("Species Richness", "r.sq")
overview[order(overview$r.sq),]

RMSE_mdl_gpm <- ddply(tstat, .(model_response, model_selector), 
                   summarise, RMSE = rmse(testing_response, testing_predicted))

levels(RMSE_mdl_gpm$model_response) <- c("animals NMDS I", "plants NMDS I", 
                                         "animals NMDS II", "plants NMDS II")

  
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

  
