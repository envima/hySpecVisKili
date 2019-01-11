# Combine hyperspectral predictores and biodiversity variables in gpm class.
if(Sys.info()["sysname"] == "Windows"){
  filepath_base = "C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R"
} else {
  filepath_base = "/mnt/sd19006/data/users/tnauss/KI-Hyperspec/HySpec_KiLi/src/000_set_environment_linux.R"
}
source(filepath_base)

if(length(showConnections()) == 0){
  cores = 3
  cl = parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
}

dir.create(path_analysis_sr, showWarnings = FALSE)

all_models = readRDS(file.path(path_compile_analysis_sr, "models_sr.rds"))


# Collect model performance
gam_sr = modelPerformance(all_models[["gam"]])
pls_sr = modelPerformance(all_models[["pls"]])
rf_sr = modelPerformance(all_models[["rf"]])

summary(gam_sr)
summary(pls_sr)
summary(rf_sr)

models_sr = rbind(pls_sr[, -4], rf_sr[, -4])
models_sr$mptype = paste0(models_sr$mtype, "_", models_sr$ptype)
models_sr$mptype = factor(models_sr$mptype, levels = c("pls_elsp", "rf_elsp",
                                                       "pls_elui", "rf_elui",
                                                       "pls_kmra", "rf_kmra",
                                                       "pls_spec", "rf_spec"))

# Plot model performance
ggplot(data = pls_sr[pls_sr$ptype == "elui" | pls_sr$ptype == "spec",], aes(x = resp, y = RMSE_normSD, fill = ptype)) + 
  geom_boxplot()+
  labs(list(title = "PLS", fill = "Predictor Set")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data = rf_sr[rf_sr$ptype == "elui" | rf_sr$ptype == "spec",], aes(x = resp, y = RMSE_normSD, fill = ptype)) + 
  geom_boxplot() +
  labs(list(title = "RF", fill = "Predictor Set")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data = models_sr[models_sr$ptype == "elui" | models_sr$ptype == "spec",], aes(x = resp, y = RMSE_normSD, fill = mptype)) + 
  geom_boxplot() +
  labs(list(title = "PLS and RF", fill = "Predictor Set")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Collect variable importance
var_imp <- compVarImp(all_models[["pls"]][["spec"]]@model[[1]], scale = FALSE)
plotVarImp(var_imp)
plotVarImpHeatmap(var_imp, xlab = "Species", ylab = "Band")

var_imp <- compVarImp(all_models[["rf"]][["spec"]]@model[[1]], scale = FALSE)
plotVarImp(var_imp)
plotVarImpHeatmap(var_imp, xlab = "Species", ylab = "Band")


