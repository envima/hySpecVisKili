# Analyse species richness prediction models

source("C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R")

dir.create(path_analysis_sr, showWarnings = FALSE)

all_models = readRDS(file.path(path_compile_analysis_sr, "models_sr.rds"))


# Collect model performance
gam_sr = modelPerformance(all_models[["gam"]])
pls_sr = modelPerformance(all_models[["pls"]])
rf_sr = modelPerformance(all_models[["rf"]])

summary(gam_sr)
summary(pls_sr)
summary(rf_sr)


# Compare pls and rf
models_sr = rbind(pls_sr[, -4], rf_sr[, -4])
models_sr$mptype = paste0(models_sr$mtype, "_", models_sr$ptype)
models_sr$mptype = factor(models_sr$mptype, levels = c("pls_elsp", "rf_elsp",
                                                       "pls_elui", "rf_elui",
                                                       "pls_kmra", "rf_kmra",
                                                       "pls_spec", "rf_spec"))

ggplot(data = models_sr[models_sr$ptype == "elui" | models_sr$ptype == "spec",], aes(x = resp, y = RMSE_normSD, fill = mptype)) + 
  geom_boxplot() +
  labs(list(title = "PLS and RF", fill = "Predictor Set")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


pls_rf_sr = merge(pls_sr, rf_sr, by = c("ptype", "resp", "Resample"), all.y = TRUE)
colnames(pls_rf_sr)[grep("\\.x", colnames(pls_rf_sr))] = 
  gsub("\\.x", "_pls", colnames(pls_rf_sr)[grep("\\.x", colnames(pls_rf_sr))])
colnames(pls_rf_sr)[grep("\\.y", colnames(pls_rf_sr))] = 
  gsub("\\.y", "_rf", colnames(pls_rf_sr)[grep("\\.y", colnames(pls_rf_sr))])
# nrow(pls_rf_sr)

ptypes = c("elui", "kmra", "spec", "elsp")
perf_check = lapply(ptypes, function(pt){
  subdf = pls_rf_sr[!is.na(pls_rf_sr$RMSE_pls) & 
                      pls_rf_sr$ptype == pt &
                      pls_rf_sr$Resample == "Mean", ]
  rownames(subdf[subdf$RMSE_pls < subdf$RMSE_rf, ])
})
names(perf_check) = ptypes

# Check performance of PLS and RF for ELUI
pls_rf_sr[as.numeric(perf_check[[1]]),]

# Check performance of PLS and RF for KMRA
pls_rf_sr[as.numeric(perf_check[[2]]),]
sort(round(1-pls_rf_sr[as.numeric(perf_check[[2]]), "RMSE_pls"] / pls_rf_sr[as.numeric(perf_check[[2]]), "RMSE_rf"],2))
sort(round(pls_rf_sr[as.numeric(perf_check[[2]]), "nvars_rf"] / pls_rf_sr[as.numeric(perf_check[[2]]), "nvars_pls"],2))

# Check performance of PLS and RF for SPEC
pls_rf_sr[as.numeric(perf_check[[3]]),]
sort(round(1-pls_rf_sr[as.numeric(perf_check[[3]]), "RMSE_pls"] / pls_rf_sr[as.numeric(perf_check[[3]]), "RMSE_rf"],2))
sort(round(pls_rf_sr[as.numeric(perf_check[[3]]), "nvars_rf"] / pls_rf_sr[as.numeric(perf_check[[3]]), "nvars_pls"],2))



# models_sr_wide = spread(models_sr[models_sr$Resample == "Mean",], "ptype", "RMSE_normSD")
# head(models_sr_wide)



# Collect variable importance
var_imp <- compVarImp(all_models[["pls"]][["spec"]]@model[[1]], scale = FALSE)
plotVarImp(var_imp)
plotVarImpHeatmap(var_imp, xlab = "Species", ylab = "Band")

var_imp <- compVarImp(all_models[["rf"]][["spec"]]@model[[1]], scale = FALSE)
plotVarImp(var_imp)
plotVarImpHeatmap(var_imp, xlab = "Species", ylab = "Band")


