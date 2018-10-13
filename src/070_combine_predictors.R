# Compute mean and sd of final predictor sets.

source("C:/Users/tnauss/permanent/plygrnd/KI-Hyperspec/HySpec_KiLi/src/000_set_environment.R")
if(length(showConnections()) == 0){
  cores = 3
  cl = parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
}

dir.create(paste0(path_hyp_pred), showWarnings = FALSE)

hd_files = c(list.files(path_hyp_vegidcs, recursive = FALSE, full.names = TRUE),
             list.files(path_hyp_kmdc, recursive = FALSE, full.names = TRUE),
             list.files(path_hyp_raoq, recursive = FALSE, full.names = TRUE))

h_meta = readRDS(paste0(path_meta, "hyp_meta.rds"))

preds = foreach (i = seq(length(hd_files))) %do% {
  
  print(i)
  
  r = readRDS(hd_files[[i]])
  
  nms = names(r)
  plotid = substr(nms[1], 1, 4)
  productid = substr(nms, 6, nchar(nms))
  
  l = lapply(seq(nlayers(r)), function(l){
    df = data.frame(mean(getValues(r[[l]]), na.rm = TRUE), sd(getValues(r[[l]]), na.rm = TRUE))
    colnames(df) = c(paste0(productid[l], c("_mean", "_sd")))
    return(df)
  })
  df = do.call("cbind", l)
  df = data.frame(plotID = plotid, df)
  return(df)
} 

grp = list(grep("vegidcs.rds", hd_files),
           grep("vegidcs_kmdc.rds", hd_files),
           grep("vegidcs_raoq_3.rds", hd_files),
           grep("vegidcs_kmdc_raoq_3.rds", hd_files),
           grep("pcai_kmdc.rds", hd_files),
           grep("pcai_raoq_3.rds", hd_files),
           grep("pcai_kmdc_raoq_3.rds", hd_files))

if(length(unlist(grp)) == length(preds)){
  df = lapply(grp, function(g){
    do.call("rbind", preds[g])
  })
  df = do.call("cbind", df)
  df = df[, -grep("plotID", colnames(df))[-1]]
}

saveRDS(df, file = paste0(path_hyp_pred, "hyperspec_preds.rds"))

stopCluster(cl)

# Visually check data
corrplot(cor(df[, -1]))

