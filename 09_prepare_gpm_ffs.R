library(hsdar)
library(doParallel)
library(dplyr)
library(reshape2)
library(ggplot2)
library(mgcv)
library(plyr)
library(RColorBrewer)

setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/")


#####
###
## load remote sensing data
# vegindex based on pca_inverse
veg_dat_1 <- read.csv2("./1st/norm_441/re_norm/pca/pca_inverse/pci_vegIndex/pci_vegIndex_dia_50m/pci_vegIndex_dia_50m_stats_1st.csv", header = TRUE)
veg_dat_2 <- read.csv2("./2nd/norm_441/re_norm/pca/pca_inverse/pci_vegIndex/pci_vegIndex_dia_50m/pci_vegIndex_dia_50m_stats_2nd.csv", header = TRUE)

veg_dat <- rbind(veg_dat_1, veg_dat_2)

colnames(veg_dat) <- c("vegindex", "min", "max", "mean", "median", "sd", 
                       "stqrt", "rdqrt", "IQR", "IQR_M", "plotID")
vegindex <- vegindex()

# mean
veg_dat_mean <- veg_dat[,c(11,4)]
veg_dat_mean$plotID <- as.character(veg_dat_mean$plotID)
veg_dat_mean$vegindex <- paste("mean", rep(vegindex[c(4:17, 20:21, 23:25, 28:29, 34, 
                                                      38:47, 50:57, 59, 61:65, 68:70, 
                                                      72:83, 85:98, 106:113, 115)], 68),
                               sep = "_")

veg_dat_mean_sub = subset(veg_dat_mean, substr(veg_dat_mean$vegindex, 6, 11) == "Carter"|
                       substr(veg_dat_mean$vegindex, 6, 7) == "CI"|  
                       substr(veg_dat_mean$vegindex, 6, 9) == "CRI2"|
                       substr(veg_dat_mean$vegindex, 6, 9) == "CRI4"|
                       substr(veg_dat_mean$vegindex, 6, 9) == "Datt"|
                       substr(veg_dat_mean$vegindex, 6, 10) == "DWSI4"|
                       substr(veg_dat_mean$vegindex, 6, 8) == "EVI"|
                       substr(veg_dat_mean$vegindex, 6, 15) == "Green NDVI"|
                       substr(veg_dat_mean$vegindex, 6, 11) == "mND705"|   
                       substr(veg_dat_mean$vegindex, 6, 10) == "mNDVI"|
                       substr(veg_dat_mean$vegindex, 6, 8) == "mSR"|
                       substr(veg_dat_mean$vegindex, 6, 11) == "mSR705"|
                       substr(veg_dat_mean$vegindex, 6, 9) == "MTCI"|
                       substr(veg_dat_mean$vegindex, 6, 8) == "PRI"|
                       substr(veg_dat_mean$vegindex, 6, 13) == "PRI_norm"|
                       substr(veg_dat_mean$vegindex, 6, 8) == "PWI"|
                       substr(veg_dat_mean$vegindex, 6, 11) == "REP_Li"|
                       substr(veg_dat_mean$vegindex, 6, 9) == "SPVI"|
                       substr(veg_dat_mean$vegindex, 6, 8) == "SR8"|
                       substr(veg_dat_mean$vegindex, 6, 18) == "TCARI2/OSAVI2"|
                       substr(veg_dat_mean$vegindex, 6, 8) == "TGI")

veg_dat_mean_sub_cl = subset(veg_dat_mean_sub, 
                             veg_dat_mean_sub$vegindex != "mean_Carter2"&
                               veg_dat_mean_sub$vegindex != "mean_Carter3"&
                               veg_dat_mean_sub$vegindex != "mean_Carter4"&
                               veg_dat_mean_sub$vegindex != "mean_Carter5"&
                               veg_dat_mean_sub$vegindex != "mean_Carter6"&
                               veg_dat_mean_sub$vegindex != "mean_CI2"&
                               veg_dat_mean_sub$vegindex != "mean_Datt2"&
                               veg_dat_mean_sub$vegindex != "mean_Datt4"&
                               veg_dat_mean_sub$vegindex != "mean_Datt5"&
                               veg_dat_mean_sub$vegindex != "mean_Datt6"&
                               veg_dat_mean_sub$vegindex != "mean_mSR2"&
                               veg_dat_mean_sub$vegindex != "mean_PRI*CI2")

veg_dat_mean_sub_cl <- veg_dat_mean_sub_cl[,c(1,3,2)]
veg_mean_cst <- dcast(veg_dat_mean_sub_cl, plotID ~ vegindex, mean)


# sd
veg_dat_sd <- veg_dat[,c(11,6)]
veg_dat_sd$plotID <- as.character(veg_dat_sd$plotID)
veg_dat_sd$vegindex <- paste("sd", rep(vegindex[c(4:17, 20:21, 23:25, 28:29, 34, 
                                        38:47, 50:57, 59, 61:65, 68:70, 
                                        72:83, 85:98, 106:113, 115)], 68),
                             sep = "_")

veg_dat_sd_sub = subset(veg_dat_sd, substr(veg_dat_sd$vegindex, 6, 11) == "Carter"|
                          substr(veg_dat_sd$vegindex, 6, 7) == "CI"|  
                          substr(veg_dat_sd$vegindex, 6, 9) == "CRI2"|
                          substr(veg_dat_sd$vegindex, 6, 9) == "CRI4"|
                          substr(veg_dat_sd$vegindex, 6, 9) == "Datt"|
                          substr(veg_dat_sd$vegindex, 6, 10) == "DWSI4"|
                          substr(veg_dat_sd$vegindex, 6, 8) == "EVI"|
                          substr(veg_dat_sd$vegindex, 6, 15) == "Green NDVI"|
                          substr(veg_dat_sd$vegindex, 6, 11) == "mND705"|   
                          substr(veg_dat_sd$vegindex, 6, 10) == "mNDVI"|
                          substr(veg_dat_sd$vegindex, 6, 8) == "mSR"|
                          substr(veg_dat_sd$vegindex, 6, 11) == "mSR705"|
                          substr(veg_dat_sd$vegindex, 6, 9) == "MTCI"|
                          substr(veg_dat_sd$vegindex, 6, 8) == "PRI"|
                          substr(veg_dat_sd$vegindex, 6, 13) == "PRI_norm"|
                          substr(veg_dat_sd$vegindex, 6, 8) == "PWI"|
                          substr(veg_dat_sd$vegindex, 6, 11) == "REP_Li"|
                          substr(veg_dat_sd$vegindex, 6, 9) == "SPVI"|
                          substr(veg_dat_sd$vegindex, 6, 8) == "SR8"|
                          substr(veg_dat_sd$vegindex, 6, 18) == "TCARI2/OSAVI2"|
                          substr(veg_dat_sd$vegindex, 6, 8) == "TGI")

veg_dat_sd_sub_cl = subset(veg_dat_sd_sub, 
                             veg_dat_sd_sub$vegindex != "sd_Carter2"&
                               veg_dat_sd_sub$vegindex != "sd_Carter3"&
                               veg_dat_sd_sub$vegindex != "sd_Carter4"&
                               veg_dat_sd_sub$vegindex != "sd_Carter5"&
                               veg_dat_sd_sub$vegindex != "sd_Carter6"&
                               veg_dat_sd_sub$vegindex != "sd_CI2"&
                               veg_dat_sd_sub$vegindex != "sd_Datt2"&
                               veg_dat_sd_sub$vegindex != "sd_Datt4"&
                               veg_dat_sd_sub$vegindex != "sd_Datt5"&
                               veg_dat_sd_sub$vegindex != "sd_Datt6"&
                               veg_dat_sd_sub$vegindex != "sd_mSR2"&
                               veg_dat_sd_sub$vegindex != "sd_PRI*CI2")

veg_dat_sd_sub_cl <- veg_dat_sd_sub_cl[,c(1,3,2)]
veg_sd_cst <- dcast(veg_dat_sd_sub_cl, plotID ~ vegindex, mean)

veg_sd_cst <- veg_sd_cst[,-1]
veg_dat_all <- cbind(veg_mean_cst, veg_sd_cst)

#####
###
# load biodiversity data
#bd_dat <- read.csv2("Biodiversity_Data_Marcel.csv", header = TRUE)
#rs_veg_hara_bd_df <- merge(rs_veg_hara_dat_all, bd_dat, by = "plotID")

#write.csv2(rs_veg_hara_bd_df, 
#           "rs_veg_hara_biodiv_df.csv", 
#           row.names = FALSE)

#####
###
# load turnover data
#to_ani_dat <- read.csv2(" taxa_animals_moths_TO_NE_AC_NMDS_sites.csv", header = TRUE)
#to_plt_dat <- read.csv2(" taxa_plants_TO_NE_AC_NMDS_sites.csv", header = TRUE)
#to_dat <- merge(to_ani_dat, to_plt_dat, all.x = TRUE)

#veg_to_plt_ffs_df <- merge(veg_dat_all, to_dat, by = "plotID")

write.csv2(veg_to_plt_ffs_df, 
           "veg_beta_diversity_all_taxa_ffs_TO_NE_AC_df.csv",
           row.names = FALSE)

