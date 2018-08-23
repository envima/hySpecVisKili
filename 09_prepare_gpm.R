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

# min
veg_dat_min <- veg_dat[,c(11,2)]
veg_dat_min$plotID <- as.character(veg_dat_min$plotID)
veg_dat_min$vegindex <- paste("min", rep(vegindex[c(4:17, 20:21, 23:25, 28:29, 34, 
                                   38:47, 50:57, 59, 61:65, 68:70, 
                                   72:83, 85:98, 106:113, 115)], 68),
                              sep = "_")
veg_dat_min <- veg_dat_min[,c(1,3,2)]
veg_min_cst <- dcast(veg_dat_min, plotID ~ vegindex, mean)


# max
veg_dat_max <- veg_dat[,c(11,3)]
veg_dat_max$plotID <- as.character(veg_dat_max$plotID)
veg_dat_max$vegindex <- paste("max", rep(vegindex[c(4:17, 20:21, 23:25, 28:29, 34, 
                                       38:47, 50:57, 59, 61:65, 68:70, 
                                       72:83, 85:98, 106:113, 115)], 68), 
                              sep = "_")
veg_dat_max <- veg_dat_max[,c(1,3,2)]
veg_max_cst <- dcast(veg_dat_max, plotID ~ vegindex, mean)

veg_max_cst <- veg_max_cst[, -1]
veg_dat_all <- cbind(veg_min_cst, veg_max_cst)


# mean
veg_dat_mean <- veg_dat[,c(11,4)]
veg_dat_mean$plotID <- as.character(veg_dat_mean$plotID)
veg_dat_mean$vegindex <- paste("mean", rep(vegindex[c(4:17, 20:21, 23:25, 28:29, 34, 
                                       38:47, 50:57, 59, 61:65, 68:70, 
                                       72:83, 85:98, 106:113, 115)], 68),
                               sep = "_")
veg_dat_mean <- veg_dat_mean[,c(1,3,2)]
veg_mean_cst <- dcast(veg_dat_mean, plotID ~ vegindex, mean)

veg_mean_cst <- veg_mean_cst[, -1]
veg_dat_all <- cbind(veg_dat_all, veg_mean_cst)


# median 
veg_dat_median <- veg_dat[,c(11,5)]
veg_dat_median$plotID <- as.character(veg_dat_median$plotID)
veg_dat_median$vegindex <- paste("median", rep(vegindex[c(4:17, 20:21, 23:25, 28:29, 34, 
                                        38:47, 50:57, 59, 61:65, 68:70, 
                                        72:83, 85:98, 106:113, 115)], 68),
                                 sep = "_")

veg_dat_median <- veg_dat_median[,c(1,3,2)]
veg_median_cst <- dcast(veg_dat_median, plotID ~ vegindex, mean)

veg_median_cst <- veg_median_cst[, -1]
veg_dat_all <- cbind(veg_dat_all, veg_median_cst)


# sd
veg_dat_sd <- veg_dat[,c(11,6)]
veg_dat_sd$plotID <- as.character(veg_dat_sd$plotID)
veg_dat_sd$vegindex <- paste("sd", rep(vegindex[c(4:17, 20:21, 23:25, 28:29, 34, 
                                        38:47, 50:57, 59, 61:65, 68:70, 
                                        72:83, 85:98, 106:113, 115)], 68),
                             sep = "_")
veg_dat_sd <- veg_dat_sd[,c(1,3,2)]
veg_sd_cst <- dcast(veg_dat_sd, plotID ~ vegindex, mean)

veg_sd_cst <- veg_sd_cst[,-1]
veg_dat_all <- cbind(veg_dat_all, veg_sd_cst)


# stqrt
veg_dat_stqrt <- veg_dat[,c(11,7)]
veg_dat_stqrt$plotID <- as.character(veg_dat_stqrt$plotID)
veg_dat_stqrt$vegindex <- paste("stqrt",rep(vegindex[c(4:17, 20:21, 23:25, 28:29, 34, 
                                        38:47, 50:57, 59, 61:65, 68:70, 
                                        72:83, 85:98, 106:113, 115)], 68),
                                sep = "_")
veg_dat_stqrt <- veg_dat_stqrt[,c(1,3,2)]
veg_stqrt_cst <- dcast(veg_dat_stqrt, plotID ~ vegindex, mean)

veg_stqrt_cst <- veg_stqrt_cst[,-1]
veg_dat_all <- cbind(veg_dat_all, veg_stqrt_cst)


# rdqrt
veg_dat_rdqrt <- veg_dat[,c(11,8)]
veg_dat_rdqrt$plotID <- as.character(veg_dat_rdqrt$plotID)
veg_dat_rdqrt$vegindex <- paste("rdqrt",rep(vegindex[c(4:17, 20:21, 23:25, 28:29, 34, 
                                         38:47, 50:57, 59, 61:65, 68:70, 
                                         72:83, 85:98, 106:113, 115)], 68),
                                sep = "_")
veg_dat_rdqrt <- veg_dat_rdqrt[,c(1,3,2)]
veg_rdqrt_cst <- dcast(veg_dat_rdqrt, plotID ~ vegindex, mean)

veg_rdqrt_cst <- veg_rdqrt_cst[,-1]
veg_dat_all <- cbind(veg_dat_all, veg_rdqrt_cst)


# IQR
veg_dat_IQR <- veg_dat[,c(11,9)]
veg_dat_IQR$plotID <- as.character(veg_dat_IQR$plotID)
veg_dat_IQR$vegindex <- paste("IQR",rep(vegindex[c(4:17, 20:21, 23:25, 28:29, 34, 
                                         38:47, 50:57, 59, 61:65, 68:70, 
                                         72:83, 85:98, 106:113, 115)], 68),
                              sep = "_")
veg_dat_IQR <- veg_dat_IQR[,c(1,3,2)]
veg_IQR_cst <- dcast(veg_dat_IQR, plotID ~ vegindex, mean)

veg_IQR_cst <- veg_IQR_cst[,-1]
veg_dat_all <- cbind(veg_dat_all, veg_IQR_cst)


# IQR_M
veg_dat_IQR_M <- veg_dat[,c(11,10)]
veg_dat_IQR_M$plotID <- as.character(veg_dat_IQR_M$plotID)
veg_dat_IQR_M$vegindex <- paste("IQR_M",rep(vegindex[c(4:17, 20:21, 23:25, 28:29, 34, 
                                         38:47, 50:57, 59, 61:65, 68:70, 
                                         72:83, 85:98, 106:113, 115)], 68),
                                sep = "_")
veg_dat_IQR_M <- veg_dat_IQR_M[,c(1,3,2)]
veg_IQR_M_cst <- dcast(veg_dat_IQR_M, plotID ~ vegindex, mean)

veg_IQR_M_cst <- veg_IQR_M_cst[,-1]
veg_dat_all <- cbind(veg_dat_all, veg_IQR_M_cst)


##
# sd and dist to centroid based on pca_inverse
rs_dat_1 <- read.csv2("./1st/norm_441/re_norm/pca/pca_inverse/pca_inverse_dia_50m/pci_dia_50m_sd_distCent_1st.csv", 
                     header = TRUE)
rs_dat_2 <- read.csv2("./2nd/norm_441/re_norm/pca/pca_inverse/pca_inverse_dia_50m/pci_dia_50m_sd_distCent_2nd.csv", 
                      header = TRUE)
rs_dat <- rbind(rs_dat_1, rs_dat_2)

rs_veg_dat_all <- merge(rs_dat, veg_dat_all, by = "plotID")


## haralick
hara_1_08 <- read.csv2("./1st_sd_hara/pci_sd_08/pci_SD_haralick_stats_1st_wide.csv", 
                       header = TRUE)
hara_1_16 <- read.csv2("./1st_sd_hara/pci_sd_16/pci_16_SD_haralick_stats_1st_wide.csv", 
                       header = TRUE)
hara_2_08 <- read.csv2("./2nd_sd_hara/pci_sd_08/pci_08_haralick_stats_2nd_wide.csv", 
                       header = TRUE)
hara_2_16 <- read.csv2("./2nd_sd_hara/pci_sd_16/pci_16_haralick_stats_2nd_wide.csv", 
                       header = TRUE)

hara_1 <- cbind(hara_1_08[,-1], hara_1_16[,-1])
hara_1 <- hara_1[, -234]

hara_2 <- cbind(hara_2_08[, -1], hara_2_16[, -1])
hara_2 <- hara_2[, -234]

hara <- rbind(hara_1, hara_2)
#hara <- hara[,-1]

rs_veg_hara_dat_all <- merge(rs_veg_dat_all, hara, by = "plotID")


#####
###
# load biodiversity data
#bd_dat <- read.csv2("Biodiversity_Data_Marcel.csv", header = TRUE)
#rs_veg_hara_bd_df <- merge(rs_veg_hara_dat_all, bd_dat, by = "plotID")

#write.csv2(rs_veg_hara_bd_df, 
#           "rs_veg_hara_biodiv_df_NEW.csv", 
#           row.names = FALSE)

#####
###
# load turnover data
#to_ani_dat <- read.csv2("./taxa_animals_moths_TO_NE_AC_NMDS_sites.csv", header = TRUE)
#to_plt_dat <- read.csv2("plants_TO_NE_AC_NMDS_sites.csv", header = TRUE)
#to_dat <- merge(to_ani_dat, to_plt_dat)

to_ani_dat <- read.csv2("_taxa_animals_moths_TO_NE_AC_NMDS_sites.csv",header = TRUE)
to_plt_dat <- read.csv2(" taxa_plants_TO_NE_AC_NMDS_sites.csv",header = TRUE)
to_dat <- merge(to_ani_dat, to_plt_dat)
 
rs_veg_hara_to_df <- merge(rs_veg_hara_dat_all, to_dat, by = "plotID")

write.csv2(rs_veg_hara_to_df, 
           "rs_veg_hara_species_turnover_df_control.csv",
           row.names = FALSE)

