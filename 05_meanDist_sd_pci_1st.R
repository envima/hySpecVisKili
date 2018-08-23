library(hsdar)
library(data.table)
library(pryr)
library(foreach)
library(doParallel)
library(tidyr)
library(RStoolbox)


setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/1st/norm_441/re_norm/pca/pca_inverse/pca_inverse_dia_50m/")
filepath_raster <- "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/1st/norm_441/re_norm/pca/pca_inverse/pca_inverse_dia_50m/"

# meta data
band_meta <- read.csv(paste0("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/1st/", 
                             "band_meta.csv"))
band_meta_1 <- band_meta[-c(1:2,10),]

# raster files
fls_hps <- list.files(paste0(filepath_raster), full.names = FALSE,
                      recursive = FALSE, pattern = "*pci_dia_50m.tif")

## cof1
hyperspectral_data <- stack(fls_hps[1])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist$spec_div_mean_dis <- mean(rs)
dist$plotID <- "cof1"

## cof2
hyperspectral_data <- stack(fls_hps[2])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist2 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist2$spec_div_mean_dis <- mean(rs)
dist2$plotID <- "cof2"
dist.plts.all <- rbind(dist, dist2)

## cof3
hyperspectral_data <- stack(fls_hps[3])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist3 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist3$spec_div_mean_dis <- mean(rs)
dist3$plotID <- "cof3"
dist.plts.all <- rbind(dist.plts.all, dist3)

## cof4
hyperspectral_data <- stack(fls_hps[4])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist4 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist4$spec_div_mean_dis <- mean(rs)
dist4$plotID <- "cof4"
dist.plts.all <- rbind(dist.plts.all, dist4)

## cof5
hyperspectral_data <- stack(fls_hps[5])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist5 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist5$spec_div_mean_dis <- mean(rs)
dist5$plotID <- "cof5"
dist.plts.all <- rbind(dist.plts.all, dist5)

## flm1
hyperspectral_data <- stack(fls_hps[6])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist7 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist7$spec_div_mean_dis <- mean(rs)
dist7$plotID <- "flm1"
dist.plts.all <- rbind(dist.plts.all, dist7)

## flm2
hyperspectral_data <- stack(fls_hps[7])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist8 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist8$spec_div_mean_dis <- mean(rs)
dist8$plotID <- "flm2"
dist.plts.all <- rbind(dist.plts.all, dist8)

## flm3
hyperspectral_data <- stack(fls_hps[8])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist9 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist9$spec_div_mean_dis <- mean(rs)
dist9$plotID <- "flm3"
dist.plts.all <- rbind(dist.plts.all, dist9)

## flm4
hyperspectral_data <- stack(fls_hps[9])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist10 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist10$spec_div_mean_dis <- mean(rs)
dist10$plotID <- "flm4"
dist.plts.all <- rbind(dist.plts.all, dist10)

## flm6
hyperspectral_data <- stack(fls_hps[10])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist11 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist11$spec_div_mean_dis <- mean(rs)
dist11$plotID <- "flm6"
dist.plts.all <- rbind(dist.plts.all, dist11)

## foc1
hyperspectral_data <- stack(fls_hps[11])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist12 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist12$spec_div_mean_dis <- mean(rs)
dist12$plotID <- "foc1"
dist.plts.all <- rbind(dist.plts.all, dist12)

## foc6
hyperspectral_data <- stack(fls_hps[12])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist13 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist13$spec_div_mean_dis <- mean(rs)
dist13$plotID <- "foc6"
dist.plts.all <- rbind(dist.plts.all, dist13)

## fod5
hyperspectral_data <- stack(fls_hps[13])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist14 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist14$spec_div_mean_dis <- mean(rs)
dist14$plotID <- "fod5"
dist.plts.all <- rbind(dist.plts.all, dist14)

## gra1
hyperspectral_data <- stack(fls_hps[14])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist15 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist15$spec_div_mean_dis <- mean(rs)
dist15$plotID <- "gra1"
dist.plts.all <- rbind(dist.plts.all, dist15)

## gra2
hyperspectral_data <- stack(fls_hps[15])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist16 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist16$spec_div_mean_dis <- mean(rs)
dist16$plotID <- "gra2"
dist.plts.all <- rbind(dist.plts.all, dist16)

## gra3
hyperspectral_data <- stack(fls_hps[16])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist17 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist17$spec_div_mean_dis <- mean(rs)
dist17$plotID <- "gra3"
dist.plts.all <- rbind(dist.plts.all, dist17)

## gra4
hyperspectral_data <- stack(fls_hps[17])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist18 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist18$spec_div_mean_dis <- mean(rs)
dist18$plotID <- "gra4"
dist.plts.all <- rbind(dist.plts.all, dist18)

## gra5
hyperspectral_data <- stack(fls_hps[18])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist19 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist19$spec_div_mean_dis <- mean(rs)
dist19$plotID <- "gra5"
dist.plts.all <- rbind(dist.plts.all, dist19)

## gra6
hyperspectral_data <- stack(fls_hps[19])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist20 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist20$spec_div_mean_dis <- mean(rs)
dist20$plotID <- "gra6"
dist.plts.all <- rbind(dist.plts.all, dist20)

## hom1
hyperspectral_data <- stack(fls_hps[20])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_s_miv <- sd(m) 
dist21 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist21$spec_div_mean_dis <- mean(rs)
dist21$plotID <- "hom1"
dist.plts.all <- rbind(dist.plts.all, dist21)

## hom2
hyperspectral_data <- stack(fls_hps[21])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist22 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist22$spec_div_mean_dis <- mean(rs)
dist22$plotID <- "hom2"
dist.plts.all <- rbind(dist.plts.all, dist22)

## hom3
hyperspectral_data <- stack(fls_hps[22])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist23 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist23$spec_div_mean_dis <- mean(rs)
dist23$plotID <- "hom3"
dist.plts.all <- rbind(dist.plts.all, dist23)

## hom4
hyperspectral_data <- stack(fls_hps[23])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist24 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist24$spec_div_mean_dis <- mean(rs)
dist24$plotID <- "hom4"
dist.plts.all <- rbind(dist.plts.all, dist24)

## hom5
hyperspectral_data <- stack(fls_hps[24])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist25 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist25$spec_div_mean_dis <- mean(rs)
dist25$plotID <- "hom5"
dist.plts.all <- rbind(dist.plts.all, dist25)

## mai1
hyperspectral_data <- stack(fls_hps[25])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist26 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist26$spec_div_mean_dis <- mean(rs)
dist26$plotID <- "mai1"
dist.plts.all <- rbind(dist.plts.all, dist26)

## mai2
hyperspectral_data <- stack(fls_hps[26])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist27 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist27$spec_div_mean_dis <- mean(rs)
dist27$plotID <- "mai2"
dist.plts.all <- rbind(dist.plts.all, dist27)

## mai3
hyperspectral_data <- stack(fls_hps[27])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist28 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist28$spec_div_mean_dis <- mean(rs)
dist28$plotID <- "mai3"
dist.plts.all <- rbind(dist.plts.all, dist28)

## mai4
hyperspectral_data <- stack(fls_hps[28])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist29 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist29$spec_div_mean_dis <- mean(rs)
dist29$plotID <- "mai4"
dist.plts.all <- rbind(dist.plts.all, dist29)

## mai5
hyperspectral_data <- stack(fls_hps[29])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist30 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist30$spec_div_mean_dis <- mean(rs)
dist30$plotID <- "mai5"
dist.plts.all <- rbind(dist.plts.all, dist30)

## sav1
hyperspectral_data <- stack(fls_hps[30])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist31 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist31$spec_div_mean_dis <- mean(rs)
dist31$plotID <- "sav1"
dist.plts.all <- rbind(dist.plts.all, dist31)

## sav2
hyperspectral_data <- stack(fls_hps[31])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist32 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist32$spec_div_mean_dis <- mean(rs)
dist32$plotID <- "sav2"
dist.plts.all <- rbind(dist.plts.all, dist32)

## sav3
hyperspectral_data <- stack(fls_hps[32])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist33 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist33$spec_div_mean_dis <- mean(rs)
dist33$plotID <- "sav3"
dist.plts.all <- rbind(dist.plts.all, dist33)

# sav4
hyperspectral_data <- stack(fls_hps[33])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist34 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist34$spec_div_mean_dis <- mean(rs)
dist34$plotID <- "sav4"
dist.plts.all <- rbind(dist.plts.all, dist34)

# sav5
hyperspectral_data <- stack(fls_hps[34])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist35 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist35$spec_div_mean_dis <- mean(rs)
dist35$plotID <- "sav5"
dist.plts.all <- rbind(dist.plts.all, dist35)


write.csv2(dist.plts.all, 
           file = "pci_dia_50m_sd_distCent_1st.csv", 
           row.names = FALSE)
