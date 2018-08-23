library(hsdar)
library(data.table)
library(pryr)
library(foreach)
library(doParallel)
library(tidyr)
library(RStoolbox)


setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/1st/norm_441/pca/pca_inverse/")
filepath_raster <- "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/1st/norm_441/pca/pca_inverse/"

# meta data
band_meta <- read.csv(paste0("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/1st/", 
                             "band_meta.csv"))
band_meta_1 <- band_meta[-c(8,159:160),]

# raster files
fls_hps <- list.files(paste0(filepath_raster), full.names = FALSE,
                      recursive = FALSE, pattern = "*inverse.tif")

## fed1
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
dist$plotID <- "fed1"

## fed2
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
dist2$plotID <- "fed2"
dist.plts.all <- rbind(dist, dist2)

## fed3
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
dist3$plotID <- "fed3"
dist.plts.all <- rbind(dist.plts.all, dist3)

## fed4
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
dist4$plotID <- "fed4"
dist.plts.all <- rbind(dist.plts.all, dist4)

## fed5
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
dist5$plotID <- "fed5"
dist.plts.all <- rbind(dist.plts.all, dist5)

## fer0
hyperspectral_data <- stack(fls_hps[6])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(values1) 
dist6 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(values1^2)) 
dist6$spec_div_mean_dis <- mean(rs)
dist6$plotID <- "fer0"
dist.plts.all <- rbind(dist.plts.all, dist6)

## fer2
hyperspectral_data <- stack(fls_hps[7])
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
dist7$plotID <- "fer2"
dist.plts.all <- rbind(dist.plts.all, dist7)

## fer3
hyperspectral_data <- stack(fls_hps[8])
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
dist8$plotID <- "fer3"
dist.plts.all <- rbind(dist.plts.all, dist8)

## fer4
hyperspectral_data <- stack(fls_hps[9])
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
dist9$plotID <- "fer4"
dist.plts.all <- rbind(dist.plts.all, dist9)

## foc0
hyperspectral_data <- stack(fls_hps[10])
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
dist10$plotID <- "foc0"
dist.plts.all <- rbind(dist.plts.all, dist10)

## foc1
hyperspectral_data <- stack(fls_hps[11])
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
dist11$plotID <- "foc1"
dist.plts.all <- rbind(dist.plts.all, dist11)

## foc2
hyperspectral_data <- stack(fls_hps[12])
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
dist12$plotID <- "foc2"
dist.plts.all <- rbind(dist.plts.all, dist12)

## foc3
hyperspectral_data <- stack(fls_hps[13])
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
dist13$plotID <- "foc3"
dist.plts.all <- rbind(dist.plts.all, dist13)

## foc4
hyperspectral_data <- stack(fls_hps[14])
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
dist14$plotID <- "foc4"
dist.plts.all <- rbind(dist.plts.all, dist14)

## foc5
hyperspectral_data <- stack(fls_hps[15])
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
dist15$plotID <- "foc5"
dist.plts.all <- rbind(dist.plts.all, dist15)

## foc6
hyperspectral_data <- stack(fls_hps[16])
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
dist16$plotID <- "foc6"
dist.plts.all <- rbind(dist.plts.all, dist16)

## fod1
hyperspectral_data <- stack(fls_hps[17])
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
dist17$plotID <- "fod1"
dist.plts.all <- rbind(dist.plts.all, dist17)

## fod2
hyperspectral_data <- stack(fls_hps[18])
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
dist18$plotID <- "fod2"
dist.plts.all <- rbind(dist.plts.all, dist18)

## fod3
hyperspectral_data <- stack(fls_hps[19])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_s_miv <- sd(m) 
dist19 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist19$spec_div_mean_dis <- mean(rs)
dist19$plotID <- "fod3"
dist.plts.all <- rbind(dist.plts.all, dist19)

## fpd0
hyperspectral_data <- stack(fls_hps[20])
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
dist20$plotID <- "fpd0"
dist.plts.all <- rbind(dist.plts.all, dist20)

## fpd2
hyperspectral_data <- stack(fls_hps[21])
spec_lib <- speclib(as.matrix(hyperspectral_data), 
                    wavelength = band_meta_1$wavelength, 
                    fwhm = band_meta_1$fwhm) 

m <- spec_lib@spectra@spectra_ma 
m <- na.omit(m)

# calculate sd
rs_sd_miv <- sd(m) 
dist21 <- as.data.frame(rs_sd_miv)

# calculate distance to centroid
rs <- sqrt(rowSums(m^2)) 
dist21$spec_div_mean_dis <- mean(rs)
dist21$plotID <- "fpd2"
dist.plts.all <- rbind(dist.plts.all, dist21)

## fpd3
hyperspectral_data <- stack(fls_hps[22])
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
dist22$plotID <- "fpd3"
dist.plts.all <- rbind(dist.plts.all, dist22)

## fpd4
hyperspectral_data <- stack(fls_hps[23])
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
dist23$plotID <- "fpd4"
dist.plts.all <- rbind(dist.plts.all, dist23)

## fpd5
hyperspectral_data <- stack(fls_hps[24])
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
dist24$plotID <- "fpd5"
dist.plts.all <- rbind(dist.plts.all, dist24)

## fpo0
hyperspectral_data <- stack(fls_hps[25])
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
dist25$plotID <- "fpo0"
dist.plts.all <- rbind(dist.plts.all, dist25)

## fpo1
hyperspectral_data <- stack(fls_hps[26])
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
dist26$plotID <- "fpo1"
dist.plts.all <- rbind(dist.plts.all, dist26)

## fpo2
hyperspectral_data <- stack(fls_hps[27])
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
dist27$plotID <- "fpo2"
dist.plts.all <- rbind(dist.plts.all, dist27)

## fpo3
hyperspectral_data <- stack(fls_hps[28])
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
dist28$plotID <- "fpo3"
dist.plts.all <- rbind(dist.plts.all, dist28)

## fpo4
hyperspectral_data <- stack(fls_hps[29])
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
dist29$plotID <- "fpo4"
dist.plts.all <- rbind(dist.plts.all, dist29)

## fpo5
hyperspectral_data <- stack(fls_hps[30])
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
dist30$plotID <- "fpo5"
dist.plts.all <- rbind(dist.plts.all, dist30)

## hel1
hyperspectral_data <- stack(fls_hps[31])
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
dist31$plotID <- "hel1"
dist.plts.all <- rbind(dist.plts.all, dist31)

# hel2
hyperspectral_data <- stack(fls_hps[32])
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
dist32$plotID <- "hel2"
dist.plts.all <- rbind(dist.plts.all, dist32)

# hel3
hyperspectral_data <- stack(fls_hps[33])
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
dist33$plotID <- "hel3"
dist.plts.all <- rbind(dist.plts.all, dist33)

# hel4
hyperspectral_data <- stack(fls_hps[34])
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
dist34$plotID <- "hel4"
dist.plts.all <- rbind(dist.plts.all, dist34)

dist.plts.all.tst <- dist.plts.all

write.csv2(dist.plts.all.tst, 
           file = "clean_norm441_pcainverse_sd_distCent_2nd.csv", 
           row.names = FALSE)
