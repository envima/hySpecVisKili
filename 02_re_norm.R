library(raster)
library(foreach)
library(doParallel)

cl <- 6
registerDoParallel(cl)

setwd("/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd/norm_441/")
filepath_raster <- "/media/sd19006/data/users/iotte/R-Server/hyperspectral/clean/2nd/norm_441/"

#read files
fls_hps <- list.files(paste0(filepath_raster), full.names = FALSE,
                      recursive = FALSE, pattern = "*.tif")

odr <- file.path(unique(dirname(fls_hps)), "re_norm")
ofl <- file.path(odr, gsub(".tif$", "_re.tif", basename(fls_hps)))

#stack files
fls_hps_stck <- lapply(seq(fls_hps), function(i){
  stack(fls_hps[[i]])
})

#define function
fun <- function(x){
  ((x + 1)/2)
}

#calc
re_norm <- foreach(i = seq(fls_hps_stck), .packages = "raster") %dopar% {
  calc(fls_hps_stck[[i]], fun)
}

#write
foreach(i = seq(re_norm), .packages = "raster") %dopar% {
  writeRaster(re_norm[[i]], filename = ofl[i], overwrite = TRUE)
}

-----------------------------------
z = sav5[[10]]
r = sav5[[74]]
n = sav5[[107]]
df = data.frame(r = getValues(r),
                n = getValues(n),
                z = getValues(z))

df$rnorm = (df$r-df$z)/(df$r+df$z)
df$nnorm = (df$n-df$z)/(df$n+df$z)
df$rnormnorm = (df$rnorm+1)/2
df$nnormnorm = (df$nnorm+1)/2
head(df[df$rnorm<0,])

df$r_vs_n = df$r / df$n
df$rnorm_vs_nnorm = df$rnorm / df$nnorm
df$rnormnorm_vs_nnormnorm = df$rnormnorm / df$nnormnorm

plot(df$r_vs_n, df$rnormnorm_vs_nnormnorm)
plot(df$r, df$rnormnorm)
plot(df$r, df$rnorm)



summary(df)

r = 0.2
n = 0.6

rn = r /0.3
nn = n /0.3

(n-r)/(n+r)
(nn-rn)/(nn+rn)

df = data.frame(df,
                ndvi = (df$n-df$r)/(df$n+df$r))
head(df)
summary(df$ndvi)
df[!is.na(df$ndvi) & (df$ndvi < -1 | df$ndvi > 1), ]

s0 = lapply(sav5, function(l){
  data.frame(l = l,
             s0 = sum(getValues(l) < 0))
})
s0 = do.call("rbinc", s0)
s0
