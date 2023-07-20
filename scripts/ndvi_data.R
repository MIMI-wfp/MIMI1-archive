### NDVI data

library(terra)
s <- rast("../data/VIPPHEN_NDVI.A2011.004.2018171122539.hdf")
s

y <- t(s)
z <- flip(y, "v")
ext(z) <- c(-180, 180, -50, 50)
z
