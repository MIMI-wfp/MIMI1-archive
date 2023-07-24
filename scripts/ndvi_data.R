### NDVI data

library(terra)
s <- sds("../data/VIPPHEN_NDVI.A2011.004.2018171122539.hdf")
s
r <- s$VIPPHEN_NDVI.A2011.004.2018171122539
r

sds_info("../data/VIPPHEN_NDVI.A2011.004.2018171122539.hdf")

plot(r)
