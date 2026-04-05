library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)
library(tidyterra)

#read data
#from 31.12.1981 to 31.12.2018

LAI_spatial <- metR::ReadNetCDF("arctic_data/spatial/LAI_AVHRR_global.nc",
                    out = "data.frame")

#spatial data in northern latidues for 2011
LAI_2011 <- LAI_spatial |> dplyr::filter((as.Date(time) == as.Date("2011-12-31")) & (latitude >= 55))


ggplot(LAI_2011, aes(longitude, latitude, fill = LAI)) +
  geom_raster() +
  scale_fill_viridis_c(na.value = "white", name = "LAI") +
  coord_fixed() +
  labs(title = "LAI: Latitudes >= 55, 2011-12-31")
