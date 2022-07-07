library(raster)
library(rgdal)
library(sp)
library(sf)
library(tidyverse)

## read in the land cover layer database released by MASSGIS
NLCD<-raster('C:/Users/funke/Documents/GitHub/bumblebee_spatial/data/NLCD30m/NLCD_2016_LULC_MA.tif')
NLCD<-as.factor(NLCD)
levels(NLCD)

IMPSURF<-raster('C:/Users/funke/Downloads/impsurf.tif')

## Read in the data (I converted it to a shapefile and reprojected it in ArcMap) 
data<-st_read("C:/Users/funke/Downloads/data1.shp")

## plot it in a map
plot(NLCD, ext=extent(data))
plot(data, add=T, pch = 1, col="black",cex=2, lwd=2)


#Extract the Land use values. 
ex.mat.r = raster::extract(NLCD,
                           data,
                           buffer = 100, #100m buffer around site
                           df = T) # raster extract
ex.mat.r <- as.data.frame(ex.mat.r)
colnames(ex.mat.r)[2] <- "lc_type" # rename for ease
colnames(ex.mat.r)[1] <- "ID" # rename for ease

attach(ex.mat.r)

prop.lc = ex.mat.r %>%
  group_by(ID, lc_type) %>%               # group by point (ID) and lc class 
  summarise(n = n()) %>%                  # count the number of occurrences/class
  mutate(pland = n / sum(n)) %>%          # calculate percentage
  ungroup() %>%                           # convert back to original form
  dplyr::select(ID, lc_type, pland) %>%   # keep only these vars
  spread(lc_type, pland)                  # convert to long format

colnames(prop.lc) = c(
  "Site",
  "11:Open Water",
  "21:Developed, Open Space",
  "22:Developed, Low Intensity",
  "23:Developed, Medium Intensity",
  "24:Developed, High Intensity",
  "31:Barren Land",
  "41:Deciduous Forest",
  "42:Evergreen Forest",
  "43:Mixed Forest",
  "52:Shrub/Scrub",
  "71:Herbaceous",
  "81:Hay/Pasture",
  "82:Cultivated Crops",
  "90:Woody Wetlands",
  "95:Emergent Herbaceous Wetlands")

total <- cbind(data,prop.lc)

ex.mat.r2 = raster::extract(IMPSURF,
                           data,
                           buffer = 100, #100m buffer around site
                           df = T) # raster extract
ex.mat.r2 <- as.data.frame(ex.mat.r2)
colnames(ex.mat.r2)[2] <- "impsurf_type" # rename for ease
colnames(ex.mat.r2)[1] <- "ID" # rename for ease


attach(ex.mat.r2)

prop.lc2 = ex.mat.r2 %>%
  group_by(ID, impsurf_type) %>%               # group by point (ID) and lc class 
  summarise(n = n()) %>%                  # count the number of occurrences/class
  mutate(pland = n / sum(n)) %>%          # calculate percentage
  ungroup() %>%                           # convert back to original form
  dplyr::select(ID, impsurf_type, pland) %>%   # keep only these vars
  spread(impsurf_type, pland)                  # convert to long format

colnames(prop.lc2) = c(
  "Site","0:PervSurf",
  "1:ImpSurf")

total <- cbind(total,prop.lc2)
