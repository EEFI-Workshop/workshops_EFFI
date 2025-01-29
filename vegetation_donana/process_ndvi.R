
#### Script to cut large NDVI raster for entire Doñana Area to the size of the Doñana Biological Reserve
#### Author: Maria Paniw
### Date: 07 January 2025

library(sf)
library(terra)
library(raster)
library(dplyr)
library(ggplot2)
library(readr)

# Set your working directory
setwd("/Users/maria/Dropbox/collaborations/EEFI/workshop")

## The script has the following sections:
# 1. Load and cut NDVI rasters to polygon deliniating reserve
# 2. Load the spatial data. I load the coordinates of the monitoring plots (and convert them into spatial points) and a series satellite-based NDVI raster data from a local drive 
# The rater data are available on GitHub, and will be made available via a WCS client (a separate script using a WCS client is avaiable on GitHub)
# 3. Process the spatial data: I get the value of the raster at a given point to be used as a covariate to predict abundance change 

############### 
#1. LOAD AND CUT NDVI TO RESERVE POLYGON

ndvi.names <- list.files("NDVI", pattern="*.tif")

crdref <- "+proj=utm +zone=30 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"

newcrs <- "+proj=utm +zone=29 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"

for(i in 1:length(ndvi.names)){
  # Load each rater
  temp <- raster(paste0("NDVI/", ndvi.names[i]))
  
  #Cut exactly to the shape of the reserve (res)
  temp2 <- crop(temp, extent(res))
  temp3 <- mask(temp2, res)
  
  # Deine CRS
  crs(temp3) <- crdref
  
  # Project to 29 zone as this is what the other data are in
  temp_p=projectRaster(temp3,crs=newcrs)
  
  writeRaster(temp_p,paste0("NDVI_reserve/",ndvi.names[i]))
}

############### 
#2. LOAD SPATIAL DATA

### Load location of the study plots with abundance monitoring since 2007:
### Reference system is WGS84
# coords_long=read_csv(url("https://raw.githubusercontent.com/MariaPaniw/workshops_EFFI/refs/heads/main/vegetation_donana/coords_plot_since2007.csv?token=GHSAT0AAAAAAC2TAOO4TOA3HUOY7SYFFRE4Z3RUREQ"))

coords_long <- read.csv("coords_plot_since2007.csv")

crdref <- "+proj=longlat +datum=WGS84"
pts_long <- vect(cbind(coords_long$Long,coords_long$Lat), atts=coords_long[,c("ID","Elevation")],crs=crdref)
pts_long

newcrs <- "+proj=utm +zone=29 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"

rob <- project(pts_long, newcrs)

### Load location of the landscape plots (monitored since 2023):
### Reference system is WGS84
# ab_land=read_csv(url("https://raw.githubusercontent.com/MariaPaniw/workshops_EFFI/refs/heads/main/vegetation_donana/coordinates_2023_02.csv?token=GHSAT0AAAAAAC2TAOO4TOA3HUOY7SYFFRE4Z3RUREQ"))

ab_land <- read.csv("coordinates_2023_02.csv")

ab_land$ID <- factor(paste(ab_land$lon,ab_land$lat))

levels(ab_land$ID) = 1:length(levels(ab_land$ID))

ab_land <- droplevels(ab_land[ab_land$ID!=c("119","120"),])
ab_land <- droplevels(ab_land[ab_land$ID!=c("119","120"),])

coords_land <- ab_land[!duplicated(ab_land$ID),] 

crdref <- "+proj=longlat +datum=WGS84"

pts_land <- vect(cbind(coords_land$lon,coords_land$lat), atts=coords_land[,c("ID","week")],crs=crdref)
pts_land

newcrs <- "+proj=utm +zone=29 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"

rob_land <- project(pts_land, newcrs)

### Load the NDVI maps 

ndvi.names <- list.files("NDVI_mask_reserva_new", pattern="*.tif")

# Only uncomment below if you have auxiliary files in the folder

# ndvi.names <- ndvi.names[-which(stringr::str_detect(ndvi.names,'aux.xml')==T)]

############### 
#3. GET SPATIAL COVARIATE DATA AT POINT COORDINATES

### go through maps OR load the data frame (that was processed before): 
df.ndvi=NULL 

df.ndvi_land=NULL 

for(i in 1:length(ndvi.names)){
  
  print(paste("Running map ",i))
  
  cov_data <- raster(paste0("NDVI_mask_reserva_new/", ndvi.names[i]))# use forlder namer from step 1 
  
  year=substr(ndvi.names[i],6,9)
  month=substr(ndvi.names[i],10,11)
  day=substr(ndvi.names[i],12,13)
  
  #Get NDVI value for point coordinates
  df.ndvi=rbind(df.ndvi,data.frame(plot=rob$ID,ndvi=extract(cov_data, st_as_sf(rob)),year,month,day))
  df.ndvi_land=rbind(df.ndvi_land,data.frame(plot=rob_land$ID,ndvi=extract(cov_data, st_as_sf(rob_land)),year,month,day))
  
}

# Check if everything ran well! 

hist(df.ndvi$ndvi)

unique(df.ndvi$year)
unique(df.ndvi$month)
unique(df.ndvi$day)

write.csv(df.ndvi,"df.ndvi.csv",row.names = F)

write.csv(df.ndvi_land,"df.ndvi_land.csv",row.names = F)
