
library(ows4R)
library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(readr)

# Set your working directory
setwd("/Users/maria/Dropbox/collaborations/EEFI/workshop")

## The script has the following sections:

# 1. Load the spatial data. I load the coordinates of the monitoring plots (and convert them into spatial points) and a series placeholder flooding satellite-based rasters using a WCS client
# 2. Process the spatial data: I get the value of the raster at a given point to be used as a covariate to predict abundacne change 
    # NOTE: Because flooding values are 0, I created a placeholder covariate: density of interspecific neighbors in a 5x5m plot
# 3. Model abundance change. Here, I just use a simple autoregerssive model done in JAGS
# 4. Do the forecast and quantify forecast skill
############### 
#1. LOAD SPATIAL DATA

### Load location of the study plots with abundace monitoring since 2007:
### Reference system is WGS84
coords_long=read_csv(url("https://raw.githubusercontent.com/MariaPaniw/workshops_EFFI/refs/heads/main/vegetation_donana/coords_plot_since2007.csv?token=GHSAT0AAAAAAC2TAOO4TOA3HUOY7SYFFRE4Z3RUREQ"))

crdref <- "+proj=longlat +datum=WGS84"
pts_long <- vect(cbind(coords_long$Long,coords_long$Lat), atts=coords_long[,c("ID","Elevation")],crs=crdref)
pts_long

newcrs <- "+proj=utm +zone=29 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"

rob <- project(pts_long, newcrs)

### Load the hydro period maps
WCS <- WCSClient$new("https://icts-donana.es/geoserver/hidroperiodov1/wms", "2.0.1")
caps <- WCS$getCapabilities()


############### 
#2. GET SPATIAL COVARIATE DATA AT POINT COORDINATES

### go through maps (37 years): 
df.hydro=NULL 

for(i in 1:37){
 
  print(paste("Running year ",i))
  name=caps$getCoverageSummaries()[[i]]$CoverageId # ID of a map
  
  hydro_map <- caps$findCoverageSummaryById(name, exact = T)
  hydro_map_dims <- hydro_map$getDimensions()
  hydro_map_des <- hydro_map$getDescription()
  
  
  cov_data <- hydro_map$getCoverage(
    # bbox = OWSUtils$toBBOX(700000,408000,740000, 4130000),
    # time = hydro_map_dims[[1]]$uom[[1]],  
    # filename = paste("/Users/maria/Dropbox/collaborations/EEFI/workshop/maps/hydro/hydro_",i,".tiff",sep="")
    
  )
  
  #Get hydrology value for point coordinates
  df.hydro=rbind(df.hydro,data.frame(plot=rob$ID,hydroperiod=extract(cov_data, rob, sp = T)[,2],year=i))
}

unique(df.hydro$hydroperiod)
unique(df.hydro$year)

df.hydro[is.na(df.hydro$hydroperiod),]

df.hydro$year=factor(df.hydro$year)
levels(df.hydro$year)=1985:2022

plot(cov_data,xlim = c(719000,722100),
     ylim = c(4097200, 4099400))

points(rob)

plot(pts_long)

