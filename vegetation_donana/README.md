# Forecasting vegetation dynamics in Doñana National Park

**Workshop designers**: Billur Bektaş, Patrícia Singh, Mary E Lofton, Freya Olsson, Maria Paniw

**Contributors**: María Teresa Mejia Sánchez, Matthew Clements, Francisco Lloret

## Background

The workshop uses data from long-term monitoring of shrub abundance and cover in Doñana National Park. The Doñana area is located in southwestern Spain around the Guadalquivir estuary, although the original marshland in the left bank of the river has been totally lost (Fig. 1). Its ecological value has been recognised for centuries, and the Doñana National Park was created in 1969; it was classified as a UNESCO Biosphere Reserve in 1980 and as a UNESCO World Heritage Site in 1994. The Doñana Natural Area, corresponding to the Biosphere Reserve, includes the National Park, the Natural Park, formed by some peripheral areas of different habitats surrounding the National Park, and areas with lower protection in contact with agricultural areas (Fig. 1). It is considered a Mediterranean-climate region with a subhumid climate characterized by variable rainy seasons historically centred in autumn and spring but shifting towards winter over the last decades, and hot and dry summers. Doñana integrates a large variety of terrestrial and aquatic ecosystems, ranging from pine and cork oak forests to shrublands, grasslands, sand dunes, and marshlands with different levels of salinity. This mosaic of habitats is the main reason for its great biodiversity.

![Doñana](https://github.com/MariaPaniw/workshops_EFFI/blob/main/vegetation_donana/Figures/Donana.png)

**Figure 1**. Distribution of main land uses and land covers in the Doñana Natural Area and surrounding
areas (Figure by LAST-EBD/CSIC 2014, with permission)

## Biodiversity monitoring

The Biological Research Station, operated by the [ICTS-RBD](https://www.ebd.csic.es/icts-rbd), has been continuously monitoring biodiveristy in Doñana for decades (along with other measurements, such as satellite derived [indices](https://www.ebd.csic.es/servicios/laboratorio-sig-y-teledeteccion)), and has attracted research projects from all over the world. One of these projects is the long-term monitoring of shrubs, which was started by [Paco Lloret](https://www.creaf.cat/en/about-us/our-people/francisco-lloret-maya) (CREAF, Spain) in 2007 after an extreme drought event in the 2005 hydrological year. 

In order to assess community composition before, during, and after the severe drought event, 18 permanent plots of 25 m2 each were established in November 2007 (two years after the drought) on a gradient of drought impact. The plots were located at three sites (with six plots per site): Raposo, Marquas, and Ojillo. To avoid spatial autocorrelation, all plots were separated by at least 50 m from each other. Species plant cover was estimated from contacts with branches along transects within plots; these contacts were divided into two categories corresponding to living or dead canopy. Relative abundance of each species per plot in years after the extreme drought was calculated as the proportion of their contacts of living canopy relative to the sum of the contacts of living canopy of all species. Similarly, the total vegetation cover per plot was calculated as the summed contacts of living canopy of all species. Since 2007, the plots have been visited regularly (annually since 2019), and the project has expanded. The monitroing is now led by María Teresa Tejia Sánchez.

More recently (since 2023), another project, led by Matthew Celments, started shrub vegetation surveys at the landscape level in the Reserve. Mattew has conducted structured density surveys of the dominant shrub species (_Halimium halimifolium_, _Lavandula stoechas_) in 10x10m grids every 100 meters over 5km along five transects, yielding 250 estimates of density status.

![Monitoring](https://github.com/MariaPaniw/workshops_EFFI/blob/main/vegetation_donana/Figures/shrub_monitoring_plots.png)

**Figure 2**. Distribution of shrub monitoring plots in the Biological Reserved in Doñana National Park.

## Forecasting abundance change of shrubs

The rasters (30 by 30 m resolution) we use are provided by the geospatial laboratory (LAST) at EBD CSIC:

   Díaz-Delgado, R., Afan, I., Aragones, D., Garcia, D., & Bustamante, J. (2019). NDVIs Doñana 1984/2019 (v1.0) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.3518879

There are 5 steps in the analysis:

1. Download the NDVI data for the entire Doñana region from Zenodo to a local folder. 

2. Use the script _process_ndvi.R_ to subset the raster files (which are pretty large) to the borders of the Doñana Biological Reserve and save in a local folder.
 
3. Use the script _main_analyses_donana.R_ to:
   
   3.1 Load the temporal series of the raster as well as the coordinates (converted into spatial points) from the center of the monitoring plots.
   
   3.2 Extract the value of the raster at a given point, and use it as a plot-specific covariate value

   **Note**: You can skip the previous steps and load all the data (preprocessed by us) directly.

   3.3 Develop the statistical model to quantify shrub abundance as a function of the covariate

   3.4 We do the forecast and assess skill
