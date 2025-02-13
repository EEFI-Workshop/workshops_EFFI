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

# NDVI Analysis and Vegetation Prediction Workflow

This repository contains a collection of R scripts for analyzing NDVI (Normalized Difference Vegetation Index) data and predicting vegetation patterns in Doñana. The workflow processes satellite imagery, calculates various NDVI metrics, and uses these to predict shrub abundance under different climate scenarios.

## Script Overview

### 1. workflow.R
Main workflow script that orchestrates the entire analysis pipeline by calling other scripts in sequence. Controls the execution flow and checks for existing outputs to avoid redundant processing.

**Key Variables:**
- `ndvi_metrics`: List of NDVI metrics (integrated_ndvi, winter_spring_integrated, etc.)
- `scenarios`: Climate scenarios (ssp370, ssp245, ssp585)
- `bioclims`: Bioclimatic variables (bio1, bio12, bio9, etc.)
- `models`: Model types (lm, rf, gam)

### 2. load_packages.R
Handles all package dependencies for the project by checking, installing, and loading required R packages. Includes configuration for conflict resolution between packages.

| Function | Input | Output |
|----------|--------|---------|
| install_and_load_packages() | List of required package names | Loaded packages in R environment |

**Key Variables:**
- `required_packages`: List of package names to be installed and loaded
- `missing_packages`: Packages that need to be installed

### 3. process_ndvi.R
Processes raw NDVI raster data by cutting it to the size of the Doñana Biological Reserve and extracting values for monitoring plots. Converts spatial data between coordinate systems and saves processed NDVI values.

| Function | Input | Output |
|----------|--------|---------|
| N/A (Script only) | NDVI raster files (.tif), Plot coordinates | df.ndvi.csv, df.ndvi_land.csv |

**Key Variables:**
- `ndvi.names`: List of NDVI raster file names
- `coords_long`: Coordinates of study plots
- `crdref`: Reference coordinate system
- `newcrs`: Target coordinate system
- `df.ndvi`: Processed NDVI data frame
- `df.ndvi_land`: Processed NDVI data for landscape plots

### 4. ndvi_raster_indices_calculations.R
Calculates various NDVI metrics from raster data including seasonal and annual indices. Processes NDVI data cell by cell with BISE correction and interpolation.

| Function | Input | Output |
|----------|--------|---------|
| process_cell_ndvi() | NDVI values, dates | Processed daily NDVI values |
| calculate_cell_metrics() | Processed NDVI dataframe | NDVI metrics |
| process_raster_ndvi() | Raster files | Processed raster stack |
| main() | NDVI raster files | Metric rasters in ndvi_metrics_2024/ |

**Key Variables:**
- `ndvi_values`: Raw NDVI values
- `ndvi_processed`: BISE-corrected NDVI values
- `ndvi_interpolated`: Interpolated NDVI values
- `ndvi_smoothed`: Smoothed NDVI values
- `result_stack`: Final raster stack with all metrics
- `metrics`: List of calculated metrics (int.NDVI, annual_mean, etc.)

### 5. ndvi_indices_calculations.R
Calculates NDVI metrics for individual plots and analyzes trends over time. Creates visualizations of NDVI changes and calculates significance of trends.

| Function | Input | Output |
|----------|--------|---------|
| process_ndvi_data() | Plot NDVI data | Processed NDVI time series |
| get_slope_significance() | Time series data | Slope and p-value |
| calculate_ndvi_metrics() | Processed data | NDVI metrics by plot and year |
| calculate_trends() | NDVI metrics | Trend analysis results |

**Key Variables:**
- `df_ndvi`: Raw NDVI data frame
- `processed_data`: List of processed NDVI data
- `all_processed_data`: Combined processed data
- `plot_significance`: Significance of NDVI trends
- `ndvi_metrics`: Calculated NDVI metrics
- `trends`: Trend analysis results

### 6. ndvi_calculation_functions.R
Contains core functions for NDVI data processing, including BISE correction, interpolation, and metric calculations. Includes functions for both temporal and spatial analyses.

| Function | Input | Output |
|----------|--------|---------|
| process_ndvi_data() | Raw NDVI dataframe | Processed NDVI dataframe |
| get_slope_significance() | NDVI data | Statistical results |
| calculate_ndvi_metrics() | NDVI data | Calculated metrics |
| calculate_trends() | Metrics data | Trend analysis |
| get_future_clim() | Coordinates, scenario | Future climate predictions |
| calculate_bioclim_vars() | Climate data | Bioclimatic variables |

**Key Variables:**
- `raw_ndvi_df`: Input NDVI data
- `ndvi_metrics`: Calculated metrics
- `trends`: Trend analysis results
- `monthly_stats`: Monthly NDVI statistics
- `quarterly_stats`: Quarterly NDVI statistics
- `bioclim_vars`: Bioclimatic variables

### 7. ndvi_predictions.R
Generates predictions of future NDVI values based on climate scenarios. Processes both current and future climate data to predict NDVI patterns.

| Function | Input | Output |
|----------|--------|---------|
| main() | Climate data, NDVI metrics | ndvi_predictions.rds |
| get_all_futures() | Scenario data | future_climate_all.csv |

**Key Variables:**
- `coords_long`: Plot coordinates
- `present_climate`: Current climate data
- `present_bioclim`: Current bioclimatic variables
- `future_climate`: Predicted future climate data
- `ndvi_predictions`: Predicted NDVI values
- `ndvi_plot`: Visualization data

### 8. run_spatial_model.R
Implements spatial modeling of vegetation patterns using NDVI metrics. Uses JAGS for Bayesian modeling of spatial relationships.

| Function | Input | Output |
|----------|--------|---------|
| run_spatial_model() | NDVI metric | Model results and 2023 observations |

**Key Variables:**
- `C.hal`, `C.lav`: Abundance matrices for Halimium and Lavandula
- `cov.hal`, `cov.lav`: Covariate matrices
- `bdata`: Bundled data for JAGS model
- `params`: Model parameters to monitor
- `out1`: JAGS model output

### 9. run_spatial_predictions.R
Generates spatial predictions of shrub abundance using model parameters from temporal analysis. Creates prediction rasters for different species.

| Function | Input | Output |
|----------|--------|---------|
| run_spatial_predictions() | NDVI metric | Prediction rasters for species |

**Key Variables:**
- `temporal_model`: Model from temporal analysis
- `ndvi_raster`: Input NDVI raster
- `pred_raster_hal`, `pred_raster_lav`: Prediction rasters
- `par.sub`: Sampled parameters
- `hal_predictions`, `lav_predictions`: Species predictions

### 10. run_predictions_shrubs.R
Executes predictions of shrub abundance using various model combinations and NDVI metrics. Implements Bayesian models for abundance predictions.

| Function | Input | Output |
|----------|--------|---------|
| run_model_combination() | Model parameters | Prediction results and parameters |

**Key Variables:**
- `params`: Model parameters (ndvi_metric, scenario, bioclim, model)
- `C.hal`, `C.lav`: Abundance matrices
- `n.hal.pred`, `n.lav.pred`: Prediction arrays
- `mse.hal`, `mse.lav`: Mean square error arrays
- `halimium_predictions`, `lavandula_predictions`: Final predictions

## Data Flow
1. Raw NDVI rasters → Processed NDVI values
2. Processed NDVI → NDVI metrics
3. NDVI metrics + Climate data → Future predictions
4. Predictions → Spatial and temporal abundance estimates

## Dependencies
See load_packages.R for a complete list of required R packages and their dependencies.

