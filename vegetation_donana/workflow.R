
# Get working directory
library(here)
here()

# Load all packages for the analysis ----
source(here("load_packages.R"))

# If you want to run the app locally: ----
here::i_am("workflow.R")
runApp(here())

# If you want to perform the analysis yourself: -----

## Get NDVI rasters and get NDVI time-series for the shrub plots ----
if(!file.exists(here("df.ndvi.csv"))){
  source(here("process_ndvi.R"))
}else{
  cat("NDVI is already processed.")
}

## Get NDVI metric calculations ----
if(!file.exists(here("ndvi_metrics.csv"))){
  source(here("ndvi_indices_calculations.R"))
}else{
  cat("NDVI metrics are already calculated.")
}

## Get climate data, future climate data and predict NDVI in the future ----
if(!file.exists(here("ndvi_predictions.rds"))){
  source(here("ndvi_predictions.R"))
}else{
  cat("NDVI predictions are already calculated.")
}

## Forecast shrub abundances with NDVI predictions ----
source(here("run_predictions_shrubs.R"))

## Run a single model ----
run_eg = run_model_combination(ndvi_metric = "integrated_ndvi", # NDVI metric as the explanatory variable for the shrub model
                               scenario = "ssp370", # Future scenario with which we predict future NDVI
                               bioclim = "bio1",  # Climatic variable with which we predict future NDVI
                               model = "lm" # Model with which we predict the future NDVI
                               )

## Run all the model combinations for temporal predictions ----

if(!file.exists(here("observed_totals.rds"))){
# Load future observations data
num_fut <- read.csv(here("shrub_number_2324.csv"), sep=" ")
colnames(num_fut)[4:6] <- c("adults", "saplings", "seedlings")
sub_fut <- num_fut[num_fut$species %in% c("Halimium halimifolium", "Lavandula stoechas"), ]

# Calculate observed totals
# For Lavandula
lav_obs <- aggregate(adults~year, 
                     data=sub_fut[sub_fut$species=="Lavandula stoechas",], 
                     function(x) sum(x,na.rm=T))
lav_obs$species <- "Lavandula stoechas"
lav_obs$tot <- lav_obs$adults
lav_obs$year <- as.numeric(as.character(lav_obs$year))

# For Halimium
hal_obs <- aggregate(adults~year, 
                     data=sub_fut[sub_fut$species=="Halimium halimifolium",], 
                     function(x) sum(x,na.rm=T))
hal_obs$species <- "Halimium halimifolium"
hal_obs$tot <- hal_obs$adults
hal_obs$year <- as.numeric(as.character(hal_obs$year))

# Combine observed data
observed_totals <- rbind(
  lav_obs[, c("year", "species", "tot")],
  hal_obs[, c("year", "species", "tot")]
)

# Save observed data
saveRDS(observed_totals, file = here("observed_totals.rds"))
}else{
  observed_totals = readRDS(here("observed_totals.rds"))
}

# Define parameter combinations - predictions and forecasting skill are saved to "model_runs".
ndvi_metrics <- c("integrated_ndvi", "winter_spring_integrated")
scenarios <- c("ssp370", "ssp245", "ssp585")
bioclims <- c("bio1", "bio12", "bio9", "bio18", "bio1_bio12", "bio1_bio9", 
              "bio1_bio18", "bio12_bio9", "bio12_bio18", "bio9_bio18", "bio1_bio12_bio9", 
              "bio1_bio12_bio18", "bio1_bio9_bio18", "bio12_bio9_bio18", "bio1_bio12_bio9_bio18"
)
models <- c("lm", "rf", "gam")

# Create empty list to store results
all_predictions <- list()
counter <- 1

# Run analysis for all combinations
for(metric in ndvi_metrics) {
  for(scen in scenarios) {
    for(bio in bioclims) {
      for(mod in models) {
        
        pred_file = here("model_runs",
                         sprintf("model_predictions_%s_%s_%s_%s.rds", 
                                 metric, scen, bio, mod))
        
        if(!file.exists(pred_file)){
          cat(sprintf("Running combination %s, %s, %s, %s\n", metric, scen, bio, mod))
          
          # Run model
          results <- try(run_model_combination(metric, scen, bio, mod))
          
          if(!inherits(results, "try-error")) {
            # Prepare predictions dataframe (already includes MSE)
            shrub_predictions <- rbind(
              results$predictions$halimium,
              results$predictions$lavandula
            )
            
            # Save predictions (which now include MSE values)
            saveRDS(shrub_predictions, 
                    file = here("model_runs",
                                sprintf("model_predictions_%s_%s_%s_%s.rds", 
                                        metric, scen, bio, mod)))
          }
        } else {
          cat(sprintf("Combination %s, %s, %s, %s\n", metric, scen, bio, mod), " already exists!")
        }
      }
    }
  }
}

## Run the models for spatial projections ----
source(here("run_spatial_predictions.R"))
ndvi_metrics <- c(
  "annual_mean",
  "annual_median", 
  "integrated_ndvi",
  "summer_integrated",
  "summer_max",
  "summer_mean",
  "summer_median",
  "winter_spring_integrated",
  "winter_spring_max",
  "winter_spring_mean",
  "winter_spring_median"
)

# Create list to store results
spatial_predictions <- list()

# Run predictions for each metric
for(metric in ndvi_metrics) {
  cat(sprintf("\nProcessing %s...\n", metric))
  
  # Try to run predictions
  tryCatch({
    results <- run_spatial_predictions(metric)
    
    # Store results
    spatial_predictions[[metric]] <- results
    
    cat(sprintf("Successfully processed %s\n", metric))
  }, error = function(e) {
    cat(sprintf("Error processing %s: %s\n", metric, e$message))
  })
}

# Save all predictions to RDS file
saveRDS(spatial_predictions, file = here("spatial_predictions","spatial_predictions.rds"))

## Start the shiny app locally ----
runApp('vegetation_donana')

