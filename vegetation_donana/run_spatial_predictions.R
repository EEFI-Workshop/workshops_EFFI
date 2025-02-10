run_spatial_predictions <- function(ndvi_metric) {
  # Run model to get parameters
  temporal_model <- run_spatial_model(ndvi_metric)
  
  ndvi_raster <- rast(here("ndvi_metrics_2024", paste0(ndvi_metric, "_2024.tif")))
  
  # Create empty rasters for predictions
  pred_raster_hal <- ndvi_raster
  pred_raster_lav <- ndvi_raster
  
  # Get model parameters from temporal run
  par.sub <- sample(1:length(temporal_model$mcmc$sims.list$a0.h), 1000)
  
  # Function to make predictions for one cell or vector of cells
  predict_cell <- function(ndvi_values, species = "halimium") {
    predictions <- rep(NA, length(ndvi_values))  # Initialize output vector
    
    # Skip if all values are NA
    if(all(is.na(ndvi_values))) return(predictions)
    
    # Process only non-NA values
    valid_idx <- !is.na(ndvi_values)
    
    if (species == "halimium") {
      # Use observed 2023 abundance as initial density
      
      # Get MCMC parameters
      a0.h <- temporal_model$mcmc$sims.list$a0.h[par.sub]
      a1.h <- temporal_model$mcmc$sims.list$a1.h[par.sub]
      a2.h <- temporal_model$mcmc$sims.list$a2.h[par.sub]
      
      # Make predictions for valid cells
      valid_predictions <- sapply(ndvi_values[valid_idx], function(ndvi_value) {
        # Make predictions for each MCMC iteration
        N.h.initial <- sample(temporal_model$observed_2023$halimium, 1)
        print(N.h.initial)
        cell_predictions <- sapply(1:length(par.sub), function(i) {
          # Calculate components
          ndvi_component <- a1.h[i] * ndvi_value
          density_component <- a2.h[i] * log(N.h.initial + 0.001)
          log_expected <- a0.h[i] + ndvi_component + density_component
          expected_abundance <- exp(log_expected)
        })
        
        median(cell_predictions)
      })
      
      predictions[valid_idx] <- valid_predictions
      return(predictions)
      
    } else {
      # Use observed 2023 abundance as initial density
      
      # Get MCMC parameters
      a0.l <- temporal_model$mcmc$sims.list$a0.l[par.sub]
      a1.l <- temporal_model$mcmc$sims.list$a1.l[par.sub]
      a2.l <- temporal_model$mcmc$sims.list$a2.l[par.sub]
      
      # Make predictions for valid cells
      valid_predictions <- sapply(ndvi_values[valid_idx], function(ndvi_value) {
        # Because we do not know the spatial 2023 shrub abundance across the whole landscape we sample randomly from the observed abundance
        N.l.initial <- sample(temporal_model$observed_2023$lavandula, 1)
        
        # Make predictions for each MCMC iteration
        cell_predictions <- sapply(1:length(par.sub), function(i) {
          # Calculate components
          ndvi_component <- a1.l[i] * ndvi_value
          density_component <- a2.l[i] * log(N.l.initial + 0.001)
          log_expected <- a0.l[i] + ndvi_component + density_component
          expected_abundance <- exp(log_expected)
        })
        
        median(cell_predictions)
      })
      
      predictions[valid_idx] <- valid_predictions
      return(predictions)
    }
  }
  
  # Get all raster values at once
  hal_predictions <- predict_cell(values(ndvi_raster), "halimium")
  lav_predictions <- predict_cell(values(ndvi_raster), "lavandula")
  
  # Assign predictions back to rasters
  values(pred_raster_hal) <- hal_predictions
  values(pred_raster_lav) <- lav_predictions
  
  # Save predictions
  if (!dir.exists(here("spatial_predictions"))) {
    dir.create(here("spatial_predictions"))
  }
  
  writeRaster(pred_raster_hal, 
              filename = here("spatial_predictions", 
                              sprintf("halimium_%s.tif", ndvi_metric)),
              overwrite = TRUE)
  
  writeRaster(pred_raster_lav, 
              filename = here("spatial_predictions", 
                              sprintf("lavandula_%s.tif", ndvi_metric)),
              overwrite = TRUE)
  
  # Return the prediction rasters
  return(list(
    halimium = pred_raster_hal,
    lavandula = pred_raster_lav
  ))
}
