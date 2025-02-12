# NDVI Raster Processing Script
library(terra)
library(tidyverse)
library(lubridate)
library(here)
library(signal)
library(zoo)

# Function to apply BISE correction and interpolation to a single cell
process_cell_ndvi <- function(ndvi_values, dates) {
  # Initial validation
  if(all(is.na(ndvi_values))) {
    return(data.frame(
      date = dates,
      int.NDVI = NA
    ))
  }
  
  # Ensure values are numeric and handle any special values
  ndvi_values <- as.numeric(ndvi_values)
  ndvi_values[ndvi_values == 255] <- NA  # Handle no-data values
  
  # Validate NDVI range
  if(any(!is.na(ndvi_values) & (ndvi_values < -1 | ndvi_values > 1))) {
    warning("Found NDVI values outside expected range [-1, 1]")
  }
  
  # Create initial dataframe
  raw_ndvi_df <- data.frame(
    date = dates,
    ndvi = ndvi_values
  ) %>%
    arrange(date)
  
  # Initialize vectors for processed data
  ndvi_raw <- raw_ndvi_df$ndvi
  ndvi_processed <- ndvi_raw
  
  # Validate data before BISE correction
  if(all(is.na(ndvi_raw)) || length(ndvi_raw) < 2) {
    return(data.frame(
      date = dates,
      int.NDVI = rep(NA, length(dates))
    ))
  }
  
  # 1. BISE correction
  tryCatch({
    diff_vals <- diff(ndvi_raw)
    decrease_idx <- which(diff_vals < 0) + 1  # points showing decrease
    
    if(length(decrease_idx) > 0) {
      # Using a slope threshold of 0.2 (20%)
      data_threshold <- c(NA, ndvi_raw[-1] - 0.2 * diff_vals)
      
      # Forward sliding period (3 values window)
      val_sliding_period <- running(ndvi_raw, fun=max, width=3)
      val_sliding_period <- c(val_sliding_period, NA, NA)
      
      # Identify points to reject
      law_check <- val_sliding_period[decrease_idx] - data_threshold[decrease_idx]
      reject_decrease <- decrease_idx[which(law_check > 0)]
      
      # Check for sudden increases
      increase_idx <- which(diff_vals > 0.2)
      reject_increase <- increase_idx[!increase_idx %in% decrease_idx]
      reject_points <- c(reject_increase, reject_decrease)
      
      # Apply corrections
      ndvi_processed[reject_points] <- NA
    }
    
    # 2. Fill start and end gaps
    if(length(ndvi_processed) >= 11) {
      start_window <- ndvi_processed[7:11]
      ndvi_processed[1:6] <- start_window[!is.na(start_window)][1]
      
      end_window <- na.omit(ndvi_processed[(length(ndvi_processed)-4):(length(ndvi_processed)-1)])
      if(length(end_window) > 0) {
        ndvi_processed[length(ndvi_processed)] <- min(end_window)
      } else {
        ndvi_processed[length(ndvi_processed)] <- 0
      }
    }
    
    # 3. Interpolate missing values and apply Savitzky-Golay filter
    ndvi_interpolated <- na.spline(ndvi_processed)
    ndvi_smoothed <- sgolayfilt(ndvi_interpolated, p=3, n=7, m=0)
    
    # Create daily interpolation
    dates_seq <- seq(min(raw_ndvi_df$date), max(raw_ndvi_df$date), by="days")
    daily_values <- spline(x=as.numeric(raw_ndvi_df$date), 
                           y=ndvi_smoothed, 
                           xout=as.numeric(dates_seq))$y
    
    # Return results
    result_df <- data.frame(
      date = dates_seq,
      int.NDVI = daily_values
    )
    
    return(result_df)
  }, error = function(e) {
    warning(paste("Error in BISE correction:", e$message))
    return(data.frame(
      date = dates,
      int.NDVI = rep(NA, length(dates))
    ))
  })
}

# Function to calculate metrics from processed NDVI
calculate_cell_metrics <- function(processed_df) {
  if(all(is.na(processed_df$int.NDVI))) {
    return(data.frame(
      int.NDVI = NA,
      annual_mean = NA,
      annual_median = NA,
      winter_spring_mean = NA,
      winter_spring_median = NA,
      winter_spring_max = NA,
      winter_spring_integrated = NA,
      summer_mean = NA,
      summer_median = NA,
      summer_max = NA,
      summer_integrated = NA
    ))
  }
  
  # Add month information
  processed_df$month <- month(processed_df$date)
  
  # Calculate metrics
  metrics <- processed_df %>%
    summarise(
      # Annual metrics
      int.NDVI = sum(int.NDVI, na.rm = TRUE),
      annual_mean = mean(int.NDVI, na.rm = TRUE),
      annual_median = median(int.NDVI, na.rm = TRUE),
      
      # Winter-Spring metrics (January-May)
      winter_spring_mean = mean(int.NDVI[month %in% 1:5], na.rm = TRUE),
      winter_spring_median = median(int.NDVI[month %in% 1:5], na.rm = TRUE),
      winter_spring_max = max(int.NDVI[month %in% 1:5], na.rm = TRUE),
      winter_spring_integrated = sum(int.NDVI[month %in% 1:5], na.rm = TRUE),
      
      # Summer metrics (July-September)
      summer_mean = mean(int.NDVI[month %in% 7:9], na.rm = TRUE),
      summer_median = median(int.NDVI[month %in% 7:9], na.rm = TRUE),
      summer_max = max(int.NDVI[month %in% 7:9], na.rm = TRUE),
      summer_integrated = sum(int.NDVI[month %in% 7:9], na.rm = TRUE)
    )
  
  return(metrics)
}

# Process raster stack
process_raster_ndvi <- function(raster_files) {
  # Read all rasters into a stack
  ndvi_stack <- rast(raster_files)
  
  # Extract dates from filenames
  dates <- as.Date(str_extract(raster_files, "\\d{8}"), format = "%Y%m%d")
  
  # Create empty rasters for each metric
  metrics <- c("int.NDVI", "annual_mean", "annual_median",
               "winter_spring_mean", "winter_spring_median", "winter_spring_max", 
               "winter_spring_integrated", "summer_mean", "summer_median",
               "summer_max", "summer_integrated")
  
  result_rasters <- lapply(metrics, function(x) {
    rast(ndvi_stack[[1]])  # Use first raster as template
  })
  names(result_rasters) <- metrics
  
  # Process cell by cell
  total_cells <- ncell(ndvi_stack)
  cat("\nProcessing", total_cells, "cells...\n")
  
  for(cell in 1:total_cells) {
    if(cell %% 1000 == 0) cat("Processing cell", cell, "of", total_cells, "\n")
    
    # Extract time series for current cell
    cell_values <- tryCatch({
      as.vector(ndvi_stack[cell])
    }, error = function(e) {
      warning(paste("Error reading cell", cell, ":", e$message))
      return(rep(NA, length(dates)))
    })
    
    # Process cell values with BISE correction and interpolation
    processed_df <- process_cell_ndvi(cell_values, dates)
    
    # Calculate metrics from processed data
    cell_metrics <- calculate_cell_metrics(processed_df)
    
    # Assign results to output rasters
    for(metric in metrics) {
      result_rasters[[metric]][cell] <- cell_metrics[[metric]]
    }
  }
  
  # Stack all result rasters
  result_stack <- rast(result_rasters)
  
  return(result_stack)
}

# Main execution
main <- function(save_intermediates = FALSE, debug = TRUE) {
  if(debug) {
    cat("\nSystem information:")
    cat("\nWorking directory:", getwd())
    cat("\nR version:", R.version.string)
    cat("\nPackage versions:")
    cat("\n  terra:", as.character(packageVersion("terra")))
    cat("\n  tidyverse:", as.character(packageVersion("tidyverse")))
    cat("\n  here:", as.character(packageVersion("here")))
    cat("\n")
  }
  
  # Get all raster files
  raster_files <- list.files(here("NDVI_rasters"), 
                             pattern = "\\.tif$", 
                             full.names = TRUE)
  
  if(length(raster_files) == 0) {
    stop("No NDVI raster files found in NDVI_rasters folder")
  }
  
  # Print the files found for debugging
  cat("Found", length(raster_files), "raster files:\n")
  print(basename(raster_files))
  
  # Create output directory
  output_dir <- here("ndvi_metrics_2024")
  if(!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # Process rasters for metrics
  cat("Processing NDVI rasters for metrics...\n")
  result_stack <- process_raster_ndvi(raster_files)
  
  # Save metric results
  for(i in 1:nlyr(result_stack)) {
    metric_name <- names(result_stack)[i]
    writeRaster(result_stack[[i]], 
                filename = file.path(output_dir, paste0(metric_name, "_2024.tif")),
                overwrite = TRUE)
  }
  
  cat("Processing complete. Results saved in:", output_dir, "\n")
}

# Run the script
main(save_intermediates = FALSE, debug = TRUE)