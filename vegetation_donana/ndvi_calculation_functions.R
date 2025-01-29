# Functions ----
# Function to preprocess NDVI data with BISE correction and interpolation
process_ndvi_data <- function(raw_ndvi_df) {
  # Ensure data is sorted by date
  raw_ndvi_df <- raw_ndvi_df %>%
    arrange(date)
  
  # Initialize vectors for processed data
  ndvi_raw <- raw_ndvi_df$ndvi
  ndvi_processed <- ndvi_raw
  
  # 1. BISE correction
  diff_vals <- diff(ndvi_raw)
  decrease_idx <- which(diff_vals < 0) + 1  # points showing decrease
  
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
  
  # 2. Fill start and end gaps
  start_window <- ndvi_processed[7:11]
  ndvi_processed[1:6] <- start_window[!is.na(start_window)][1]
  
  end_window <- na.omit(ndvi_processed[(length(ndvi_processed)-4):(length(ndvi_processed)-1)])
  if(length(end_window) > 0) {
    ndvi_processed[length(ndvi_processed)] <- min(end_window)
  } else {
    ndvi_processed[length(ndvi_processed)] <- 0
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
}

get_slope_significance <- function(data) {
  # Convert date to numeric for linear model
  data$time <- as.numeric(data$date)
  
  # Fit linear model
  model <- lm(int.NDVI ~ time, data = data)
  
  # Get model statistics
  stats <- tidy(model)
  
  # Return slope and p-value
  return(data.frame(
    slope = stats$estimate[2],
    p.value = stats$p.value[2]
  ))
}

# Function to calculate NDVI metrics
calculate_ndvi_metrics <- function(data) {
  
  # Add month column if not present
  if(!"month" %in% colnames(data)) {
    data$month <- month(data$date)
  }
  
  # Calculate metrics for each plot and year
  ndvi_metrics <- data %>%
    group_by(plot, year) %>%
    summarise(
      # Annual metrics
      integrated_ndvi = sum(int.NDVI, na.rm = TRUE),
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
      summer_integrated = sum(int.NDVI[month %in% 7:9], na.rm = TRUE),
      
      # Count number of observations in each period
      n_winter_spring = sum(month %in% 1:5),
      n_summer = sum(month %in% 7:9),
      n_total = n()
    ) %>%
    ungroup()
  
  return(ndvi_metrics)
}

# Function to calculate slope and significance with proper percent change
calculate_trends <- function(data) {
  # Convert the data to long format for the three variables
  long_data <- data %>%
    select(plot, year, 
           integrated_ndvi, 
           summer_integrated, 
           winter_spring_integrated) %>%
    pivot_longer(cols = c(integrated_ndvi, summer_integrated, winter_spring_integrated),
                 names_to = "variable",
                 values_to = "value")
  
  # Calculate trends for each plot and variable
  trends <- long_data %>%
    group_by(plot, variable) %>%
    summarise(
      # Get first and last year values from the fitted model
      model = list(lm(value ~ year)),
      first_year = min(year),
      last_year = max(year),
      n_years = last_year - first_year,
      # Get fitted values for first and last year
      first_fitted = predict(first(model), newdata = data.frame(year = min(year))),
      last_fitted = predict(first(model), newdata = data.frame(year = max(year))),
      # Calculate total and annual percent change
      total_percent_change = ((last_fitted - first_fitted) / first_fitted) * 100,
      percent_change_per_year = total_percent_change / n_years,
      # Extract model statistics using broom
      stats = list(tidy(lm(value ~ year))),
      # Get R-squared
      rsq = summary(first(model))$r.squared,
      .groups = "drop"
    ) %>%
    # Extract slope and p-value
    mutate(
      slope = sapply(stats, function(x) x$estimate[2]),
      p_value = sapply(stats, function(x) x$p.value[2]),
      significant = p_value < 0.05
    ) %>%
    # Select final columns
    select(plot, variable, slope, p_value, significant, 
           rsq, total_percent_change, percent_change_per_year,
           first_year, last_year, first_fitted, last_fitted)
  
  return(trends)
}

# Get CHELSA future climate
get_future_clim = function(coords, scenario, output, years){
  expand_by = 2
  print(coords)
  if(!dir.exists(here("vegetation_donana", output))){
    dir.create(here("vegetation_donana", output))
  }
  for(yearx in years){
    print(paste0("Getting chelsa files for ", output," , ", yearx))
    print(paste0("Saving the files to: ", here("vegetation_donana", output)))
    chelsa_cmip6$GetClim$chelsa_cmip6(activity_id='ScenarioMIP', 
                                      table_id='Amon', 
                                      experiment_id=scenario, 
                                      institution_id='MPI-M', 
                                      source_id='MPI-ESM1-2-LR', 
                                      member_id='r1i1p1f1', 
                                      refps='1981-01-15', 
                                      refpe='2010-12-15', 
                                      fefps= paste0(yearx,'-01-15'), 
                                      fefpe= paste0(yearx+1,'-01-15'),
                                      xmin=min(coords$Long) - expand_by, 
                                      xmax=max(coords$Long) + expand_by,
                                      ymin=min(coords$Lat) - expand_by, 
                                      ymax=max(coords$Lat) + expand_by,
                                      output=paste0(here("vegetation_donana", output), "/"))
    
    print("Creating the .csv for the data points")
    chelsa.files = list.files(here("vegetation_donana", output), 
                              pattern = as.character(yearx), full.names = TRUE)
    chelsa = rast(chelsa.files)
    
    clim = 
      coords %>%
      st_as_sf(coords = c("Long", "Lat"), crs = 4326, agr = "constant")%>%
      mutate((terra::extract(chelsa, .)))%>%
      mutate(ID = coords$ID)%>%
      st_drop_geometry()%>%
      pivot_longer(cols = -c(ID, Elevation))%>%
      mutate(year = as.character(yearx))
      
    print(head(clim))
    
    write.csv(as.data.frame(clim), file = paste0(here("vegetation_donana"),"/", output, "_", yearx, ".csv"))
    
    print(paste0("Deleting the chelsa files for the year ", yearx))
    file.remove(chelsa.files)
  }
  
}

get_all_futures = function(scenarios) {
  future_climate = map_df(scenarios, function(scenario) {
    future = 
      list(list.files(here("vegetation_donana"), 
                      pattern = paste0("future_climate_", scenario), 
                      full.names = TRUE)) %>% 
      map(read_csv) %>% 
      bind_rows() %>%
      dplyr::select(-`...1`) %>%
      mutate(
        scenario = scenario,
        variable = str_extract(name, "^[^_]+"),
        month_doy = as.numeric(str_extract(name, "(?<=month=)\\d+"))) %>%
      mutate(month = as.numeric(month_doy) / 30) %>%
      mutate(month = round(month)+1) %>%
      mutate(variable = ifelse(!is.na(month), paste0(variable, "_", month), variable))%>%
      dplyr::select(-name, -month_doy, -month)
    
    return(future)
  })
  
  # Write combined results to CSV
  write.csv(future_climate, 
            file = here("vegetation_donana", "future_climate_all.csv"))
  
  return(future_climate)
}

get_present_climate = function(df){
  df = df %>%
    pivot_longer(
      cols = -time)%>%
    mutate(
      lon = as.numeric(str_extract(name, "(?<=lon:)[0-9.-]+")),  # Extract longitude
      lat = as.numeric(str_extract(name, "(?<=lat:)[0-9.-]+"))  # Extract latitude
    )%>%
    dplyr::select(-name)
  return(df)
}

calculate_bioclim_vars <- function(data, 
                                   date_col = "date", 
                                   temp_col = "temperature",
                                   precip_col = "precipitation",
                                   id_col = "ID") {
  
  # Ensure date column is in proper format
  data[[date_col]] <- as.Date(data[[date_col]])
  
  # Add year column for grouping
  data$year <- year(data[[date_col]])
  
  # Main calculation function for one site-year combination
  calculate_site_year_bioclim <- function(site_year_data) {
    # First check if we have precipitation data
    if(is.null(precip_col) || 
       all(is.na(site_year_data[[precip_col]])) || 
       all(site_year_data[[precip_col]] == 0)) {
      # Return all NAs if no precipitation data
      return(tibble(
        bio1 = NA_real_, bio2 = NA_real_, bio3 = NA_real_, 
        bio4 = NA_real_, bio5 = NA_real_, bio6 = NA_real_,
        bio7 = NA_real_, bio8 = NA_real_, bio9 = NA_real_,
        bio10 = NA_real_, bio11 = NA_real_, bio12 = NA_real_,
        bio13 = NA_real_, bio14 = NA_real_, bio15 = NA_real_,
        bio16 = NA_real_, bio17 = NA_real_, bio18 = NA_real_,
        bio19 = NA_real_
      ))
    }
    
    # Calculate monthly statistics
    monthly_stats <- site_year_data %>%
      mutate(
        month = month(!!sym(date_col))
      ) %>%
      group_by(month) %>%
      summarise(
        mean_temp = mean(!!sym(temp_col), na.rm = TRUE),
        min_temp = min(!!sym(temp_col), na.rm = TRUE),
        max_temp = max(!!sym(temp_col), na.rm = TRUE),
        total_precip = sum(!!sym(precip_col), na.rm = TRUE),
        .groups = "drop"
      )
    
    # Calculate quarterly values
    n_months <- nrow(monthly_stats)
    quarterly_temp_means <- numeric(n_months)
    quarterly_precip <- numeric(n_months)
    
    for(i in 1:n_months) {
      if(i <= 2) {
        indices <- c((n_months-2+i):n_months, 1:i)
      } else if(i > n_months-2) {
        indices <- c((i-1):n_months, 1:(3-length((i-1):n_months)))
      } else {
        indices <- (i-1):(i+1)
      }
      quarterly_temp_means[i] <- mean(monthly_stats$mean_temp[indices], na.rm = TRUE)
      quarterly_precip[i] <- sum(monthly_stats$total_precip[indices], na.rm = TRUE)
    }
    
    # Calculate all bioclimatic variables
    results <- tibble(
      # Temperature-based variables
      bio1 = mean(monthly_stats$mean_temp, na.rm = TRUE),
      bio2 = mean(monthly_stats$max_temp - monthly_stats$min_temp, na.rm = TRUE),
      bio4 = sd(monthly_stats$mean_temp, na.rm = TRUE) * 100,
      bio5 = max(monthly_stats$max_temp, na.rm = TRUE),
      bio6 = min(monthly_stats$min_temp, na.rm = TRUE),
      bio7 = max(monthly_stats$max_temp, na.rm = TRUE) - min(monthly_stats$min_temp, na.rm = TRUE),
      bio3 = (mean(monthly_stats$max_temp - monthly_stats$min_temp, na.rm = TRUE) / 
                (max(monthly_stats$max_temp, na.rm = TRUE) - min(monthly_stats$min_temp, na.rm = TRUE))) * 100,
      
      # Temperature of wettest/driest quarters
      bio8 = quarterly_temp_means[which.max(quarterly_precip)],
      bio9 = quarterly_temp_means[which.min(quarterly_precip)],
      
      # Temperature of warmest/coldest quarters
      bio10 = max(quarterly_temp_means, na.rm = TRUE),
      bio11 = min(quarterly_temp_means, na.rm = TRUE),
      
      # Precipitation-based variables
      bio12 = sum(monthly_stats$total_precip, na.rm = TRUE),
      bio13 = max(monthly_stats$total_precip, na.rm = TRUE),
      bio14 = min(monthly_stats$total_precip, na.rm = TRUE),
      bio15 = sd(monthly_stats$total_precip, na.rm = TRUE) / mean(monthly_stats$total_precip, na.rm = TRUE) * 100,
      bio16 = max(quarterly_precip, na.rm = TRUE),
      bio17 = min(quarterly_precip, na.rm = TRUE),
      bio18 = quarterly_precip[which.max(quarterly_temp_means)],
      bio19 = quarterly_precip[which.min(quarterly_temp_means)]
    )
    
    return(results)
  }
  
  # Calculate bioclim variables for each site and year
  results <- data %>%
    group_by(!!sym(id_col), year) %>%
    group_modify(~calculate_site_year_bioclim(.x)) %>%
    ungroup()
  
  return(results)
}

### PREDICTIONS ----
# Helper function to get all possible combinations of bioclim variables
get_bioclim_combinations <- function() {
  bioclim_vars <- paste0("bio", c(1, 12, 9, 18))
  all_combs <- list()
  
  # Generate combinations of different lengths
  for(i in 1:4) {  # You can adjust this range
    combs <- combn(bioclim_vars, i, simplify = FALSE)
    all_combs <- c(all_combs, combs)
  }
  
  return(all_combs)
}

# Helper function to prepare data for a specific NDVI metric
prepare_data <- function(data, ndvi_metric, bioclim_vars) {
  selected_cols <- c("plot", ndvi_metric, bioclim_vars)
  
  prepared_data <- data %>%
    select(all_of(selected_cols)) %>%
    mutate(plot = as.factor(plot)) %>%
    drop_na()
  
  return(prepared_data)
}

# Model training functions
train_lm <- function(data, ndvi_metric, bioclim_vars) {
  formula <- as.formula(paste(ndvi_metric, "~ plot*", paste(bioclim_vars, collapse = " + plot*")))
  model <- lm(formula, data = data)
  return(model)
}

train_rf <- function(data, ndvi_metric, bioclim_vars) {
  data$plot <- as.factor(data$plot)
  formula <- as.formula(paste(ndvi_metric, "~ plot +", paste(bioclim_vars, collapse = " + ")))
  model <- ranger(formula, data = data, num.trees = 500)
  return(model)
}

train_gam <- function(data, ndvi_metric, bioclim_vars) {
  data$plot <- as.factor(data$plot)
  smooths_by_plot <- paste(paste("s(", bioclim_vars, ", by=plot)", sep = ""), collapse = " + ")
  formula <- as.formula(paste(ndvi_metric, "~ plot +", smooths_by_plot))
  model <- gam(formula, data = data, method = "REML")
  return(model)
}

# Main function to run models and get predictions
run_models_scenarios <- function(base_data, scenario_data_list, ndvi_metrics) {
  all_predictions <- list()
  
  for (metric in ndvi_metrics) {
    cat("\nProcessing metric:", metric, "\n")
    
    for (bioclim_set in get_bioclim_combinations()) {
      cat("Processing bioclim combination:", paste(bioclim_set, collapse = ", "), "\n")
      
      # Prepare base data
      prepared_base_data <- prepare_data(base_data, metric, bioclim_set)
      
      if(nrow(prepared_base_data) == 0) {
        cat("No complete cases in base data for this combination\n")
        next
      }
      
      # Train models
      models <- list()
      tryCatch({
        models$lm <- train_lm(prepared_base_data, metric, bioclim_set)
        models$rf <- train_rf(prepared_base_data, metric, bioclim_set)
        models$gam <- train_gam(prepared_base_data, metric, bioclim_set)
      }, error = function(e) {
        cat("Error in model training:", conditionMessage(e), "\n")
        return(NULL)
      })
      
      # Generate predictions for each scenario
      for (scenario_name in names(scenario_data_list)) {
        # Prepare scenario data (ensure plot is factor)
        scenario_data <- as.data.frame(scenario_data_list[[scenario_name]]) %>%
          mutate(plot = as.factor(plot))
        
        # Make predictions for each model type
        for (model_type in names(models)) {
          # Make predictions
          preds <- if (model_type == "rf") {
            predict(models[[model_type]], data = scenario_data)$predictions
          } else {
            predict(models[[model_type]], newdata = scenario_data)
          }
          
          # Store predictions with all relevant information
          predictions_df <- data.frame(
            metric = metric,
            model = model_type,
            bioclim_vars = paste(bioclim_set, collapse = "_"),
            plot = scenario_data$plot,
            scenario = scenario_name,
            year = scenario_data$year
          ) %>%
            bind_cols(
              scenario_data %>% select(all_of(bioclim_set))
            ) %>%
            mutate(predicted = preds)
          
          # Store in list
          key <- paste(metric, scenario_name, model_type, 
                       paste(bioclim_set, collapse = "_"), 
                       sep = "-")
          all_predictions[[key]] <- predictions_df
        }
      }
    }
  }
  
  # Combine all predictions into one dataframe
  final_predictions <- bind_rows(all_predictions)
  
  # Save predictions
  saveRDS(final_predictions, here("vegetation_donana", paste0(ndvi_metrics,"_predictions.rds")))
  
  return(final_predictions)
}