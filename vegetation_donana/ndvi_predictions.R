#NDVI Predictions Script

#Get the working directory
library(here)
here()

# Get the libraries and install if necessary
source(here("vegetation_donana", "load_packages.R"))

# Load functions 
source(here("vegetation_donana","ndvi_calculation_functions.R"))

# Download current and future climate from CHELSA
coords_long <- read.csv(here("vegetation_donana","coords_plot_since2007.csv"))

if(!file.exists(here("vegetation_donana", "present_climate.csv"))){
# Get present climate
coords = data.frame(lon = coords_long$Long, lat = coords_long$Lat)
tas = getChelsa('tas',coords=coords, startdate=as.Date("1985-1-1"), enddate=as.Date("2021-1-1"))
tasc = getChelsa('tas',coords=coords, startdate=as.Date("2021-1-1"), enddate=as.Date("2021-1-31")) # problem with chelsa dataset
# Due to lack of certain files, precipitation needs to be downloaded in this way...
pr = getChelsa('pr',coords=coords, startdate=as.Date("1985-1-1"), enddate=as.Date("2020-1-1"))
prc = getChelsa('pr',coords=coords, startdate=as.Date("2020-1-1"), enddate=as.Date("2020-1-4"))
prc2 = getChelsa('pr',coords=coords, startdate=as.Date("2020-1-6"), enddate=as.Date("2020-3-1"))
prc3 = getChelsa('pr',coords=coords, startdate=as.Date("2020-3-3"), enddate=as.Date("2020-4-4"))
prc4 = getChelsa('pr',coords=coords, startdate=as.Date("2020-4-6"), enddate=as.Date("2020-5-1"))
prc5 = getChelsa('pr',coords=coords, startdate=as.Date("2020-5-3"), enddate=as.Date("2020-6-1"))
prc6 = getChelsa('pr',coords=coords, startdate=as.Date("2020-6-3"), enddate=as.Date("2020-11-4"))
prc7 = getChelsa('pr',coords=coords, startdate=as.Date("2020-11-6"), enddate=as.Date("2020-12-31"))

tasx = 
  list(tas, tasc) %>% 
  map(get_present_climate) %>% 
  bind_rows()%>%
  left_join(coords_long %>% rename(lon = Long, lat = Lat))%>%
  mutate(value = value - 273.15)%>% # celcius
  mutate(time = as.Date(time, "Y%-m%-%d"))%>%
  rename(temperature = value)%>%
  select(ID, time, temperature)

  
prx = 
  list(pr, prc, prc2, prc3, prc4, prc5, prc6, prc7) %>% 
  map(get_present_climate) %>% 
  bind_rows()%>%
  left_join(coords_long %>% rename(lon = Long, lat = Lat))%>%
  mutate(time = as.Date(time, "Y%-m%-%d"))%>%
  rename(precipitation = value)%>%
  select(ID, time, precipitation)

present_climate = left_join(tasx, prx)
write.csv(present_climate, file = here("vegetation_donana", "present_climate.csv"))
present_bioclim = calculate_bioclim_vars(present_climate, date_col = "time", temp_col = "temperature", precip_col = "precipitation", id_col = "ID")
}else{
  present_bioclim = calculate_bioclim_vars(read.csv(here("vegetation_donana", "present_climate.csv")), date_col = "time", temp_col = "temperature", precip_col = "precipitation", id_col = "ID")
  
}

if(!file.exists(here("vegetation_donana", "future_climate_all.csv"))){
# worst scenario: ssp585
get_future_clim(coords = coords_long, scenario = 'ssp585', 
                output = "future_climate_ssp585", years = c(2022:2025))
# mid scenario: ssp370
get_future_clim(coords = coords_long, scenario = 'ssp370', 
                output = "future_climate_ssp370", years = 2023:2025)
# better scenario: ssp245
get_future_clim(coords = coords_long, scenario = 'ssp245', 
                output = "future_climate_ssp245", years = 2022:2025)

folders_to_delete = list.files(here("vegetation_donana"),pattern = "future_climate", full.names = TRUE)
unlink(folders_to_delete[file.info(folders_to_delete)$isdir], recursive = TRUE)

# Get all the future clim with different scenario
future_climate <- get_all_futures(scenario = c('ssp370', 'ssp585', 'ssp245'))%>%
  rename(plot = ID)%>%
  select(plot, scenario, year, variable, value)%>%
  filter(stringr::str_detect(variable, "bio"))%>%
  mutate(value = ifelse(variable %in% c("bio1", "bio2", "bio5", "bio6", "bio7", "bio8",
                                        "bio9", "bio10", "bio11"),
                        value - 273.15,
                        value))%>%
  pivot_wider(names_from = "variable", values_from = "value")
}else{
  future_climate = read.csv(here("vegetation_donana", "future_climate_all.csv"))%>%
    rename(plot = ID)%>%
    select(plot, scenario, year, variable, value)%>%
    filter(stringr::str_detect(variable, "bio"))%>%
    mutate(value = ifelse(variable %in% c("bio1", "bio2", "bio5", "bio6", "bio7", "bio8",
                                          "bio9", "bio10", "bio11"),
                          value - 273.15,
                          value))%>%
    pivot_wider(names_from = "variable", values_from = "value")
}

# Get the NDVI indices and merge it with the present bioclim data
ndvi_data <- read.csv(here("vegetation_donana","ndvi_metrics.csv"))%>%
  left_join(present_bioclim %>% rename(plot = ID))

scenario_data_list <- list(
  ssp370 = future_climate %>% filter(scenario == "ssp370"),
  ssp245 = future_climate %>% filter(scenario == "ssp245"),
  ssp585 = future_climate %>% filter(scenario == "ssp585")
)  

if(!file.exists(here("vegetation_donana", "ndvi_predictions.rds"))){
integrated_ndvi <- run_models_scenarios(base_data = ndvi_data, 
                                        scenario_data_list = scenario_data_list, 
                                        ndvi_metrics = "integrated_ndvi")

winter_spring_integrated <- run_models_scenarios(base_data = ndvi_data, 
                                                 scenario_data_list = scenario_data_list, 
                                                 ndvi_metrics = "winter_spring_integrated")

summer_integrated <- run_models_scenarios(base_data = ndvi_data, 
                                          scenario_data_list = scenario_data_list, 
                                          ndvi_metrics = "summer_integrated")

ndvi_predictions = bind_rows(integrated_ndvi, 
                             winter_spring_integrated,
                             summer_integrated)

saveRDS(ndvi_predictions, file = here("vegetation_donana", "ndvi_predictions.rds"))
}else{
  ndvi_predictions = readRDS(here("vegetation_donana", "ndvi_predictions.rds"))
}

# Create the figure for the shiny app
ndvi_plot =
ndvi_predictions %>%
  select(metric, model, bioclim_vars, scenario, plot, year, predicted)%>%
  rename(value = predicted)%>%
  mutate(type = "predicted")%>%
  bind_rows(
    ndvi_data %>%
      select(plot, year, integrated_ndvi, winter_spring_integrated, summer_integrated)%>%
      pivot_longer(cols = integrated_ndvi:summer_integrated, 
                   names_to = "metric", 
                   values_to = "value")%>%
      mutate(type = "observed",
             scenario = "observed",
             bioclim_vars = "observed",
             model = "observed")
    
  )

saveRDS(ndvi_plot, file = here("vegetation_donana", "ndvi_plot.rds"))
