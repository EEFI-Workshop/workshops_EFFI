#NDVI Preprocessing and Analysis Script

#Get the working directory
here()

# Get the libraries and install if necessary
source(here("vegetation_donana", "load_packages.R"))

# Load functions 
source(here("vegetation_donana","ndvi_calculation_functions.R"))

# Read and process the NDVI data
df_ndvi <- read.csv(here("vegetation_donana","df.ndvi.csv"))
df_ndvi$date <- as.Date(paste(df_ndvi$year, df_ndvi$month, df_ndvi$day, sep="-"))
df_ndvi = df_ndvi[df_ndvi$year != 1984,] #Non complete year
df_ndvi$ndvi[df_ndvi$ndvi == 255]<-NA #No data cases
# df_ndvi$ndvi <- (df_ndvi$ndvi / 100) - 1 # Transform from Byte to NDVI
hist(df_ndvi$ndvi) # Check if the NDVI values are within the -1 to 1 range

cat("Cleaning, interpolating and smoothening the NDVI data per plot.", "\n")
# Process data for each unique plot
plots <- unique(df_ndvi$plot)
processed_data <- list()

for(iplot in plots) {
  plot_data <- subset(df_ndvi, plot == iplot)
  plot_data <- data.frame(
    date = plot_data$date,
    ndvi = plot_data$ndvi
  )
  
  processed <- process_ndvi_data(plot_data)
  processed$plot <- iplot
  processed$year <- year(processed$date)
  
  processed_data[[iplot]] <- processed
}

# Combine all processed data
all_processed_data <- bind_rows(processed_data)

plot_significance <- all_processed_data %>%
  group_by(plot) %>%
  do(get_slope_significance(.)) %>%
  mutate(significant = p.value < 0.05)

# Join significance information back to the data
all_processed_data <- all_processed_data %>%
  left_join(plot_significance, by = "plot")
saveRDS(all_processed_data, file = here("vegetation_donana", "ndvi_processed.rds"))
cat("ndvi_trends_plots.pdf figure is created.", "\n")
# Create plot with transparent non-significant trends
pdf(file = here("vegetation_donana", "ndvi_trends_plots.pdf"), height = 12, width = 15)
p1 = ggplot(all_processed_data, aes(date, int.NDVI, color = plot), message = FALSE) +
  geom_line(alpha = 0.2) +
  geom_smooth(aes(linetype = significant), method = "lm", se = FALSE) +
  theme_bw() +
  labs(title = "NDVI Trends Over Time",
       subtitle = "Solid lines = significant trends over the years (p < 0.05)\nDashed lines = non-significant trends over the years",
       x = "Date",
       y = "NDVI") +
  theme(legend.position = "right")+
  scale_linetype_manual(values = c("dashed", "solid"))

p2 = ggplot(all_processed_data, aes(date, int.NDVI, color = plot), message = FALSE) +
  geom_line(alpha = 0.2) +
  geom_smooth(aes(linetype = significant), method = "lm", se = FALSE) +
  facet_wrap(.~plot, ncol = 4)+
  theme_bw() +
  labs(title = "NDVI Trends Over Time",
       subtitle = "Solid lines = significant trends over the years (p < 0.05)\nDashed lines = non-significant trends over the years",
       x = "Date",
       y = "NDVI") +
  theme(legend.position = "right")+
  scale_linetype_manual(values = c("dashed", "solid"))
print(p1)
print(p2)
dev.off()

cat("Calculating NDVI metrics per plot.", "\n")
# Calculate metrics for all processed data
ndvi_metrics <- calculate_ndvi_metrics(all_processed_data)

cat("Calculating changes in NDVI metrics over the years in each plot.", "\n")
# Calculate trends
trends <- calculate_trends(ndvi_metrics)

cat("changes_integrated_ndvi.pdf is created.", "\n")
pdf(file = here("vegetation_donana", "changes_integrated_ndvi.pdf"), height = 10, width = 15)

p1 = trends %>%
  dplyr::select(plot, variable, slope)%>%
  pivot_wider(names_from = variable, values_from = c("slope"))%>%
  ggplot(aes(summer_integrated, winter_spring_integrated, color = integrated_ndvi), message = FALSE)+
  geom_point(size = 3)+
  geom_text_repel(aes(label = plot), size = 5)+
  geom_abline(intercept = 0, slope = 1, color = "grey")+
  geom_hline(yintercept = 0, color = "grey")+
  geom_vline(xintercept = 0, color = "grey")+
  geom_smooth(method = "lm", color = "black")+
  theme_bw()+
  scale_color_viridis_c()+
  labs(title = "Slope of the linear model (Integrated-NDVI ~ Year)",
       x = "Integrated-NDVI (July - September)",
       y = "Integrated-NDVI (January - May)",
       color = "Integrated-NDVI (annual)")+
  theme(legend.position = "bottom")+
  stat_cor(color = "black")

p2 = trends %>%
  dplyr::select(plot, variable, percent_change_per_year)%>%
  pivot_wider(names_from = variable, values_from = c("percent_change_per_year"))%>%
  ggplot(aes(summer_integrated, winter_spring_integrated, color = integrated_ndvi), message = FALSE)+
  geom_point(size = 3)+
  geom_text_repel(aes(label = plot), size = 5)+
  geom_abline(intercept = 0, slope = 1, color = "grey")+
  geom_hline(yintercept = 0, color = "grey")+
  geom_vline(xintercept = 0, color = "grey")+
  geom_smooth(method = "lm", color = "black")+
  theme_bw()+
  scale_color_viridis_c()+
  labs(title = "Percent change per year",
       x = "Integrated-NDVI (July - September)",
       y = "Integrated-NDVI (January - May)",
       color = "Integrated-NDVI (annual)")+
  theme(legend.position = "bottom")+
  stat_cor(color = "black")

print(ggarrange(p1, p2))
dev.off()

cat("ndvi_metrics.csv is created.", "\n")
write.csv(ndvi_metrics, file = here("vegetation_donana", "ndvi_metrics.csv"))
