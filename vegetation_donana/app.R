# Required libraries
library(shiny)
library(bs4Dash)
library(tidyverse)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)
library(plotly)
library(colorspace)
library(here)
library(terra)
library(tidyterra)#added to use the rast function

options(warn = -1)
ndvi_plot <- readRDS(here("ndvi_plot.rds"))
ndvi_processed_raw <- readRDS(here("ndvi_processed.rds"))
shrub_data <- read.csv(here("shrub_number.csv"))
colnames(shrub_data)[5:7]=c("adults","saplings","seedlings")
observed_totals <- readRDS(here("observed_totals.rds"))

# UI
ui <- bs4DashPage(
  bs4DashNavbar(
    title = "Doñana National Park",
    status = "success",
    skin = "light",
    compact = TRUE,
    fixed = TRUE,
    controlbarIcon = NULL
  ),
  bs4DashSidebar(
    status = "success",
    collapsed = FALSE,
    minified = FALSE,
    expandOnHover = FALSE,
    bs4SidebarMenu(
      bs4Dash::menuItem("NDVI Data", tabName = "ndvi_data", icon = icon("leaf")),
      bs4Dash::menuItem("Shrub Data", tabName = "shrub_data", icon = icon("tree")),
      bs4Dash::menuItem("NDVI Predictions", tabName = "ndvi_pred", icon = icon("leaf")),
      bs4Dash::menuItem("Shrub Predictions", tabName = "shrub_pred", icon = icon("clock")),
      bs4Dash::menuItem("Shrub Predictions", tabName = "shrub_pred_spatial", icon = icon("map"))
      
    )
  ),
  
  bs4DashBody(
    tabItems(
      # NDVI Data Tab
      tabItem(
        tabName = "ndvi_data",
        fluidRow(
          column(
            width = 12,
            bs4Card(
              width = 8,
              title = "NDVI Raw Data",
              status = "success",
              plotlyOutput("ndvi_raw_plot", height = "600px") %>% withSpinner(type = 8)
            )
          )
        )
      ),
      
      # Shrub Data Tab
      tabItem(
        tabName = "shrub_data",
        fluidRow(
          column(
            width = 3,
            bs4Card(
              width = 12,
              title = "Select",
              status = "success",
              selectInput("shrub_data_species", "Choose species:",
                          choices = unique(shrub_data$species))
            )
          ),
          column(
            width = 9,
            bs4Card(
              width = 12,
              title = "Vegetation surveys",
              status = "success",
              plotlyOutput("shrub_data_plot", height = "600px") %>% withSpinner(type = 8)
            )
          )
        )
      ),
      
      # NDVI Predictions Tab
      tabItem(
        tabName = "ndvi_pred",
        fluidRow(
          column(
            width = 3,
            bs4Card(
              width = 12,
              title = "Select Parameters",
              status = "success",
              selectInput("ndvi_pred_metric", "Choose NDVI metric:",
                          choices = c("integrated_ndvi", "winter_spring_integrated", "summer_integrated"),
                          selected = "integrated_ndvi"),
              selectInput("ndvi_pred_scenario", "Choose IPCC scenario:",
                          choices = c("ssp370", "ssp245", "ssp585"),
                          selected = "ssp370"),
              selectInput("ndvi_pred_model", "Choose type of model:",
                          choices = c("lm", "rf", "gam"),
                          selected = "lm"),
              selectInput("ndvi_pred_climate", "Choose climatic variables:",
                          choices = c("bio1", "bio12", "bio9", "bio18"),
                          multiple = TRUE,
                          selected = "bio1")
            )
          ),
          column(
            width = 9,
            bs4Card(
              width = 12,
              title = "NDVI Prediction Results",
              status = "success",
              plotlyOutput("ndvi_pred_plot", height = "600px") %>% withSpinner(type = 8)
            )
          )
        )
      ),
      
      # Shrub Predictions Tab
      tabItem(
        tabName = "shrub_pred",
        fluidRow(
          column(
            width = 3,
            bs4Card(
              width = 12,
              title = "Select Parameters",
              status = "success",
              selectInput("shrub_pred_metric", "Choose NDVI metric:",
                          choices = c("integrated_ndvi", "winter_spring_integrated", "summer_integrated")),
              selectInput("shrub_pred_scenario", "Choose IPCC scenario:",
                          choices = c("ssp370", "ssp245", "ssp585")),
              selectInput("shrub_pred_climate", "Choose climatic variables:",
                          choices = c("bio1", "bio12", "bio9", "bio18"),
                          multiple = TRUE,
                          selected = "bio1"),
              selectInput("shrub_pred_model", "Choose NDVI model:",
                          choices = c("lm", "rf", "gam")),
              selectInput("shrub_pred_species", "Choose species variables:",
                          choices = c("Lavandula stoechas","Halimium halimifolium"),
                          multiple = FALSE,
                          selected = "Lavandula stoechas")
              
            )
          ),
          column(
            width = 9,
            bs4Card(
              width = 12,
              title = "Shrub Prediction Results",
              status = "success",
              plotlyOutput("shrub_pred_plot", height = "600px") %>% withSpinner(type = 8)
            ),
            
            bs4Card(
              width = 12,
              title = "Forecasting skill",
              status = "success",
              plotlyOutput("shrub_mse_plot", height = "600px") %>% withSpinner(type = 8)
            )
          )
        )
      ),
      
      # Shrub Predictions Tab
      tabItem(
        tabName = "shrub_pred_spatial",
        fluidRow(
          column(
            width = 3,
            bs4Card(
              width = 12,
              title = "Select Parameters",
              status = "success",
              selectInput("ndvi_spatial_metric", "Choose NDVI metric:",
                          choices = c("integrated_ndvi", "winter_spring_integrated")),
              selectInput("shrub_spatial_species", "Choose species variables:",
                          choices = c("Lavandula stoechas","Halimium halimifolium"),
                          multiple = FALSE,
                          selected = "Lavandula stoechas")
              
            )
          ),
          column(
            width = 9,
            bs4Card(
              width = 12,
              title = "NDVI indice across the landscape",
              status = "success",
              plotlyOutput("ndvi_spatial_plot", height = "600px") %>% withSpinner(type = 8)
            ),
            
            bs4Card(
              width = 12,
              title = "Shrub abundance across the landscape",
              status = "success",
              plotlyOutput("shrub_spatial_plot", height = "600px") %>% withSpinner(type = 8)
            )
          )
        )
      )
      
      
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Create reactive dataset for NDVI predictions
  ndvi_filtered_data <- reactive({
    # Filter predictions based on user selections
    ndvi_plot %>%
      filter(
        metric %in% c(input$ndvi_pred_metric, "observed"),
        model %in%  c(input$ndvi_pred_model, "observed"),
        scenario %in% c(input$ndvi_pred_scenario, "observed"),
        bioclim_vars %in% c(paste(input$ndvi_pred_climate, collapse = "_"), "observed")
      ) 
  })
  
  # Render the plotly plot
  output$ndvi_pred_plot <- renderPlotly({
    # Create base ggplot
    p <- ndvi_filtered_data() %>%
      filter(year >= 2020, year <= 2025) %>%
      ggplot(aes(year, value, group = plot, color  = type)) +
      xlim(2020, 2025)+
      geom_point(alpha = 0.3) +
      geom_line(alpha = 0.3) +
      stat_summary(aes(year, value, group = type, color = type), 
                   fun = mean, geom = "line", size = 1.5) +
      stat_summary(aes(year, value, group = type, color = type), 
                   fun = mean, geom = "point", size = 3) +
      theme_minimal() +
      labs(
        x = "Year",
        y = "NDVI Value",
        title = paste("NDVI Predictions-", input$ndvi_pred_metric)
      ) +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )+
      scale_color_manual(values = c("grey50", "black"))
    
    # Convert to plotly for interactivity
    ggplotly(p) %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 50)
      )
  })

  
  # Raw NDVI Plot
  output$ndvi_raw_plot <- renderPlotly({
    ggplot(ndvi_processed_raw, aes(date, int.NDVI, color = plot), message = FALSE) +
      geom_line(alpha = 0.2) +
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal() +
      labs(title = "NDVI Trends Over Time",
           subtitle = "Solid lines = significant trends over the years (p < 0.05)\nDashed lines = non-significant trends over the years",
           x = "Date",
           y = "NDVI") +
      theme(legend.position = "right")
  })
  
  # Shrub Plot
  output$shrub_data_plot <- renderPlotly({
    shrub_data %>%
      filter(species == input$shrub_data_species)%>%
   ggplot(aes(year, adults))+
      geom_point(aes(group = plot))+
      geom_line(aes(group = plot))+
      stat_summary(aes(year, adults), 
                   fun = mean, geom = "line", size = 1.5,
                   color = "green") +
      stat_summary(aes(year, adults), 
                   fun = mean, geom = "point", size = 3,
                   color = "green") +
      theme_minimal()+
      labs(x = "Year", y = "Number of adults")+
      ggtitle("Shrub species trends over time")
  })
  
  # Shrub Plot
  output$shrub_pred_plot <- renderPlotly({
    shrub_pred_data = readRDS(paste0(here("model_runs/"),
                                 paste(c("model_predictions", 
                                    input$shrub_pred_metric,
                                    input$shrub_pred_scenario,
                                    paste(input$shrub_pred_climate, collapse = "_"),
                                    input$shrub_pred_model),
                                    collapse = "_"), ".rds"))%>%
      filter(species == input$shrub_pred_species)%>%
      group_by(year, sim, metric, scenario, bioclim, model, species)%>%
      summarize(N = sum(N, na.rm = TRUE))
    
    shrub_observed = 
      observed_totals %>% 
      filter(species == input$shrub_pred_species)%>%
      rename(observed = tot)
    
    ggplot(data = shrub_pred_data)+
      geom_line(aes(year, N, group = sim), alpha = 0.1)+
      stat_summary(aes(x = year, y = N), 
                   fun = mean, 
                   geom = "line",
                   color = "black",
                   linewidth = 1)+
      geom_point(data=shrub_observed, aes(year, observed), size=3,col="blue")+
      scale_color_viridis_c(direction = -1)+
      xlab("Year")+ylab("Number of adults")+
      theme_minimal(base_size=20)+
      ggtitle(input$shrub_pred_species)+
      theme(legend.position = "bottom")+
      labs(x = "Year", y = "Number of adults across the landscape",
           subtitle = "Blue points show the observed numbers across the landscape")
  })
  
  # Shrub forecasting skill
  output$shrub_mse_plot <- renderPlotly({
    shrub_pred_data = readRDS(paste0(here("model_runs/"),
                                     paste(c("model_predictions", 
                                             input$shrub_pred_metric,
                                             input$shrub_pred_scenario,
                                             paste(input$shrub_pred_climate, collapse = "_"),
                                             input$shrub_pred_model),
                                           collapse = "_"), ".rds")) %>%
      filter(species == input$shrub_pred_species) %>%
      group_by(year, sim, metric, scenario, bioclim, model, species) %>%
      summarize(mse = median(mse, na.rm = TRUE))
    
    p <- ggplot(shrub_pred_data, aes(x = as.factor(year), mse)) +
      geom_boxplot(outliers = FALSE)+
      theme_minimal() +
      labs(
        title = "Forecasting skill",
        x = "Year",
        y = "MSE"
      ) +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
    
    ggplotly(p) %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 50)
      )
  })
  
  output$ndvi_spatial_plot <- renderPlotly({
    ndvi_spatial = rast(here("ndvi_metrics_2024", paste0(input$ndvi_spatial_metric, "_2024.tif")))
    
    p <- ggplot() +
      geom_spatraster(data = ndvi_spatial) +
      scale_fill_gradientn(
        colors = c("#f7fcf5", "#e5f5e0", "#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#006d2c", "#00441b"),
        na.value = NA
      ) +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(
        showlegend = TRUE,
        margin = list(t = 50)
      )
  })

  output$shrub_spatial_plot <- renderPlotly({
    sp = ifelse(input$shrub_spatial_species == "Lavandula stoechas", "lavandula", "halimium")
    shrub_spatial = rast(here("spatial_predictions", paste0(sp,"_", input$ndvi_spatial_metric, ".tif")))
    
    violet_gradient = c("#f3e5f5","#6a1b9a")
    orange_gradient = c("#fff3e0","#ef6c00")
    
    p <- ggplot() +
      geom_spatraster(data = shrub_spatial) +
      scale_fill_gradient(
        low = ifelse(sp == "lavandula", violet_gradient[1], orange_gradient[1]),
        high = ifelse(sp == "lavandula", violet_gradient[2], orange_gradient[2]),
        na.value = NA
      ) +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(
        showlegend = TRUE,
        margin = list(t = 50)
      )
  })
}
# Run the apptest
# shinyApp(ui = ui, server = server)
# Run the app
shinyApp(
  ui = ui,
  server = server,
  options = list(
    host = "0.0.0.0",  # Listen on all network interfaces
    port = 3838         # The port to be used (change to 3839 if needed)
  )
)


