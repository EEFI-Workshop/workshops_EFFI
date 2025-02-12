run_spatial_model <- function(ndvi_metric) {
  # Load and process NDVI data
  df.ndvi <- read.csv(here("ndvi_metrics.csv"))
  df.ndvi <- df.ndvi[,c("plot", "year", ndvi_metric)]
  
  # Load abundance data including 2023
  num <- read.csv(here("shrub_number.csv"))
  num_2023 <- read.csv(here("shrub_number_2324.csv"), sep=" ")
  num_2023 = num_2023[num_2023$year == 2023,]
    
  colnames(num)[5:7] <- c("adults", "saplings", "seedlings")
  colnames(num_2023)[4:6] <- c("adults", "saplings", "seedlings")
  num = num[, c("plot", "year", "species", "adults", "saplings", "seedlings")]
  num_2023 = num_2023[, c("plot", "year", "species", "adults", "saplings", "seedlings")]
  
  # Combine data
  num <- rbind(num, num_2023)
  
  # Add satellite derived measure to abundance data 
  lag <- 2
  df.ndvi.mu <- df.ndvi
  df.ndvi.mu$year <- as.numeric(df.ndvi.mu$year) + lag
  df.ndvi.mu$cov <- df.ndvi.mu[, ndvi_metric]
  
  # Focus on two abundant shrubs
  sub <- num[num$species %in% c("Halimium halimifolium", "Lavandula stoechas"),]
  
  ############### 
  # Setup for JAGS model
  n.plots <- 18
  n.years <- length(2007:2022)
  
  ### Halimium
  C.hal <- array(NA, c(n.plots, n.years))
  cov.hal <- array(NA, c(n.plots, n.years))
  
  hal <- sub[sub$species %in% "Halimium halimifolium",]
  hal$saplings[is.na(hal$saplings)] <- 0
  hal$adults[is.na(hal$adults)] <- 0
  
  for(i in 1:n.plots) { # site loop
    for(k in 1:n.years) {
      year <- as.character(2007:2022)[k]
      plot <- unique(sub$plot)[i]
      
      sum <- as.numeric(hal[hal$plot %in% plot & hal$year %in% year, c("adults")])
      cov <- df.ndvi.mu$cov[df.ndvi.mu$plot %in% plot & df.ndvi.mu$year %in% year]
      
      if(length(sum) > 0) C.hal[i,k] <- sum
      if(length(cov) > 0) cov.hal[i,k] <- cov
    }
  }
  
  C.hal[5,16] <- 0
  
  ## Lavandula
  C.lav <- array(NA, c(n.plots, n.years))
  cov.lav <- array(NA, c(n.plots, n.years))
  
  lav <- sub[sub$species %in% "Lavandula stoechas",]
  lav$saplings[is.na(lav$saplings)] <- 0
  lav$adults[is.na(lav$adults)] <- 0
  
  for(i in 1:n.plots) { # site loop
    for(k in 1:n.years) {
      year <- as.character(2007:2022)[k]
      plot <- unique(sub$plot)[i]
      
      sum <- as.numeric(lav[lav$plot %in% plot & lav$year %in% year, c("adults")])
      cov <- df.ndvi.mu$cov[df.ndvi.mu$plot %in% plot & df.ndvi.mu$year %in% year]
      
      if(length(sum) > 0) C.lav[i,k] <- sum
      if(length(cov) > 0) cov.lav[i,k] <- cov
    }
  }
  
  C.lav[5,16] <- 0
  
  # Get observed 2023 values
  hal_2023 <- hal[hal$year == "2023", ]
  lav_2023 <- lav[lav$year == "2023", ]
  
  # Bundle data for JAGS model
  bdata <- list(n.hal = C.hal, C.hal=C.hal,
                n.lav = C.lav, C.lav=C.lav,
                nsites = dim(C.hal)[1],
                cov.hal=cov.hal,
                cov.lav=cov.lav,
                nyears = dim(C.hal)[2]
  )
  
  # Specify model in BUGS language
  cat(file = "abundance change shrubs.txt","
    model {
    # Priors
    a0.h ~ dnorm( 0 , 1.0E-05 )
    a1.h ~ dnorm( 0 , 1.0E-05 ) 
    a2.h ~ dnorm( 0 , 1.0E-05 )
  
    a0.l ~ dnorm( 0 , 1.0E-05 )
    a1.l ~ dnorm( 0 , 1.0E-05 ) 
    a2.l ~ dnorm( 0 , 1.0E-05 )

    for(i in 1:nsites) { # Loop over sites
    
    #Initial abundance
    N.h[i,1] <- C.hal[i,1]
    N.l[i,1] <- C.lav[i,1]

    #Specify the model for years 2 through nYears
    for(t in 1:(nyears-1)) {
    
    # Halimium halimifolium
    n.hal[i,t+1] ~ dpois(N.h[i,t+1])
    log(N.h[i,t+1]) <- a0.h + a1.h *cov.hal[i,t]  + a2.h * log(N.h[i,t]+0.001) 
 
    # Lavandula stoechas 
    n.lav[i,t+1] ~ dpois(N.l[i,t+1])
    log(N.l[i,t+1]) <- a0.l + a1.l *cov.lav[i,t]  + a2.l * log(N.l[i,t]+0.001) 

    }
    }
    }
    ")
  
  # Initial values for MCMC
  inits <- function(){list(a0.h=rnorm(1,0,0.01),
                           a1.h=rnorm(1,0,0.01),
                           a2.h=rnorm(1,0,0.01),
                           a0.l=rnorm(1,0,0.01),
                           a1.l=rnorm(1,0,0.01),
                           a2.l=rnorm(1,0,0.01))}
  
  # Parameters to monitor
  params <- c("a0.h", "a1.h", "a2.h",
              "a0.l", "a1.l", "a2.l")
  
  # MCMC settings
  na <- 10000 ; ni <- 450000 ; nt <- 500 ; nb <- 100000 ; nc <- 3
  
  # Run JAGS model
  out1 <- jags(bdata, inits, params, "abundance change shrubs.txt", 
               n.adapt = na, n.chains = nc,
               n.thin = nt, n.iter = ni, n.burnin = nb, parallel=T)
  
  # Return only what we need for spatial predictions
  return(list(
    mcmc = out1,
    observed_2023 = list(
      halimium = hal_2023$adults,
      lavandula = lav_2023$adults
    )
  ))
}