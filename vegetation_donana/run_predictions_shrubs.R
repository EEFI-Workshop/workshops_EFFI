# Function to run model for a specific parameter combination
run_model_combination <- function(ndvi_metric, scenario, bioclim, model) {
  # Store parameters
  params <- list(
    ndvi_metric = ndvi_metric,
    scenario = scenario,
    bioclim = bioclim,
    model = model
  )
  
  # Load and process NDVI data
  df.ndvi <- read.csv(here("ndvi_metrics.csv"))
  df.ndvi <- df.ndvi[,c("plot", "year", ndvi_metric)]
  
  # Load NDVI predictions with specific parameters
  ndvi_predictions <- readRDS(here("ndvi_predictions.rds"))
  ndvi_predictions <- ndvi_predictions[ndvi_predictions$metric == ndvi_metric, ]
  ndvi_predictions <- ndvi_predictions[ndvi_predictions$scenario == scenario, ]
  ndvi_predictions <- ndvi_predictions[ndvi_predictions$bioclim_vars == bioclim, ]
  ndvi_predictions <- ndvi_predictions[ndvi_predictions$model == model, ]
  ndvi_predictions <- ndvi_predictions[, c("plot", "year", "predicted")]
  colnames(ndvi_predictions) <- c("plot", "year", ndvi_metric)
  ndvi_predictions <- ndvi_predictions[ndvi_predictions$year>2024, ]
  df.ndvi = rbind(df.ndvi, ndvi_predictions)
  
  
  # num=read_csv(url("https://raw.githubusercontent.com/MariaPaniw/workshops_EFFI/refs/heads/main/vegetation_donana/shrub_number.csv?token=GHSAT0AAAAAAC2TAOO5VDUV3XKSXUPCSRRUZ3RURUA"))
  
  num <- read.csv(here("shrub_number.csv"))
  
  colnames(num)[5:7]=c("adults","saplings","seedlings")
  
  # Add satellite derived measure to abundance data 
  
  lag=2
  # Here we use a lag (e.g., 2): summer NDVI of year t-lag affects abundance change from year t to t+1
  
  ## !!!! PLACEHOLDER: Mean NDVI per year:
  
  #add 1 to the year to match with the abundance data: 
  df.ndvi.mu <- df.ndvi
  df.ndvi.mu$year = as.numeric(df.ndvi.mu$year)+lag
  # Set the ndvi metric as a covariate
  
  df.ndvi.mu$cov <- df.ndvi.mu[, ndvi_metric]
  
  # we focus on two very abundant shrubs with lots of data
  
  sub=num[num$species%in%c("Halimium halimifolium","Lavandula stoechas"),]
  
  
  ############### 
  #3. STATISTICAL MODEL
  
  # We could use the new SpAbudance package https://doi.org/10.1111/2041-210X.14332
  # To do: make it a proper spatial model 
  # Here I just use JAGS
  
  #Create C matrix 
  
  n.plots=18
  n.years=length(2007:2022)
  
  ### Halimium
  C.hal=array(NA,c(n.plots,n.years))
  cov.hal=array(NA,c(n.plots,n.years))
  
  hal=sub[sub$species%in%"Halimium halimifolium",]
  
  hal$saplings[is.na(hal$saplings)]=0
  hal$adults[is.na(hal$adults)]=0
  
  for(i in 1:n.plots){ # site loop
    for(k in 1:n.years){
      
      year=as.character(2007:2022)[k]
      plot=unique(sub$plot)[i]
      
      sum=as.numeric(hal[hal$plot%in%plot&hal$year%in%year,c("adults")])
      cov= df.ndvi.mu$cov[df.ndvi.mu$plot%in%plot&df.ndvi.mu$year%in%year]
      
      if(length(sum)>0) C.hal[i,k]=sum
      
      if(length(cov)>0)  cov.hal[i,k]=cov
      
      
      
    }
    
  }
  
  C.hal[5,16]=0
  
  ## Lavandula
  C.lav=array(NA,c(n.plots,n.years))
  cov.lav=array(NA,c(n.plots,n.years))
  
  lav=sub[sub$species%in%"Lavandula stoechas",]
  
  lav$saplings[is.na(lav$saplings)]=0
  lav$adults[is.na(lav$adults)]=0
  
  for(i in 1:n.plots){ # site loop
    for(k in 1:n.years){
      
      year=as.character(2007:2022)[k]
      plot=unique(sub$plot)[i]
      
      sum=as.numeric(lav[lav$plot%in%plot&lav$year%in%year,c("adults")])
      cov= df.ndvi.mu$cov[df.ndvi.mu$plot%in%plot&df.ndvi.mu$year%in%year]
      
      if(length(sum)>0) C.lav[i,k]=sum
      
      if(length(cov)>0)  cov.lav[i,k]=cov
      
      
    }
    
  }
  
  C.lav[5,16]=0
  
  #### Bundle data for JAGS model
  
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
  

    # # Goodness of fit

    n.pred[i,t+1] ~ dpois(N.h[i,t+1]) # Part 2 of HM
    e1[i,t+1] <- N.h[i,t+1]
    resid1[i,t+1] <- pow(pow(n.hal[i,t+1], 0.5) - pow(e1[i,t+1], 0.5), 2)
    resid1.pred[i,t+1] <- pow(pow(n.pred[i,t+1], 0.5) - pow(e1[i,t+1], 0.5), 2)

    }

    }
     
    fit1.data <- sum(resid1[,2:nyears]) # Fit statistic 
    fit1.pred <- sum(resid1.pred[,2:nyears])
    
    for( t in 2:nyears){
    Ntot.h[t] <- sum(N.h[,t])
   
    
    Ntot.l[t] <- sum(N.l[,t])
    
    
    }
    
}
    ")
  
  inits <- function(){list(a0.h=rnorm(1,0,0.01),
                           a1.h=rnorm(1,0,0.01),
                           a2.h=rnorm(1,0,0.01),
                           
                           a0.l=rnorm(1,0,0.01),
                           a1.l=rnorm(1,0,0.01),
                           a2.l=rnorm(1,0,0.01))}
  
  # What to monitor 
  
  params <- c( "a0.h",
               "a1.h",
               "a2.h",
               "a0.l",
               "a1.l",
               "a2.l",
               
               "Ntot.h",
               "N.h",
               
               "Ntot.l",
               "N.l",
               "fit1.data",
               "fit1.pred")
  
  na <- 10000 ; ni <- 450000 ; nt <- 500 ; nb <- 100000 ; nc <- 6
  
  
  out1 <- jags(bdata, inits, params, "abundance change shrubs.txt", n.adapt = na, n.chains = nc,
               n.thin = nt, n.iter = ni, n.burnin = nb,parallel=T)
  

  ############### 
  #4. FORECAST NEXT YEARS
  
  # Here we use the abudances for 2022 to predict to 2023 and 2024
  
  # Load shrub abundances at 18 study sites for 2023 and 2024
  num_fut = read.csv(here("shrub_number_2324.csv"), sep=" ")
  
  colnames(num_fut)[4:6] <- c("adults","saplings","seedlings")
  
  sub_fut <- num_fut[num_fut$species%in%c("Halimium halimifolium","Lavandula stoechas"),]
  
  # The abundances at the landscape level are already in the data frame ab_land
  
  # Predict NDVI based on weather (needs to be done)
  
  # !!!!! PLACEHOLDER: MEAN NDVI FOR 23/24
  
  n.years.pred = 4  # Extend predictions to 4 years (2023-2026)
  n.years.obs = 2   # Observed data only until 2024 (2023-2024)
  
  # Extend NDVI predictions array
  ndvi.pred = array(NA, c(n.plots, n.years.pred))
  for(i in 1:n.plots){ # site loop
    for(k in 1:n.years.pred){
      year = as.character(2022:2025)[k]  # Extended years
      plot = unique(sub$plot)[i]
      
      cov = df.ndvi.mu$cov[df.ndvi.mu$plot%in%plot & df.ndvi.mu$year%in%year]
      
      if(length(cov)>0)  ndvi.pred[i,k] = cov
    }
  }
  
  # Sample posterior values
  par.sub = sample(1:length(out1$sims.list$a0.h), 1000)
  
  # Arrays for predictions (4 years)
  n.hal.pred = array(NA, c(length(par.sub), n.plots, n.years.pred))
  n.lav.pred = array(NA, c(length(par.sub), n.plots, n.years.pred))
  
  # Arrays for deviance (only 2 years - matching observed data)
  deviance.hal = array(NA, c(length(par.sub), n.plots, n.years.obs))
  deviance.lav = array(NA, c(length(par.sub), n.plots, n.years.obs))
  
  for(x in 1:length(par.sub)){
    for(i in 1:n.plots) {
      # Initial abundance
      N.h <- C.hal[i,ncol(C.hal)]
      N.l <- C.lav[i,ncol(C.hal)]
      
      # Loop for all predictions (4 years)
      for(t in 1:n.years.pred) {
        # Halimium predictions
        N.h <- exp(out1$sims.list$a0.h[par.sub[x]] + 
                     out1$sims.list$a1.h[par.sub[x]] * ndvi.pred[i,t] + 
                     out1$sims.list$a2.h[par.sub[x]] * log(N.h+0.001))
        
        n.hal.pred[x,i,t] <- rpois(1,N.h)
        
        # Calculate deviance only for years with observed data
        if(t <= n.years.obs) {
          ad.obs.hal = sub_fut$adults[sub_fut$species=="Halimium halimifolium" &
                                        sub_fut$year==unique(sub_fut$year)[t] &
                                        sub_fut$plot==unique(sub_fut$plot)[i]]
          
          if(n.hal.pred[x,i,t]>0 & length(ad.obs.hal)>0){
            deviance.hal[x,i,t] = (ad.obs.hal - n.hal.pred[x,i,t])^2
          }
        }
        
        # Lavandula predictions
        N.l <- exp(out1$sims.list$a0.l[par.sub[x]] + 
                     out1$sims.list$a1.l[par.sub[x]] * ndvi.pred[i,t] + 
                     out1$sims.list$a2.l[par.sub[x]] * log(N.l+0.001))
        
        n.lav.pred[x,i,t] <- rpois(1,N.l)
        
        # Calculate deviance only for years with observed data
        if(t <= n.years.obs) {
          ad.obs.lav = sub_fut$adults[sub_fut$species=="Lavandula stoechas" &
                                        sub_fut$year==unique(sub_fut$year)[t] &
                                        sub_fut$plot==unique(sub_fut$plot)[i]]
          
          if(n.lav.pred[x,i,t]>0 & length(ad.obs.lav)>0){
            deviance.lav[x,i,t] = (ad.obs.lav - n.lav.pred[x,i,t])^2
          }
        }
      }
    }
  }
  
  
  ### Forecast skill as mean square error:
  
  MSE.Hal=apply(deviance.hal,c(1),sum,na.rm=T) # uncertainty due to posterior samples kept
  hist(MSE.Hal)
  
  MSE.Lav=apply(deviance.lav,c(1),sum,na.rm=T) # uncertainty due to posterior samples kept
  hist(MSE.Lav)
  
  ### PLOTS
  ### Prepare the data for plotting 
  
  n.hal.tot.pred=apply(n.hal.pred,c(1,3),sum)
  
  n.lav.tot.pred=apply(n.lav.pred,c(1,3),sum)
  
  ## Halimium
  df.pred = data.frame(
    N = as.numeric(n.hal.tot.pred),
    year = rep(c(2023:2026), each=dim(n.hal.tot.pred)[1]),  # Extended to 2026 to match n.years.pred=4
    sim = 1:dim(n.hal.tot.pred)[1]
  )
  
  hal.tot=aggregate(adults~year,data=sub_fut[sub_fut$species=="Halimium halimifolium",],function(x) sum(x,na.rm=T))
  hal.tot$sim=NA
  hal.tot$tot=hal.tot$adults
  hal.tot$year=as.numeric(as.character(hal.tot$year))
  
  # a.pred=ggplot(df.pred,aes(year,N,group=sim))+
  #   geom_line(alpha=0.1)+
  #   geom_point(data=hal.tot, aes(year,tot),size=3,col="blue")+
  #   # scale_color_viridis(discrete = T,option="A")+
  #   xlab("Year")+ylab("Abundance")+theme_bw(base_size=20)+
  #   theme(panel.grid = element_blank())+
  #   ggtitle("Halimium halimifolium")+
  #   theme(panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         strip.background = element_blank(),
  #         panel.border = element_rect(colour = "black"),
  #         legend.position = "none",
  #         legend.title = element_blank())+
  #   scale_x_continuous(breaks=c(2023:2026))
  # 
  # a.pred
  
  ## Lavandula
  df.pred = data.frame(
    N = as.numeric(n.hal.tot.pred),
    year = rep(c(2023:2026), each=dim(n.hal.tot.pred)[1]),  # Extended to 2026 to match n.years.pred=4
    sim = 1:dim(n.hal.tot.pred)[1]
  )
  
  lav.tot=aggregate(adults~year,data=sub_fut[sub_fut$species=="Lavandula stoechas",],function(x) sum(x,na.rm=T))
  lav.tot$sim=NA
  lav.tot$tot=lav.tot$adults
  lav.tot$year=as.numeric(as.character(lav.tot$year))
  
  # b.pred=ggplot(df.pred,aes(year,N,group=sim))+
  #   geom_line(alpha=0.1)+
  #   geom_point(data=lav.tot, aes(year,tot),size=3,col="blue")+
  #   # scale_color_viridis(discrete = T,option="A")+
  #   xlab("Year")+ylab("Abundance")+theme_bw(base_size=20)+
  #   theme(panel.grid = element_blank())+
  #   ggtitle("Lavandula stoechas")+
  #   theme(panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         strip.background = element_blank(),
  #         panel.border = element_rect(colour = "black"),
  #         legend.position = "none",
  #         legend.title = element_blank())+
  #   scale_x_continuous(breaks=c(2023:2026))
  # 
  # b.pred
  
  
  # Create results list
  results <- list(
    parameters = params,
    predictions = list(
      halimium = data.frame(
        N = as.numeric(n.hal.tot.pred),
        year = rep(c(2023:2026), each=dim(n.hal.tot.pred)[1]),
        sim = 1:dim(n.hal.tot.pred)[1],
        metric = ndvi_metric,
        scenario = scenario,
        bioclim = bioclim,
        model = model,
        species = "Halimium halimifolium"
      ),
      lavandula = data.frame(
        N = as.numeric(n.lav.tot.pred),
        year = rep(c(2023:2026), each=dim(n.lav.tot.pred)[1]),
        sim = 1:dim(n.lav.tot.pred)[1],
        metric = ndvi_metric,
        scenario = scenario,
        bioclim = bioclim,
        model = model,
        species = "Lavandula stoechas"
      )
    ),
    mse = list(
      halimium = MSE.Hal,
      lavandula = MSE.Lav
    )
  )
  
  return(results)
}
