
library(ows4R)
library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(readr)

# Set your working directory
setwd("/Users/maria/Dropbox/collaborations/EEFI/workshop")

## The script has the following sections:

# 1. Load the spatial data obtained form process_ndvi.R 
# 2. Model abundance change. Here, I just use a simple autoregressive model done in JAGS
# 3. Do the forecast and quantify forecast skill

############### 
#1. LOAD SPATIAL COVARIATE DATA AT POINT COORDINATES

df.ndvi <- read.csv("df.ndvi.csv")
df.ndvi_land <- read.csv("df.ndvi_land.csv")

### Add shrub abundance data: 
# num=read_csv(url("https://raw.githubusercontent.com/MariaPaniw/workshops_EFFI/refs/heads/main/vegetation_donana/shrub_number.csv?token=GHSAT0AAAAAAC2TAOO5VDUV3XKSXUPCSRRUZ3RURUA"))

num <- read.csv("shrub_number.csv")

colnames(num)[5:7]=c("adults","saplings","seedlings")

# Add satellite derived measure to abundance data 

lag=2
# Here we use a lag (e.g., 2): summer NDVI of year t-lag affects abundance change from year t to t+1

## !!!! PLACEHOLDER: Mean NDVI per year:

df.ndvi.mu <- aggregate(ndvi~plot+year,mean,data=df.ndvi)

#add 1 to the year to match with the abundance data: 

df.ndvi.mu$year = as.numeric(df.ndvi.mu$year)+lag

# Set covariate (already scaled -1 to 1)

df.ndvi.mu$cov <- df.ndvi.mu$ndvi

#### We can add another covariate if we want: interspecific abundances
### For now, I comment this out

# num$cov2=NA
# 
# years=unique(num$year)
# plots=unique(num$plot)
# species=unique(num$species)
# 
# for(a in years){
#   
#   for(b in plots){
#     
#     for(c in species){
#       
#       if(length(num$cov[num$species%in%c&num$plot%in%b&num$year%in%a])>0){
#         
#         num$cov2[num$species%in%c&num$plot%in%b&num$year%in%a]=sum(num$adults[!num$species%in%c&num$plot%in%b&num$year%in%a],na.rm=T)
#         
#       }
#     }
#   }
# }

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
# cov2.hal=array(NA,c(n.plots,n.years))

hal=sub[sub$species%in%"Halimium halimifolium",]

hal$saplings[is.na(hal$saplings)]=0
hal$adults[is.na(hal$adults)]=0

for(i in 1:n.plots){ # site loop
  for(k in 1:n.years){
    
    year=as.character(2007:2022)[k]
    plot=unique(sub$plot)[i]
  
    sum=as.numeric(hal[hal$plot%in%plot&hal$year%in%year,c("adults")])
    cov= df.ndvi.mu$cov[df.ndvi.mu$plot%in%plot&df.ndvi.mu$year%in%year]
    # cov2= hal$cov2[hal$plot%in%plot&hal$year%in%year]
    if(length(sum)>0) C.hal[i,k]=sum
    
    if(length(cov)>0)  cov.hal[i,k]=cov
    
    # if(length(cov2)>0)  cov2.hal[i,k]=cov2
    
  }
  
}

C.hal[5,16]=0

# We have to interpolate the neighbors if we use neighbors
# 
# cov2.hal[,3]=round(rowMeans(cov.hal[,c(2,4)]))
# cov2.hal[,5:6]=round(rowMeans(cov.hal[,c(4,7)]))
# cov2.hal[,8:10]=round(rowMeans(cov.hal[,c(7,11)]))
# cov2.hal[,12]=round(rowMeans(cov.hal[,c(11,13)]))
# 
# cov2.hal[5,16]=10

## Lavandula
C.lav=array(NA,c(n.plots,n.years))
cov.lav=array(NA,c(n.plots,n.years))
# cov2.lav=array(NA,c(n.plots,n.years))
lav=sub[sub$species%in%"Lavandula stoechas",]

lav$saplings[is.na(lav$saplings)]=0
lav$adults[is.na(lav$adults)]=0

for(i in 1:n.plots){ # site loop
  for(k in 1:n.years){
    
    year=as.character(2007:2022)[k]
    plot=unique(sub$plot)[i]
    
    sum=as.numeric(lav[lav$plot%in%plot&lav$year%in%year,c("adults")])
    cov= df.ndvi.mu$cov[df.ndvi.mu$plot%in%plot&df.ndvi.mu$year%in%year]
    # cov2= lav$cov2[lav$plot%in%plot&lav$year%in%year]
    
    if(length(sum)>0) C.lav[i,k]=sum
    
    if(length(cov)>0)  cov.lav[i,k]=cov
    
    # if(length(cov2)>0)  cov2.lav[i,k]=cov
    
  }
  
}

C.lav[5,16]=0

# We have to interpolate the neighbors

# cov.lav[,3]=round(rowMeans(cov.lav[,c(2,4)]))
# cov.lav[,5:6]=round(rowMeans(cov.lav[,c(4,7)]))
# cov.lav[,8:10]=round(rowMeans(cov.lav[,c(7,11)]))
# cov.lav[,12]=round(rowMeans(cov.lav[,c(11,13)]))
# 
# cov.lav[5,16]=10

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

library(jagsUI)

na <- 10000 ; ni <- 450000 ; nt <- 500 ; nb <- 100000 ; nc <- 3


out1 <- jags(bdata, inits, params, "abundance change shrubs.txt", n.adapt = na, n.chains = nc,
             n.thin = nt, n.iter = ni, n.burnin = nb,parallel=T)

traceplot(out1) # the output is not too bad; chains look convereged

print(out1, 3) # Looks good

# Bayesian p-value

mean(out1$sims.list$fit1.pred > out1$sims.list$fit1.data)

# Predicted values underestimate observed (for Halimium)
plot(out1$sims.list$fit1.pred, out1$sims.list$fit1.data)
abline(1,1)

out1$mean

hist(out1$sims.list$a1.h)## Effect of NDVI is not signifcant (95% CI  crosses 0)
hist(out1$sims.list$a1.l)## Effect of NDVI is signifcant (95% CI does not cross 0)

## Plot observed vs Predicted

# Total abundance: 

# HALIMIUM
bay.out=out1$sims.list$Ntot.h[sample(c(1:dim(out1$sims.list$Ntot.h)[1]),100),] # take a random sample of 100 posterior values

df.pred=data.frame(N=as.numeric(bay.out[,-1]),
                   year=rep(c(2008:2022),each=dim(bay.out)[1]),
                   sim=1:dim(bay.out)[1])

hal.tot=aggregate(cbind(adults,saplings)~year,data=hal,function(x) sum(x,na.rm=T))
hal.tot$sim=NA
hal.tot$tot=hal.tot$adults
hal.tot$year=as.numeric(as.character(hal.tot$year))

a=ggplot(df.pred,aes(year,N,group=sim))+
  geom_line(alpha=0.1)+
  geom_point(data=hal.tot, aes(year,tot),size=3,col="blue")+
  # scale_color_viridis(discrete = T,option="A")+
  xlab("Year")+ylab("Abundance")+theme_bw(base_size=20)+
  theme(panel.grid = element_blank())+
  ggtitle("Halimium halimifolium")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.position = "none",
        legend.title = element_blank())

a
# LAVANDULA
bay.out=out1$sims.list$Ntot.l[sample(c(1:dim(out1$sims.list$Ntot.l)[1]),100),] # take a random sample of 100 posterior values

df.pred=data.frame(N=as.numeric(bay.out[,-1]),
                   year=rep(c(2008:2022),each=dim(bay.out)[1]),
                   sim=1:dim(bay.out)[1])

lav.tot=aggregate(cbind(adults,saplings)~year,data=lav,function(x) sum(x,na.rm=T))
lav.tot$sim=NA
lav.tot$tot=lav.tot$adults
lav.tot$year=as.numeric(as.character(lav.tot$year))

b=ggplot(df.pred,aes(year,N,group=sim))+
  geom_line(alpha=0.1)+
  geom_point(data=lav.tot, aes(year,tot),size=3,col="blue")+
  # scale_color_viridis(discrete = T,option="A")+
  xlab("Year")+ylab("Abundance")+theme_bw(base_size=20)+
  theme(panel.grid = element_blank())+
  ggtitle("Lavandula stoechas")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.position = "none",
        legend.title = element_blank())

b
############### 
#4. FORECAST NEXT YEARS

# Here we use the abudances for 2022 to predict to 2023 and 2024

# Load shrub abundances at 18 study sites for 2023 and 2024
num_fut =read.csv("shrub_number_2324.csv", sep=" ")

colnames(num_fut)[4:6] <- c("adults","saplings","seedlings")

sub_fut <- num_fut[num_fut$species%in%c("Halimium halimifolium","Lavandula stoechas"),]

# The abundances at the landscape level are already in the data frame ab_land

# Predict NDVI based on weather (needs to be done)

# !!!!! PLACEHOLDER: MEAN NDVI FOR 23/24

n.years.pred=2 #predictions over 2 years

ndvi.pred=array(NA,c(n.plots,n.years.pred))

for(i in 1:n.plots){ # site loop
  for(k in 1:n.years.pred){
    
    year=as.character(2022:2023)[k]
    plot=unique(sub$plot)[i]
   
    cov= df.ndvi.mu$cov[df.ndvi.mu$plot%in%plot&df.ndvi.mu$year%in%year]

    if(length(cov)>0)  ndvi.pred[i,k]=cov
    
  }
  
}

## Sample 100 posterior values from 2100 ones (to speed up predictions)

par.sub=sample(1:length(out1$sims.list$a0.h),1000)

n.hal.pred=array(NA,c(length(par.sub),n.plots,n.years.pred)) #save predictions for Halimium 

n.lav.pred=array(NA,c(length(par.sub),n.plots,n.years.pred)) #save predictions for Lavandula 

### Forecast skill
deviance.hal=array(NA,c(length(par.sub),n.plots,n.years.pred)) #save predictions for Lavandula 
deviance.lav=array(NA,c(length(par.sub),n.plots,n.years.pred)) 

for(x in 1:length(par.sub)){
  
  for(i in 1:n.plots) { # Loop over sites
    
    #Initial abundance (last year of data with which the model was fitted)
    
    N.h <- C.hal[i,ncol(C.hal)]
    
    N.l <- C.lav[i,ncol(C.hal)]
    
    
    #Specify the model for years 2023 anbd 2024
    
    for(t in 1:n.years.pred) {
      
      
      # Halimium halimifolium
      
      N.h <- exp(out1$sims.list$a0.h[par.sub[x]] + out1$sims.list$a1.h[par.sub[x]] *ndvi.pred[i,t]  + out1$sims.list$a2.h[par.sub[x]] * log(N.h+0.001)) 
      
      n.hal.pred[x,i,t] <-  rpois(1,N.h)
      
      ad.obs.hal=sub_fut$adults[sub_fut$species=="Halimium halimifolium"&sub_fut$year==unique(sub_fut$year)[t]&sub_fut$plot==unique(sub_fut$plot)[i]]
      
      if(n.hal.pred[x,i,t]>0&length(ad.obs.hal)>0){
        
        deviance.hal[x,i,t] = ( ad.obs.hal- n.hal.pred[x,i,t])^2
        
      }
     
       # Lavandula stoechas 
      
      N.l <- exp(out1$sims.list$a0.l[par.sub[x]] + out1$sims.list$a1.l[par.sub[x]] *ndvi.pred[i,t]  + out1$sims.list$a2.l[par.sub[x]] * log(N.l+0.001)) 
      
      n.lav.pred[x,i,t] <-  rpois(1,N.l)
      
      ad.obs.lav =sub_fut$adults[sub_fut$species=="Lavandula stoechas"&sub_fut$year==unique(sub_fut$year)[t]&sub_fut$plot==unique(sub_fut$plot)[i]]
      if(n.lav.pred[x,i,t]>0&length(ad.obs.lav)>0){
        
        deviance.lav[x,i,t] = ( ad.obs.lav - n.lav.pred[x,i,t])^2
        
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

## Halimuim
df.pred=data.frame(N=as.numeric(n.hal.tot.pred),
                   year=rep(c(2023:2024),each=dim(n.hal.tot.pred)[1]),
                   sim=1:dim(n.hal.tot.pred)[1])

hal.tot=aggregate(adults~year,data=sub_fut[sub_fut$species=="Halimium halimifolium",],function(x) sum(x,na.rm=T))
hal.tot$sim=NA
hal.tot$tot=hal.tot$adults
hal.tot$year=as.numeric(as.character(hal.tot$year))

a.pred=ggplot(df.pred,aes(year,N,group=sim))+
  geom_line(alpha=0.1)+
  geom_point(data=hal.tot, aes(year,tot),size=3,col="blue")+
  # scale_color_viridis(discrete = T,option="A")+
  xlab("Year")+ylab("Abundance")+theme_bw(base_size=20)+
  theme(panel.grid = element_blank())+
  ggtitle("Halimium halimifolium")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.position = "none",
        legend.title = element_blank())+
  scale_x_continuous(breaks=c(2023,2024))

a.pred

## Lavandula
df.pred=data.frame(N=as.numeric(n.lav.tot.pred),
                   year=rep(c(2023:2024),each=dim(n.lav.tot.pred)[1]),
                   sim=1:dim(n.lav.tot.pred)[1])

lav.tot=aggregate(adults~year,data=sub_fut[sub_fut$species=="Lavandula stoechas",],function(x) sum(x,na.rm=T))
lav.tot$sim=NA
lav.tot$tot=lav.tot$adults
lav.tot$year=as.numeric(as.character(lav.tot$year))

b.pred=ggplot(df.pred,aes(year,N,group=sim))+
  geom_line(alpha=0.1)+
  geom_point(data=lav.tot, aes(year,tot),size=3,col="blue")+
  # scale_color_viridis(discrete = T,option="A")+
  xlab("Year")+ylab("Abundance")+theme_bw(base_size=20)+
  theme(panel.grid = element_blank())+
  ggtitle("Lavandula stoechas")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.position = "none",
        legend.title = element_blank())+
  scale_x_continuous(breaks=c(2023,2024))

b.pred

############### 
#4. FORECAST NEXT YEARS & TO NEXT SITES

# Here we use the abudances for 2023 to predict to 2024 and 2025

# Load shrub abundances at newly monitored > 100 sites for 2023 and 2024
num_fut =read.csv("coordinates_2023_02.csv")

colnames(num_fut)[3] <- c("adults")

num_fut$ID <- factor(paste(num_fut$lon,num_fut$lat))

levels(num_fut$ID) = 1:length(levels(num_fut$ID))

num_fut <- droplevels(num_fut[num_fut$ID!=c("119","120"),])
num_fut <- droplevels(num_fut[num_fut$ID!=c("119","120"),])

sub_fut=num_fut

n.plots=length(levels(sub_fut$ID))

# Predict NDVI based on weather (needs to be done)

# !!!!! PLACEHOLDER: MEAN NDVI FOR 23/24

df.ndvi.mu <- aggregate(ndvi~plot+year,mean,data=df.ndvi_land)

#add 1 to the year to match with the abundance data: 

df.ndvi.mu$year = as.numeric(df.ndvi.mu$year)+lag

# Set covariate (already scaled -1 to 1)

df.ndvi.mu$cov <- df.ndvi.mu$ndvi


n.years.pred=2 #predictions over 2 years

ndvi.pred=array(NA,c(n.plots,n.years.pred))

for(i in 1:n.plots){ # site loop
  for(k in 1:n.years.pred){
    
    year=as.character(2023:2024)[k]
    plot=unique(sub_fut$ID)[i]
    
    cov= df.ndvi.mu$cov[df.ndvi.mu$plot%in%plot&df.ndvi.mu$year%in%year]
    
    if(length(cov)>0)  ndvi.pred[i,k]=cov
    
  }
  
}

## Sample 100 posterior values from 2100 ones (to speed up predictions)

par.sub=sample(1:length(out1$sims.list$a0.h),100)

n.hal.pred=array(NA,c(length(par.sub),n.plots,n.years.pred)) #save predictions for Halimium 

n.lav.pred=array(NA,c(length(par.sub),n.plots,n.years.pred)) #save predictions for Lavandula 

### Forecast skill (here only for 1 year, because we don't have data for 2025)
deviance.hal=array(NA,c(length(par.sub),n.plots))  
deviance.lav=array(NA,c(length(par.sub),n.plots)) 

for(x in 1:length(par.sub)){
  
  for(i in 1:n.plots) { # Loop over sites
    
    #Initial abundance (last year of data with which the model was fitted)
    
    N.h <- num_fut$adults[num_fut$spp=="Halimium halimifolium"&num_fut$year==2023][i]
    
    N.l <- num_fut$adults[num_fut$spp=="Lavandula stoechas"&num_fut$year==2023][i]
    
    
    
    #Specify the model for years 2024 anbd 2025
    
    for(t in 1:n.years.pred) {
      
      
      # Halimium halimifolium
      
      N.h <- exp(out1$sims.list$a0.h[par.sub[x]] + out1$sims.list$a1.h[par.sub[x]] *ndvi.pred[i,t]  + out1$sims.list$a2.h[par.sub[x]] * log(N.h+0.001)) 
      
      n.hal.pred[x,i,t] <-  rpois(1,N.h)
      
      if(t==1){ # for 2024 only 
        
        ad.obs.hal=sub_fut$adults[sub_fut$spp=="Halimium halimifolium"&sub_fut$year==unique(sub_fut$year)[2]&sub_fut$ID==unique(sub_fut$ID)[i]]
        
        if(n.hal.pred[x,i,t]>0&length(ad.obs.hal)>0){
          
          deviance.hal[x,i] = ( ad.obs.hal- n.hal.pred[x,i,t])^2
          
        }
      }
      
      
      # Lavandula stoechas 
      
      N.l <- exp(out1$sims.list$a0.l[par.sub[x]] + out1$sims.list$a1.l[par.sub[x]] *ndvi.pred[i,t]  + out1$sims.list$a2.l[par.sub[x]] * log(N.l+0.001)) 
      
      n.lav.pred[x,i,t] <-  rpois(1,N.l)
      
      if(t==1){ # for 2024 only 
        
        ad.obs.lav =sub_fut$adults[sub_fut$spp=="Lavandula stoechas"&sub_fut$year==unique(sub_fut$year)[2]&sub_fut$ID==unique(sub_fut$ID)[i]]
        if(n.lav.pred[x,i,t]>0&length(ad.obs.lav)>0){
          
          deviance.lav[x,i] = ( ad.obs.lav - n.lav.pred[x,i,t])^2
          
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

# Haven't done those
