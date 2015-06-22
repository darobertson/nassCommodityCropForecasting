require(rgdal)
require(raster)

HOME <- Sys.getenv("HOME")

# build and project total area model from FAO/World Bank
source("six_state_forecasting_models.R")

future_area_harvested <- t_future_plot # pull our future_area_harvested from the t_future_plot data.frame
  future_area_harvested[future_area_harvested < 0] <- 0
    future_area_harvested_mid <- data.frame(aggregate(future_area_harvested$area, FUN=sum, by=list(future_area_harvested$year)))
    future_area_harvested_upper <- data.frame(aggregate(future_area_harvested$upper, FUN=sum, by=list(future_area_harvested$year)))
    future_area_harvested_lower <- data.frame(aggregate(future_area_harvested$lower, FUN=sum, by=list(future_area_harvested$year)))
      rm(future_area_harvested)
names(future_area_harvested_mid) <- c("year","total_area")
names(future_area_harvested_upper) <- c("year","total_area")
names(future_area_harvested_lower) <- c("year","total_area")

s <- readOGR(paste(HOME,"/PLJV/products/tillage_predictions/",sep=""),"gplcc_pilot_region_tillage_aggregated_by_clu", verbose=F)

# sort regional CLUs by mean tillage likelihood
cat(" -- ordering attribute table by tillage likelihood\n")
t_data <- s@data
  s <- s[order(t_data$zmean, decreasing = T),]
    t_data <- t_data[order(t_data$zmean, decreasing = T),]

# figure out the number of sorted CLUs needed to satisfy future_area_harvested for the focal year
cat(" -- numerically integrating total area across CLU fields\n")
  seq   <- seq(1,nrow(t_data),5) # let's do our calculations 5 fields at a time
    areas <- vector(); for(j in 1:length(seq)){ areas<-append(areas,sum(t_data[1:seq[j],4])) }
      areas <- areas/(10^6) # meters^2 -> km^2

for(year in c(2015,2020,2025)){ # World Bank projections scoped to 2025
  # number of CLUs needed to satisfy future_area_harvested, split into upper, middle, and lower -- based on the p95 interval
  CLUs_needed_upper <- seq[min(which(areas >= future_area_harvested_upper[c(2015,2020,2025) %in% year,2]))]
  CLUs_needed_mid   <- seq[min(which(areas >= future_area_harvested_mid[c(2015,2020,2025) %in% year,2]))]
  CLUs_needed_lower <- seq[min(which(areas >= future_area_harvested_lower[c(2015,2020,2025) %in% year,2]))] 
  # assign tilled/not-tilled to attribute table for focal year
  for(prob in c("upper","mid","lower")){
  	focal <- rep(1,get(ls(pattern=paste("CLUs.*_",prob,"$",sep=""))))
      focal <- c(focal,rep(0,nrow(s)-length(focal)))
         t_data[,paste(year,"tilled",prob,sep="_")] <- focal
  }
}
# write predictions to shapefile
cat(" -- writing output to disk\n")
s@data <- t_data
writeOGR(s,paste(HOME,"/PLJV/products/tillage_predictions/",sep=""),"gplcc_pilot_region_tillage_clu_with_worldbank_price_yield_rojections", driver="ESRI Shapefile", overwrite=T)