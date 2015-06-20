require(rgdal)
require(raster)

HOME <- Sys.getenv("HOME")

setwd("/media/ktaylora/80C0-4F29/Code")

# build and project total area model from FAO/World Bank
source("wheat_commodity_price_area_model.R")

s <- readOGR(paste(HOME,"/PLJV/products/tillage_predictions/",sep=""),"gplcc_pilot_region_tillage_aggregated_by_clu", verbose=F)

# sort regional CLUs by mean tillage likelihood
cat(" -- ordering attribute table by tillage likelihood\n")
t_data <- s@data
s <- s[order(t_data$zmean, decreasing = T),]
t_data <- t_data[order(t_data$zmean, decreasing = T),]

# figure out the number of sorted CLUs needed to satisfy future_area_harvested for the focal year
cat(" -- numerically integrating total area across CLU fields\n")
clus_required <- rep(NA,length(2013:2025))
seq   <- seq(1,nrow(t_data),5) # let's do our calculations 5 fields at a time
areas <- vector(); for(j in 1:length(seq)){ areas<-append(areas,sum(t_data[1:seq[j],4])) }

for(year in 2016:2025){ # World Bank projections scoped to 2025
  # number of CLUs needed to satisfy future_area_harvested, split into upper, middle, and lower -- based on the P80 interval
  CLUs_needed_upper <- seq[min(which(areas >= future_area_harvested[which(2013:2025 %in% year),1]))]
  CLUs_needed_mid   <- seq[min(which(areas >= future_area_harvested[which(2013:2025 %in% year),2]))]
  CLUs_needed_lower <- seq[min(which(areas >= future_area_harvested[which(2013:2025 %in% year),3]))] 
  # assign tilled/not-tilled to attribute table for focal year
  focal <- rep(1,clus_required[which(2013:2025 %in% year)])
    focal <- c(focal,rep(0,nrow(s)-length(focal)))
     t_data[,paste(year,"tilled",sep="_")] <- focal
}
# write predictions to shapefile
cat(" -- writing output to disk\n")
s@data <- t_data
writeOGR(s,paste(HOME,"/PLJV/products/tillage_predictions/",sep=""),"gplcc_pilot_region_tillage_clu_with_worldbank_price_yield_rojections", driver="ESRI Shapefile", overwrite=T)