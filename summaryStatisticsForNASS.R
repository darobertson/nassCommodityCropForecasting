require(raster)
require(rgdal)

argv <- commandArgs(trailingOnly=T) # argv[1] == "zipfile directory path", argv[2] == "full shapefile path"
argv <- c("/media/ktaylora/big_black/intermediates/CDL/", "/home/ktaylora/PLJV/boundaries/GPLCC Pilot/GPLCC_Pilot_Region/GPLCC_pilot_region_boundary_aggregated.shp")
zips <- list.files(argv[1],full.names=T,pattern="zip$")

b <- strsplit(argv[2],split="/")
 layer <- strsplit(b[[length(na.omit(b))]],split="[.]")[[length(b[[1]])-0]][1]
   path <- paste(b[[1]][1:(length(b[[1]])-1)],collapse="/")
     b <- readOGR(path,layer,verbose=F)

corn    <- list();
cotton  <- list();
wheat   <- list();
sorghum <- list();
other   <- list();

years <- sort(na.omit(suppressWarnings(as.numeric(unlist(strsplit(list.files(argv[1],pattern="zip$"),split="_"))))))

for(z in zips){ 
  cat(" -- decompressing:",z,"\n");
  unlink("/tmp/focal_zip", force=T, recursive=T); utils::unzip(z, exdir="/tmp/focal_zip")
  cat(" -- reading raster data and masking\n")
  r <- raster(list.files("/tmp/focal_zip/", full.names=T, pattern="img$")[1]);
    r <- crop(r,spTransform(b,CRS(projection(r))))
      r <- mask(r,spTransform(b,CRS(projection(r))))
  cat(" -- sampling raster surface: ")
  s <- sampleRandom(r,size=85000)
  # corn
  corn[[length(corn)+1]] <- sum(s%in%c(1,12))/length(na.omit(s)); cat(".")
  # cotton
  cotton[[length(cotton)+1]] <- sum(s==2)/length(na.omit(s)); cat(".")
  # wheat
  wheat[[length(wheat)+1]] <- sum(s%in%21:27)/length(na.omit(s)); cat(".")
  # sorghum
  sorghum[[length(sorghum)+1]] <- sum(s==4)/length(na.omit(s)); cat(".")
  # other crops
  other[[length(other)+1]] <- sum(s%in%c(5:6,28:38,41:44,52,53,57:59,61,205,241,246))/length(na.omit(s)); cat(".\n")
}
# write table to CWD
cat(" -- parsing output table to:",paste(getwd(),"nass_crops_output.csv",sep="/"),"\n")
t<-data.frame(year=years, 
	       corn=unlist(corn), 
	       cotton=unlist(cotton),
	       wheat=unlist(wheat),
	       sorghum=unlist(sorghum),
               other=unlist(other))

write.csv(t,"nass_crops_output.csv")




