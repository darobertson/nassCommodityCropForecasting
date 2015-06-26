require(raster)
require(rgdal)
require(snow)

beginCluster(n=4)

HOME <- Sys.getenv("HOME");
cat(" -- reading raster data\n")
f<-lapply(as.list(list.files(pattern="*.burn.*tif$")),FUN=raster)
lc <- raster(paste(HOME,"/PLJV/products/landcover/gplcc_LD/pljv_landcover_gplcc_pilot_region.tif",sep=""))
for(r in f){
  n <- names(r)
  cat(" -- processing:",n,"\n")
  r <- raster::subs(r,data.frame(from=c(32,0),to=c(-32,NA)))
  r <- raster::merge(lc,projectRaster(r,lc,progress='text'))
  r <- raster::subs(r,data.frame(from=c(-32,37,38,201,202,203,
                                        205,206,207,208,209,210,211,212),
                                 to=c(37,34,34,34,34,34,34,34,34,34,34,34,34,34)))
  writeRaster(r,filename=paste(n,"full_landcover_burned.tif"))    
}; cat(" -- done\n");
