#
# Model Format: 
# area planted = f(year,yield,price,pdsi)
# yield under future conditions is only provided by NASS at the scale of the continental US.  We correct the difference between the US and the six states of the GP using
# a simple regression of historical data of yield in the six states vs. yield in the US from 1960-2014.
#

#
# parseStateYieldsByYear()
#

parseStateYieldsByYear <- function(x){
  years <- yields <- areas <- vector()
  for(year in min(x$Year):max(x$Year)){
    t_focal <- x[x$Year == year,]
    years   <- append(years,year)
    yields  <- append(yields,sum(as.numeric(as.vector(t_focal[grepl(as.vector(t_focal$Data.Item),pattern="YIELD"),]$Value))))
    areas   <- append(areas, sum(as.numeric(gsub(x=as.vector(t_focal[grepl(as.vector(t_focal$Data.Item),pattern="PLANTED"),]$Value),pattern=",",replacement=""))))
  }
  return(data.frame(year=years,yield=yields,area=areas))
} 

#
# parsePDSIByYear()
#

parsePDSIByYear <- function(x,year=NULL,fun=mean){
  years <- pdsis <- tmins <- vector()
  for(y in year){
    years <- append(years,y);
    #tmins  <- append(tmins, fun(t_drought[grepl(as.character(t_drought$YearMonth),pattern=as.character(y)),]$TMIN))
    pdsis <- append(pdsis,fun(t_drought[grepl(as.character(t_drought$YearMonth),pattern=as.character(y)),]$PDSI))
  }
  return(data.frame(pdsi=pdsis))
}

#
# MAIN
# 

HOME <- Sys.getenv("HOME")

# parse full historical tables for our individual commodity crops
t         <- read.csv("data/NASS_Historical_Yield_Area_Corn_Cotton_Wheat_Sorghum_Six_States.csv")
t_drought <- read.csv("data/NNDC_Drought_and_Climate_Central_US.csv") # Historical drought data compiled from Cook et al. and NOAA

t_wheat    <- parseStateYieldsByYear(t[grepl(as.vector(t$Data.Item),pattern="WHEAT"),])
  t_wheat_price <- read.csv("data/wheat_crop_yields_us.csv")
    t_wheat <- cbind(t_wheat,
                     price=t_wheat_price$Price[match(t_wheat$year,t_wheat_price$Year)],
                     parsePDSIByYear(t_drought, year=t_wheat$year))
    
t_corn     <- parseStateYieldsByYear(t[grepl(as.vector(t$Data.Item),pattern="CORN"),])
  t_corn_price <- read.csv("data/corn_crop_yields_us.csv")
    t_corn <- cbind(t_corn,
                    price=t_corn_price$Price[match(t_corn$year,t_corn_price$Year)],
                    parsePDSIByYear(t_drought, year=t_corn$year))
    
t_cotton   <- parseStateYieldsByYear(t[grepl(as.vector(t$Data.Item),pattern="COTTON"),])
  t_cotton_price <- read.csv("data/cotton_crop_yields_us.csv")
    t_cotton <- cbind(t_cotton,
                      price=t_cotton_price$Price[match(t_cotton$year,t_corn_price$Year)],
                      parsePDSIByYear(t_drought, year=t_cotton$year))
    
t_sorghum  <- parseStateYieldsByYear(t[grepl(as.vector(t$Data.Item),pattern="SORGHUM"),])
  t_sorghum_price <- read.csv("data/sorghum_crop_yields_us.csv")
    t_sorghum <- cbind(t_sorghum,
                       price=t_sorghum_price$Price[match(t_sorghum$year,t_sorghum_price$Year)],
                       parsePDSIByYear(t_drought, year=t_sorghum$year))

# build our raw "area harvested" models for the six states of the GP
summary(m_wheat_current <- glm(area~.,data=t_wheat))
summary(m_corn_current <- glm(area~.,data=t_corn))
summary(m_cotton_current <- glm(area~.,data=t_cotton))
summary(m_sorghum_current <- glm(area~.,data=t_sorghum))

# let's ensure consistent lowercase lettering in column names across all tables
for(ts in ls(pattern="t_.*price$")){ n<-tolower(names(get(ts))); t <- get(ts); names(t) <- n; assign(ts,value=t) }

# make some yield correction models so we can downscale our future yield projection variables for
# each commodity crop for the US to match our six states.  These glm's should also be able to indicate 
# how appropriate that is.

t_wheat_yield <- data.frame(six_states_yield=t_wheat$yield,us_yield=t_wheat_price$yield, year=t_wheat_price$year)
  m_wheat_yield_from_us <- glm(six_states_yield~.,data=t_wheat_yield)
t_corn_yield <- data.frame(six_states_yield=t_corn$yield,us_yield=t_corn_price$yield, year=t_corn_price$year)
  m_corn_yield_from_us <- glm(six_states_yield~.,data=t_corn_yield)
t_cotton_yield <- data.frame(six_states_yield=t_cotton$yield,us_yield=t_cotton_price$yield, year=t_cotton_price$year)
  m_cotton_yield_from_us  <- glm(six_states_yield ~ .,data=t_cotton_yield)
t_sorghum_yield <- data.frame(six_states_yield=t_sorghum$yield,us_yield=t_sorghum_price$yield, year=t_sorghum_price$year)
  m_sorghum_yield_from_us <- glm(six_states_yield ~ .,data=t_sorghum_yield)

# generate corrected yield forecasts for each commodity crop using the correction factors fit above (Data from: USDA Long-term Projections, February 2015)
wheat_bu_acre    <- data.frame(us_yield=c(47.1,43.7,45.5,46.2,46.6,47.0,47.3,47.7,48.1,48.5,48.8,49.2),year=2014:2025)
  wheat_bu_acre  <- as.vector(predict(m_wheat_yield_from_us,newdata=wheat_bu_acre))
corn_bu_acre     <- data.frame(us_yield=c(158.8,173.4,167.2,169.2,171.2,173.2,175.3,177.3,179.3,181.3,183.3,185.3),year=2014:2025)
  corn_bu_acre   <- as.vector(predict(m_corn_yield_from_us,newdata=corn_bu_acre))
cotton_lbs_acre  <- data.frame(us_yield=c(802,783,800,805,810,815,820,825,830,835,840,845),year=2014:2025)
  cotton_lbs_acre <- as.vector(predict(m_cotton_yield_from_us,newdata=cotton_lbs_acre))
sorghum_bu_acre  <- data.frame(us_yield=c(69.6,66.1,65.0,65.0,65.0,65.0,65.0,65.0,65.0,65.0,65.0,65.0),year=2014:2025)
sorghum_bu_acre  <- as.vector(predict(m_sorghum_yield_from_us,newdata=sorghum_bu_acre))

# generate future commodity price projections for each crop (Data from: WorldBank Projections, April 2015)
corn_price_future    <- data.frame(price=c(192.9,180.0,183.6,187.4,191.2,195.0,199.0,203.0,207.1,211.3,215.6,220.0),year=2014:2025)
cotton_price_future  <- data.frame(price=c(1.83,1.60,1.65,1.71,1.76,1.82,1.88,1.94,2.00,2.06,2.13,2.20),year=2014:2025)
wheat_price_future   <- data.frame(price=c(284.9,240.0,243.3,246.6,250.0,253.4,256.9,260.4,264.0,267.6,271.3,275.0),year=2014:2025)
sorghum_price_future <- NA

# construct a table of future conditions from the above yield corrections and commodity price data forecasts to come up with future predictions of 
# total area harvested

t_pdsi_future <- read.csv("data/Cook_et_al_pdsi_projections_2100.csv") # time-series starts at 2014.  Lets interpolate and smooth this projection and treat it as a trend surface
  t_pdsi_future <- spline(t_pdsi_future$pdsi)
    t_pdsi_future$x <- t_pdsi_future$x+2014
      m_pdsi_future <- lm(y~x,data.frame(y=t_pdsi_future$y,x=t_pdsi_future$x))     
        t_pdsi_future <- as.vector(predict(m_pdsi_future,newdata=data.frame(x=2014:2025)))
  
t_corn_future <- cbind(yield=corn_bu_acre,corn_price_future,pdsi=t_pdsi_future)
  t_corn_future <- predict(m_corn_current,newdata=t_corn_future)
t_wheat_future <- cbind(yield=wheat_bu_acre,wheat_price_future,pdsi=t_pdsi_future)
  t_wheat_future <- predict(m_wheat_current,newdata=t_wheat_future)
t_cotton_future <- cbind(yield=cotton_lbs_acre,cotton_price_future,pdsi=t_pdsi_future)
  t_cotton_future <- predict(m_cotton_current,newdata=t_cotton_future)
  
ratioToPilot <- (147327927594)/(1867211959388) # ratio of the pilot project area to the six states tallied for NASS statistics representing the GP


#
# make some plots
#

par(mfrow=c(3,1))

# corn
plot(t_corn$area*ratioToPilot,type="l",x=1960:2014,xlab=NA, ylab=NA, lwd=1.8,col="white",yaxt="n")
grid();grid();grid()
abline(h=mean(t_corn$area*ratioToPilot),col="orange",lwd=0.8)
lines(t_corn$area*ratioToPilot,x=1960:2014,xlab=NA, ylab=NA, lwd=1.8,cex=1.3)
lines(predict(m_corn, newdata=t_corn)*ratioToPilot, x=1960:2014,lwd=1.5,col="orange")
points(predict(m_corn, newdata=t_corn)*ratioToPilot,x=1960:2014,col="red")
text(y=(max(t_corn$area*ratioToPilot)*0.96),x=1960, "A",cex=1.5)

# cotton
plot(t_cotton$area*ratioToPilot,type="l",x=1960:2014,xlab=NA, ylab="Area Planted (GPLCC Pilot Region [Acres])", lwd=1.8, cex.lab=1.5,col="white")
grid();grid();grid()
abline(h=mean(t_cotton$area*ratioToPilot),col="orange",lwd=0.8)
lines(t_cotton$area*ratioToPilot,type="l",x=1960:2014,xlab=NA, ylab="Area Planted (GPLCC Pilot Region [Acres])", lwd=1.8, cex.lab=1.5)
lines(predict(m_cotton, newdata=t_cotton)*ratioToPilot, x=1960:2014,lwd=1.5,col="orange")
points(predict(m_cotton, newdata=t_cotton)*ratioToPilot,x=1960:2014,col="red")
text(y=(max(t_cotton$area*ratioToPilot)*0.96),x=1960, "B",cex=1.5)

# wheat
plot(t_wheat$area*ratioToPilot,type="l",x=1960:2014,xlab="Year (Commodity Price + PDSI + Yield)", ylab=NA, lwd=1.8, cex.lab=1.5,col="white")
grid();grid();grid()
abline(h=mean(t_wheat$area*ratioToPilot),col="orange",lwd=0.8)
lines(t_wheat$area*ratioToPilot,type="l",x=1960:2014,xlab="Year (Commodity Price + PDSI + Yield)", ylab=NA, lwd=1.8, cex.lab=1.5)
lines(predict(m_wheat, newdata=t_wheat)*ratioToPilot, x=1960:2014,lwd=1.5,col="orange")
points(predict(m_wheat, newdata=t_wheat)*ratioToPilot,x=1960:2014,col="red")
text(y=(max(t_wheat$area*ratioToPilot)*0.96),x=1960, "C",cex=1.5)

# do future projections of commodity crops
  # correct national yields -> six states yields
