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

# parse full historical tables for our individual commodity crops.  Units here are tallied so they are consistent with the 6 states that make up the Great Plains.
# We will project these forward to 2025 then scale the projected areas for each crop to the extent of the GPLCC pilot region as we work our way through the data.

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
# how appropriate it is to do this kind of correction.

t_wheat_yield <- data.frame(six_states_yield=t_wheat$yield[match(t_wheat$year,t_wheat_price$year)],
                            us_yield=t_wheat_price$yield[match(t_wheat$year,t_wheat_price$year)], 
                            year=t_wheat_price$year[match(t_wheat$year,t_wheat_price$year)])
  m_wheat_yield_from_us <- glm(six_states_yield~.,data=t_wheat_yield)
t_corn_yield <- data.frame(six_states_yield=t_corn$yield[match(t_corn$year,t_corn_price$year)],
                           us_yield=t_corn_price$yield[match(t_corn$year,t_corn_price$year)], 
                           year=t_corn_price$year[match(t_corn$year,t_corn_price$year)])
  m_corn_yield_from_us <- glm(six_states_yield~.,data=t_corn_yield)
t_cotton_yield <- data.frame(six_states_yield=t_cotton$yield[match(t_cotton$year,t_cotton_price$year)],
                             us_yield=t_cotton_price$yield[match(t_cotton$year,t_cotton_price$year)], 
                             year=t_cotton_price$year[match(t_cotton$year,t_cotton_price$year)])
  q  <- glm(six_states_yield ~ .,data=t_cotton_yield)
t_sorghum_yield <- data.frame(six_states_yield=t_sorghum$yield[match(t_sorghum$year,t_sorghum_price$year)],
                              us_yield=t_sorghum_price$yield[match(t_sorghum$year,t_sorghum_price$year)], 
                              year=t_sorghum_price$year[match(t_sorghum$year,t_sorghum_price$year)])
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

# future commodity price projections for each crop (Data from: WorldBank Projections, April 2015)
corn_price_future    <- data.frame(price=c(192.9,180.0,183.6,187.4,191.2,195.0,199.0,203.0,207.1,211.3,215.6,220.0),year=2014:2025)
cotton_price_future  <- data.frame(price=c(1.83,1.60,1.65,1.71,1.76,1.82,1.88,1.94,2.00,2.06,2.13,2.20),year=2014:2025)
wheat_price_future   <- data.frame(price=c(284.9,240.0,243.3,246.6,250.0,253.4,256.9,260.4,264.0,267.6,271.3,275.0),year=2014:2025)
sorghum_price_future <- data.frame(price=c(4.28,3.45,3.30,3.40,3.40,3.40,3.45,3.45,3.50,3.55,3.60,3.65), year=2014:2025) # from USDA Long-term projections

# construct a table of future drought conditions from the above yield corrections and commodity price data forecasts to forecast future predictions of 
# total area harvested.  The future PDSI trend here is represented as trend surface fit to an interpolated smoothing spline.

t_pdsi_future <- read.csv("data/Cook_et_al_pdsi_projections_2100.csv") # time-series starts at 2014.  Lets interpolate and smooth this projection and treat it as a trend surface
  t_pdsi_future <- spline(t_pdsi_future$pdsi)
    t_pdsi_future$x <- t_pdsi_future$x+2014
      m_pdsi_future <- lm(y~x,data.frame(y=t_pdsi_future$y,x=t_pdsi_future$x))     
        t_pdsi_future <- as.vector(predict(m_pdsi_future,newdata=data.frame(x=2014:2025)))

# predict the total area harvested for the GPLCC pilot region, using commodity prices, time, yield, and drought conditions
  
ratioToPilot <- (147327927594)/(1867211959388) # meters/meters ratio of the pilot project area to the six states tallied for NASS statistics representing the GP

t_corn_future <- cbind(yield=corn_bu_acre,corn_price_future,pdsi=t_pdsi_future)
  t_m <- predict(m_corn_current,newdata=t_corn_future,se.fit=T)
    t_m$fit <- t_m$fit*ratioToPilot
      t_m$se.fit <- t_m$se.fit*ratioToPilot
  upr <- t_m$fit + (1.96 * t_m$se.fit)
  lwr <- t_m$fit - (1.96 * t_m$se.fit)
  fit <- t_m$fit
    t_corn_future <- data.frame(area=fit,upper=upr,lower=lwr,year=2014:2025)

t_wheat_future <- cbind(yield=wheat_bu_acre,wheat_price_future,pdsi=t_pdsi_future)
  t_m <- predict(m_wheat_current,newdata=t_wheat_future,se.fit=T)
    t_m$fit <- t_m$fit*ratioToPilot
      t_m$se.fit <- t_m$se.fit*ratioToPilot
  upr <- t_m$fit + (1.96 * t_m$se.fit)
  lwr <- t_m$fit - (1.96 * t_m$se.fit)
  fit <- t_m$fit
    t_wheat_future <- data.frame(area=fit,upper=upr,lower=lwr,year=2014:2025)

t_cotton_future <- cbind(yield=cotton_lbs_acre,cotton_price_future,pdsi=t_pdsi_future)
  t_m <- predict(m_cotton_current,newdata=t_cotton_future,se.fit=T)
    t_m$fit <- t_m$fit*ratioToPilot
      t_m$se.fit <- t_m$se.fit*ratioToPilot
  upr <- t_m$fit + (1.96 * t_m$se.fit)
  lwr <- t_m$fit - (1.96 * t_m$se.fit)
  fit <- t_m$fit
    t_cotton_future <- data.frame(area=fit,upper=upr,lower=lwr,year=2014:2025)

t_sorghum_future <- cbind(yield=sorghum_bu_acre,sorghum_price_future,pdsi=t_pdsi_future)
  t_m <- predict(m_sorghum_current,newdata=t_sorghum_future,se.fit=T)
    t_m$fit <- t_m$fit*ratioToPilot
      t_m$se.fit <- t_m$se.fit*ratioToPilot
  upr <- t_m$fit + (1.96 * t_m$se.fit)
  lwr <- t_m$fit - (1.96 * t_m$se.fit)
  fit <- t_m$fit
    t_sorghum_future <- data.frame(area=fit,upper=upr,lower=lwr,year=2014:2025)
  
# convert our area units from acres -> kilometers^2 -- for scale, the total area of the GPLCC pilot region in km2 is 147327.9
for(ts in ls(pattern="t_.*future$")){ 
  if(sum(grepl(names(get(ts)),pattern="rea"))>0){ 
    t <- get(ts); 
    t[,!grepl(names(t),pattern="year")] <- t[,!grepl(names(t),pattern="year")] * 0.0040468564224
    assign(ts,value=t) 
  }
}

# total percentage of pilot region forecasted for agricultural activity for focal crops
(t_sorghum_future[,1]+t_cotton_future[,1]+t_wheat_future[,1]+t_corn_future[,1])/147327.9

#
# make some plots of historical (training) data vs. model predictions to demonstrate residual error
#

require(reshape2)
require(ggplot2)
require(gridExtra)

# indicate : Year (Commodity Price + PDSI + Yield) in figure caption
t_corn_plot <- data.frame(observed=t_corn$area*ratioToPilot,
                          predicted=predict(m_corn_current, newdata=t_corn)*ratioToPilot,
                          year=1960:2014)
p1 <- ggplot(t_corn_plot) + 
      geom_line(aes(y=observed, x=year), colour="black") + 
      geom_point(aes(y=observed, x=year), colour="black") + 
      geom_line(aes(y=predicted, x=year), colour="orange") + 
      geom_point(aes(y=predicted, x=year), colour="orange") +      
      scale_x_continuous(breaks = round(seq(min(t_corn_plot$year,na.rm=T), max(t_corn_plot$year,na.rm=T), by = 4),1)) + 
      xlab("") + ylab("Area Planted") +
      annotate("text", y=max(t_corn_plot$observed,na.rm=T), x=min(t_corn_plot$year,na.rm=T)*1.0011, label = "A.) Corn", size=4) +
      geom_abline(intercept = mean(t_corn$area*ratioToPilot), slope = 1, colour="red") +
      theme_bw();

t_cotton_plot <- data.frame(observed=t_cotton$area*ratioToPilot,
                          predicted=predict(m_cotton_current, newdata=t_cotton)*ratioToPilot,
                          year=1960:2014)
p2 <- ggplot(t_cotton_plot) + 
      geom_line(aes(y=observed, x=year), colour="black") + 
      geom_point(aes(y=observed, x=year), colour="black") + 
      geom_line(aes(y=predicted, x=year), colour="orange") + 
      geom_point(aes(y=predicted, x=year), colour="orange") +      
      xlab("") + ylab("Area Planted") +
      annotate("text", y=max(t_cotton_plot$observed,na.rm=T), x=max(t_cotton_plot$year,na.rm=T)*0.9982, label = "B.) Cotton", size=4) +
      geom_abline(intercept = mean(t_cotton$area*ratioToPilot), slope = 1, colour="red") +
      theme_bw();

t_wheat_plot <- data.frame(observed=t_wheat$area*ratioToPilot,
                          predicted=predict(m_wheat_current, newdata=t_wheat)*ratioToPilot,
                          year=1960:2014)
p3 <- ggplot(t_wheat_plot) + 
      geom_line(aes(y=observed, x=year), colour="black") + 
      geom_point(aes(y=observed, x=year), colour="black") + 
      geom_line(aes(y=predicted, x=year), colour="orange") + 
      geom_point(aes(y=predicted, x=year), colour="orange") +      
      xlab("") + ylab("Area Planted") +
      annotate("text", y=max(t_wheat_plot$observed,na.rm=T), x=min(t_wheat_plot$year,na.rm=T)*1.0014, label = "C.) Wheat", size=4) +
      geom_abline(intercept = mean(t_wheat$area*ratioToPilot), slope = 1, colour="red") +
      theme_bw();

t_sorghum_plot <- data.frame(observed=t_sorghum$area*ratioToPilot,
                          predicted=predict(m_sorghum_current, newdata=t_wheat)*ratioToPilot,
                          year=1960:2014)
p4 <- ggplot(t_sorghum_plot) + 
      geom_line(aes(y=observed, x=year), colour="black") + 
      geom_point(aes(y=observed, x=year), colour="black") + 
      geom_line(aes(y=predicted, x=year), colour="orange") + 
      geom_point(aes(y=predicted, x=year), colour="orange") +      
      xlab("") + ylab("Area Planted") +
      annotate("text", y=max(t_sorghum_plot$observed,na.rm=T), x=max(t_sorghum_plot$year,na.rm=T)*0.9980, label = "D.) Sorghum", size=4) +
      geom_abline(intercept = mean(t_sorghum$area*ratioToPilot), slope = 1, colour="red") +
      theme_bw();

grid.arrange(p1,p2,p3,p4)

# make barplots of forecasts for 2015, 2020, 2025, with p=0.95 intervals for each crop

p1 <- ggplot()





