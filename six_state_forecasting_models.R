#
# NASS COMMODITY CROP PROJECTIONS FOR DRYLAND AGRICULTURE IN THE SOUTHERN GREAT PLAINS
#
# These regression models utilize long-term historical USDA survey data collected from the six states that make up the Southern Great Plains (New Mexico,
# Texas, Oklahoma, Colorado, Kansas, and Nebraska) and forecast changes in total area planted for major crops (Corn, cotton, wheat, sorghum).  Total area harvested 
# is treated as a function of annual international commodity price, prevailing drought conditions, annual yield, and an assumption of long-term productivity trends 
# for each crop.
#
# Author: Kyle Taylor (kyle.taylor@pljv.org)
# 
# Model Format: 
# observed area planted = f(year,yield,price,pdsi)
# future area planted = f(year,yield(trend),price(forecast),pdsi(trend,forecast))
#
# yield under future conditions is only provided by NASS at the scale of the continental US.  We normalize the difference between the US and the six states of the GP using
# a simple regression of historical data of yield in the six states vs. yield in the US from 1960-2014.  This makes a strong assumption of linearity between crop yields and area
# harvested between the Central US and the continental US as a whole (but a defensible one; check residual error committed by regression for proof). Projections of total area harvested for each commodity crop 
# are then normalized (post-hoc) to the extent of the GPLCC pilot region using 30m gridded total area observations for each crop taken from NASS. 
#

## Local functions

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

## MAIN

HOME <- Sys.getenv("HOME")

# parse full historical tables for our individual commodity crops.  Units here are tallied so they are consistent with the 6 states that make up the Great Plains.
# We will project these forward to 2025 then scale the projected areas for each crop to the extent of the GPLCC pilot region as we work our way through the data.

t         <- read.csv("data/NASS_Historical_Yield_Area_Corn_Cotton_Wheat_Sorghum_Six_States.csv") # Area planted here is in acres
t_drought <- read.csv("data/NNDC_Drought_and_Climate_Central_US.csv") # Historical drought data compiled from Cook et al. and NOAA

t_wheat    <- parseStateYieldsByYear(t[grepl(as.vector(t$Data.Item),pattern="WHEAT"),])
  t_wheat_price <- read.csv("data/wheat_crop_yields_us.csv") # Area planted here is in acres
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

# build our raw "area planted" models for the six states of the GP
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
  m_cotton_yield_from_us <- glm(six_states_yield ~ .,data=t_cotton_yield)
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
# acres/km2 = 0.0040468564224
acresToKm <- 0.0040468564224

t_corn_future <- cbind(yield=corn_bu_acre,corn_price_future,pdsi=t_pdsi_future)
  t_m <- predict(m_corn_current,newdata=t_corn_future,se.fit=T)
    t_m$fit <- t_m$fit*acresToKm*ratioToPilot
      t_m$se.fit <- t_m$se.fit*acresToKm*ratioToPilot
  upr <- t_m$fit + (1.96 * t_m$se.fit)
  lwr <- t_m$fit - (1.96 * t_m$se.fit)
  fit <- t_m$fit
    t_corn_future <- data.frame(area=fit,upper=upr,lower=lwr,year=2014:2025)

t_wheat_future <- cbind(yield=wheat_bu_acre,wheat_price_future,pdsi=t_pdsi_future)
  t_m <- predict(m_wheat_current,newdata=t_wheat_future,se.fit=T)
    t_m$fit <- t_m$fit*acresToKm*ratioToPilot
      t_m$se.fit <- t_m$se.fit*acresToKm*ratioToPilot
  upr <- t_m$fit + (1.96 * t_m$se.fit)
  lwr <- t_m$fit - (1.96 * t_m$se.fit)
  fit <- t_m$fit
    t_wheat_future <- data.frame(area=fit,upper=upr,lower=lwr,year=2014:2025)

t_cotton_future <- cbind(yield=cotton_lbs_acre,cotton_price_future,pdsi=t_pdsi_future)
  t_m <- predict(m_cotton_current,newdata=t_cotton_future,se.fit=T)
    t_m$fit <- t_m$fit*acresToKm*ratioToPilot
      t_m$se.fit <- t_m$se.fit*acresToKm*ratioToPilot
  upr <- t_m$fit + (1.96 * t_m$se.fit)
  lwr <- t_m$fit - (1.96 * t_m$se.fit)
  fit <- t_m$fit
    t_cotton_future <- data.frame(area=fit,upper=upr,lower=lwr,year=2014:2025)

t_sorghum_future <- cbind(yield=sorghum_bu_acre,sorghum_price_future,pdsi=t_pdsi_future)
  t_m <- predict(m_sorghum_current,newdata=t_sorghum_future,se.fit=T)
    t_m$fit <- t_m$fit*acresToKm*ratioToPilot
      t_m$se.fit <- t_m$se.fit*acresToKm*ratioToPilot
  upr <- t_m$fit + (1.96 * t_m$se.fit)
  lwr <- t_m$fit - (1.96 * t_m$se.fit)
  fit <- t_m$fit
    t_sorghum_future <- data.frame(area=fit,upper=upr,lower=lwr,year=2014:2025)
  
##
## The overall area conversion process works as such : predicted/observed area [Acres] * (Acres/Km2) * (GPLCC Mean Crop Area Observed Historical / GPLCC Mean Crop Area Predicted)
## In effect, we normalize the NASS observations between the mean yearly % area of each commodity crop using 30m resolution grid cells in NASS data to the total area predicted from 
## course-grain county-level NASS predictions. 
##

# total percentage of pilot region forecasted for agricultural activity for focal crops
cat(" -- total area of GPLCC pilot region dedicated to agricultural development (pre-normalization):",round((t_sorghum_future[,1]+t_cotton_future[,1]+t_wheat_future[,1]+t_corn_future[,1])/147327.9,3),"\n")

# NASS scaling factors -- this ratio scaling is an adjustment of total area derived from NASS CDL remote sensing data from 2008-2013 (see: summaryStatisticsForNASS.R)
nass_wheatNormalizationRatio   <- (0.1259009/mean(t_wheat_future[,1]/147327.9)) # ratio: mean nass total area wheat GPLCC region [2008-2013] / calculated mean GPLCC region area ratio
nass_cornNormalizationRatio    <- (0.03355856/mean(t_corn_future[,1]/147327.9))
nass_cottonNormalizationRatio  <- (0.1231231/mean(t_cotton_future[,1]/147327.9))
nass_sorghumNormalizationRatio <- (0.01291291/mean(t_sorghum_future[,1]/147327.9))

cat(" -- normalization ratios :", round(unlist(lapply(as.list(ls(pattern="Normalization")), get)),2),"\n")

## make some plots of historical (training) data vs. model predictions to demonstrate residual error

cat(" -- plotting...\n")

require(reshape2)
require(ggplot2)
require(gridExtra)

dev.new(width=10,height=6)

# indicate : Year (Commodity Price + PDSI + Yield) in figure caption
t_corn_plot <- data.frame(observed=m_corn_current$model$area*acresToKm*ratioToPilot*nass_cornNormalizationRatio,
                          predicted=m_corn_current$fit*acresToKm*ratioToPilot*nass_cornNormalizationRatio,
                          year=m_corn_current$model$year)
p1 <- ggplot(t_corn_plot) + 
      geom_line(aes(y=observed, x=year), colour="black") + 
      geom_point(aes(y=observed, x=year), colour="black") + 
      geom_line(aes(y=predicted, x=year), colour="orange") + 
      geom_point(aes(y=predicted, x=year), colour="orange") +      
      scale_x_continuous(breaks = round(seq(min(t_corn_plot$year,na.rm=T), max(t_corn_plot$year,na.rm=T), by = 5),1)) + 
      xlab("") + ylab(expression(paste('Area Planted (',km^{2},')'))) +
      annotate("text", y=max(t_corn_plot$observed,na.rm=T), x=min(t_corn_plot$year,na.rm=T)*1.0011, label = "A.) Corn", size=4) +
      geom_abline(intercept = mean(m_corn_current$model$area*acresToKm*ratioToPilot*nass_cornNormalizationRatio), slope = 1, colour="red") +
      theme_bw();

t_cotton_plot <- data.frame(observed=m_cotton_current$model$area*acresToKm*ratioToPilot*nass_cottonNormalizationRatio,
                            predicted=m_cotton_current$fit*acresToKm*ratioToPilot*nass_cottonNormalizationRatio,
                            year=m_cotton_current$model$year)
p2 <- ggplot(t_cotton_plot) + 
      geom_line(aes(y=observed, x=year), colour="black") + 
      geom_point(aes(y=observed, x=year), colour="black") + 
      geom_line(aes(y=predicted, x=year), colour="orange") + 
      geom_point(aes(y=predicted, x=year), colour="orange") +      
      scale_x_continuous(breaks = round(seq(min(t_cotton_plot$year,na.rm=T), max(t_cotton_plot$year,na.rm=T), by = 5),1)) + 
      xlab("") + ylab(expression(paste('Area Planted (',km^{2},')'))) +
      annotate("text", y=max(t_cotton_plot$observed,na.rm=T), x=max(t_cotton_plot$year,na.rm=T)*0.9982, label = "B.) Cotton", size=4) +
      geom_abline(intercept = mean(m_cotton_current$model$area*acresToKm*ratioToPilot*nass_cottonNormalizationRatio), slope = 1, colour="red") +
      theme_bw();

t_wheat_plot <- data.frame(observed=m_wheat_current$model$area*acresToKm*ratioToPilot*nass_wheatNormalizationRatio,
                           predicted=m_wheat_current$fit*acresToKm*ratioToPilot*nass_wheatNormalizationRatio,
                           year=m_wheat_current$model$year)
p3 <- ggplot(t_wheat_plot) + 
      geom_line(aes(y=observed, x=year), colour="black") + 
      geom_point(aes(y=observed, x=year), colour="black") + 
      geom_line(aes(y=predicted, x=year), colour="orange") + 
      geom_point(aes(y=predicted, x=year), colour="orange") +     
      scale_x_continuous(breaks = round(seq(min(t_wheat_plot$year,na.rm=T), max(t_wheat_plot$year,na.rm=T), by = 5),1)) +  
      xlab("") + ylab(expression(paste('Area Planted (',km^{2},')'))) +
      annotate("text", y=max(t_wheat_plot$observed,na.rm=T), x=min(t_wheat_plot$year,na.rm=T)*1.0014, label = "C.) Wheat", size=4) +
      geom_abline(intercept = mean(t_wheat$area*acresToKm*ratioToPilot*nass_wheatNormalizationRatio), slope = 1, colour="red") +
      theme_bw();

t_sorghum_plot <- data.frame(observed=m_sorghum_current$model$area*acresToKm*ratioToPilot*nass_sorghumNormalizationRatio,
                          predicted=m_sorghum_current$fit*acresToKm*ratioToPilot*nass_sorghumNormalizationRatio,
                          year=m_sorghum_current$model$year)

p4 <- ggplot(t_sorghum_plot) + 
      geom_line(aes(y=observed, x=year), colour="black") + 
      geom_point(aes(y=observed, x=year), colour="black") + 
      geom_line(aes(y=predicted, x=year), colour="orange") + 
      geom_point(aes(y=predicted, x=year), colour="orange") +      
      scale_x_continuous(breaks = round(seq(min(t_sorghum_plot$year,na.rm=T), max(t_sorghum_plot$year,na.rm=T), by = 5),1)) + 
      xlab("") + ylab(expression(paste('Area Planted (',km^{2},')'))) +
      annotate("text", y=max(t_sorghum_plot$observed,na.rm=T), x=max(t_sorghum_plot$year,na.rm=T)*0.9980, label = "D.) Sorghum", size=4) +
      geom_abline(intercept = mean(t_sorghum$area*acresToKm*ratioToPilot*nass_sorghumNormalizationRatio), slope = 1, colour="red") +
      theme_bw();

grid.arrange(p1,p2,p3,p4);

## make barplots of forecasts for 2015, 2020, 2025, with p=0.95 intervals for each crop

t_corn_future[,1:3]    <- t_corn_future[,1:3]*nass_cornNormalizationRatio
t_wheat_future[,1:3]   <- t_wheat_future[,1:3]*nass_wheatNormalizationRatio
t_sorghum_future[,1:3] <- t_sorghum_future[,1:3]*nass_sorghumNormalizationRatio
t_cotton_future[,1:3]  <- t_cotton_future[,1:3]*nass_cottonNormalizationRatio

t_future_plot <- rbind(cbind(t_corn_future, crop="corn"),
                       cbind(t_wheat_future, crop="wheat"),
                       cbind(t_sorghum_future, crop="sorghum"),
                       cbind(t_cotton_future, crop="cotton"))
t_future_plot <- t_future_plot[t_future_plot$year %in% c(2015,2020,2025),]

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

dev.new(width=5, height=7)

p1 <- ggplot(t_future_plot, aes(fill=factor(crop))) + 
      geom_bar(aes(y=area, x=year),stat="identity",position=position_dodge(3.1),width=3) + 
      geom_errorbar(aes(y=area, x=year, ymin=lower, ymax=upper), colour="black", alpha=0.5,width=0.1, position=position_dodge(3.1)) + 
      geom_abline(intercept = -2020, slope = 1, colour="black") +
      scale_fill_manual(values=cbPalette) + 
      scale_x_continuous(breaks=c(2015,2020,2025)) + 
      xlab("") + ylab(expression(paste("Area Planted (",km^{2},")"))) + 
      coord_flip() + 
      theme_light() + theme(legend.title=element_blank());

grid.arrange(p1);




