# Kristin Meier

# this is part of a function that calculates 'overnight bank funding rates'
# (as well as volumes).

# 1. Calculate Rates: calc_rates()
# 2. Import and clean data: import_clean_data()
#     type = "type1" or "type2"

# libraries
require("plyr")
require("xtable")
require("data.table")
require("reshape2")

###################################################
# 1. Calculate Rates    
###################################################

calc_rates <- function(){
  
  # bring in data
  brffed <- import_clean_data(type = "type1")
  
  ########################################################
  #################### CALCULATE RATES ###################
  ########################################################
 
  #### Weighted median function
  wmedian <- function(r,w){
    w <- w[order(r)]
    r <- r[order(r)]
    r[which.max(cumsum(w)/sum(w) > .5)]
  }
  
  #### Weighted quantile function
  wquantile <- function( r, w, p ){ 
    w <- w[order(r)]
    r <- r[order(r)]
    r [ which.max( cumsum(w) / sum(w) >= p ) ]
  }
  
  ### rate and volume calculations
  brffed.new <- brffed[,
                       list(count = length(volume),
                            vol = sum(volume),
                            min.v = min(volume),
                            med.v = quantile(volume,c(.5)),
                            max.v = max(volume),
                            sd.v = sd(volume),
                            min.r = min(rate) * 100,
                            wmed.r = wmedian(rate,volume)* 100,
                            max.r = max(rate)* 100,
                            wsd.r = sqrt(sum(volume *
                                               (rate-weighted.mean(rate,volume))^2)/sum(volume))* 100,
                            wrate = weighted.mean(rate,volume)* 100,
                            trate = weighted.mean(rate[rate > wquantile(rate,volume,.05) & 
                                                       rate < wquantile(rate,volume,.95)],
                                                  volume[rate > wquantile(rate,volume,.05) & 
                                                        rate < wquantile(rate,volume,.95)])*100,
                            ff = weighted.mean(rate[ffdummy == 1],volume[ffdummy == 1])*100,
                            ed = weighted.mean(rate[ffdummy == 0],volume[ffdummy == 0])*100,
                            wmed.ff = wmedian(rate[ffdummy == 1],volume[ffdummy == 1])*100,
                            wmed.ed=wmedian(rate[ffdummy == 0],volume[ffdummy == 0])*100
                            ),
                       by = list(date)
                       ]
  
  return(brffed.new)
  
}
  
  
###################################################
# 2. Import and Clean Data    
###################################################

import_clean_data <- function(type = "type1"){
  
# get data in the format we want - all rates & vols for each FFED rate
# aka append the FF,ED data for each (already combined for type2)
  
###################################################
################## type1    ####################
###################################################
  
  if(type == "type1"){

# type1 daily volumes by rate
# this is currenlty updated every Monday through the previous Wed
# restricted by the min of the max dates available...(it is updated daily for the ED file)
  
    ffpanel <-read.csv("data1.csv",head=TRUE,sep=",",as.is=TRUE,stringsAsFactors=FALSE)
    ffpanel <- ffpanel[ffpanel$rates !=".",]
    ffpanel <- ffpanel[ffpanel$volumes !=".",]
    ffpanel$rates <- as.numeric(ffpanel$rates)
    ffpanel$volumes <- as.numeric(ffpanel$volumes)
    ffpanel$date <- as.Date(ffpanel$date,format="%m/%d/%Y")
    colnames(ffpanel) <- c("date","rate","volume")
  
    edpanel<-read.csv("data2.csv",head=TRUE,sep=",",as.is=TRUE,stringsAsFactors=FALSE)
    edpanel <- edpanel[edpanel$rate !=".",]
    edpanel <- edpanel[edpanel$volume !=".",]
    edpanel$rate <- as.numeric(edpanel$rate)
    edpanel$volume <- as.numeric(edpanel$volume)
    edpanel$eurodate <- as.Date(edpanel$eurodate,format="%m/%d/%Y")
    colnames(edpanel) <- c("date","rate","volume")
  
  ### ff csv file is not updating regularly
  ### this appends the [other] files to the data for the dates between
  ### the last date on ffpanel and the last date on edpanel
  ### since eurodollar data is updated daily this will be sufficient and
  ### the dates will always match what's available for ffsniffer files
  
    if(max(ffpanel$date) < max(edpanel$date)){
      datevector <- unique(edpanel$date[edpanel$date > max(ffpanel$date)])
      datevectorday <- format(datevector,"%d")
      datevectoryear <- format(datevector,"%y")
      datevectormonth <- format(datevector,"%b")
      datevectormonth <- toupper(substr(datevectormonth,0,3))
      csvdates <- data.frame(datevectorday,datevectormonth,datevectoryear)
      csvdates$date <-
      paste(datevectorday,datevectormonth,datevectoryear,".csv",sep="")
      csvdates <- csvdates[,4]
    
    for(i in 1:length(csvdates)){
      append <-
      read.csv(paste("filepath",csvdates[i],sep=""),head=FALSE,sep=",",as.is=TRUE,stringsAsFactors=FALSE)
      colnames(append) <- c("date","rate","volume")
      append$date <- as.Date(paste(append$date), format("%d%b%Y"))
    
      ffpanel <- rbind(ffpanel,append) 
      
    }
    }
    
    # line up dates
    min <- max(min(ffpanel$date),min(edpanel$date))
    max <- min(max(ffpanel$date),max(edpanel$date))
    
    ffpanel <- ffpanel[ffpanel$date >= min,]
    ffpanel <- ffpanel[ffpanel$date <= max,]
    edpanel <- edpanel[edpanel$date >= min,]
    edpanel <- edpanel[edpanel$date <= max,]
    
    ffpanel$ffdummy <- 1
    edpanel$ffdummy <- 0
    # append data
    brffed <- rbind(ffpanel,edpanel)
    brffed <- brffed[order(brffed$date, brffed$rate),]
    
    brffed <- data.table(brffed)
    brffed$volume <- brffed$volume*1
    
    return(brffed)
  }
  
###################################################
################## type2      ####################
###################################################
  
  if(type == "type2"){
    
   source("rfunction.R")
   ## ## ## ## ##
   type2 <- gettype2(ff.ed.filters=TRUE,overnight.only=TRUE)
   # restrictions
   type2 <- type2[type2$trade_type %in% c("FF","ED"),]
   type2 <- type2[!is.na(type2$rate) & !is.na(type2$maturity_date),]
   type2 <- type2[type2$overnight == TRUE,]
   # keep what was used before
   ffed <- as.data.frame(type2)[c("dt","id_rssd","rate","amount",
                                  "trade_type","maturity_date")]
   colnames(ffed) <- c("date","id_rssd","rate","volume","type","matdate")
  
   ffed <- data.table(ffed)
   return(ffed)
  }
  
}

 
