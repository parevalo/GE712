# This is a script to read in temperature data from CRU at 0.5-degree
# and calculate monthly seasonal standardized anomalies in mean temperature
# created by Radost Stanimirova April 2017

# init ----------------------

# load libraries ------------------
library(ncdf4)
library(chron)
library(RColorBrewer)
library(lattice)

library(sp)
library(raster)
library(rgdal)
library(gtools)
library(maptools)

# set functions ----------------------

# make a matrix of all the z score values from a raster stack
get_raster_vals <- function(data){
  
  z_values <- matrix(NA, nrow=259200,ncol=dim(data)[3])
  for (i in 1:dim(data)[3]) {
    sub_values1 <- getValues(data[[i]])
    
    if(i==1){
      z_values <-sub_values1
      
    }else{
      z_values <- cbind(z_values, sub_values1)
      
    }
  }
  return(z_values)
  
}

# assign in variables ------------------------
year <- rep(1:13, each=12)
m_year <- rep(1:12, 13)
seasons <- rep(rep(1:4, each=3),13)
index <- rep(1:13, each=3)

# read in South America boundary 
data(wrld_simpl)
SA <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", 
        "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")
sa <- wrld_simpl[which(wrld_simpl@data$NAME %in% SA),]

# read in CRU data --------------------------
temp <- brick("/projectnb/modislc/users/rkstan/GE712/data/cru.ts4.00/cru_ts4_pasture.tif") # read in the red SDS as a raster
temp_time_sub <- temp[[1224:1380]] # subset only desired time period 2003-2016

temp_sub_unstack <- unstack(dropLayer(temp_time_sub,157)) # unstack the raster brick into raster layers
temp <- values(dropLayer(temp_time_sub,157))
colnames(temp) <- rep(c("December", "January", "February", "March", "April", 
                          "May", "June", "July", "August", "September", "October", "November"), 13)
write.csv(temp, file="/projectnb/modislc/users/rkstan/GE712/outputs/temp_monthly_vals.csv", quote=FALSE, row.names=FALSE)

# calculate monthly temperature anomalies --------------------------------
month_list <- list()
year_list <- list()

# calculate the anomalies for each month (e.g., subtract January temp from mean January temperature for 14 year time span)
for (t in 1:13){
  for (m in 1:12){
    # calculate the mean temperature of each month over the whole 14 year period (excluding each time the month of interest)
    temp_month_mean <- calc(stack(temp_sub_unstack[m_year==m]),mean, na.rm=T)
    temp_month_sd <- calc(stack(temp_sub_unstack[m_year==m]),sd, na.rm=T) # calculate the standard deviation 
    temp_month_year <- stack(temp_sub_unstack[m_year==m])[[t]]
    
    # calculate the anomaly 
    temp_anomaly <- (temp_month_year - temp_month_mean)/temp_month_sd
    month_list[[m]] <- temp_anomaly

    
  }
   year_list[[t]] <- month_list
}

temp_monthly_anomalies <- stack(unlist(year_list, recursive = T))

# get the anomalies in a matrix and save as csv 
temp_monthly_anomalies_vals <- get_raster_vals(stack(temp_monthly_anomalies))
colnames(temp_monthly_anomalies_vals) <- rep(c("December", "January", "February", "March", "April", 
                                               "May", "June", "July", "August", "September", "October", "November"), 13)
write.csv(temp_monthly_anomalies_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/temp_monthly_anomalies.csv", quote=FALSE, row.names=FALSE)

#monthly lagged values 
temp_monthly_anomalies_lag1 <- temp_monthly_anomalies_vals[,-156]
write.csv(temp_monthly_anomalies_lag1, file="/projectnb/modislc/users/rkstan/GE712/outputs/temp_monthly_anomalies_lag1.csv", quote=FALSE, row.names=FALSE)

temp_monthly_anomalies_lag2 <- temp_monthly_anomalies_vals[,-c(155,156)]
write.csv(temp_monthly_anomalies_lag2, file="/projectnb/modislc/users/rkstan/GE712/outputs/temp_monthly_anomalies_lag2.csv", quote=FALSE, row.names=FALSE)

temp_monthly_anomalies_lag3 <- temp_monthly_anomalies_vals[,-c(154,155,156)]
write.csv(temp_monthly_anomalies_lag3, file="/projectnb/modislc/users/rkstan/GE712/outputs/temp_monthly_anomalies_lag3.csv", quote=FALSE, row.names=FALSE)

temp_monthly_anomalies_lag4 <- temp_monthly_anomalies_vals[,-c(153,154,155,156)]
write.csv(temp_monthly_anomalies_lag4, file="/projectnb/modislc/users/rkstan/GE712/outputs/temp_monthly_anomalies_lag4.csv", quote=FALSE, row.names=FALSE)

# calculate seasonal temperature anomalies --------------------------------
seas_list <- list()
seas_all <- list()
seas_vals <- list()
seas_vals_all <- list()

for (t in 1:13){
  for (m in 1:4){
    # calculate the mean temperature of each month over the whole 14 year period (excluding each time the month of interest)
    temp_seas_mean <-calc(stack(temp_sub_unstack[seasons==m]),mean, na.rm=T)
    temp_seas_sd <- calc(stack(temp_sub_unstack[seasons==m]),sd, na.rm=T) # calculate the standard deviation 
    temp_mean <- stackApply(stack(temp_sub_unstack[seasons==m]), index, mean, na.rm=T)[[t]]
    
    # calculate the anomaly 
    temp_anomaly <- (temp_mean - temp_seas_mean)/temp_seas_sd
    seas_list[[m]] <- temp_anomaly
    seas_vals[[m]] <- temp_mean
    
  }
  seas_all[[t]] <- seas_list
  seas_vals_all[[t]] <- seas_vals
}

temp_seas_anomalies <- stack(unlist(seas_all, recursive = T))
temp_seas_vals <- stack(unlist(seas_vals_all, recursive = T))

# get the seasonal anomalies in a matrix and save as csv 
temp_seas_anomalies_vals <- get_raster_vals(stack(temp_seas_anomalies))
colnames(temp_seas_anomalies_vals) <- rep(c("DJF", "MAM", "JJA", "SON"), 13)
write.csv(temp_seas_anomalies_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/temp_seas_anomalies.csv", quote=FALSE, row.names=FALSE)

# get the seasonal values in a matrix and save as csv 
temp_seasonal_vals <- get_raster_vals(stack(temp_seas_vals))
colnames(temp_seasonal_vals) <- rep(c("DJF", "MAM", "JJA", "SON"), 13)
write.csv(temp_seasonal_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/temp_seas_vals.csv", quote=FALSE, row.names=FALSE)

#lagged seasonal values 
temp_seas_anomalies_lag1 <- temp_seas_anomalies_vals[,-52]
write.csv(temp_seas_anomalies_lag1, file="/projectnb/modislc/users/rkstan/GE712/outputs/temp_seas_anomalies_lag1.csv", quote=FALSE, row.names=FALSE)

temp_seas_anomalies_lag2 <- temp_seas_anomalies_vals[,-c(51,52)]
write.csv(temp_seas_anomalies_lag2, file="/projectnb/modislc/users/rkstan/GE712/outputs/temp_seas_anomalies_lag2.csv", quote=FALSE, row.names=FALSE)

# calculate yearly anomalies -------------------------------------------------
year_anom <- list()
temp_sub_year <- dropLayer(temp_time_sub, 1)
temp_year <- stackApply(temp_sub_year, year, fun=mean, na.rm=T)

for (t in 1:13){
temp_year_mean <-calc(temp_sub_year,mean, na.rm=T)
temp_year_sd <-calc(temp_sub_year,sd, na.rm=T)
temp_year_t <- temp_year[[t]]

# calculate the anomaly 
temp_anomaly <- (temp_year_t - temp_year_mean)/temp_year_sd
year_anom[[t]] <- temp_anomaly

}
temp_year_anomalies <- stack(unlist(year_anom, recursive = T))

# get the year anomalies in a matrix and save as csv 
temp_year_anomalies_vals <- get_raster_vals(stack(temp_year_anomalies))
colnames(temp_year_anomalies_vals) <- c("2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012",
                                        "2013", "2014", "2015")
write.csv(temp_year_anomalies_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/temp_year_anomalies.csv", quote=FALSE, row.names=FALSE)


# plot monthly anomalies -------------------------------------------------
# names(temp_monthly_anomalies) <- paste0("Y", 2003:2015)
# spplot(temp_monthly_anomalies[[3]], bty='n',xaxt='n', yaxt='n', box=FALSE)
# spSA = list("sp.polygons", sa)
# spplot(temp_monthly_anomalies[[3]], bty='n', xaxt='n', yaxt='n', box=FALSE, sp.layout=spSA)


plot(temp_monthly_anomalies[[15]],col=brewer.pal(6, "RdBu"), xlim=c(xmin(temp_monthly_anomalies[[15]]), xmax(temp_monthly_anomalies[[15]])),ylim=c(ymin(temp_monthly_anomalies[[15]]), ymax(temp_monthly_anomalies[[15]])),
     cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
plot(sa, add=T,lty=1,lwd=0.5)
#axis(side=1,pos=ymin(precip_anomaly[[8]]),at=seq(xmin(precip_anomaly[[8]]),xmax(precip_anomaly[[8]]),10), cex.axis=1.25)
#axis(side=2,pos=xmin(precip_anomaly[[8]]),at=seq(ymin(precip_anomaly[[8]]),ymax(precip_anomaly[[8]]),10), cex.axis=1.25)
#title(xlab="Longitude", cex.lab=1.25, line=2.5)
#title(ylab="Latitude", cex.lab=1.25, line=2.5)
#color.bar(igbpcol_real,1,17)
#legend("right", inset=c(-0.4,-0.4), legend="Land Cover Class")
plot(temp_monthly_anomalies[[15]], col=brewer.pal(6, "RdBu"),legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("Temperature Standardized Anomalies", side=4, line=2.9, cex=1.25))

# plot seasonal anomalies -------------------------------------------------
names(temp_seas_anomalies[[3]]) <- paste0("Y", 2003:2015)
spplot(temp_seas_anomalies[[3]], bty='n',xaxt='n', yaxt='n', box=FALSE)
spSA = list("sp.polygons", sa)
spplot(temp_seas_anomalies[[3]], bty='n', xaxt='n', yaxt='n', box=FALSE, sp.layout=spSA)

class_vals = seq(-2,2, by=0.5)

setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")
dev.copy(png,"seas_anomalies_temp.png", width=800,height=720)

plot(temp_seas_anomalies[[5]],col=rev(brewer.pal(6, "RdBu")), xlim=c(xmin(temp_seas_anomalies[[5]]), xmax(temp_seas_anomalies)),ylim=c(ymin(temp_seas_anomalies[[5]]), ymax(temp_seas_anomalies[[5]])),
     cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
plot(sa, add=T,lty=1,lwd=0.5)
plot(temp_seas_anomalies[[5]], col=rev(brewer.pal(6, "RdBu")),legend.only=TRUE, legend.width=1, legend.shrink=0.85,legend.args=list("Temperature Standardized Anomalies", side=4, line=2.9, cex=1.5), axis.args=list(at=class_vals,labels=class_vals))

dev.off ()
