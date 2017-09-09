# This is a script to read in filetered EVI data from MOD13C2 at 0.5-degree
# and calculate monthly seasonal standardized anomalies in EVI
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

# set functions ---------------------

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

# read in EVI data --------------------------
#EVI <- brick("/projectnb/modislc/users/rkstan/GE712/data/MOD13C2/filtered_EVI_pastures.tif") # read in the red SDS as a raster
EVI <- brick("/projectnb/modislc/users/rkstan/GE712/data/MOD13C2/filtered_EVI_resample_05.tif")

EVI_sub_unstack <- unstack(dropLayer(EVI,157)) # unstack the raster brick into raster layers
EVI_vals <- values(dropLayer(EVI,157))
colnames(EVI_vals) <- rep(c("December", "January", "February", "March", "April", 
                        "May", "June", "July", "August", "September", "October", "November"), 13)
write.csv(EVI_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_monthly_vals_005.csv", quote=FALSE, row.names=FALSE)


# calculate monthly EVI anomalies --------------------------------
month_list <- list()
year_list <- list()

# calculate the anomalies for each month (e.g., subtract January EVI from mean January EVI for 14 year time span)
for (t in 1:13){
  for (m in 1:12){
    # calculate the mean EVI of each month over the whole 14 year period (excluding each time the month of interest)
    EVI_month_mean <- calc(stack(EVI_sub_unstack[m_year==m]),mean, na.rm=T)
    EVI_month_sd <- calc(stack(EVI_sub_unstack[m_year==m]),sd, na.rm=T) # calculate the standard deviation 
    EVI_month_year <- stack(EVI_sub_unstack[m_year==m])[[t]]
    
    # calculate the anomaly 
    EVI_anomaly <- (EVI_month_year - EVI_month_mean)/EVI_month_sd
    month_list[[m]] <- EVI_anomaly
    
  }
  year_list[[t]] <- month_list
}

EVI_monthly_anomalies <- stack(unlist(year_list, recursive = T))

# get the anomalies in a matrix and save as csv 
EVI_monthly_anomalies_vals <- getValues(stack(EVI_monthly_anomalies))
colnames(EVI_monthly_anomalies_vals) <- rep(c("December", "January", "February", "March", "April", 
                                              "May", "June", "July", "August", "September", "October", "November"), 13)
write.csv(EVI_monthly_anomalies_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_monthly_anomalies_005.csv", quote=FALSE, row.names=FALSE)

#monthly lagged values 
EVI_monthly_anomalies_lag1 <- EVI_monthly_anomalies_vals[,-1]
write.csv(EVI_monthly_anomalies_lag1, file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_monthly_anomalies_lag1.csv", quote=FALSE, row.names=FALSE)

EVI_monthly_anomalies_lag2 <- EVI_monthly_anomalies_vals[,-c(1,2)]
write.csv(EVI_monthly_anomalies_lag2, file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_monthly_anomalies_lag2.csv", quote=FALSE, row.names=FALSE)

EVI_monthly_anomalies_lag3 <- EVI_monthly_anomalies_vals[,-c(1,2,3)]
write.csv(EVI_monthly_anomalies_lag3, file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_monthly_anomalies_lag3.csv", quote=FALSE, row.names=FALSE)

EVI_monthly_anomalies_lag4 <- EVI_monthly_anomalies_vals[,-c(1,2,3,4)]
write.csv(EVI_monthly_anomalies_lag4, file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_monthly_anomalies_lag4.csv", quote=FALSE, row.names=FALSE)


# calculate seasonal EVI anomalies --------------------------------
seas_list <- list()
seas_all <- list()
seas_vals <- list()
seas_vals_all <- list()

for (t in 1:13){
  for (m in 1:4){
    # calculate the mean EVI of each month over the whole 14 year period (excluding each time the month of interest)
    EVI_seas_mean <-calc(stack(EVI_sub_unstack[seasons==m]),mean, na.rm=T)
    EVI_seas_sd <- calc(stack(EVI_sub_unstack[seasons==m]),sd, na.rm=T) # calculate the standard deviation 
    EVI_mean <- stackApply(stack(EVI_sub_unstack[seasons==m]), index, mean, na.rm=T)[[t]]
    
    # calculate the anomaly 
    EVI_anomaly <- (EVI_mean - EVI_seas_mean)/EVI_seas_sd
    seas_list[[m]] <- EVI_anomaly
    seas_vals[[m]] <- EVI_mean
    
  }
  seas_all[[t]] <- seas_list
  seas_vals_all[[t]] <- seas_vals
}

EVI_seas_anomalies <- stack(unlist(seas_all, recursive = T))
EVI_seas_vals <- stack(unlist(seas_vals_all, recursive = T))

# get the seasonal anomalies in a matrix and save as csv 
EVI_seas_anomalies_vals <- getValues(stack(EVI_seas_anomalies))
colnames(EVI_seas_anomalies_vals) <- rep(c("DJF", "MAM", "JJA", "SON"), 13)
write.csv(EVI_seas_anomalies_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_seas_anomalies_005.csv", quote=FALSE, row.names=FALSE)

# get the seasonal anomalies in a matrix and save as csv 
EVI_seas_vals <- getValues(stack(EVI_seas_vals))
colnames(EVI_seas_vals) <- rep(c("DJF", "MAM", "JJA", "SON"), 13)
write.csv(EVI_seas_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_seas_vals_005.csv", quote=FALSE, row.names=FALSE)

#lagged seasonal values 
EVI_seas_anomalies_lag1 <- EVI_seas_anomalies_vals[,-1]
write.csv(EVI_seas_anomalies_lag1, file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_seas_anomalies_lag1.csv", quote=FALSE, row.names=FALSE)

EVI_seas_anomalies_lag2 <- EVI_seas_anomalies_vals[,-c(1,2)]
write.csv(EVI_seas_anomalies_lag2, file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_seas_anomalies_lag2.csv", quote=FALSE, row.names=FALSE)


# calculate yearly anomalies -------------------------------------------------
year_anom <- list()
EVI_sub_year <- dropLayer(EVI, 1)

EVI_year <- stackApply(EVI_sub_year, year, fun=mean, na.rm=T)
for (t in 1:13){
  EVI_year_mean <-calc(EVI_sub_year,mean, na.rm=T)
  EVI_year_sd <-calc(EVI_sub_year,sd, na.rm=T)
  EVI_year_t <- EVI_year[[t]]
  
  # calculate the anomaly 
  EVI_anomaly <- (EVI_year_t - EVI_year_mean)/EVI_year_sd
  year_anom[[t]] <- EVI_anomaly
}
EVI_year_anomalies <- stack(unlist(year_anom, recursive = T))

# get the year anomalies in a matrix and save as csv 
EVI_year_anomalies_vals <- getValues(stack(EVI_year_anomalies))
colnames(EVI_year_anomalies_vals) <- c("2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012",
                                        "2013", "2014", "2015")
write.csv(EVI_year_anomalies_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_year_anomalies.csv", quote=FALSE, row.names=FALSE)

EVI_year_vals <- getValues(EVI_year)
colnames(EVI_year_vals) <- c("2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012",
                                       "2013", "2014", "2015")
write.csv(EVI_year_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_year_vals_0083.csv", quote=FALSE, row.names=FALSE)



# plot monthly anomalies -------------------------------------------------
names(EVI_monthly_anomalies[[3]]) <- paste0("Y", 2003:2015)
spplot(EVI_monthly_anomalies[[3]], bty='n',xaxt='n', yaxt='n', box=FALSE)
spSA = list("sp.polygons", sa)
spplot(EVI_monthly_anomalies[[3]], bty='n', xaxt='n', yaxt='n', box=FALSE, sp.layout=spSA)

plot_evi <- function(x){
  plot(EVI_seas_vals, x,col=brewer.pal(5, "YlGn"), colNA="grey",xlim=c(xmin(EVI_seas_vals), xmax(EVI_seas_vals)),ylim=c(ymin(EVI_seas_vals), ymax(EVI_seas_vals)),
       cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F, main="") 
  plot(sa, add=T,lty=1,lwd=0.5)
  #axis(side=1,pos=ymin(precip_anomaly[[8]]),at=seq(xmin(precip_anomaly[[8]]),xmax(precip_anomaly[[8]]),10), cex.axis=1.25)
  #axis(side=2,pos=xmin(precip_anomaly[[8]]),at=seq(ymin(precip_anomaly[[8]]),ymax(precip_anomaly[[8]]),10), cex.axis=1.25)
  #title(xlab="Longitude", cex.lab=1.25, line=2.5)
  #title(ylab="Latitude", cex.lab=1.25, line=2.5)
  #color.bar(igbpcol_real,1,17)
  #legend("right", inset=c(-0.4,-0.4), legend="Land Cover Class")
  plot(EVI_seas_vals, x,col=brewer.pal(5, "YlGn"),legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("EVI", side=4, line=2.9, cex=1.25))
  
}

par(mfrow=c(2,2), pty="s", oma=c(0.5,0.5,0.5,0.5), mar=c(4,4,4,4))
plot_evi(1)
plot_evi(2)
plot_evi(3)
plot_evi(4)

# plot seasonal anomalies -------------------------------------------------
names(EVI_seas_anomalies[[3]]) <- paste0("Y", 2003:2015)
spplot(EVI_seas_anomalies[[3]], bty='n',xaxt='n', yaxt='n', box=FALSE)
spSA = list("sp.polygons", sa)
spplot(EVI_seas_anomalies[[3]], bty='n', xaxt='n', yaxt='n', box=FALSE, sp.layout=spSA)


plot(EVI_seas_anomalies[[2]],col=brewer.pal(6, "RdYlGn"), xlim=c(xmin(EVI_seas_anomalies[[2]]), xmax(EVI_seas_anomalies[[2]])),ylim=c(ymin(EVI_seas_anomalies[[2]]), ymax(EVI_seas_anomalies[[2]])),
     cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
plot(sa, add=T,lty=1,lwd=0.5)
plot(EVI_seas_anomalies[[2]], col=brewer.pal(6, "RdYlGn"),legend.only=TRUE, legend.width=1, legend.shrink=0.85,legend.args=list("EVI Standardized Anomalies", side=4, line=2.5, cex=1.5))

