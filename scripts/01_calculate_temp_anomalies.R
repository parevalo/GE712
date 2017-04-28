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
seasons <- rep(c(1,1,2,2,2,3,3,3,4,4,4,1), 13)
index <- rep(1:13, each=3)

# read in South America boundary 
data(wrld_simpl)
SA <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", 
        "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")
sa <- wrld_simpl[which(wrld_simpl@data$NAME %in% SA),]


# read in pastureland extent data --------------------------
pasture_2000_sub <- raster("/projectnb/modislc/users/rkstan/GE712/data/pasture_extent/pasture2000_GT_06_resample_05.tif")

# read in CRU data --------------------------
temp <- brick("/projectnb/modislc/users/rkstan/GE712/data/cru.ts4.00/cru_ts4_SA.tif") # read in the red SDS as a raster
temp_time_sub <- temp[[1225:1380]] # subset only desired time period 2003-2016
temp_sa_sub <- mask(temp_time_sub, sa) # limit to extent of South America shape file
temp_sub <- mask(temp_sa_sub, pasture_2000_sub, maskvalue=0,  updatevalue=NA) # limit to extent of rangeland extent file

temp_sub_unstack <- unstack(temp_sub) # unstack the raster brick into raster layers


temp <- values(temp_sub)


# calculate monthly temperature anomalies --------------------------------
month_list <- list()
year_list <- list()

# calculate the anomalies for each month (e.g., subtract January temp from mean January temperature for 14 year time span)
for (t in 1:13){
  for (m in 1:12){
    # calculate the mean temperature of each month over the whole 14 year period (excluding each time the month of interest)
    temp_month_mean <- calc(dropLayer(stack(temp_sub_unstack[m_year==m]), t),mean, na.rm=T)
    temp_month_sd <- calc(dropLayer(stack(temp_sub_unstack[m_year==m]), t),sd, na.rm=T) # calculate the standard deviation 
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
write.csv(temp_monthly_anomalies_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/temp_monthly_anomalies.csv", quote=FALSE, row.names=FALSE)

# calculate seasonal temperature anomalies --------------------------------
for (t in 1:13){
  for (m in 1:4){
    # calculate the mean temperature of each month over the whole 14 year period (excluding each time the month of interest)
    temp_seas_mean <-calc(dropLayer(stack(temp_sub_unstack[seasons==m]), which(index==t, arr.ind=TRUE)),mean, na.rm=T)
    temp_seas_sd <- calc(dropLayer(stack(temp_sub_unstack[seasons==m]), which(index==t, arr.ind=TRUE)),sd, na.rm=T) # calculate the standard deviation 
    temp_mean <- stackApply(stack(temp_sub_unstack[seasons==m]), index, mean, na.rm=T)
    
    # calculate the anomaly 
    temp_anomaly <- (temp_mean - temp_seas_mean)/temp_seas_sd
    
    if(m==1){
      temp_seas_anomalies <- temp_anomaly
      
    }else{
      temp_seas_anomalies  <- append(temp_seas_anomalies, temp_anomaly)    
      
    }
    
  }
  
}

# get the seasonal anomalies in a matrix and save as csv 
temp_seas_anomalies_vals <- get_raster_vals(stack(temp_seas_anomalies))
write.csv(temp_seas_anomalies_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/temp_seas_anomalies.csv", quote=FALSE, row.names=FALSE)

# plot monthly anomalies -------------------------------------------------
names(temp_monthly_anomalies[[3]]) <- paste0("Y", 2003:2015)
spplot(temp_monthly_anomalies[[3]], bty='n',xaxt='n', yaxt='n', box=FALSE)
spSA = list("sp.polygons", sa)
spplot(temp_monthly_anomalies[[3]], bty='n', xaxt='n', yaxt='n', box=FALSE, sp.layout=spSA)


plot(temp_monthly_anomalies[[3]][[5]],col=brewer.pal(6, "RdBu"), xlim=c(xmin(temp_monthly_anomalies[[3]][[5]]), xmax(temp_monthly_anomalies[[3]][[5]])),ylim=c(ymin(temp_monthly_anomalies[[3]][[5]]), ymax(temp_monthly_anomalies[[3]][[5]])),
     cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
plot(sa, add=T,lty=1,lwd=0.5)
#axis(side=1,pos=ymin(precip_anomaly[[8]]),at=seq(xmin(precip_anomaly[[8]]),xmax(precip_anomaly[[8]]),10), cex.axis=1.25)
#axis(side=2,pos=xmin(precip_anomaly[[8]]),at=seq(ymin(precip_anomaly[[8]]),ymax(precip_anomaly[[8]]),10), cex.axis=1.25)
#title(xlab="Longitude", cex.lab=1.25, line=2.5)
#title(ylab="Latitude", cex.lab=1.25, line=2.5)
#color.bar(igbpcol_real,1,17)
#legend("right", inset=c(-0.4,-0.4), legend="Land Cover Class")
plot(temp_monthly_anomalies[[3]][[5]], col=brewer.pal(6, "RdBu"),legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("Temperature Standardized Anomalies", side=4, line=2.9, cex=1.25))

# plot seasonal anomalies -------------------------------------------------
names(temp_seas_anomalies[[3]]) <- paste0("Y", 2003:2015)
spplot(temp_seas_anomalies[[3]], bty='n',xaxt='n', yaxt='n', box=FALSE)
spSA = list("sp.polygons", sa)
spplot(temp_seas_anomalies[[3]], bty='n', xaxt='n', yaxt='n', box=FALSE, sp.layout=spSA)


plot(temp_seas_anomalies[[3]][[5]],col=brewer.pal(6, "RdBu"), xlim=c(xmin(temp_seas_anomalies[[3]][[5]]), xmax(temp_seas_anomalies[[3]][[5]])),ylim=c(ymin(temp_seas_anomalies[[3]][[5]]), ymax(temp_seas_anomalies[[3]][[5]])),
     cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
plot(sa, add=T,lty=1,lwd=0.5)
plot(temp_seas_anomalies[[3]][[5]], col=brewer.pal(6, "RdBu"),legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("Temperature Standardized Anomalies", side=4, line=2.9, cex=1.25))


