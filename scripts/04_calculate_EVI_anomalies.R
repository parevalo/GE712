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


# read in EVI data --------------------------
EVI <- brick("/projectnb/modislc/users/rkstan/GE712/data/MOD13C2/filtered_EVI_resample_05.tif") # read in the red SDS as a raster
EVI_sa_sub <- mask(EVI, sa) # limit to extent of South America shape file
EVI_sub <- mask(EVI_sa_sub, pasture_2000_sub, maskvalue=0,  updatevalue=NA) # limit to extent of rangeland extent file

EVI_sub_unstack <- unstack(EVI_sub) # unstack the raster brick into raster layers
EVI_vals <- values(EVI_sub)

# calculate monthly EVI anomalies --------------------------------
month_list <- list()
year_list <- list()

# calculate the anomalies for each month (e.g., subtract January EVI from mean January EVI for 14 year time span)
for (t in 1:13){
  for (m in 1:12){
    # calculate the mean EVI of each month over the whole 14 year period (excluding each time the month of interest)
    EVI_month_mean <- calc(dropLayer(stack(EVI_sub_unstack[m_year==m]), t),mean, na.rm=T)
    EVI_month_sd <- calc(dropLayer(stack(EVI_sub_unstack[m_year==m]), t),sd, na.rm=T) # calculate the standard deviation 
    EVI_month_year <- stack(EVI_sub_unstack[m_year==m])[[t]]
    
    # calculate the anomaly 
    EVI_anomaly <- (EVI_month_year - EVI_month_mean)/EVI_month_sd
    month_list[[m]] <- EVI_anomaly
    
  }
  year_list[[t]] <- month_list
}

EVI_monthly_anomalies <- stack(unlist(year_list, recursive = T))

# get the anomalies in a matrix and save as csv 
EVI_monthly_anomalies_vals <- get_raster_vals(stack(EVI_monthly_anomalies))
write.csv(EVI_monthly_anomalies_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_monthly_anomalies.csv", quote=FALSE, row.names=FALSE)

# calculate seasonal EVI anomalies --------------------------------
for (t in 1:13){
  for (m in 1:4){
    # calculate the mean EVI of each month over the whole 14 year period (excluding each time the month of interest)
    EVI_seas_mean <-calc(dropLayer(stack(EVI_sub_unstack[seasons==m]), which(index==t, arr.ind=TRUE)),mean, na.rm=T)
    EVI_seas_sd <- calc(dropLayer(stack(EVI_sub_unstack[seasons==m]), which(index==t, arr.ind=TRUE)),sd, na.rm=T) # calculate the standard deviation 
    EVI_mean <- stackApply(stack(EVI_sub_unstack[seasons==m]), index, mean, na.rm=T)
    
    # calculate the anomaly 
    EVI_anomaly <- (EVI_mean - EVI_seas_mean)/EVI_seas_sd
    
    if(m==1){
      EVI_seas_anomalies <- EVI_anomaly
      
    }else{
      EVI_seas_anomalies  <- append(EVI_seas_anomalies, EVI_anomaly)    
      
    }
    
  }
  
}

# get the seasonal anomalies in a matrix and save as csv 
EVI_seas_anomalies_vals <- get_raster_vals(stack(EVI_seas_anomalies))
write.csv(EVI_seas_anomalies_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_seas_anomalies.csv", quote=FALSE, row.names=FALSE)

# plot monthly anomalies -------------------------------------------------
names(EVI_monthly_anomalies[[3]]) <- paste0("Y", 2003:2015)
spplot(EVI_monthly_anomalies[[3]], bty='n',xaxt='n', yaxt='n', box=FALSE)
spSA = list("sp.polygons", sa)
spplot(EVI_monthly_anomalies[[3]], bty='n', xaxt='n', yaxt='n', box=FALSE, sp.layout=spSA)


plot(EVI_monthly_anomalies[[3]][[5]],col=brewer.pal(6, "RdBu"), xlim=c(xmin(EVI_monthly_anomalies[[3]][[5]]), xmax(EVI_monthly_anomalies[[3]][[5]])),ylim=c(ymin(EVI_monthly_anomalies[[3]][[5]]), ymax(EVI_monthly_anomalies[[3]][[5]])),
     cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
plot(sa, add=T,lty=1,lwd=0.5)
#axis(side=1,pos=ymin(precip_anomaly[[8]]),at=seq(xmin(precip_anomaly[[8]]),xmax(precip_anomaly[[8]]),10), cex.axis=1.25)
#axis(side=2,pos=xmin(precip_anomaly[[8]]),at=seq(ymin(precip_anomaly[[8]]),ymax(precip_anomaly[[8]]),10), cex.axis=1.25)
#title(xlab="Longitude", cex.lab=1.25, line=2.5)
#title(ylab="Latitude", cex.lab=1.25, line=2.5)
#color.bar(igbpcol_real,1,17)
#legend("right", inset=c(-0.4,-0.4), legend="Land Cover Class")
plot(EVI_monthly_anomalies[[3]][[5]], col=brewer.pal(6, "RdBu"),legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("EVI Standardized Anomalies", side=4, line=2.9, cex=1.25))

# plot seasonal anomalies -------------------------------------------------
names(EVI_seas_anomalies[[3]]) <- paste0("Y", 2003:2015)
spplot(EVI_seas_anomalies[[3]], bty='n',xaxt='n', yaxt='n', box=FALSE)
spSA = list("sp.polygons", sa)
spplot(EVI_seas_anomalies[[3]], bty='n', xaxt='n', yaxt='n', box=FALSE, sp.layout=spSA)


plot(EVI_seas_anomalies[[3]][[5]],col=brewer.pal(6, "RdBu"), xlim=c(xmin(EVI_seas_anomalies[[3]][[5]]), xmax(EVI_seas_anomalies[[3]][[5]])),ylim=c(ymin(EVI_seas_anomalies[[3]][[5]]), ymax(EVI_seas_anomalies[[3]][[5]])),
     cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
plot(sa, add=T,lty=1,lwd=0.5)
plot(EVI_seas_anomalies[[3]][[5]], col=brewer.pal(6, "RdBu"),legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("EVI Standardized Anomalies", side=4, line=2.9, cex=1.25))

