# This is a script to read in precipitation data from TRMM at 0.25-degree
# and calculate monthly seasonal standardized anomalies in precipitation
# created by Jian Bi and adapted in R by Radost Stanimirova April 2017

# init ----------------------

#rm(list = ls())
# loading required libraries 
library(sp)
library(rgdal)
library(raster)
library(data.table)
library(optparse)
library(rgdal)
library(gtools)
library(maptools)
library(maps)
library(RColorBrewer)

# define functions --------------------------
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


# set up to run in parallel ----------------------
library(parallel)
cl <- makeCluster(8)
clusterEvalQ(cl, {library(raster)})

# read in pastureland extent data --------------------------
pasture_2000_sub <- raster("/projectnb/modislc/users/rkstan/GE712/data/pasture_extent/pasture_2000_sub.tif")
pasture_2000_sub[pasture_2000_sub<0.6] <- NA

# read in TRMM data --------------------------
path_TRMM <- "/projectnb/modislc/users/rkstan/GE712/data/TRMM/" # path to your TRMM files
TRMM_tile_files <- Sys.glob(file.path(path_TRMM, paste("3B43", "*", "*_clip",'tif', sep=".")))

Getprecip <- function(TRMM_file_path, as.int=T){
  
  SDS_precip <- TRMM_file_path
  precip <- raster(SDS_precip) # read in the red SDS as a raster
  return(precip)
  
}


# get precpitation
precip_list <- parLapply(cl, TRMM_tile_files, Getprecip)
precip_stack <- stack(unlist(precip_list)) # unlist and convert to a single RasterStack
precip_time_sub <- precip_stack[[37:192]] # subset only desired time period 2003-2016

precip_sub <- mask(precip_time_sub, sa) # limit to extent of South America shape file 
precip_sub_unstack <- unstack(precip_sub) # unstack the raster brick into raster layers

precip <- values(precip_stack)

# calculate monthly precipitation anomalies --------------------------------

# calculate the anomalies for each month (e.g., subtract January precipitation from mean January precipitation for 14 year time span)
for (t in 1:13){
  for (m in 1:12){
    # calculate the mean precipitation of each month over the whole 14 year period (excluding each time the month of interest)
    precip_month_mean <- calc(dropLayer(stack(precip_sub_unstack[m_year==m]), t),mean, na.rm=T)
    precip_month_sd <- calc(dropLayer(stack(precip_sub_unstack[m_year==m]), t),sd, na.rm=T) # calculate the standard deviation 
    
    # calculate the anomaly 
    precip_anomaly <- (stack(precip_sub_unstack[m_year==m]) - precip_month_mean)/precip_month_sd
    
    if(m==1){
      precip_monthly_anomalies <-precip_anomaly
      
    }else{
      precip_monthly_anomalies  <- append(precip_monthly_anomalies, precip_anomaly)    
      
    }
    
  }
  
}


# get the anomalies in a matrix and save as csv 
precip_monthly_anomalies_vals <- get_raster_vals(stack(precip_monthly_anomalies))
write.csv(precip_monthly_anomalies_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_monthly_anomalies.csv", quote=FALSE, row.names=FALSE)

# calculate seasonal precipitation anomalies --------------------------------
for (t in 1:13){
  for (m in 1:4){
    # calculate the mean precipitation of each month over the whole 14 year period (excluding each time the month of interest)
    precip_seas_mean <-calc(dropLayer(stack(precip_sub_unstack[seasons==m]), which(index==t, arr.ind=TRUE)),mean, na.rm=T)
    precip_seas_sd <- calc(dropLayer(stack(precip_sub_unstack[seasons==m]), which(index==t, arr.ind=TRUE)),sd, na.rm=T) # calculate the standard deviation 
    precip_mean <- stackApply(stack(precip_sub_unstack[seasons==m]), index, mean, na.rm=T)
    
    # calculate the anomaly 
    precip_anomaly <- (precip_mean - precip_seas_mean)/precip_seas_sd
    
    if(m==1){
      precip_seas_anomalies <-precip_anomaly
      
    }else{
      precip_seas_anomalies  <- append(precip_seas_anomalies, precip_anomaly)    
      
    }
    
  }
  
}

# get the seasonal anomalies in a matrix and save as csv 
precip_seas_anomalies_vals <- get_raster_vals(stack(precip_seas_anomalies))
write.csv(precip_seas_anomalies_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_seas_anomalies.csv", quote=FALSE, row.names=FALSE)


# plot monthly anomalies -------------------------------------------------
names(precip_monthly_anomalies[[3]]) <- paste0("Y", 2003:2015)
spplot(precip_monthly_anomalies[[3]], bty='n',xaxt='n', yaxt='n', box=FALSE)
spSA = list("sp.polygons", sa)
spplot(precip_monthly_anomalies[[3]], bty='n', xaxt='n', yaxt='n', box=FALSE, sp.layout=spSA)


plot(precip_monthly_anomalies[[3]][[5]],col=brewer.pal(6, "RdBu"), xlim=c(xmin(precip_monthly_anomalies[[3]][[5]]), xmax(precip_monthly_anomalies[[3]][[5]])),ylim=c(ymin(precip_monthly_anomalies[[3]][[5]]), ymax(precip_monthly_anomalies[[3]][[5]])),
     cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
plot(sa, add=T,lty=1,lwd=0.5)
#axis(side=1,pos=ymin(precip_anomaly[[8]]),at=seq(xmin(precip_anomaly[[8]]),xmax(precip_anomaly[[8]]),10), cex.axis=1.25)
#axis(side=2,pos=xmin(precip_anomaly[[8]]),at=seq(ymin(precip_anomaly[[8]]),ymax(precip_anomaly[[8]]),10), cex.axis=1.25)
#title(xlab="Longitude", cex.lab=1.25, line=2.5)
#title(ylab="Latitude", cex.lab=1.25, line=2.5)
#color.bar(igbpcol_real,1,17)
#legend("right", inset=c(-0.4,-0.4), legend="Land Cover Class")
plot(precip_monthly_anomalies[[3]][[5]], col=brewer.pal(6, "RdBu"),legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("Precipitation Standardized Anomalies", side=4, line=2.9, cex=1.25))

# plot seasonal anomalies -------------------------------------------------
names(precip_seas_anomalies[[3]]) <- paste0("Y", 2003:2015)
spplot(precip_seas_anomalies[[3]], bty='n',xaxt='n', yaxt='n', box=FALSE)
spSA = list("sp.polygons", sa)
spplot(precip_seas_anomalies[[3]], bty='n', xaxt='n', yaxt='n', box=FALSE, sp.layout=spSA)


plot(precip_seas_anomalies[[3]][[5]],col=brewer.pal(6, "RdBu"), xlim=c(xmin(precip_seas_anomalies[[3]][[5]]), xmax(precip_seas_anomalies[[3]][[5]])),ylim=c(ymin(precip_seas_anomalies[[3]][[5]]), ymax(precip_seas_anomalies[[3]][[5]])),
     cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
plot(sa, add=T,lty=1,lwd=0.5)
plot(precip_seas_anomalies[[3]][[5]], col=brewer.pal(6, "RdBu"),legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("Precipitation Standardized Anomalies", side=4, line=2.9, cex=1.25))


# Extras --------------------------------------
# mean_all_precip <- mean(precip_mean_all_years, na.rm=T)*24*365
# cutpts <- c(0,500, 1000, 1500, 2000, 2500, 3000, 4000, 5000,7000)
# 
# plot(mean_all_precip,col=rev(brewer.pal(9, "RdBu")),  breaks=cutpts, xlim=c(xmin(mean_all_precip), xmax(mean_all_precip)),ylim=c(ymin(mean_all_precip), ymax(mean_all_precip)),
#      cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
# plot(sa, add=T,lty=1,lwd=0.5)
# plot(mean_all_precip, col=rev(brewer.pal(9, "RdBu")),breaks=cutpts,legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("Precipitation Mean (mm/year)", side=4, line=3.5, cex=1.25))
# 

# plot(precip_year_sd[[8]],col=brewer.pal(8, "OrRd"),  xlim=c(xmin(precip_year_sd[[8]]), xmax(precip_year_sd[[8]])),ylim=c(ymin(precip_year_sd[[8]]), ymax(precip_year_sd[[8]])),
#      cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
# plot(sa, add=T,lty=1,lwd=0.5)
# plot(precip_year_sd[[8]], col=brewer.pal(8, "OrRd"),legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("Precipitation Standard Deviation", side=4, line=2.9, cex=1.25))

# cutpts <- c(-5, -2, -1, 0,1, 2,11)
# at_cb = seq(0.5,15.5,1)
# class_vals = c("< -2", "-2", "-1", "0", "1", "2", ">2")
# red <- c(178,24,43)
# orange <- c(239,138,98)
# peach <- c(253,219,199)
# baby_blue <- c(209,229,240)
# blue <- c(103,169,207)
# dark_blue <- c(33,102,172)
# plotcol=rgb(red, orange, peach, baby_blue, blue, dark_blue, names=NULL, maxColorValue=255)
