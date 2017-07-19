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
seasons <- rep(rep(1:4, each=3),13)
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

# read in TRMM data --------------------------
path_TRMM <- "/projectnb/modislc/users/rkstan/GE712/data/TRMM/" # path to your TRMM files
TRMM_tile_files <- Sys.glob(file.path(path_TRMM, paste("3B43", "*", "*_clip",'tif', sep=".")))
#TRMM_tile_files <- Sys.glob(file.path(path_TRMM, paste("3B43", "*", '*','HDF', sep=".")))


Getprecip <- function(TRMM_file_path, as.int=T){
  
  SDS_precip <- TRMM_file_path
  #SDS_precip <- paste("HDF4_SDS:UNKNOWN:\"", TRMM_file_path, "\":0", sep = "")
  precip <- raster(SDS_precip) # read in the red SDS as a raster
  return(precip)
  
}


# get precpitation
precip_list <- parLapply(cl, TRMM_tile_files, Getprecip)
precip_stack <- stack(unlist(precip_list)) # unlist and convert to a single RasterStack
precip_time_sub <- precip_stack[[36:192]] # subset only desired time period 2003-2016

precip_sub_unstack <- unstack(dropLayer(precip_time_sub,157)) # unstack the raster brick into raster layers
precip <- values(dropLayer(precip_time_sub,157))
colnames(precip) <- rep(c("December", "January", "February", "March", "April", 
                                                 "May", "June", "July", "August", "September", "October", "November"), 13)
write.csv(precip, file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_monthly_vals.csv", quote=FALSE, row.names=FALSE)

# calculate monthly precipitation anomalies --------------------------------
month_list <- list()
year_list <- list()

# calculate the anomalies for each month (e.g., subtract January precipitation from mean January precipitation for 14 year time span)
for (t in 1:13){
  for (m in 1:12){

    # calculate the mean precipitation of each month over the whole 14 year period (excluding each time the month of interest)
    # NOTE that all 1s are Decembers from 2002 to 2015
    #precip_month_mean <- calc(dropLayer(stack(precip_sub_unstack[m_year==m]), t),mean, na.rm=T)
    precip_month_mean <- calc(stack(precip_sub_unstack[m_year==m]),mean, na.rm=T)
    precip_month_sd <- calc(stack(precip_sub_unstack[m_year==m]),sd, na.rm=T) # calculate the standard deviation 
    precip_month_year <- stack(precip_sub_unstack[m_year==m])[[t]]
    
    # calculate the anomaly 
    precip_anomaly <- (precip_month_year - precip_month_mean)/precip_month_sd
    month_list[[m]] <- precip_anomaly
  
    } 
    year_list[[t]] <- month_list
    
  }
  

precip_monthly_anomalies <- stack(unlist(year_list, recursive = T))
# points_extract <- extract(precip_monthly_anomalies, pasture_points, sp=T)
# points_extract@data
# 
# 
# plot(precip_monthly_anomalies[[5]])
# points(points_extract)
# 
# p_coords <- points_extract@coords
# full <- as.data.frame(cbind(points_extract@data$ID, p_coords))
# colnames(full) <- c("ID","lat", "lon")
# full_new <- full[full$ID %in% f$ID,]

# get the anomalies in a matrix and save as csv 
precip_monthly_anomalies_vals <- get_raster_vals(stack(precip_monthly_anomalies))
colnames(precip_monthly_anomalies_vals) <- rep(c("December", "January", "February", "March", "April", 
                                               "May", "June", "July", "August", "September", "October", "November"), 13)
write.csv(precip_monthly_anomalies_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_monthly_anomalies.csv", quote=FALSE, row.names=FALSE)

#monthly lagged values 
precip_monthly_anomalies_lag1 <- precip_monthly_anomalies_vals[,-156]
write.csv(precip_monthly_anomalies_lag1, file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_monthly_anomalies_lag1.csv", quote=FALSE, row.names=FALSE)

precip_monthly_anomalies_lag2 <- precip_monthly_anomalies_vals[,-c(155,156)]
write.csv(precip_monthly_anomalies_lag2, file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_monthly_anomalies_lag2.csv", quote=FALSE, row.names=FALSE)

precip_monthly_anomalies_lag3 <- precip_monthly_anomalies_vals[,-c(154,155,156)]
write.csv(precip_monthly_anomalies_lag3, file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_monthly_anomalies_lag3.csv", quote=FALSE, row.names=FALSE)

precip_monthly_anomalies_lag4 <- precip_monthly_anomalies_vals[,-c(153,154,155,156)]
write.csv(precip_monthly_anomalies_lag4, file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_monthly_anomalies_lag4.csv", quote=FALSE, row.names=FALSE)

# calculate seasonal precipitation anomalies --------------------------------
seas_list <- list()
seas_all <- list()
seas_vals <- list()
seas_vals_all <- list()

for (t in 1:13){
  for (m in 1:4){
    # calculate the mean precipitation of each month over the whole 14 year period (excluding each time the month of interest)
    #precip_seas_sum <- stackApply(stack(precip_sub_unstack[seasons==m]), index, mean, na.rm=F)
    precip_seas_mean <-calc(stack(precip_sub_unstack[seasons==m]),mean, na.rm=T)
    #precip_seas_mean <-calc(precip_seas_sum,mean, na.rm=F)
    #precip_seas_mean <-calc(dropLayer(stack(precip_sub_unstack[seasons==m]), which(index==t, arr.ind=TRUE)),mean, na.rm=T)
    #precip_seas_sd <- calc(precip_seas_sum,sd, na.rm=F) # calculate the standard deviation 
    precip_seas_sd <- calc(stack(precip_sub_unstack[seasons==m]),sd, na.rm=T)
    #precip_mean <- stackApply(stack(precip_sub_unstack[seasons==m]), index, mean, na.rm=T)[[t]]'
    precip_sum <- stackApply(stack(precip_sub_unstack[seasons==m]), index, mean, na.rm=T)[[t]]
    
    
    # calculate the anomaly 
    precip_anomaly <- (precip_sum - precip_seas_mean)/precip_seas_sd
    #precip_anomaly <- (precip_mean - precip_seas_mean)/precip_seas_sd
    seas_list[[m]] <- precip_anomaly
    seas_vals[[m]] <- precip_sum
    
  }
  seas_all[[t]] <- seas_list
  seas_vals_all[[t]] <- seas_vals
}

precip_seas_anomalies <- stack(unlist(seas_all, recursive = T))
precip_seas_vals <- stack(unlist(seas_vals_all, recursive = T))
  
# points_seas <- extract(precip_seas_anomalies, pasture_points, sp=T)
# points_seas@data

# get the seasonal anomalies in a matrix and save as csv 
precip_seas_anomalies_vals <- get_raster_vals(stack(precip_seas_anomalies))
colnames(precip_seas_anomalies_vals) <- rep(c("DJF", "MAM", "JJA", "SON"), 13)
#write.csv(precip_seas_anomalies_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_seas_anomalies.csv", quote=FALSE, row.names=FALSE)
write.csv(precip_seas_anomalies_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_sum_seas_anomalies.csv", quote=FALSE, row.names=FALSE)

# get the seasonal values in a matrix and save as csv 
precip_seasonal_vals <- get_raster_vals(stack(precip_seas_vals))
colnames(precip_seasonal_vals) <- rep(c("DJF", "MAM", "JJA", "SON"), 13)
#write.csv(precip_seas_anomalies_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_seas_anomalies.csv", quote=FALSE, row.names=FALSE)
write.csv(precip_seasonal_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_sum_seas_vals.csv", quote=FALSE, row.names=FALSE)


#lagged seasonal values 
precip_seas_anomalies_lag1 <- precip_seas_anomalies_vals[,-52]
write.csv(precip_seas_anomalies_lag1, file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_seas_anomalies_lag1.csv", quote=FALSE, row.names=FALSE)

precip_seas_anomalies_lag2 <- precip_seas_anomalies_vals[,-c(51,52)]
write.csv(precip_seas_anomalies_lag2, file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_seas_anomalies_lag2.csv", quote=FALSE, row.names=FALSE)


# calculate yearly anomalies -------------------------------------------------
year_anom <- list()
precip_sub_year <- dropLayer(precip_time_sub, 1)
precip_year <- stackApply(precip_sub_year, year, fun=mean, na.rm=T)

for (t in 1:13){
  precip_year_mean <-calc(precip_sub_year,mean, na.rm=T)
  precip_year_sd <-calc(precip_sub_year,sd, na.rm=T)
  precip_year_t <- precip_year[[t]]
  
  # calculate the anomaly 
  precip_anomaly <- (precip_year_t - precip_year_mean)/precip_year_sd
  year_anom[[t]] <- precip_anomaly
  
}

precip_year_anomalies <- stack(unlist(year_anom, recursive = T))

# get the year anomalies in a matrix and save as csv 
precip_year_anomalies_vals <- get_raster_vals(stack(precip_year_anomalies))
colnames(precip_year_anomalies_vals) <- c("2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012",
                                        "2013", "2014", "2015")
write.csv(precip_year_anomalies_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_year_anomalies.csv", quote=FALSE, row.names=FALSE)


# plot monthly anomalies -------------------------------------------------
names(precip_monthly_anomalies[[3]]) <- paste0("Y", 2003:2015)
spplot(precip_monthly_anomalies[[3]], bty='n',xaxt='n', yaxt='n', box=FALSE)
spSA = list("sp.polygons", sa)
spplot(precip_monthly_anomalies[[3]], bty='n', xaxt='n', yaxt='n', box=FALSE, sp.layout=spSA)


plot(precip_monthly_anomalies[[2]],col=brewer.pal(6, "RdBu"), xlim=c(xmin(precip_monthly_anomalies[[2]]), xmax(precip_monthly_anomalies[[2]])),ylim=c(ymin(precip_monthly_anomalies[[2]]), ymax(precip_monthly_anomalies[[2]])),
     cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
plot(sa, add=T,lty=1,lwd=0.5)
#axis(side=1,pos=ymin(precip_anomaly[[8]]),at=seq(xmin(precip_anomaly[[8]]),xmax(precip_anomaly[[8]]),10), cex.axis=1.25)
#axis(side=2,pos=xmin(precip_anomaly[[8]]),at=seq(ymin(precip_anomaly[[8]]),ymax(precip_anomaly[[8]]),10), cex.axis=1.25)
#title(xlab="Longitude", cex.lab=1.25, line=2.5)
#title(ylab="Latitude", cex.lab=1.25, line=2.5)
#color.bar(igbpcol_real,1,17)
#legend("right", inset=c(-0.4,-0.4), legend="Land Cover Class")
plot(precip_monthly_anomalies[[2]], col=brewer.pal(6, "RdBu"),legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("Precipitation Standardized Anomalies", side=4, line=2.9, cex=1.25))

# plot seasonal anomalies -------------------------------------------------
names(precip_seas_anomalies[[3]]) <- paste0("Y", 2003:2015)
spplot(precip_seas_anomalies[[3]], bty='n',xaxt='n', yaxt='n', box=FALSE)
spSA = list("sp.polygons", sa)
spplot(precip_seas_anomalies[[3]], bty='n', xaxt='n', yaxt='n', box=FALSE, sp.layout=spSA)

setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")
dev.copy(png,"seas_anomalies_precip.png", width=800,height=720)

plot(precip_seas_anomalies[[2]],col=brewer.pal(6, "RdBu"), xlim=c(xmin(precip_seas_anomalies[[2]]), xmax(precip_seas_anomalies[[2]])),ylim=c(ymin(precip_seas_anomalies[[2]]), ymax(precip_seas_anomalies[[2]])),
     cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
plot(sa, add=T,lty=1,lwd=0.5)
plot(precip_seas_anomalies[[2]], col=brewer.pal(6, "RdBu"),legend.only=TRUE, legend.width=1, legend.shrink=0.85,legend.args=list("Precipitation Standardized Anomalies", side=4, line=2.5, cex=1.5))

dev.off()

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
