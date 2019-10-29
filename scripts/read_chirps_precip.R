# This is a script to read in precipitation data from CHIRPS at 0.05-degree
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

#set command arguments for qsub 
library(argparse)
arg_parser <- ArgumentParser()
arg_parser$add_argument("-type", type="character", default="values") # set up the MODIS tile 
#zscore or data
args <- arg_parser$parse_args()


# set up to run in parallel ----------------------
library(parallel)
cl <- makeCluster(8)
clusterEvalQ(cl, {library(raster)})

# read in pastureland extent data --------------------------
pasture_2000_sub <- raster("/projectnb/modislc/users/rkstan/GE712/data/pasture_extent/pasture2000_GT_06_resample_005.tif")

# read in TRMM data --------------------------
path_CHIRPS <- sprintf("/projectnb/modislc/users/rkstan/GE712/data/CHIRPS/%s/3monthly", args$type) # path to your TRMM files
CHIRPS_tile_files <- Sys.glob(file.path(path_CHIRPS, paste("*_clip",'tif', sep=".")))

Getprecip <- function(CHIRPS_file_path, as.int=T){
  
  SDS_precip <- CHIRPS_file_path
  #SDS_precip <- paste("HDF4_SDS:UNKNOWN:\"", TRMM_file_path, "\":0", sep = "")
  precip <- raster(SDS_precip) # read in the red SDS as a raster
  return(precip)
  
}


# get precpitation
precip_list <- parLapply(cl, CHIRPS_tile_files, Getprecip)
precip_stack <- stack(unlist(precip_list)) # unlist and convert to a single RasterStack

#precip_sa_sub <- mask(precip_stack, sa) # limit to extent of South America shape file
#precip_sub <- mask(precip_stack, pasture_2000_sub,maskvalue=NA,  updatevalue=NA) # limit to extent of rangeland extent file
#precip_sub_unstack <- unstack(dropLayer(precip_stack,157)) # unstack the raster brick into raster layers

precip <- getValues(precip_stack)
colnames(precip) <- rep(c("DJF", "MAM", "JJA", "SON"), 13)
# colnames(precip) <- rep(c("December", "January", "February", "March", "April", 
#                           "May", "June", "July", "August", "September", "October", "November"), 13)
setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")
write.csv(precip, file="/projectnb/modislc/users/rkstan/GE712/outputs/CHIRPSprecip_seas_vals_005.csv", quote=FALSE, row.names=FALSE)


plot_precip <- function(x){
  plot(precip_stack, x,col=brewer.pal(5, "RdBu"), xlim=c(xmin(EVI_seas_vals), xmax(EVI_seas_vals)),ylim=c(ymin(EVI_seas_vals), ymax(EVI_seas_vals)),
       cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F, main="") 
  plot(sa, add=T,lty=1,lwd=0.5)
  #axis(side=1,pos=ymin(precip_anomaly[[8]]),at=seq(xmin(precip_anomaly[[8]]),xmax(precip_anomaly[[8]]),10), cex.axis=1.25)
  #axis(side=2,pos=xmin(precip_anomaly[[8]]),at=seq(ymin(precip_anomaly[[8]]),ymax(precip_anomaly[[8]]),10), cex.axis=1.25)
  #title(xlab="Longitude", cex.lab=1.25, line=2.5)
  #title(ylab="Latitude", cex.lab=1.25, line=2.5)
  #color.bar(igbpcol_real,1,17)
  #legend("right", inset=c(-0.4,-0.4), legend="Land Cover Class")
  plot(precip_stack, x,col=brewer.pal(5, "RdBu"),legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("EVI", side=4, line=2.9, cex=1.25))
  
}

par(mfrow=c(2,2), pty="s", oma=c(0.5,0.5,0.5,0.5), mar=c(4,4,4,4))
plot_precip(1)
plot_precip(2)
plot_precip(3)
plot_precip(4)


# calculate yearly anomalies -------------------------------------------------
path_CHIRPS <- "/projectnb/modislc/users/rkstan/GE712/data/CHIRPS/values/annual/" # path to your TRMM files
CHIRPS_tile_files <- Sys.glob(file.path(path_CHIRPS, paste("*_clip",'tif', sep=".")))

Getprecip <- function(CHIRPS_file_path, as.int=T){
  
  SDS_precip <- CHIRPS_file_path
  #SDS_precip <- paste("HDF4_SDS:UNKNOWN:\"", TRMM_file_path, "\":0", sep = "")
  precip <- raster(SDS_precip) # read in the red SDS as a raster
  return(precip)
  
}


# get precpitation
precip_list <- parLapply(cl, CHIRPS_tile_files, Getprecip)
precip_stack <- stack(unlist(precip_list)) # unlist and convert to a single RasterStack

precip <- getValues(precip_stack)
colnames(precip) <- c("2003", "2004", "2005", "2006", "2007", "2008", "2009", 
                      "2010", "2011", "2012", "2013", "2014", "2015", "2016")
setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")
write.csv(precip, file="/projectnb/modislc/users/rkstan/GE712/outputs/CHIRPSprecip_annual_vals_005.csv", quote=FALSE, row.names=FALSE)

###############################################################
# calculate from monthly data 

path_CHIRPS <- "/projectnb/modislc/users/rkstan/GE712/data/CHIRPS/values/" # path to your TRMM files
CHIRPS_tile_files <- Sys.glob(file.path(path_CHIRPS, paste("*_clip",'tif', sep=".")))

Getprecip <- function(CHIRPS_file_path, as.int=T){
  
  SDS_precip <- CHIRPS_file_path
  #SDS_precip <- paste("HDF4_SDS:UNKNOWN:\"", TRMM_file_path, "\":0", sep = "")
  precip <- raster(SDS_precip) # read in the red SDS as a raster
  return(precip)
  
}


# get precpitation
precip_list <- parLapply(cl, CHIRPS_tile_files, Getprecip)
precip_stack <- stack(unlist(precip_list)) # unlist and convert to a single RasterStack
precip_sub_unstack <- unstack(dropLayer(precip_stack,157)) # unstack the raster brick into raster layers

# calculate seasonal precipitation anomalies --------------------------------
seas_list <- list()
seas_all <- list()
seas_vals <- list()
seas_vals_all <- list()

for (t in 1:13){
  for (m in 1:4){
    # calculate the mean precipitation of each month over the whole 14 year period (excluding each time the month of interest)
    precip_seas_sum <- stackApply(stack(precip_sub_unstack[seasons==m]), index, sum, na.rm=F)
    #precip_seas_mean <-calc(stack(precip_sub_unstack[seasons==m]),mean, na.rm=T)
    precip_seas_mean <-calc(precip_seas_sum,mean, na.rm=F)
    #precip_seas_mean <-calc(dropLayer(stack(precip_sub_unstack[seasons==m]), which(index==t, arr.ind=TRUE)),mean, na.rm=T)
    precip_seas_sd <- calc(precip_seas_sum,sd, na.rm=F) # calculate the standard deviation 
    #precip_seas_sd <- calc(stack(precip_sub_unstack[seasons==m]),sd, na.rm=T)
    #precip_mean <- stackApply(stack(precip_sub_unstack[seasons==m]), index, mean, na.rm=T)[[t]]'
    precip_sum <- stackApply(stack(precip_sub_unstack[seasons==m]), index, sum, na.rm=F)[[t]]
    
    
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

# get the seasonal anomalies in a matrix and save as csv 
precip_seas_anomalies_vals <- getValues(stack(precip_seas_anomalies))
colnames(precip_seas_anomalies_vals) <- rep(c("DJF", "MAM", "JJA", "SON"), 13)
#write.csv(precip_seas_anomalies_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_seas_anomalies.csv", quote=FALSE, row.names=FALSE)
write.csv(precip_seas_anomalies_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/CHIRPSprecip_sum_seas_anomalies.csv", quote=FALSE, row.names=FALSE)

# get the seasonal values in a matrix and save as csv 
precip_seasonal_vals <- getValues(stack(precip_seas_vals))
colnames(precip_seasonal_vals) <- rep(c("DJF", "MAM", "JJA", "SON"), 13)
#write.csv(precip_seas_anomalies_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_seas_anomalies.csv", quote=FALSE, row.names=FALSE)
write.csv(precip_seasonal_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/CHIRPSprecip_sum_seas_vals.csv", quote=FALSE, row.names=FALSE)

