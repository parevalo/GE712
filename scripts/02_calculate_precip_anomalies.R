# This is a script to get the dry season EVI at 0.05-degree using MOD13C2
# created by Jian Bi and adapted in R by Radost Stanimirova 

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
data(wrld_simpl)
SA <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", 
        "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela", "Costa Rica", "Nicaragua", "Panama")
NorthAm <- c("Canada", "Mexico", "United States")

sa <- wrld_simpl[which(wrld_simpl@data$NAME %in% SA),]
na <- wrld_simpl[which(wrld_simpl@data$NAME %in% NorthAm),]

# set up to run in parallel 
library(parallel)
cl <- makeCluster(8)
clusterEvalQ(cl, {library(raster)})

pasture_2000_sub <- raster("/projectnb/modislc/users/rkstan/GE712/data/pasture_extent/pasture_2000_sub.tif")
pasture_2000_sub[pasture_2000_sub<0.6] <- NA

path_TRMM <- "/projectnb/modislc/users/rkstan/GE712/data/TRMM/" # path to your MOD13C2 files

TRMM_tile_files <- Sys.glob(file.path(path_TRMM, paste("3B43", "*", "7_clip",'tif', sep=".")))
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

precip_sub <- mask(precip_stack, pasture_2000_sub)

precip <- values(precip_stack)

# GetError <- function(TRMM_file_path, as.int=T){
#   #fun <- function(x) {x[x==32767] <- NA; return(x)}
#   
#   SDS_error <- paste("HDF4_SDS:UNKNOWN:\"", TRMM_file_path, "\":1", sep = "")
#   error <- raster(SDS_error) # read in the red SDS as a raster
#   return(error)
#   
# }
# 
# error_list <- parLapply(cl, TRMM_tile_files, GetError)
# error_stack <- stack(unlist(error_list)) # unlist and convert to a single RasterStack
# error <- values(q_stack)


year <- rep(1:10, each=3)
year_sub <- rep(1:9, each=3)
  

precip_mean_all_years <- stackApply(precip_sub, year, mean, na.rm=T)
precip_year_sd <- stackApply(precip_sub, year, sd, na.rm=T)
precip_year_cv <- stackApply(precip_sub, year, cv, na.rm=T)


for (i in 1:10){
  out_stack_mean <- calc(dropLayer(precip_mean_all_years, i), mean)
  out_stack_sd <- calc(dropLayer(precip_mean_all_years, i), sd)
  out_stack_year_mean <- precip_mean_all_years[[i]]
  
  if(i==1){
    precip_dry_season_mean <-out_stack_mean 
    #assign(paste("stack", GetYear(files[i]), sep="_"), out_stack)
    precip_dry_season_sd <-out_stack_sd
    precip_year_mean <-out_stack_year_mean
    
  }else{
    precip_dry_season_mean  <- append(precip_dry_season_mean, out_stack_mean)
    precip_dry_season_sd  <- append(precip_dry_season_sd, out_stack_sd)
    precip_year_mean  <- append(precip_year_mean, out_stack_year_mean)
    
  }
}


precip_dry_season_mean <- stack(precip_dry_season_mean)
precip_dry_season_sd <- stack(precip_dry_season_sd)
precip_year_mean <- stack(precip_year_mean)

precip_anomaly <- (precip_year_mean - precip_dry_season_mean)/precip_dry_season_sd
names(precip_anomaly) <- paste0("Y", 2002:2011)
spplot(precip_anomaly, bty='n',xaxt='n', yaxt='n', box=FALSE)
spSA = list("sp.polygons", sa)
spplot(precip_anomaly, bty='n', xaxt='n', yaxt='n', box=FALSE, sp.layout=spSA)

precip_dry_season_anomaly <- get_raster_vals(precip_anomaly)
write.csv(precip_dry_season_anomaly, file="/projectnb/modislc/users/rkstan/GE529/GE529_Jian_Lab1_Codes/precip_dry_season_anomaly_SA.csv", quote=FALSE, row.names=FALSE)

precip_dry_season_mean_2005 <- calc(dropLayer(precip_mean_all_years, 10), mean, na.rm=F)
precip_dry_season_sd_2005 <- calc(dropLayer(precip_mean_all_years, 10), sd, na.rm=F)
precip_2005_mean <- precip_mean_all_years[[10]]
precip_2005_anomaly <- (precip_2005_mean - precip_dry_season_mean_2005)/precip_dry_season_sd_2005

# plot -------------------------------------------------

cutpts <- c(-5, -2, -1, 0,1, 2,11)
at_cb = seq(0.5,15.5,1)
class_vals = c("< -2", "-2", "-1", "0", "1", "2", ">2")
red <- c(178,24,43)
orange <- c(239,138,98)
peach <- c(253,219,199)
baby_blue <- c(209,229,240)
blue <- c(103,169,207)
dark_blue <- c(33,102,172)
plotcol=rgb(red, orange, peach, baby_blue, blue, dark_blue, names=NULL, maxColorValue=255)

plot(precip_anomaly[[9]],col=brewer.pal(4, "RdBu"), xlim=c(xmin(precip_anomaly[[9]]), xmax(precip_anomaly[[9]])),ylim=c(ymin(precip_anomaly[[9]]), ymax(precip_anomaly[[9]])),
     cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
plot(sa, add=T,lty=1,lwd=0.5)
#axis(side=1,pos=ymin(precip_anomaly[[8]]),at=seq(xmin(precip_anomaly[[8]]),xmax(precip_anomaly[[8]]),10), cex.axis=1.25)
#axis(side=2,pos=xmin(precip_anomaly[[8]]),at=seq(ymin(precip_anomaly[[8]]),ymax(precip_anomaly[[8]]),10), cex.axis=1.25)
#title(xlab="Longitude", cex.lab=1.25, line=2.5)
#title(ylab="Latitude", cex.lab=1.25, line=2.5)
#color.bar(igbpcol_real,1,17)
#legend("right", inset=c(-0.4,-0.4), legend="Land Cover Class")
plot(precip_anomaly[[9]], col=brewer.pal(4, "RdBu"),legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("Precipitation Standardized Anomalies", side=4, line=2.9, cex=1.25))


plot(precip_anomaly[[8]],col=brewer.pal(6, "RdBu"),  xlim=c(xmin(precip_anomaly[[9]]), xmax(precip_anomaly[[9]])),ylim=c(ymin(precip_anomaly[[9]]), ymax(precip_anomaly[[9]])),
     cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
plot(sa, add=T,lty=1,lwd=0.5)
plot(precip_anomaly[[8]], col=brewer.pal(6, "RdBu"),legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("Precipitation Anomaly", side=4, line=2.9, cex=1.25))


mean_all_precip <- mean(precip_mean_all_years, na.rm=T)*24*365
cutpts <- c(0,500, 1000, 1500, 2000, 2500, 3000, 4000, 5000,7000)

plot(mean_all_precip,col=rev(brewer.pal(9, "RdBu")),  breaks=cutpts, xlim=c(xmin(mean_all_precip), xmax(mean_all_precip)),ylim=c(ymin(mean_all_precip), ymax(mean_all_precip)),
     cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
plot(sa, add=T,lty=1,lwd=0.5)
plot(mean_all_precip, col=rev(brewer.pal(9, "RdBu")),breaks=cutpts,legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("Precipitation Mean (mm/year)", side=4, line=3.5, cex=1.25))

cv_all_precip <- cv(precip_mean_all_years, na.rm=T)
plot(cv_all_precip,col=brewer.pal(8, "OrRd"),  xlim=c(xmin(cv_all_precip), xmax(cv_all_precip)),ylim=c(ymin(cv_all_precip), ymax(cv_all_precip)),
     cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
plot(sa, add=T,lty=1,lwd=0.5)
plot(cv_all_precip, col=brewer.pal(8, "OrRd"),legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("Precipitation Coefficient of Variation", side=4, line=2.9, cex=1.25))

# plot(precip_year_sd[[8]],col=brewer.pal(8, "OrRd"),  xlim=c(xmin(precip_year_sd[[8]]), xmax(precip_year_sd[[8]])),ylim=c(ymin(precip_year_sd[[8]]), ymax(precip_year_sd[[8]])),
#      cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
# plot(sa, add=T,lty=1,lwd=0.5)
# plot(precip_year_sd[[8]], col=brewer.pal(8, "OrRd"),legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("Precipitation Standard Deviation", side=4, line=2.9, cex=1.25))


# Extras 
# ---------

# precip_dry_season_mean_2005 <- calc(dropLayer(precip_year_mean, 4), mean)
# precip_dry_season_sd_2005 <- calc(dropLayer(precip_year_mean, 4), sd)
# 
# precip_dry_season_mean_2008 <- calc(dropLayer(precip_year_mean, 7), mean)
# precip_dry_season_sd_2008 <- calc(dropLayer(precip_year_mean, 7), sd)
# 
# precip_dry_season_mean_2010 <- calc(dropLayer(precip_year_mean, 9), mean)
# precip_dry_season_sd_2010 <- calc(dropLayer(precip_year_mean, 9), sd)
# 
# precip_2005_mean <- precip_year_mean[[4]]
# precip_2008_mean <- precip_year_mean[[7]]
# precip_2010_mean <- precip_year_mean[[9]]
# 
# precip_2005_anomaly <- (precip_2005_mean - precip_dry_season_mean_2005)/precip_dry_season_sd_2005
# precip_2008_anomaly <- (precip_2008_mean - precip_dry_season_mean_2008)/precip_dry_season_sd_2008
# precip_2010_anomaly <- (precip_2010_mean - precip_dry_season_mean_2010)/precip_dry_season_sd_2010
# 
# cutpts <- c(-3, -2, -1, 0,1, 2,3,4)

# setwd("/projectnb/modislc/users/rkstan/GE529/GE529_Jian_Lab1_Codes/")
# pdf("precip_anomalies_SA.pdf", height=9, width=9)
# par(mfrow=c(1,3),pty="s", oma=c(4,4,4,4), mar=c(1,1,1,1))
# plot(precip_2005_anomaly, col=brewer.pal(7, "RdBu"),legend=FALSE,colNA="gray",
#      #legend.width=1, legend.shrink=1,
#      #legend.args=list("0.25 degree precip. standardized anomalies", side=4, line=2.5, cex=1.25),
#      #ylim=c(-20,10), xlim=c(-80,-50)
# )
# plot(sa, add=T)
# legend("topright", legend=c("2005"), bty='n', cex=1.5)
# 
# plot(precip_2008_anomaly, col=brewer.pal(7, "RdBu"), legend=FALSE, colNA="gray",
#      #legend.width=1, legend.shrink=1,
#      #legend.args=list("0.25 degree precip. standardized anomalies", side=4, line=2.5, cex=1.25),
#      #ylim=c(-20,10), xlim=c(-80,-50)
# )
# plot(sa, add=T)
# legend("topright", legend=c("2008"), bty='n', cex=1.5)
# 
# plot(precip_2010_anomaly, col=brewer.pal(7, "RdBu"),colNA="gray",
#      legend.width=1, legend.shrink=1, 
#      #legend.args=list("0.25 degree precip. standardized anomalies", side=4, line=2.5, cex=1.25),
#      #ylim=c(-20,10), xlim=c(-80,-50)
# )
# plot(sa,add=T)
# legend("topright", legend=c("2010"), bty='n', cex=1.5)
# dev.off()
# # par(fig=c(0,1,0,1), oma=c(0, 0, 0, 0), mar=c(0,0,0,0), pty="s", new=TRUE)
# # plot(0,0, type="n", bty="n", xaxt="n", yaxt="n",legend.width=1, legend.shrink=1,
# #        legend.args=list("0.25 degree precip. standardized anomalies", side=4, line=2.5, cex=1.25))
# # 


