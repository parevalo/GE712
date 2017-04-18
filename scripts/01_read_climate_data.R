###############################################################
# Read in GPCP (daily precipitation data) and CRU daily 
# temperature max and min 

# for reference on how to read netCDF files: http://geog.uoregon.edu/bartlein/courses/geog607/Rmd/netCDF_01.htm

# script last edited by Radost Stanimirova Summer 2016 
###############################################################

# set working directory ----------------

setwd("/projectnb/modislc/users/rkstan/pasturelands/cru.ts3.21/")


# init ----------------------

# set command arguments for qsub 
library(argparse)
arg_parser <- ArgumentParser()
arg_parser$add_argument("-reg", type="character", default="SA") # set up the MODIS tile 
arg_parser$add_argument("-num", type="integer", default=1) # set up the MODIS tile 
arg_parser$add_argument("-vname", type="character", default="tmp")
args <- arg_parser$parse_args()


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

# set variables ----------------------------
m <- rep(1:12, 15)
yr <- rep(1:15, each=12)

data(wrld_simpl)
SA <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", 
        "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")
NorthAm <- c("Canada", "Mexico", "United States")
  
sa <- wrld_simpl[which(wrld_simpl@data$NAME %in% SA),]
na <- wrld_simpl[which(wrld_simpl@data$NAME %in% NorthAm),]
reg_list <- list(na, sa)


# read in data ----------------------------

# read in pasture data 
########################

pasture_2000_sub <- raster("/projectnb/modislc/users/rkstan/pasturelands/pasture.data/pasture_2000_sub_NA_res.tif")
precip_mask <- list(raster("/projectnb/modislc/users/rkstan/pasturelands/cru.ts3.21/precip_mask_na.rst"),
                    raster("/projectnb/modislc/users/rkstan/pasturelands/cru.ts3.21/precip_mask_sa.rst"))


# read in netcdf data for CRU 
##############################

ncname <- sprintf("cru_ts3.23.1901.2014.%s.dat", args$vname)
ncfname <- paste(ncname, ".nc", sep="")

# open a NetCDF file 
ncin <- nc_open(ncfname)
print(ncin) # to see the metadata

doy <- ncvar_get(ncin, "time")
nt <- dim(doy)
tunits <- ncatt_get(ncin, "time", "units")
lon <-  ncvar_get(ncin, "lon")
nlon <- dim(lon)
lat <-  ncvar_get(ncin, "lat")
nlat <- dim(lat)


tmp.array <- ncvar_get(ncin, args$vname) 
dlname <- ncatt_get(ncin, args$vname, "long_name")
dunits <- ncatt_get(ncin, args$vname, "units")
fillvalue <- ncatt_get(ncin, args$vname, "_FillValue")
dim(tmp.array)

ttt =list(
  x=ncvar_get(ncin, "lon"),
  y=ncvar_get(ncin, "lat"),
  z=ncvar_get(ncin, args$vname) 
)

nc_close(ncin)

# get netcdf file into raster format ---------------------

# change to x and y coordinate system 
for (i in 1:1368){

xp <- data.frame(x=ttt$x, y=0)
coordinates(xp)=~x+y
proj4string(xp)=CRS("+proj=longlat +datum=WGS84")
#proj4string(xp)=CRS("+proj=longlat +ellps=sphere")
xp <- spTransform(xp, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#xp <- spTransform(xp, CRS=CRS("+proj=longlat +no_defs +ellps=sphere"))

yp <- data.frame(x=0, y=ttt$y)
coordinates(yp)=~x+y
proj4string(yp)=CRS("+proj=longlat +datum=WGS84")
#roj4string(yp)=CRS("+proj=longlat +ellps=sphere")
yp <- spTransform(yp, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#yp <- spTransform(yp, CRS=CRS("+proj=longlat +no_defs +ellps=sphere"))


ttt$xp <- coordinates(xp)[,1] 
ttt$yp <- coordinates(yp)[,2]


tmp <- raster(list(x=ttt$xp, y=ttt$yp, z=ttt$z[,,i]), crs=proj4string(reg_list[[args$num]]))
tmp_sub <-mask(tmp, pasture_2000_sub)
tmp_r <- mask(tmp_sub, pasture_2000_sub, maskvalue=0,  updatevalue=NA)
r2 <- crop(tmp_r, extent(reg_list[[args$num]]))
r3 <- mask(r2, reg_list[[args$num]])

if(i==1){
  CRU_stack <-r3
  
  
}else{
  CRU_stack  <- append(CRU_stack, r3)

}

}


#stack NPP rasters 
CRU <- stack(CRU_stack)

CRU_s <- CRU[[1189:1368]]
CRU_sub <- mask(CRU_s, precip_mask[[args$num]],maskvalue=0,  updatevalue=NA)
CRU_stack_sub <- unstack(CRU_sub)

a <- crop(CRU_sub, extent(-100,-80, 40,50))
a_not <- unstack(a)

cru_months <- get_raster_vals(CRU_sub)
write.csv(cru_months, file=sprintf("/projectnb/modislc/users/rkstan/pasturelands/results/cru_months_%s.csv", args$reg), quote=FALSE, row.names=FALSE)

cru_av <- cellStats(CRU_sub, stat='mean', na.rm=T)
write.csv(cru_av, file=sprintf("/projectnb/modislc/users/rkstan/pasturelands/results/cru_spave_%s.csv", args$reg), quote=FALSE, row.names=FALSE)


# #calculate average by year
# ##############################
# 
# # average climate variable per month (mm/month for each year and over the whole time period)
# cru_ave_yr <- stackApply(CRU_sub, yr, fun=mean, na.rm=T)
# cru_sd_yr <- stackApply(CRU_sub, yr, fun=sd, na.rm=T)
# cru_cv <- cru_sd_yr/cru_ave_yr
# 
# cru_ave_yr_vals <- get_raster_vals(cru_ave_yr)
# write.csv(cru_ave_yr_vals, file=sprintf("/projectnb/modislc/users/rkstan/pasturelands/results/cru_ave_yr_vals_%s_%s.csv",args$vname, args$reg), quote=FALSE, row.names=FALSE)
# 
# cru_ave_all <- calc(cru_ave_yr, mean, na.rm=T)
# cru_ave_all_vals <- get_raster_vals(cru_ave_all)
# write.csv(cru_ave_all_vals, file=sprintf("/projectnb/modislc/users/rkstan/pasturelands/results/cru_mean_00_14_%s_%s.csv",args$vname, args$reg), quote=FALSE, row.names=FALSE)
# 
# cru_sd_all <- calc(cru_ave_yr, sd, na.rm=T)
# 
# # average climate variable per year (mm/yr) -- sum over each year 
# cru_sum_yr <- stackApply(CRU_sub, yr, fun=sum, na.rm=F)
# 
# # calculate the overall mean and standard deviation for the whole 12 year period
# cru_ave_pyear <- calc(cru_sum_yr, mean, na.rm=T)
# cru_sd_pyear <- calc(cru_sum_yr, sd, na.rm=T)
# 
# cru_sum_all_vals <-  get_raster_vals(cru_ave_pyear)
# write.csv(cru_sum_all_vals, file=sprintf("/projectnb/modislc/users/rkstan/pasturelands/results/cru_sum_mean_00_14_%s_%s.csv",args$vname, args$reg), quote=FALSE, row.names=FALSE)
# 
# # calculate the yearly z score - 12 values for the 15 years in the record
# cru_yr_z_score <- (cru_ave_yr-cru_ave_all)/cru_sd_all
# cru_yr_z_score_values <- get_raster_vals(cru_yr_z_score)
# 
# # write out the yearly z score values to a csv 
# write.csv(cru_yr_z_score_values, file=sprintf("/projectnb/modislc/users/rkstan/pasturelands/results/cru_yr_z_%s_%s.csv",args$vname, args$reg), quote=FALSE, row.names=FALSE)
# 
# spNA = list("sp.polygons", na)
# spSA = list("sp.polygons", sa)
# 
# names(cru_yr_z_score) <- paste0("Y", 2000:2014)
# spplot(cru_yr_z_score, bty='n', xlim=c(-140,-60),xaxt='n', yaxt='n', box=FALSE, sp.layout=spNA)
# spplot(cru_yr_z_score, bty='n', xaxt='n', yaxt='n', box=FALSE, sp.layout=spSA)


# calculate z score by month 
############################

cru_ave_month <- stackApply(CRU_sub, m, fun=mean, na.rm=T)
cru_sd_month <- stackApply(CRU_sub, m, fun=sd, na.rm=T)
cru_cv_month <- cru_sd_month/cru_ave_month 


if (args$reg=="NA"){
  cru_ave_month_sub <- subset(cru_ave_month,5:9)
}else{
  cru_ave_month_sub <- subset(cru_ave_month, c(1,2,3,11,12))
}


cru_ave_month_vals <- get_raster_vals(cru_ave_month_sub)
write.csv(cru_ave_month_vals, file=sprintf("/projectnb/modislc/users/rkstan/pasturelands/results/cru_ave_month_vals_%s_%s.csv",args$vname, args$reg), quote=FALSE, row.names=FALSE)





for (p in unique(m)){
  m_temp <- (stack(CRU_stack_sub[m==p])-cru_ave_month[[p]])/cru_sd_month[[p]]
  #m_temp <- stack(CRU_stack_sub[m==p])-cru_ave_month[[p]]
  
  if(p==1){
    cru_month_z_score <- m_temp
    #assign(paste("stack", GetYear(files[i]), sep="_"), out_stack)
    
    
  }else{
    cru_month_z_score  <- append(cru_month_z_score, m_temp)
    #assign(paste("stack", GetYear(files[i+1]), sep="_"), final_stack)
  }
}

# calculate the yearly z score - 12 values for the 15 years in the record
c <- stack(cru_month_z_score)
cru_month_z_score_values <- get_raster_vals(c)

cell_ave_cru <- cellStats(c, stat='mean', na.rm=T)
write.csv(cell_ave_cru, file=sprintf("/projectnb/modislc/users/rkstan/pasturelands/results/cru_cell_ave_%s_%s.csv",args$vname, args$reg), quote=FALSE, row.names=FALSE)

# write out the yearly z score values to a csv 
write.csv(cru_month_z_score_values, file=sprintf("/projectnb/modislc/users/rkstan/pasturelands/results/cru_month_z_%s_%s.csv",args$vname, args$reg), quote=FALSE, row.names=FALSE)


cru_ave_z <- stackApply(c, m, fun=mean, na.rm=T)

if (args$reg=="NA"){
  cru_ave_z_sub <- subset(cru_ave_z,5:9)
}else{
  cru_ave_z_sub <- subset(cru_ave_z, c(1,2,3,11,12))
}

cru_ave_z_vals <- get_raster_vals(cru_ave_z_sub)
write.csv(cru_ave_z_vals, file=sprintf("/projectnb/modislc/users/rkstan/pasturelands/results/cru_ave_z_vals_%s_%s.csv",args$vname, args$reg), quote=FALSE, row.names=FALSE)


# calculate average by season 
###############################

m_year <- rep(1:12, 15)
djf <- stack(a_not[m_year==1 | m_year==2 | m_year==12])
mam <- stack(a_not[m_year==3 | m_year==4 | m_year==5])
jja <- stack(a_not[m_year==6 | m_year==7 | m_year==8])
son <- stack(a_not[m_year==9 | m_year==10 | m_year==11])

get_seas <- function(seas1, seas2, seas3, x){
  
  
  for (k in unique(yr)){
    temp <- CRU_stack_sub[yr==k]
    m_year <- 1:12
    
    temp_seas <- stack(temp[m_year==seas1 | m_year==seas2 | m_year==seas3])
    #m_sub_seas <- m[m==seas1 | m==seas2 | m==seas3]
    #cru_seas <- stackApply(temp_seas, m_sub_seas, fun=x, na.rm=T)
    cru_seas <- calc(temp_seas, fun=x, na.rm=T)
    
    if(k==1){
      cru_seas_stack <-cru_seas
      #assign(paste("stack", GetYear(files[i]), sep="_"), out_stack)
      
      
    }else{
      cru_seas_stack  <- append(cru_seas_stack, cru_seas)
      #assign(paste("stack", GetYear(files[i+1]), sep="_"), final_stack)
    }
    
  }
  return(cru_seas_stack)
}

cru_ave_djf <- stack(get_seas(1, 2, 12, mean))
cru_sd_djf <- stack(get_seas(1, 2, 12, sd))
cru_cv_djf <- cru_sd_djf/cru_ave_djf 
cru_ave_mam <- stack(get_seas(3, 4, 5, mean))
cru_sd_mam <- stack(get_seas(3, 4, 5, sd))
cru_cv_mam <- cru_sd_mam/cru_ave_mam
cru_ave_jja <- stack(get_seas(6, 7, 8, mean))
cru_sd_jja <- stack(get_seas(6, 7, 8, sd))
cru_cv_jja <- cru_sd_jja/cru_ave_jja 
cru_ave_son <- stack(get_seas(9, 10, 11, mean))
cru_sd_son <- stack(get_seas(9, 10, 11, sd))
cru_cv_son <- cru_sd_son/cru_ave_son 


# # calculating average of variables taken over specially-selected time periods witha common characteristic (selective climatology)
# # Dec, Jan, and Feb
# temp_seas_djf <- stack(CRU_stack_sub[m==1 | m==2 | m==12])
# cru_full_mean_djf <- calc(temp_seas_djf, fun=mean, na.rm=T)
# cru_full_sd_djf <- calc(temp_seas_djf, fun=sd, na.rm=T)
# 
# # March-May 
# temp_seas_mam <- stack(CRU_stack_sub[m==3 | m==4 | m==5])
# cru_full_mean_mam <- calc(temp_seas_mam, fun=mean, na.rm=T)
# cru_full_sd_mam <- calc(temp_seas_mam, fun=sd, na.rm=T)
# 
# # June-August
# temp_seas_jja <- stack(CRU_stack_sub[m==6 | m==7 | m==8])
# cru_full_mean_jja <- calc(temp_seas_jja, fun=mean, na.rm=T)
# cru_full_sd_jja <- calc(temp_seas_jja, fun=sd, na.rm=T)
# 
# # Sept-Nov
# temp_seas_son <- stack(CRU_stack_sub[m==9 | m==10 | m==11])
# cru_full_mean_son <- calc(temp_seas_son, fun=mean, na.rm=T)
# cru_full_sd_son <- calc(temp_seas_son, fun=sd, na.rm=T)
# 
# 
# #calculate 3-month seasonal anomalies 
# #############################################
# 
# z_seas_djf <- (cru_ave_djf-cru_full_mean_djf)/cru_full_sd_djf
# z_seas_mam <- (cru_ave_mam-cru_full_mean_mam)/cru_full_sd_mam
# z_seas_jja <- (cru_ave_jja-cru_full_mean_jja)/cru_full_sd_jja
# z_seas_son <- (cru_ave_son-cru_full_mean_son)/cru_full_sd_son
# 
# z_score_seas <- stack(z_seas_djf, z_seas_mam, z_seas_jja, z_seas_son)
# 
# cru_seas_z_score_values <- get_raster_vals(z_score_seas)
# 
# write.csv(cru_seas_z_score_values, file=sprintf("/projectnb/modislc/users/rkstan/pasturelands/results/cru_seas_z_%s_%s.csv",args$vname, args$reg),  quote=FALSE, row.names=FALSE)
# 
# 
# #calculate monthly climatological anomalies  
# #############################################
# 
# z_djf <- (temp_seas_djf-cru_full_mean_djf)/cru_full_sd_djf
# z_mam <- (temp_seas_mam-cru_full_mean_mam)/cru_full_sd_mam
# z_jja <- (temp_seas_jja-cru_full_mean_jja)/cru_full_sd_jja
# z_son <- (temp_seas_son-cru_full_mean_son)/cru_full_sd_son
# 
# z_score <- stack(z_djf, z_mam, z_jja, z_son)
# 
# cru_clim_z_score_values <- get_raster_vals(z_score)
# 
# write.csv(cru_clim_z_score_values, file=sprintf("/projectnb/modislc/users/rkstan/pasturelands/results/cru_clim_z_%s_%s.csv", args$vname, args$reg), quote=FALSE, row.names=FALSE)
# 

# plot --------------------------------------------------------------------------

# plot CRU means 
##########################

# #plotting function for temperature
# par(mfrow=c(2,1), pty="s", oma=c(0.5,0.5,0.5,0.5), mar=c(0.5,0.5,0.5,0.5))
# cutpts <- c(-30, -20, -10, 0, 10, 20, 30)
# plot(cru_ave_all,
#      legend.width=1, legend.shrink=1, col=(rev(brewer.pal(6, "RdBu"))), breaks=cutpts, bty='n',axes=FALSE, frame.plot=F,
#     xaxt='n', ann=FALSE, yaxt='n', box=FALSE, legend=FALSE, xlim=c(-140,-60))
# plot(na, add=T)
# plot(cru_ave_all_1, 
#      legend.width=1, legend.shrink=1,  col=(rev(brewer.pal(6, "RdBu"))), breaks=cutpts, bty='n',axes=FALSE, frame.plot=F,
#      xaxt='n', ann=FALSE, yaxt='n', box=FALSE)
# plot(sa, add=T)
# 
# plot(cru_sd_pyear,  col=(rev(brewer.pal(10, "RdBu"))),main="Standard deviation precipitation mm/day (2003-2014)")
# plot(boundaries(precip_mask_sa, classes=T), add=T, col=c(0,"black"), legend=FALSE)
# 
# par(mfrow=c(2,1), pty="s", oma=c(0.5,0.5,0.5,0.5), mar=c(0.5,0.5,0.5,0.5))
# # plotting function for precpitation 
# cutpts <- c(0,500, 1000, 1500, 2000, 2500, 3000, 4000, 5000,7000, 8000)
# plot(cru_ave_pyear,
#      legend.width=1, legend.shrink=1, col=(rev(brewer.pal(10, "RdBu"))), breaks=cutpts, bty='n',axes=FALSE, frame.plot=F,
#      xaxt='n', ann=FALSE, yaxt='n', box=FALSE, legend=FALSE, xlim=c(-141,-54))
# plot(na, add=T)
# plot(cru_ave_pyear_1, 
#      legend.width=1, legend.shrink=1, col=(rev(brewer.pal(10, "RdBu"))), breaks=cutpts, bty='n',axes=FALSE, frame.plot=F,
#      xaxt='n', ann=FALSE, yaxt='n', box=FALSE)
# plot(sa, add=T)

# 
# plot(cru_ave_pyear, main="Average precipitation mm/year (2003-2014)", 
#      legend.width=1, legend.shrink=1, col=brewer.pal(9, "PuBuGn"), breaks=cutpts)
# 
# cutpts <- c(50, 100, 150, 200, 250, 300, 350, 400)
# plot(cru_sd_pyear,  col=brewer.pal(9, "PuBuGn"),main="Standard deviation precipitation mm/year (2003-2014)")

# # plot some examples of anomalies 
############################################ 

# m <- seq(from=as.Date("2003/1/1"),  by="month", length=144)
# # a pasture pixel in Sub-Saharan Africa (0.788)
# plot(m, cru_z_values[111285,], type='l', main="Pixel in Sub Saharan Africa",xlab="", ylab="Precipitation Anomalies (mm)", 
#      cex.lab=1.35, cex.axis=1.5)
# abline(h=0, col="red", lwd=2)
# 
# # a pasture pixel in USA (0.947)
# plot(m, cru_z_values[66387,], type='l', main="Pixel in USA",xlab="", ylab="Precipitation Anomalies (mm)",
#      cex.lab=1.35, cex.axis=1.5)
# abline(h=0, col="red", lwd=2)
# 
# # a pasture pixel in Latin America (0.858)
# plot(m, cru_z_values[161538,], type='l', main="Pixel in Latin America",xlab="", ylab="Precipitation Anomalies (mm)",
#      cex.lab=1.35, cex.axis=1.5)
# abline(h=0, col="red", lwd=2)
# 




#EXTRAS
#--------

# 
# # read and plot CRU netcdf ------------------------
# 
# # split the time units string into fields
# tustr <- strsplit(tunits$value, " ")
# tdstr <- strsplit(unlist(tustr)[3], "-")
# tmonth = as.integer(unlist(tdstr)[2])
# tday = as.integer(unlist(tdstr)[3])
# tyear = as.integer(unlist(tdstr)[1])
# chron(doy, origin = c(tmonth, tday, tyear))
# 
# tmp.array[tmp.array == fillvalue$value] <- NA
# x <- apply(tmp.array, 3, mean, na.rm=T)
# y <-apply(tmp.array, 3, var, na.rm=T)
# 
# m <- 1362
# tmp.slice <- tmp.array[,,m]
# image(lon, lat, tmp.slice, col=rev(brewer.pal(10, "RdBu")))
# 
# grid <- expand.grid(lon = lon, lat = lat)
# cutpts <- c(-50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50)
# levelplot(tmp.slice ~ lon * lat, data = grid, at = cutpts, cuts = 11, pretty = T, 
#           scales=list(x=list(cex=2), y=list(cex=2)), xlab=list(cex=2), ylab=list(cex=2),
#           col.regions = (rev(brewer.pal(10, "RdBu"))), main=list(label="Mean temperature June 2014 (oC)", cex=2), colorkey=list(labels=list(cex=2)))
# 
# 
# #main=sprintf("%s temperature June 2014", dname[t])
# 
# # grid <- expand.grid(lon = lon, lat = lat)
# # cutpts <- c(0, 1, 2.5, 5, 7.5, 10, 15, 50, 250, 1000)
# # levelplot(tmp.slice ~ lon * lat,  data=grid, at = cutpts, cuts = 9, pretty = T, 
# #           scales=list(x=list(cex=2), y=list(cex=2)), xlab=list(cex=2), ylab=list(cex=2),
# #           col.regions = (brewer.pal(9, "PuBuGn")), main=list(label="Precipitation June 2014 (mm/month)", cex=2), colorkey=list(labels=list(cex=2)))
# 
# 
# 
# lonlat <- expand.grid(lon,lat)
# tmp.vec <- as.vector(tmp.slice)
# length(tmp.vec)
# 
# tmp.df01 <- data.frame(cbind(lonlat, tmp.vec))
# names(tmp.df01) <- c("lon", "lat", paste(dnam[t], as.character(m), sep="_"))
# head(na.omit(tmp.df01), 20)
# 
# 
# csvfile <- sprintf("cru_%s_1.csv", dname[t])
# write.table(na.omit(tmp.df01), csvfile, row.names=FALSE, sep=",")
# 
# tmp.vec.long <- as.vector(tmp.array)
# length(tmp.vec.long)
# 
# #reshape the vector 
# tmp.mat <- matrix(tmp.vec.long, nrow=nlon*nlat, ncol=nt)
# dim(tmp.mat)
# head(na.omit(tmp.mat))
# 
# 
# lonlat <- expand.grid(lon, lat)
# tmp.df02 <- data.frame(cbind(lonlat, tmp.mat))
# names(tmp.df02) <- c("lon", "lat", "tmpJan", "tmpFeb", "tmpMar", "tmpApr", "tmpMay", 
#                      "tmpJun", "tmpJul", "tmpAug", "tmpSep", "tmpOct", "tmpNov", "tmpDec")
# options(width = 110)
# head(na.omit(tmp.df02, 20))
# 
# #get annual mean, mtwa, and mtco values and add them to the second data frame 
# tmp.df02$mtwa <- apply(tmp.df02[3:14], 1, max)  # mtwa
# tmp.df02$mtco <- apply(tmp.df02[3:14], 1, min)  # mtco
# tmp.df02$mat <- apply(tmp.df02[3:14], 1, mean)  # annual (i.e. row) means
# head(na.omit(tmp.df02))
# 
# dim(na.omit(tmp.df02))
# csvfile <- sprintf("cru_%s_2.csv", dname[t])
# write.table(na.omit(tmp.df02), csvfile, row.names=FALSE, sep=",")
# 
# tmp.df03 <- na.omit(tmp.df02)
# head(tmp.df03)
# 
# # make tif files from each CRU layer ---------------------------
# 
# 
# # for (k in 1:length(CRU_stack)){
# #   writeRaster(CRU_stack[[k]], filename=sprintf("CRU_tmp_%s.tif", k), format="GTiff", overwrite=TRUE)
# #   
# # }
# 
