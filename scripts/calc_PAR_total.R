# read in netcdf data for PAR from CERES
##########################################

setwd("/projectnb/modislc/users/rkstan/GE712/data/PAR")

# load libraries ------------------
library(ncdf4)
library(sp)
library(raster)
library(rgdal)

ncname <- sprintf("CERES_SYN1deg-Month_Terra-Aqua-MODIS_Ed3A_Subset_200301-201512")
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

# caclulate direct PAR  --------------------------------------------------
tmp.array.direct <- ncvar_get(ncin, "sfc_comp_par_direct_all_mon") 
tmp.array.diffuse <- ncvar_get(ncin, "sfc_comp_par_diffuse_all_mon") 
dlname <- ncatt_get(ncin, "sfc_comp_par_direct_all_mon", "long_name")
dunits <- ncatt_get(ncin, "sfc_comp_par_direct_all_mon", "units")
fillvalue <- ncatt_get(ncin, "sfc_comp_par_direct_all_mon", "_FillValue")
dim(tmp.array.direct)

direct =list(
  x_dir=ncvar_get(ncin, "lon"),
  y_dir=ncvar_get(ncin, "lat"),
  z_dir=ncvar_get(ncin, "sfc_comp_par_direct_all_mon") 
)

diffuse =list(
  x_dif=ncvar_get(ncin, "lon"),
  y_dif=ncvar_get(ncin, "lat"),
  z_dif=ncvar_get(ncin, "sfc_comp_par_diffuse_all_mon") 
)

nc_close(ncin)

# get netcdf file into raster format ---------------------

# change to x and y coordinate system 
for (i in 1:144){
  
  xp_dir <- data.frame(x_dir=direct$x_dir, y_dir=0)
  coordinates(xp_dir)=~x_dir+y_dir
  proj4string(xp_dir)=CRS("+proj=longlat +datum=WGS84")
  xp_dir <- spTransform(xp_dir, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  yp_dir <- data.frame(x_dir=0, y_dir=direct$y_dir)
  coordinates(yp_dir)=~x_dir+y_dir
  proj4string(yp_dir)=CRS("+proj=longlat +datum=WGS84")
  yp_dir <- spTransform(yp_dir, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

  
  direct$xp_dir <- coordinates(xp_dir)[,1] 
  direct$yp_dir <- coordinates(yp_dir)[,2]
  
  
  tmp_dir <- raster(list(x_dir=direct$xp_dir, y_dir=direct$yp_dir, z_dir=direct$z_dir[,,i]))
  
  if(i==1){
    PAR_stack <-tmp_dir
    
    
  }else{
    PAR_stack  <- append(PAR_stack, tmp_dir)
    
  }
  
}


#stack NPP rasters 
PAR_direct <- stack(PAR_stack)



# get netcdf file into raster format ---------------------

# change to x and y coordinate system 
for (i in 1:144){
  
  xp_dif <- data.frame(x_dif=diffuse$x_dif, y_dif=0)
  coordinates(xp_dif)=~x_dif+y_dif
  proj4string(xp_dif)=CRS("+proj=longlat +datum=WGS84")
  xp_dif <- spTransform(xp_dif, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  yp_dif <- data.frame(x_dif=0, y_dif=diffuse$y_dif)
  coordinates(yp_dif)=~x_dif+y_dif
  proj4string(yp_dif)=CRS("+proj=longlat +datum=WGS84")
  yp_dif <- spTransform(yp_dif, CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  
  diffuse$xp_dif <- coordinates(xp_dif)[,1] 
  diffuse$yp_dif <- coordinates(yp_dif)[,2]
  
  
  tmp_dif <- raster(list(x_dif=diffuse$xp_dif, y_dif=diffuse$yp_dif, z_dif=diffuse$z_dif[,,i]))
  
  if(i==1){
    PAR_stack <-tmp_dif
    
    
  }else{
    PAR_stack  <- append(PAR_stack, tmp_dif)
    
  }
  
}



#stack NPP rasters 
PAR_diffuse <- stack(PAR_stack)

PAR_total <- PAR_direct + PAR_diffuse

writeRaster(PAR_total, file="PAR_total.tif", format="GTiff", overwrite=TRUE)
