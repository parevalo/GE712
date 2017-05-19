#!/bin/bash

# Script to preprocess TRMM 3B43, MODIS MCD12C1, CRU data and IIASA
# thermal regimes

# Tested with GDAL 2.1

# Define bounding box for clipping
xmin=-81.5
ymin=-56.5
xmax=-34.5
ymax=12.5
overwrite=No

# CRU Data

cd /projectnb/modislc/users/rkstan/GE712/data/cru.ts4.00

if [ ! -f "cru_ts4_pasture.tif" -o $overwrite == "Yes" ]; then
    gdalwarp -t_srs EPSG:4326 -te $xmin $ymin $xmax $ymax \
    -co COMPRESS=PACKBITS -overwrite \
      NETCDF:"cru_ts4.00.1901.2015.tmp.dat.nc":tmp  cru_ts4_SA.tif     
fi

# TRMM

cd /projectnb/modislc/users/rkstan/GE712/data/TRMM

for i in $(find . -type f -name "3B43*.HDF"); do
    
    # Extract basename
    fname=$(basename $i | awk -F ".HDF" '{print $1}')

    # Assign GCP to TRMM for rotation
    if [ ! -f $fname"_GCP.tif" ]; then
        gdal_translate -a_srs EPSG:4326 -gcp 0 0 -180 -50 -gcp 0 1440 180 -50 \
         -gcp 400 0 -180 50 -gcp 400 1440 180 50 \
          HDF4_SDS:UNKNOWN:$fname".HDF:0" $fname"_GCP.tif"
    fi

    # Apply GCP to make them permament (i.e rotate)
    if [ ! -f $fname"_warp.tif" ]; then
        gdalwarp -t_srs EPSG:4326 $fname"_GCP.tif" $fname"_warp.tif"
    fi 
        
    # Extract data for mainland southamerica only
    if [ ! -f $fname"_clip.tif" -o $overwrite == "Yes" ]; then  
        gdalwarp -te $xmin $ymin $xmax $ymax -dstnodata -9999 \
         -tr 0.5 0.5 -tap -r average -co COMPRESS=PACKBITS -overwrite \
         $fname"_warp.tif" $fname"_clip.tif"
    fi
done

# PAR

cd /projectnb/modislc/users/rkstan/GE712/data/PAR

# Clip (using its native coordinates from 0 t0 360)

if [ ! -f "PAR_SA.tif" -o $overwrite == "Yes" ]; then
    gdalwarp -t_srs EPSG:4326 -te 278.5 $ymin 325.5  $ymax \
     -co COMPRESS=PACKBITS -overwrite -tr 0.5 0.5 -tap -r average \
     NETCDF:"CERES_SYN1deg-Month_Terra-Aqua-MODIS_Ed3A_Subset_200301-201512.nc":sfc_comp_par_direct_all_mon \
     PAR_SA.tif

    # Edit georreferenced bounds
    gdal_edit.py -a_ullr $xmin $ymax $xmax $ymin PAR_SA.tif
fi

# MODIS
cd /projectnb/modislc/users/rkstan/GE712/data/MOD13C2

if [ ! -f "filtered_EVI_resample_05.tif" -o $overwrite == "Yes" ]; then
    gdalwarp -te $xmin $ymin $xmax $ymax -t_srs EPSG:4326 -tr 0.5 0.5 -tap \
     -r bilinear -co COMPRESS=PACKBITS -overwrite \
    filtered_EVI.envi filtered_EVI_resample_05_bilinear.tif
fi

# Resample pastures to match the grid
cd /projectnb/modislc/users/rkstan/GE712/data/pasture_extent 

if [ ! -f "pasture2000_GT_06_resample_05.tif" -o $overwrite == "Yes" ]; then
    gdalwarp -te $xmin $ymin $xmax $ymax -t_srs EPSG:4326 -tr 0.5 0.5 -tap \
     -r average -co COMPRESS=PACKBITS -overwrite \
   pasture2000_GT_06.tif pasture2000_GT_06_resample_05.tif
fi

# Thermal regimes from IIASA v3
cd /projectnb/modislc/users/rkstan/GE712/data/thermal_regions_IIASA_v3

if [ ! -f "thermal_regimes_resample_05.tif" -o $overwrite == "Yes" ]; then
    gdalwarp -te $xmin $ymin $xmax $ymax -t_srs EPSG:4326 -tr 0.5 0.5 -tap \
     -co COMPRESS=PACKBITS -overwrite \
      data.asc thermal_regimes_resample_05.tif
fi

# Mask thermal regimes using resampled pasture

pastureraster=/projectnb/modislc/users/rkstan/GE712/data/pasture_extent/pasture2000_GT_06_resample_05.tif

if [ ! -f "thermal_regimes_resample_05_pastures.tif" -o $overwrite == "Yes" ]; then
    gdal_calc.py -A $pastureraster -B thermal_regimes_resample_05.tif \
     --type=Byte --co="COMPRESS=PACKBITS" --overwrite \
      --outfile="thermal_regimes_resample_05_pastures.tif" \
       --calc="logical_and(A >0 , B >= 1)*B" 
fi

