#!/bin/bash

# Script to clip TRMM 3B43, MODIS MCD12C1 and CRU data to polygon of
# pastures wth value >= 0.6. Setting the gdal flag to ignor bad cutlines is
# necessary to avoid problems with self intersecting polygons, even if they 
# are valid. This defaul behavior was changed in GDAL 2.1 to avoid other 
# critical problems not being noticed. 

# Tested with GDAL 2.1

# Path to pasture shapefile
pastures=/projectnb/modislc/users/rkstan/GE712/data/pasture_extent/pasture2000_GT_06.shp
overwrite=Yes

# Bounding box for clipping MODIS and make it match the other grids
xmin=-81.5   
ymin=-56.5 
xmax=-34.5
ymax=12.5

# CRU Data

cd /projectnb/modislc/users/rkstan/GE712/data/cru.ts4.00

if [ ! -f "cru_ts4_pasture.tif" -o $overwrite == "Yes" ]; then
    gdalwarp --config GDALWARP_IGNORE_BAD_CUTLINE YES \
    -cutline $pastures -cl pasture2000_GT_06 \
     -cwhere "ID=1" -co COMPRESS=PACKBITS -overwrite \
       cru_ts4_SA.tif cru_ts4_pasture.tif     
fi

# TRMM

cd /projectnb/modislc/users/rkstan/GE712/data/TRMM

for i in $(find . -type f -name "*_clip.tif"); do
    
    # Extract basename
    fname=$(basename $i | awk -F ".tif" '{print $1}')

    # Extract data for mainland southamerica only
    if [ ! -f $fname"_clip.tif" -o $overwrite="Yes"]; then
        gdalwarp  --config GDALWARP_IGNORE_BAD_CUTLINE YES \
         -cutline $pastures -cl pasture2000_GT_06 \
          -cwhere "ID=1" -co COMPRESS=PACKBITS -overwrite \
           $i $fname"_pastures.tif"
    fi
done

# PAR

cd /projectnb/modislc/users/rkstan/GE712/data/PAR

if [ ! -f "PAR_SA.tif" -o $overwrite="Yes" ]; then
    gdalwarp --config GDALWARP_IGNORE_BAD_CUTLINE YES \
    -cutline $pastures -cl pasture2000_GT_06 \
     -cwhere "ID=1" -co COMPRESS=PACKBITS -overwrite \
     PAR_SA.tif PAR_pastures.tif

fi

# MODIS, requires QA filtered input, it is also resampled here 

cd /projectnb/modislc/users/rkstan/GE712/data/MOD13C2

for i in $(find . -type f -name "MOD13C2*.hdf"); do

    fname=$(basename $i | awk -F ".tif" '{print $1}')
    
    # Get EVI 
    if [ ! -f $fname"_EVI.tif" -o $overwrite="Yes"]; then
        gdalwarp --config GDALWARP_IGNORE_BAD_CUTLINE YES \
         -cutline $pastures -cl pasture2000_GT_06 -cwhere "ID=1" \
           -te $xmin $ymin $xmax $ymax -t_srs EPSG:4326 -tr 0.5 0.5 -tap \
            -r average -co COMPRESS=PACKBITS \
        $i $fname"_pastures.tif"
    fi

done


