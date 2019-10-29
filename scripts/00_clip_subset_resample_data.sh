#!/bin/bash

# Script to preprocess TRMM 3B43, MODIS MCD12C1, CRU data and IIASA
# thermal regimes

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

# Extract data for mainland southamerica only
if [ ! -f "cru_ts4_SA.tif" ]; then
    gdalwarp -t_srs EPSG:4326 -te $xmin $ymin $xmax $ymax \
    -co COMPRESS=PACKBITS -overwrite \
      NETCDF:"cru_ts4.00.1901.2015.tmp.dat.nc":tmp  cru_ts4_SA.tif
fi

# Extract data for pastures only 
if [ ! -f "cru_ts4_pasture.tif" ]; then
    gdalwarp --config GDALWARP_IGNORE_BAD_CUTLINE YES \
    -cutline $pastures -cl pasture2000_GT_06 \
     -cwhere "ID=1" -co COMPRESS=PACKBITS -overwrite \
       cru_ts4_SA.tif cru_ts4_pasture.tif
fi

# TRMM Data

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


    # Extract data for pastures only
    if [ ! -f $fname"_warp.tif" ]; then
        gdalwarp -srcnodata -9999  --config GDALWARP_IGNORE_BAD_CUTLINE YES \
         -cutline $pastures -cl pasture2000_GT_06 -crop_to_cutline \
          -cwhere "ID=1" -co COMPRESS=PACKBITS -overwrite \
           $fname"_warp.tif" $fname"_pastures.tif"
    fi

   # Extract data for mainland southamerica only    
   # Resample data to 0.5 degree to match CRU resolution
    if [ ! -f $fname"_clip.tif" ]; then
        gdalwarp -te $xmin $ymin $xmax $ymax \
         -tr 0.5 0.5 -tap -r average -co COMPRESS=PACKBITS -overwrite \
         $fname"_pastures.tif" $fname"_clip.tif"
    fi
done

# MODIS Data 
# requires QA filtered input, it is also resampled here 

cd /projectnb/modislc/users/rkstan/GE712/data/MOD13C2

    # Get EVI 
    if [ ! -f "filtered_EVI.envi" ]; then
        gdalwarp --config GDALWARP_IGNORE_BAD_CUTLINE YES \
        -cutline $pastures -cl pasture2000_GT_06 \
        -cwhere "ID=1" -overwrite \
        filtered_EVI.envi filtered_EVI_pastures.tif
    fi

    if [ ! -f "filtered_EVI_resample_05.tif" ]; then
       gdalwarp -te $xmin $ymin $xmax $ymax -t_srs EPSG:4326 -tr 0.5 0.5 -tap \
       -r average -co COMPRESS=PACKBITS -overwrite \
       filtered_EVI_pastures.tif filtered_EVI_resample_05.tif
    fi

# Pastures Data
# Resample to match the grid

cd /projectnb/modislc/users/rkstan/GE712/data/pasture_extent

    if [ ! -f "pasture2000_GT_06_resample_05.tif" ]; then
       gdalwarp -te $xmin $ymin $xmax $ymax -t_srs EPSG:4326 -tr 0.5 0.5 -tap \
       -r average -co COMPRESS=PACKBITS -overwrite \
       pasture2000_GT_06.tif pasture2000_GT_06_resample_05.tif
    fi

    # Extract data for no land cover change pastures only 
    if [ ! -f "pastures2000_GT_06_lcc.tif" ]; then
        gdalwarp -srcnodata -9999  --config GDALWARP_IGNORE_BAD_CUTLINE YES \
         -cutline $pastures -cl pasture2000_GT_06 -crop_to_cutline \
          -cwhere "ID=1" -co COMPRESS=PACKBITS -overwrite \
           pasture2000_GT_06.tif pastures2000_GT_06_lcc.tif
    fi



# Thermal regimes from IIASA v3

cd /projectnb/modislc/users/rkstan/GE712/data/thermal_regions_IIASA_v3
pastureraster=/projectnb/modislc/users/rkstan/GE712/data/pasture_extent/pasture2000_GT_06_resample_05.tif

# Resample thermal regimes to 0.5 degree grid 
    if [ ! -f "thermal_regimes_resample_05.tif" ]; then
       gdalwarp -te $xmin $ymin $xmax $ymax -t_srs EPSG:4326 -tr 0.5 0.5 -tap \
       -co COMPRESS=PACKBITS -overwrite \
       data.asc thermal_regimes_resample_05.tif
    fi

# Mask thermal regimes using resampled pasture
    if [ ! -f "thermal_regimes_mask.tif" ]; then
       gdal_calc.py -A $pastureraster -B thermal_regimes_resample_05.tif \
       --type=Byte --co="COMPRESS=PACKBITS" --overwrite \
       --outfile="thermal_regimes_mask.tif" \
       --calc="logical_and(A >0 , B >= 1)*B"
    fi 
    
# Extract only pasture pixels 
    if [ ! -f "thermal_regimes_pastures" ]; then
        gdalwarp  --config GDALWARP_IGNORE_BAD_CUTLINE YES \
         -cutline $pastures -cl pasture2000_GT_06 \
          -cwhere "ID=1" -co COMPRESS=PACKBITS -overwrite \
           thermal_regimes_mask.tif thermal_regimes_pastures.tif
    fi

# CHIRPS rainfall data at 0.05 spatial resolution 

cd /projectnb/modislc/users/rkstan/GE712/data/CHIRPS/values/annual

for i in $(find . -type f -name "chirps-v2.0.*.tif"); do

    # Extract basename
    fname=$(basename $i | awk -F ".tif" '{print $1}')

    # Extract data for pastures only
    if [ ! -f $fname"_pastures.tif" ]; then
        gdalwarp  --config GDALWARP_IGNORE_BAD_CUTLINE YES \
         -cutline $pastures -cl pasture2000_GT_06 \
          -cwhere "ID=1" -co COMPRESS=PACKBITS -overwrite \
           $fname".tif" $fname"_pastures.tif"
    fi    


    # Extract data for mainland southamerica only
    if [ ! -f $fname"_clip.tif" ]; then
        gdalwarp -te $xmin $ymin $xmax $ymax -t_srs EPSG:4326 -srcnodata -9999 \
         -co COMPRESS=PACKBITS -overwrite \
         $fname"_pastures.tif" $fname"_clip.tif"
    fi
done


# Land cover change data (CCI) at 300 m spatial resolution 

#cd /projectnb/modislc/users/rkstan/GE712/data/CCI

#    # Extract data for mainland southamerica only
#    if [ ! -f "land_cover_change_sa.tif" ]; then
#       gdalwarp -te $xmin $ymin $xmax $ymax -t_srs EPSG:4326 -srcnodata 0 -dstnodata 0 \
#       ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif land_cover_change_sa.tif
#    fi   

   # Extract data for pastures only 
#   if [ ! -f "land_cover_change_pasture.tif" ]; then
#       gdalwarp --config GDALWARP_IGNORE_BAD_CUTLINE YES \
#       -cutline $pastures -cl pasture2000_GT_06 \
#       -cwhere "ID=1" -overwrite \
#       ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif land_cover_change_pasture.tif
#   fi

   # Extract data for mainland southamerica only
#   if [ ! -f "land_cover_change_clip.tif" ]; then
#      gdalwarp -te $xmin $ymin $xmax $ymax -t_srs EPSG:4326 -srcnodata 0 -dstnodata 0 \
#      land_cover_change_pasture.tif land_cover_change_clip.tif
#   fi   

    # Reclassify land cover to limit to grassland and herbaceous vegetation 
#     if [ ! -f "land_cover_change_reclassify.tif" ]; then
#     gdal_calc.py -A land_cover_change_clip.tif \
#     --outfile=land_cover_change_reclassify.tif \
#     --type=Byte --co="COMPRESS=PACKBITS" --overwrite \
#     --calc="(A==30)*1 + (A==40)*1 + (A==110)*1 + (A==130)*1 + (A==140)*1 + (A==150)*1 + (A==151)*1"
#     fi

# CATTLE data 

cd /projectnb/modislc/users/rkstan/pasturelands/livestock.units/CATTLE

# Extract data for mainland southamerica only
if [ ! -f "Glb_Cattle_CC2006_AD_SA.tif" ]; then
    gdalwarp -t_srs EPSG:4326 -te $xmin $ymin $xmax $ymax \
    -co COMPRESS=PACKBITS -overwrite \
     Glb_Cattle_CC2006_AD.tif  Glb_Cattle_CC2006_AD_SA.tif
fi

# Extract data for pastures only 
if [ ! -f "Glb_Cattle_pasture.tif" ]; then
    gdalwarp --config GDALWARP_IGNORE_BAD_CUTLINE YES \
    -cutline $pastures -cl pasture2000_GT_06 \
     -cwhere "ID=1" -co COMPRESS=PACKBITS -overwrite \
       Glb_Cattle_CC2006_AD_SA.tif Glb_Cattle_pasture.tif
fi


    if [ ! -f "Glb_Cattle_resample_05.tif" ]; then
       gdalwarp -te $xmin $ymin $xmax $ymax -t_srs EPSG:4326 -tr 0.5 0.5 -tap \
       -r average -co COMPRESS=PACKBITS -overwrite \
       Glb_Cattle_pasture.tif Glb_Cattle_resample_05.tif
   fi

# Land cover change Data
# Resample to match the grid

cd /projectnb/modislc/users/rkstan/GE712/outputs/

    if [ ! -f "lcc_mask_resample.tif" ]; then
       gdalwarp -te $xmin $ymin $xmax $ymax -t_srs EPSG:4326 -tr 0.5 0.5 -tap \
       -r average -co COMPRESS=PACKBITS -overwrite \
       lcc_mask.envi lcc_mask_resample.tif
    fi



# Read in FAO livestock production systems data 

cd /projectnb/modislc/users/rkstan/GE712/data/FAO

    # Get EVI 
    if [ ! -f "livestock_production_system_pastures.tif" ]; then
        gdalwarp --config GDALWARP_IGNORE_BAD_CUTLINE YES \
        -cutline $pastures -cl pasture2000_GT_06 \
        -cwhere "ID=1" -overwrite \
        livestock_production_system_SA.envi livestock_production_system_pastures.tif
    fi

# resample for 4 different management strategies 
    if [ ! -f "livestock_production_system_pastures_005.tif" ]; then
       gdalwarp -te $xmin $ymin $xmax $ymax -t_srs EPSG:4326 -tr 0.05 0.05 -tap \
       -r near -co COMPRESS=PACKBITS -overwrite \
       livestock_production_system_4class.envi livestock_production_system_pastures_005.tif
    fi

# resample for 5 different agroecological zones
    if [ ! -f "livestock_production_system_pastures_zones_005.tif" ]; then
       gdalwarp -te $xmin $ymin $xmax $ymax -t_srs EPSG:4326 -tr 0.05 0.05 -tap \
       -r near -co COMPRESS=PACKBITS -overwrite \
       livestock_production_system_5class.envi livestock_production_system_pastures_zones_005.tif
    fi

    # Get EVI 
    if [ ! -f "livestock_density_observed_pastures.tif" ]; then
        gdalwarp --config GDALWARP_IGNORE_BAD_CUTLINE YES \
        -cutline $pastures -cl pasture2000_GT_06 \
        -cwhere "ID=1" -overwrite \
        livestock_density_observed_SA.envi livestock_density_observed_pastures.tif
    fi

# clip and resample the feed requirements 

cd /projectnb/modislc/users/rkstan/GE712/data/GLOBIOM

    # Get EVI 
    if [ ! -f "tDM_yr_pastures.tif" ]; then
        gdalwarp --config GDALWARP_IGNORE_BAD_CUTLINE YES \
        -cutline $pastures -cl pasture2000_GT_06 \
        -cwhere "ID=1" -overwrite \
        tDM_yr.envi tDM_yr_pastures.tif
    fi

# resample for 4 different management strategies 
    if [ ! -f "tDM_yr_pastures_005.tif" ]; then
       gdalwarp -te $xmin $ymin $xmax $ymax -t_srs EPSG:4326 -tr 0.05 0.05 -tap \
       -r near -co COMPRESS=PACKBITS -overwrite \
       tDM_yr_pastures.tif tDM_yr_pastures_005.tif
    fi



#PAR data 

#cd /projectnb/modislc/users/rkstan/GE712/data/PAR

# Clip (using its native coordinates from 0 t0 360)
#if [ ! -f "PAR_SA.tif" -o $overwrite == "Yes" ]; then
#    gdalwarp -t_srs EPSG:4326 -te 278.5 $ymin 325.5  $ymax \
#     -co COMPRESS=PACKBITS -overwrite -tr 0.5 0.5 -tap -r average \
#     NETCDF:"CERES_SYN1deg-Month_Terra-Aqua-MODIS_Ed3A_Subset_200301-201512.nc":sfc_comp_par_direct_all_mon \
#     PAR_SA.tif

#    # Edit georreferenced bounds
#    gdal_edit.py -a_ullr $xmin $ymax $xmax $ymin PAR_SA.tif
#fi

#if [ ! -f "PAR_SA.tif" ]; then
#    gdalwarp --config GDALWARP_IGNORE_BAD_CUTLINE YES \
#    -cutline $pastures -cl pasture2000_GT_06 \
#     -cwhere "ID=1" -co COMPRESS=PACKBITS -overwrite \
#     PAR_SA.tif PAR_pastures.tif
#
#fi
