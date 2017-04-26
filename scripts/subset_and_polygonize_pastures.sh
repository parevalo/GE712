#!/bin/bash -l

# Select pixels >= 0.6 from the pasture layer and polygonize that layer to use
# it as clipping boundary for the other variables

cd /projectnb/modislc/users/rkstan/GE712/data/pasture_extent


# Create binary raster where value >= 0.6 
qsub -j y -b y -V -N filter_past \
gdal_calc.py -A pasture2000_area.tif --outfile="pasture2000_GT_06.tif" \
 --calc='"A*(A>=0.6)"' --overwrite --co="COMPRESS=PACKBITS"
 
# Polygonize
qsub -j y -b y -V -N polyg_past -hold_jid filter_past \
 gdal_polygonize.py -nomask pasture2000_GT_06.tif -f '"ESRI Shapefile"' \
 pasture2000_GT_06.shp pasture2000_GT_06 ID
   
