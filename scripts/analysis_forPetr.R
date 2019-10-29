library(raster)
library(ggplot2)
library(RColorBrewer)
library(maptools)
library(plyr)

source("/projectnb/modislc/users/rkstan/GE712/GE712/scripts/apply_stack_parallel.R")

# load in South America boundaries
data(wrld_simpl)
SA = c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Peru",
       "Suriname", "Uruguay", "Venezuela")
sa = wrld_simpl[which(wrld_simpl@data$NAME %in% SA),]


AD <- raster("/projectnb/modislc/users/rkstan/pasturelands/livestock.units/CATTLE/Glb_Cattle_CC2006_AD_SA.tif")
pasture <- raster("/projectnb/modislc/users/rkstan/pasturelands/livestock.units/CATTLE/Glb_Cattle_pasture.tif")
resample <- raster("/projectnb/modislc/users/rkstan/pasturelands/livestock.units/CATTLE/Glb_Cattle_resample_0.08.tif")
livestock_density_GLOBIOM <- raster("/projectnb/modislc/users/rkstan/GE712/data/GLOBIOM/TLU_per_kmsq_pasture.tif")

# plot raster with breaks in data to visualize better 
cbPallete <- c(brewer.pal(5, "YlOrRd"))
brks <- c(0, 20, 50, 100, 250, cellStats(resample,max))
cattle_pasture_breaks <- cut(resample, breaks=brks)
plot(cattle_pasture_breaks, col=cbPallete, legend = FALSE)
plot(sa, add=T, lty=1, lwd=0.5)
par(fig=c(0,1,0,1), oma=c(0, 0, 0, 0), mar=c(0,0,0,0), pty="s", new=TRUE)
plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
legend("bottom", fill=cbPallete, legend = c('0-20', '20-50', '50-100',
                                            '100-250', '>250'), ncol=4, bty="n")



temp <- crop(resample, extent(livestock_density_GLOBIOM))
cattle <- getValues(temp)
write.csv(cattle, "cattle_dataset.csv", quote =F, row.names = F)
cattle_globiom <- getValues(livestock_density_GLOBIOM)


cattle_combined <- cbind(cattle, cattle_globiom)
cattle_combined_clean <- as.data.frame(na.omit(cattle_combined))
colnames(cattle_combined_clean) <- c("cattle_FAO", "cattle_GLOBIOM")
ggplot(cattle_combined_clean, aes(cattle_FAO, cattle_GLOBIOM)) + geom_point()


pasture_lcc <- raster("/projectnb/modislc/users/rkstan/GE712/data/pasture_extent/pastures2000_GT_06_lcc.tif")
t <- raster("/projectnb/modislc/users/rkstan/GE712/data/pasture_extent/pasture2000_GT_06.tif")

land_cover <- raster("/projectnb/modislc/users/rkstan/GE712/data/CCI//land_cover_change_sa.tif")

EVI_year_vals <-read.csv("/projectnb/modislc/users/rkstan/GE712/outputs/EVI_year_vals_005.csv")
EVI_year_vals[,4]
#temp_year_vals <-read.csv("/projectnb/modislc/users/rkstan/GE712/outputs/temp_year_vals.csv")
#temp_year_vals[,4]
precip_year_vals <-read.csv("/projectnb/modislc/users/rkstan/GE712/outputs/CHIRPSprecip_annual_vals_005.csv")
precip_year_vals[,4]
livestock_units <- read.csv("/projectnb/modislc/users/rkstan/GE712/outputs/livestock_density_FAO.csv")
tDM_yr_pasture <- read.csv("/projectnb/modislc/users/rkstan/GE712/outputs/tDM_yr_pasture_vals.csv")
lps_regions <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/lps_resampled_vals_zones.csv")
lps_regions_production <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/lps_resampled_vals.csv")
colnames(lps_regions) <- "zones"

df_combined <- cbind(EVI_year_vals[,4], precip_year_vals[,4], cattle, cattle_globiom)
df_combined_clean <- as.data.frame(na.omit(df_combined))
colnames(df_combined_clean) <- c("evi","precip", "cattle_FAO", "cattle_globiom" )


EVI_clean <- as.data.frame(df_combined_clean[,1])
#temp_clean <- as.data.frame(df_combined_clean[,2])
precip_clean <- as.data.frame(df_combined_clean[,2])
cattle_FAO_clean <- as.data.frame(df_combined_clean[,3])
cattle_globiom_clean <- as.data.frame(df_combined_clean[,4])

#ggplot(df_combined_clean, aes(precip, evi)) + geom_point(shape=1, aes(colour=cattle_globiom))
ggplot(df_combined_clean, aes(evi, cattle_FAO)) + geom_point(shape=1) + xlab("EVI") + ylab("Livestock density Robinson (cattle per km2)")
#ggplot(df_combined_clean, aes(evi, cattle_FAO)) + geom_point(aes(colour=precip))
ggplot(df_combined_clean, aes(precip, cattle_FAO)) + geom_point(shape=1) + xlab("Precipitation (mm/year)") + ylab("Livestock density Robinson (cattle per km2)")
#ggplot(df_combined_clean, aes(precip, cattle_FAO)) + geom_point(aes(colour=evi))

ggplot(df_combined_clean, aes(evi, cattle_globiom)) + geom_point(shape=1) + xlab("EVI") + ylab("Livestock density GLOBIOM (TLU per km2)")
ggplot(df_combined_clean, aes(precip, cattle_globiom)) + geom_point(shape=1) + xlab("Precipitation (mm/year)") + ylab("Livestock density GLOBIOM (TLU per km2)")

ggplot(df_combined_clean, aes(cattle_FAO, cattle_globiom)) + geom_point(shape=1) + xlab("Livestock density Robinson (cattle per km2)") + ylab("Livestock density GLOBIOM (TLU per km2)")

livestock_density_FAO <- raster("/projectnb/modislc/users/rkstan/GE712/data/FAO/livestock_density_observed_pasture_SA.tif")
livestock_density_FAO_vals <- getValues(livestock_density_FAO)
setwd("/projectnb/modislc/users/rkstan/GE712/outputs")
write.csv(livestock_density_FAO_vals, "livestock_density_FAO.csv", quote =F, row.names = F)

tDM_yr_pasture <- raster("/projectnb/modislc/users/rkstan/GE712/data/GLOBIOM/tDM_yr_pastures_005.tif")
tDM_yr_pasture_vals <- getValues(tDM_yr_pasture)
setwd("/projectnb/modislc/users/rkstan/GE712/outputs")
write.csv(tDM_yr_pasture_vals, "tDM_yr_pasture_vals.csv", quote =F, row.names = F)



# cross sectional analysis with climate and management data 
# Merge and remove rows with at least one NA
total <- cbind(EVI_year_vals[,4], precip_year_vals[,c(3,4)], livestock_units, tDM_yr_pasture, lps_regions_production, lps_regions)
rownames(total)<- seq(1, 1297200)
total_filtered = na.omit(total)
total_filtered_sample <- ddply(total_filtered, .(zones), function(x) x[sample(nrow(x),500, replace=FALSE, prob=NULL),1:7])

# Split variables back into separate df's
EVI_clean <- as.data.frame(total_filtered[,c(1)])
#temp_clean <- as.data.frame(df_combined_clean[,2])
precip_clean <- as.data.frame(total_filtered[,c(2,3)])
LU_clean <- as.data.frame(total_filtered[,4])
tDM_yr_clean <- as.data.frame(total_filtered[,5])
lps_production_clean <- as.data.frame(total_filtered[,6])
lps_clean <- as.data.frame(total_filtered[,7])

full_dataset <- cbind(rownames(total_filtered), precip_clean[,2], EVI_clean,  LU_clean, tDM_yr_clean, lps_production_clean, lps_clean)
colnames(full_dataset) = c("cell", "precip2006","evi2006","LU", "tDM_yr", "lps_production", "lps")

seas2_raster <- brick(nrows=1380, ncols=940, nl=6, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                      xmn=-81.5, xmx=-34.5, ymn=-56.5, ymx=12.5)
setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")

seas2_raster[as.numeric(as.character(full_dataset[,1]))] <- as.matrix(full_dataset[,-1])
writeRaster(seas2_raster, file=paste("CHIRPS", "cross-sectional-analysis", ".hdr", sep=""), format="ENVI", overwrite=TRUE)


# WINTER DATA (JJA) starts in FIRST year bc we have fall(MAM) and summer(DJF) from that year
full_dataset_zone1 = cbind(full_dataset[which(full_dataset[,7]==1),c(2:6)])
colnames(full_dataset_zone1) = c("precip2006", "evi2006", "LU", "tDM_yr", "lps_production")

full_dataset_zone2 = cbind(full_dataset[which(full_dataset[,7]==2),c(2:6)])
colnames(full_dataset_zone2) = c("precip2006", "evi2006","LU", "tDM_yr", "lps_production")

full_dataset_zone3 = cbind(full_dataset[which(full_dataset[,7]==3),c(2:6)])
colnames(full_dataset_zone3) = c("precip2006", "evi2006","LU", "tDM_yr", "lps_production")

full_dataset_zone4 = cbind(full_dataset[which(full_dataset[,7]==4),c(2:6)])
colnames(full_dataset_zone3) = c("precip2006", "evi2006","LU", "tDM_yr", "lps_production")

full_dataset_zone5 = cbind(full_dataset[which(full_dataset[,7]==5),c(2:6)])
colnames(full_dataset_zone3) = c("precip2006", "evi2006","LU", "tDM_yr", "lps_production")

setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")
# Save these datasets for use in RATS
write.csv(full_dataset_zone1, "CHIRPS_cross_sectional_zone1.csv", quote =F, row.names = F)
write.csv(full_dataset_zone2, "CHIRPS_cross_sectional_zone2.csv", quote =F, row.names = F)
write.csv(full_dataset_zone3, "CHIRPS_cross_sectional_zone3.csv", quote =F, row.names = F)
write.csv(full_dataset_zone3, "CHIRPS_cross_sectional_zone4.csv", quote =F, row.names = F)
write.csv(full_dataset_zone3, "CHIRPS_cross_sectional_zone5.csv", quote =F, row.names = F)

# use sample to run the OLS 
zone2_subset <- subset(full_dataset_zone2, full_dataset_zone2$lps_production != 3)
zone2_sample <- ddply(zone2_subset, .(lps_production), function(x) x[sample(nrow(x),500, replace=FALSE, prob=NULL),1:4])
zone3_subset <- subset(full_dataset_zone3, full_dataset_zone3$lps_production != 3)
zone3_sample <- ddply(zone3_subset, .(lps_production), function(x) x[sample(nrow(x),500, replace=FALSE, prob=NULL),1:4])
zone4_subset <- subset(full_dataset_zone4, full_dataset_zone4$lps_production != 3)
zone4_sample <- ddply(zone4_subset, .(lps_production), function(x) x[sample(nrow(x),500, replace=FALSE, prob=NULL),1:4])

# run analysis with all variables 
#####################################
zone2_lm <- lm(zone2_sample$evi2006 ~., zone2_sample, zone2_sample$lps_production==2)
summary(zone2_lm)
zone3_lm <- lm(zone3_sample$evi2006 ~., zone3_sample, zone3_sample$lps_production==2)
summary(zone3_lm)
zone4_lm <- lm(zone4_sample$evi2006 ~., zone4_sample, zone4_sample$lps_production==2)
summary(zone4_lm)

# run analysis with only with LU 
#####################################
zone2_lm_lu <- lm(zone2_sample$evi2006 ~., zone2_sample[,-5], zone2_sample$lps_production==2)
summary(zone2_lm_lu)
zone3_lm_lu <- lm(zone3_sample$evi2006 ~., zone3_sample[,-5], zone3_sample$lps_production==2)
summary(zone3_lm_lu)
zone4_lm_lu <- lm(zone4_sample$evi2006 ~., zone4_sample[,-5], zone4_sample$lps_production==2)
summary(zone4_lm_lu)

# run analysis with only with tDM
#####################################
zone2_lm_tdm <- lm(zone2_sample$evi2006 ~., zone2_sample[,-3], zone2_sample$lps_production==2)
summary(zone2_lm_tdm)
zone3_lm_tdm <- lm(zone3_sample$evi2006 ~., zone3_sample[,-3], zone3_sample$lps_production==2)
summary(zone3_lm_tdm)
zone4_lm_tdm <- lm(zone4_sample$evi2006 ~., zone4_sample[,-3], zone4_sample$lps_production==2)
summary(zone4_lm_tdm)

# Extras 
# # Split variables back into separate df's
# EVI_clean <- as.data.frame(total_filtered_sample[,c(1)])
# #temp_clean <- as.data.frame(df_combined_clean[,2])
# precip_clean <- as.data.frame(total_filtered_sample[,c(2,3)])
# LU_clean <- as.data.frame(total_filtered_sample[,4])
# tDM_yr_clean <- as.data.frame(total_filtered_sample[,5])
# lps_production_clean <- as.data.frame(total_filtered_sample[,6])
# lps_clean <- as.data.frame(total_filtered_sample[,7])
# 
# sample_dataset <- cbind(precip_clean[,2], EVI_clean,  LU_clean, tDM_yr_clean, lps_production_clean, lps_clean)
# colnames(sample_dataset) = c("precip2006","evi2006","LU", "tDM_yr", "lps_production", "lps")

# 
# sample_dataset_zone1 = cbind(sample_dataset[which(sample_dataset[,6]==1),c(1:5)])
# sample_dataset_zone2 = cbind(sample_dataset[which(sample_dataset[,6]==2),c(1:5)])
# sample_dataset_zone3 = cbind(sample_dataset[which(sample_dataset[,6]==3),c(1:5)])
# sample_dataset_zone4 = cbind(sample_dataset[which(sample_dataset[,6]==4),c(1:5)])
# sample_dataset_zone5 = cbind(sample_dataset[which(sample_dataset[,6]==5),c(1:5)])

#zone1_lm <- lm(sample_dataset_zone1$evi2006 ~., sample_dataset_zone1)
#summary(zone1_lm)
