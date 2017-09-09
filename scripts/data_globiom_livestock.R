# -------------------------------------------------------------------
# script written by David Leclere (IIASA) and modified 
# by Radost Stanimirova during summer 2017 
# output from GLOBIOM provided by Petr Havlik (IIASA) on 07/26/2017
# convert livestock numbers and feed requirements from simulation units 
# into pixels 
# -------------------------------------------------------------------

# load libraries 
GAMSPath <- "/Applications/GAMS24.8/sysdir/"
library(gdxrrw)
library(plyr)
library(dplyr)
library(reshape2)
library(raster)
library(RColorBrewer)
igdx(GAMSPath)
library(maptools)


# read in South America boundary 
data(wrld_simpl)
SA <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana",
        "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")
sa <- wrld_simpl[which(wrld_simpl@data$NAME %in% SA),]

# Read gdx files (likely format of Petr's future inputs)
# Can only open one variable from GAMS at a time. Here "YIELD_WWHT_AN_SimUID_GCM1_rcp8p5_2050"
# to know what the GAMS's variable names are and the order, open GAMS and click on reset

dataPath <- "/Volumes/Bube_Drive/Dropbox/Summer Fellowships/IIASA Summer Fellowship/Grasslands"

# # read in original livestock numbers as received by ILRI 
# # -------------------------------------------------------
# sys_stat <- rgdx.param(file.path(dataPath, "data_globiom_livestock.gdx"), "SimU_SystStat_out",
#                      names = c("SimUID","syst", "animal", "value")) %>%
#   mutate(SimUID = as.numeric(as.character(SimUID)))
# 
# # select only ruminants - bovines and small ruminants   
# LU_ILRI_ruminants <- subset(sys_stat, grepl("CATL", sys_stat$animal) | grepl("BUFF", sys_stat$animal) | grepl("GOAT", sys_stat$animal) | grepl("SHEP", sys_stat$animal))
# # sum number of ruminants per simulation unit
# ILRI_ruminants_sum <- aggregate(LU_ILRI_ruminants$value, by=list(LU_ILRI_ruminants$SimUID), FUN=sum)
# colnames(ILRI_ruminants_sum) <- c("SimUID", "value")
# 
# head(sys_stat)

# read in feed requirements data (in tonnes of dry matter per per TLU per year)
# ---------------------------------------------------------------------------------
feedreq_tDM_TLU_yr <- rgdx.param(file.path(dataPath, "data_globiom_livestock.gdx"), "LIVE_DATA_out",
                     names = c("SimUID","syst", "animal", "feedtype","value")) %>%
  mutate(SimUID = as.numeric(as.character(SimUID)))

feedreq_tDM_TLU_yr <- feedreq_tDM_TLU_yr[feedreq_tDM_TLU_yr$feedtype=='GRAZING',] # select only grazing
feedreq_sum <- aggregate(feedreq_tDM_TLU_yr$value, by=list(feedreq_tDM_TLU_yr$SimUID), FUN=sum) # sum grazing feed requirements per simulation unit
colnames(feedreq_sum) <- c("SimUID", "value")

head(feedreq_tDM_TLU_yr)

# read in final livestock numbers (harmonized etc.)
# -------------------------------------------------------
livestock_numbers <- rgdx.param(file.path(dataPath, "data_globiom_livestock.gdx"), "LIVENUMBER_SIMU_out",
                     names = c("SimUID","syst", "animal", "value")) %>%
mutate(SimUID = as.numeric(as.character(SimUID)))
livestock_numbers$value <- livestock_numbers$value*1000

# select only ruminants - bovines and small ruminants   
livestock_numbers_ruminants <- subset(livestock_numbers, grepl("BOV", livestock_numbers$animal) | grepl("SGT", livestock_numbers$animal))
livestock_numbers_ruminants <- subset(livestock_numbers_ruminants, !grepl("h", livestock_numbers_ruminants$animal))
# sum number of ruminants per simulation unit
ruminants_sum <- aggregate(livestock_numbers_ruminants$value, by=list(livestock_numbers_ruminants$SimUID), FUN=sum)
colnames(ruminants_sum) <- c("SimUID", "value")
  
head(livestock_numbers)

# Read the raster of SimUIDs
# ------------------------------------
# a few pixels will have the same SimID #FID identify the pixel ID #SimID only for land, NA for ocean
# simu_raster is something I'll be using all the time

simu_raster <- raster("/Volumes/Bube_Drive/Dropbox/Summer Fellowships/IIASA Summer Fellowship/Grasslands/rasti_simu_gr/w001001.adf")
plot(simu_raster)

SimUIDs_per_pix <- cbind.data.frame('FID'=seq(1,length(values(simu_raster)),1),
                                    'SimUID'=values(simu_raster))


# # Check the difference between ILRI and harmonized TLU  
# # -----------------------------------------------------
# TLU_combined <- merge(ILRI_ruminants_sum,ruminants_sum, by='SimUID')
# colnames(TLU_combined) <- c("SimUID", "ILRI_TLU", "TLU")
# TLU_diff <- cbind(TLU_combined$SimUID, (TLU_combined$ILRI_TLU/TLU_combined$TLU))
# colnames(TLU_diff) <- c("SimUID", "TLU_diff")
# 
# # Rasterize ILRI ruminant sum and plot 
# # ------------------------------------
# # Merge the two & sort back
# input_data_to_raster <- merge(SimUIDs_per_pix,ILRI_ruminants_sum,by='SimUID',all.x=T)
# 
# input_data_to_raster <- input_data_to_raster[order(input_data_to_raster$FID),]
# 
# # plot new info
# new_raster <- simu_raster
# values(new_raster) <- input_data_to_raster$value # NEED TO make sure the data is organised in the same order, same length. SimID is not spacially explicit! 
# plot(new_raster)

# Rasterize ruminant sum and plot 
# ------------------------------------
# Merge the two & sort back
input_data_to_raster <- merge(SimUIDs_per_pix,ruminants_sum,by='SimUID',all.x=T)

input_data_to_raster <- input_data_to_raster[order(input_data_to_raster$FID),]

# plot new info
new_raster <- simu_raster
values(new_raster) <- input_data_to_raster$value # NEED TO make sure the data is organised in the same order, same length. SimID is not spacially explicit! 
plot(new_raster)

# Calculate livestock units per km^2
# -------------------------------------------
proj4string(new_raster)=CRS("+proj=longlat +datum=WGS84") # assign a lat/long projection to SimUID raster
raster_area <- area(new_raster) # calculate the area of each raster cell 
area_vals <- getValues(raster_area) # extract the area values from the raster (area in km2)

sim_area_prep <- cbind(area_vals, SimUIDs_per_pix) 
sim_area <- aggregate(sim_area_prep$area_vals, by=list(sim_area_prep$SimUID), FUN=sum) # sum pixel area to the siumlation unit
colnames(sim_area) <- c("SimUID", "area")

TLU_area <- merge(ruminants_sum,sim_area,by='SimUID',all.x=T)
TLU_per_kmsq <- cbind(TLU_area$SimUID, (TLU_area$value)/TLU_area$area) # get from TLU to LU by multiplying by 1000, get LU per km^2 by dividing by SU area
colnames(TLU_per_kmsq) <- c("SimUID", "LUperkmsq")

input_data_to_raster <- merge(SimUIDs_per_pix,TLU_per_kmsq,by='SimUID',all.x=T)
input_data_to_raster <- input_data_to_raster[order(input_data_to_raster$FID),]
new_raster <- simu_raster
values(new_raster) <- input_data_to_raster$LUperkmsq
plot(new_raster)

# plot raster with breaks in data to visualize better 
cbPallete <- c(brewer.pal(5, "YlOrRd"))
brks <- c(0, 20, 50, 100, 250, cellStats(new_raster,max))
cattle_pasture_breaks <- cut(new_raster, breaks=brks)
plot(cattle_pasture_breaks, col=cbPallete, legend = FALSE)
par(fig=c(0,1,0,1), oma=c(0, 0, 0, 0), mar=c(0,0,0,0), pty="s", new=TRUE)
plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
legend("bottom", fill=cbPallete, legend = c('0-20', '20-50', '50-100',
                                            '100-250', '>250'), ncol=4, bty="n")

setwd("/Volumes/Bube_Drive/Dropbox/Summer Fellowships/IIASA Summer Fellowship/Grasslands/outputs")
writeRaster(new_raster, filename="TLU_per_kmsq.hdr", format="ENVI", overwrite=T)

#  Rasterize feed requirements sum and plot
# -------------------------------------------
# Merge the two & sort back
input_data_to_raster <- merge(SimUIDs_per_pix,feedreq_sum,by='SimUID',all.x=T)

input_data_to_raster <- input_data_to_raster[order(input_data_to_raster$FID),]

# plot new info
new_raster <- simu_raster
values(new_raster) <- input_data_to_raster$value # NEED TO make sure the data is organised in the same order, same length. SimID is not spacially explicit! 
plot(new_raster)

# Calculate total amount consumed by ruminats
# -------------------------------------------
#livestock_numbers_ruminants$animal <- substr(livestock_numbers_ruminants$animal, 1,4)
combi <- merge(livestock_numbers_ruminants,feedreq_tDM_TLU_yr, by=c('SimUID', "animal", "syst"))
total_biomass <- as.data.frame(cbind(combi$SimUID, (combi$value.x*combi$value.y)))
colnames(total_biomass) <- c("SimUID", "value")

total_sum <- aggregate(total_biomass$value, by=list(total_biomass$SimUID), FUN=sum)
colnames(total_sum) <- c("SimUID", "value")
total_sum <- merge(total_sum,sim_area, by='SimUID',all.x=T)

total_consumed <- cbind(total_sum$SimUID,(total_sum$value/total_sum$area))
colnames(total_consumed) <- c("SimUID", "tDM_TLU")

# make into a raster
input_data_to_raster <- merge(SimUIDs_per_pix,total_consumed,by='SimUID',all.x=T)
input_data_to_raster <- input_data_to_raster[order(input_data_to_raster$FID),]
new_raster <- simu_raster
values(new_raster) <- input_data_to_raster$tDM_TLU
plot(new_raster)

new_raster_SA <- crop(new_raster, extent(sa))
plot(new_raster_SA)
plot(sa, add=T, lty=1, lwd=0.5)


# plot raster with breaks in data to visualize better (tonnes of biomass)
cbPallete <- c(brewer.pal(4, "YlGn"))
brks <- c(0, 50, 150, 250, cellStats(new_raster_SA,max))
cattle_pasture_breaks <- cut(new_raster_SA, breaks=brks)
plot(cattle_pasture_breaks, col=cbPallete, legend = FALSE)
plot(sa, add=T, lty=1, lwd=0.5)
par(fig=c(0,1,0,1), oma=c(0, 0, 0, 0), mar=c(0,0,0,0), pty="s", new=TRUE)
plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
legend("bottom", fill=cbPallete, legend = c('0-50', '50-150', '150-250',
                                            '>500'), ncol=4, bty="n")

setwd("/Volumes/Bube_Drive/Dropbox/Summer Fellowships/IIASA Summer Fellowship/Grasslands/outputs")
writeRaster(new_raster, filename="tDM_yr.hdr", format="ENVI", overwrite=T)

# in case there were 2 variables or more, make sur you select only ONE variable for the merge
#livestock_numbers_bovines_2 <- livestock_numbers_bovines
#livestock_numbers_bovines_2$var <- 'toto'
#livestock_numbers_bovines <- rbind.data.frame(livestock_numbers_bovines,livestock_numbers_bovines_2)

# input_data_to_raster <- merge(SimUIDs_per_pix,livestock_numbers_bovines[which(livestock_numbers_bovines$animal=='BOVO')],by='SimUID',all.x=T)
# input_data_to_raster <- input_data_to_raster[order(input_data_to_raster$FID),]
# 
# new_raster <- simu_raster; values(new_raster) <- input_data_to_raster$value
# plot(new_raster)

#input_data_to_raster_rf <- dcast(input_data_to_raster, FID ~ SimUID + var)

#example of the importance of the order:
# values(new_raster) <- rev(values(new_raster))
# plot(new_raster)