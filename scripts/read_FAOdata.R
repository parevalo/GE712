library(raster)
library(RColorBrewer)
setwd("/projectnb/modislc/users/rkstan/GE712/data/FAO/")

#read in livestock production systems 
lps_sa <- raster("livestock_production_system_pastures.tif")
lps_sa_full <- raster("livestock_production_system_SA.envi")

# plot livestock production systems (this plot does not work for pasture subset)
cbPallete <- c(brewer.pal(12, "Paired"), "black","grey")
brks <- seq(0,14,1)
lps_sa_breaks <- cut(lps_sa, breaks=brks)
plot(lps_sa_breaks, col=cbPallete, legend = FALSE)
plot(sa, add=T)
par(fig=c(0,1,0,1), oma=c(0, 0, 0, 0), mar=c(0,0,0,0), pty="s", new=TRUE)
plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
legend("top", fill=cbPallete, bty="n",legend = c('LGY', 'LGA', 'LGH', 'LGT',
                                            'MRY', 'MRA', 'MRH', 'MRT', 
                                            'MIY', 'MIA', 'MIH', 'MIT', 'Urban', 'Other'), ncol=4)

# aggregate LPS based on management 
fun1 <- function(x) {x[x==1 | x==2 |x==3| x==4] <- 1; return(x)}
lps_sa_1 <-  calc(lps_sa, fun1)
fun2 <- function(x) {x[x==5| x==6 | x==7 | x==8] <- 2; return(x)}
lps_sa_2 <-  calc(lps_sa_1, fun2)
fun3 <- function(x) {x[x==9| x==10 | x==11 | x==12] <- 3; return(x)}
lps_sa_3 <-  calc(lps_sa_2, fun3)
fun4 <- function(x) {x[x==13 | x==14] <- 4; return(x)}
lps_sa_4 <-  calc(lps_sa_3, fun4)


writeRaster(lps_sa_4, file= "livestock_production_system_4class.hdr", format="ENVI", overwrite=TRUE)
lps_resampled <- raster("livestock_production_system_pastures_005.tif")
lps_resampled_vals <- getValues(lps_resampled)
write.csv(lps_resampled_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/lps_resampled_vals.csv", quote=FALSE, row.names=FALSE)


# plot aggregated LPS 
cbPallete <- c(brewer.pal(3, "Paired"), "dark grey")
brks <- c(0,1,2,3,4)
lps_sa_breaks <- cut(lps_resampled_sub, breaks=brks)
plot(lps_sa_breaks, col=cbPallete, colNA="grey",legend = FALSE)
plot(sa, add=T)
par(fig=c(0,1,0,1), oma=c(0, 0, 0, 0), mar=c(0,0,0,0), pty="s", new=TRUE)
plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
legend("bottom", fill=cbPallete, bty="n",legend = c('Rangeland-based', 'Mixed rainfed', 'Mixed irrigated', 'Other'),
                                               horiz=T)

# aggregate based on agroecological zones
fun1 <- function(x) {x[x==1 | x==5 |x==9] <- 1; return(x)}
lps_sa_1 <-  calc(lps_sa, fun1)
fun2 <- function(x) {x[x==2| x==6 | x==10] <- 2; return(x)}
lps_sa_2 <-  calc(lps_sa_1, fun2)
fun3 <- function(x) {x[x==3| x==7 | x==11] <- 3; return(x)}
lps_sa_3 <-  calc(lps_sa_2, fun3)
fun4 <- function(x) {x[x==4 | x==8 | x==12] <- 4; return(x)}
lps_sa_4 <-  calc(lps_sa_3, fun4)
fun5 <- function(x) {x[x==13 | x==14] <- 5; return(x)}
lps_sa_5 <-  calc(lps_sa_4, fun5)


writeRaster(lps_sa_5, file= "livestock_production_system_5class.hdr", format="ENVI", overwrite=TRUE)
lps_resampled <- raster("livestock_production_system_pastures_zones_005.tif")
lps_resampled_vals <- getValues(lps_resampled)
write.csv(lps_resampled_vals, file="/projectnb/modislc/users/rkstan/GE712/outputs/lps_resampled_vals_zones.csv", quote=FALSE, row.names=FALSE)

# plot LPS based on agroecological zones
cbPallete <- c(brewer.pal(4, "Paired"), "grey")
brks <- c(0,1,2,3,4,5)
lps_sa_breaks <- cut(lps_resampled, breaks=brks)
plot(lps_sa_breaks, col=cbPallete, legend = FALSE)
plot(sa, add=T)
par(fig=c(0,1,0,1), oma=c(0, 0, 0, 0), mar=c(0,0,0,0), pty="s", new=TRUE)
plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
legend("bottom", fill=cbPallete, bty="n",legend = c('Hyperarid', 'Arid/Semi-arid', 'Humid/Subhumid', 'Temperate/tropical highlands', 'Other'),
       ncol=3)

# read in livestock observations 
LU_observed <- raster("livestock_density_observed_SA.envi")

cbPallete <- c(brewer.pal(5, "YlOrRd"))
brks <- c(0, 20, 50, 100, 250, cellStats(LU_observed,max))
cattle_pasture_breaks <- cut(LU_observed, breaks=brks)
plot(cattle_pasture_breaks, col=cbPallete, legend = FALSE)
map(add=TRUE)
plot(sa, add=T)
par(fig=c(0,1,0,1), oma=c(0, 0, 0, 0), mar=c(0,0,0,0), pty="s", new=TRUE)
plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
legend("bottom", fill=cbPallete, legend = c('0-20', '20-50', '50-100', 
                                            '100-250', '>250'), ncol=4)