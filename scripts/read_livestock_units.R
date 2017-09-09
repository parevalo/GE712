library(raster)
library(RColorBrewer)
library(maptools)

# read in South America boundary 
data(wrld_simpl)
SA <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", 
        "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")
sa <- wrld_simpl[which(wrld_simpl@data$NAME %in% SA),]

cattle_2006 <- raster("/projectnb/modislc/users/rkstan/pasturelands/livestock.units/CATTLE/Glb_Cattle_CC2006_AD.tif")
cattle_SA <- raster("/projectnb/modislc/users/rkstan/pasturelands/livestock.units/CATTLE/Glb_Cattle_CC2006_AD_SA.tif")
cattle_pasture <- raster("/projectnb/modislc/users/rkstan/pasturelands/livestock.units/CATTLE/Glb_Cattle_pasture.tif")


cbPallete <- c(brewer.pal(5, "YlOrRd"))
brks <- c(0, 20, 50, 100, 250, cellStats(cattle_pasture,max))
cattle_pasture_breaks <- cut(cattle_pasture, breaks=brks)
plot(cattle_pasture_breaks, col=cbPallete, legend = FALSE)
map(add=TRUE)
plot(sa, add=T)
par(fig=c(0,1,0,1), oma=c(0, 0, 0, 0), mar=c(0,0,0,0), pty="s", new=TRUE)
plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
legend("bottom", fill=cbPallete, legend = c('0-20', '20-50', '50-100', 
                                            '100-250', '>250'), ncol=4)