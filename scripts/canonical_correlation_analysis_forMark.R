########################################################
# Canonical correlation analysis monthly 
# created by Radost Stanimirova May 2017
########################################################

# load libraries 
library(CCA)
library(raster)
library(maptools)
library(RColorBrewer)

# read in South America boundary 
data(wrld_simpl)
SA <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", 
        "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")
sa <- wrld_simpl[which(wrld_simpl@data$NAME %in% SA),]

# read in monthly anomalies ------------------------------------------------------------

precip <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_monthly_anomalies.csv")
temp <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/temp_monthly_anomalies.csv")
evi <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_monthly_anomalies.csv")
thermal_regions <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/thermal_regions_vals.csv")

# read in SPI
precip_spi <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/spi_monthly_vals_lag2.csv")
precip_spi_t <- t(precip_spi)

# remove any row that contains NAs ------------------------------------------------------------

# Merge and remove rows with at least one NA
total <- cbind(evi, precip, temp, thermal_regions)
filtered = na.omit(total)
total_filtered <- cbind(filtered[,1:156], precip_spi_t, filtered[,313:468], filtered[,469])
#total_filtered <- cbind(filtered[,1:156], filtered[,157:312] , filtered[,313:468], filtered[,469])
total_filtered_t <- t(total_filtered)

# split up the analysis by zone (zone 1 tropical, zone 2 subtropical, zone 3 temperate)
total_filtered_zone1 <- total_filtered[total_filtered[,469]==1,]
total_filtered_t_zone1 <- t(total_filtered_zone1)
total_filtered_zone2 <- total_filtered[total_filtered[,469]==2,]
total_filtered_t_zone2 <- t(total_filtered_zone2)
total_filtered_zone3 <- total_filtered[total_filtered[,469]==3,]
total_filtered_t_zone3 <- t(total_filtered_zone3)

# run canonical correlation analysis (rcc) and correlation (matcor)
# rcc function is an extension of CCA when number of columns > number of rows
# matcor function computes the correlation matrices within and between two datasets 

setwd("/projectnb/modislc/users/rkstan/GE712/outputs")
run_canonical <- function(x, fname){
  total_month_EVI <- as.matrix(x[1:156,])   # get just evi 
  total_month_precip <- as.matrix(x[157:312,])  # get just precipitation 
  colnames(total_month_precip) <- paste("p", colnames(total_month_precip), sep="_")
  total_month_temp<- as.matrix(x[313:468,]) # get just temperature 
  colnames(total_month_temp) <- paste("t", colnames(total_month_temp), sep="_")
  total_month_other <- cbind(total_month_precip, total_month_temp)
  
  # calculate canonical correlation for precip, temperature and combined precipitation and  temperature
  xy.cca.month.precip <- rcc(total_month_precip, total_month_EVI, 0.1982843, 0.1514255)
  xy.cor.month.precip <- matcor(total_month_precip, total_month_EVI)
  xy.cca.month.temp <- rcc(total_month_temp, total_month_EVI, 0.1982843, 0.1514255)
  xy.cor.month.temp <- matcor(total_month_temp, total_month_EVI)
  xy.cca.month.total <- rcc(total_month_other, total_month_EVI, 0.1982843, 0.1514255)
  xy.cor.month.total <- matcor(total_month_other, total_month_EVI)
  
  x.cf.month=as.matrix(total_month_precip)%*%xy.cca.month.precip$xcoef        # compute canonical factors on X
  y.cf.month=as.matrix(total_month_EVI)%*%xy.cca.month.precip$ycoef      # compute canonical factors on Y (only use 1st 3!)
  round(cor(x.cf.month,y.cf.month),2)                  # Ok?
  
  # output plots of canonical factors
  png(fname, width=1000, height = 1000)
  plot(seq(1,156), x.cf.month[,1], ylim=c(-10,10), type="l", xlab="Time", ylab="Canonical Factor", lwd=1.25)
  lines(seq(1,156), y.cf.month[,1], col="red", lty=2, lwd=1.25)
  legend("topright", 9.5, c("Precip", "EVI"), lty=c(1,2), lwd=c(1.25, 1.25), col=c("black", "red"))     
  dev.off()
  
  # put all the outputs in a list 
  output_list <- list(total_month_precip, total_month_temp, total_month_other,
                      xy.cca.month.precip, xy.cor.month.precip,xy.cca.month.temp, 
                      xy.cor.month.temp,xy.cca.month.total, xy.cor.month.total)
  return(output_list)
  
}

# run the canonical correlation analysis by zone 
total <- run_canonical(total_filtered_t, "canonical_factor_precip_lag_total.png")
total_zone1 <- run_canonical(total_filtered_t_zone1, "canonical_factor_precip_lag4_zone1.png")
total_zone2 <- run_canonical(total_filtered_t_zone2, "canonical_factor_precip_lag4_zone2.png")
total_zone3 <- run_canonical(total_filtered_t_zone3, "canonical_factor_precip_lag4_zone3.png")

# plot results -----------------------------------------------------------------------
# make an empty raster with south america extent
south_am <- raster(nrows=138, ncols=94,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",xmn=-81.5, xmx=-34.5, ymn=-56.5, ymx=12.5)

# plot function
plot_cor_xy <- function(x, score){
  south_am[as.numeric(x$names$Ynames)] <- score  # assing the values to the cell numbers 
  plot(south_am,col=rev(brewer.pal(6, "RdBu")),
       cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
  plot(sa, add=T,lty=1,lwd=0.5)                 # add outline of South America
  plot(south_am, col=rev(brewer.pal(6, "RdBu")),legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("Correlation Coefficient (r)", side=4, line=2.9, cex=1.25))
  
}

plot_cor_yx <- function(x, score){
  south_am[as.numeric(x$names$Ynames)] <- score
  plot(south_am,col=rev(brewer.pal(6, "RdBu")),
       cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
  plot(sa, add=T,lty=1,lwd=0.5)
  plot(south_am, col=rev(brewer.pal(6, "RdBu")),legend.only=TRUE, legend.width=1, legend.shrink=1,legend.args=list("Correlation Coefficient (r)", side=4, line=2.9, cex=1.25))
  
}

# month 
par(mfrow=c(2,3))
plot_cor_xy(total[[4]], total[[4]]$scores$corr.X.yscores[,1]) # plot temperature vs EVI canonical variate
plot_cor_yx(total[[4]], total[[4]]$scores$corr.Y.xscores[,1]) # plot EVI vs temperature canonical variate 
plot_cor_xy(total[[6]], total[[6]]$scores$corr.X.yscores[,1]) # plot precip vs EVI canonical variate
plot_cor_yx(total[[6]], total[[6]]$scores$corr.Y.xscores[,1]) # plot EVI vs precip canonical variate 
plot_cor_xy(total[[8]], total[[8]]$scores$corr.X.yscores[1:511,1]) # plot combined climate vs EVI canonical variate
plot_cor_yx(total[[8]], total[[8]]$scores$corr.Y.xscores[1:511,1]) # plot EVI vs combined climate canonical variate

par(mfrow=c(1,3))
plot_cor_xy(total[[4]], total[[4]]$cor) # plot the canonical correlation between temp and evi 
plot_cor_xy(total[[6]], total[[6]]$cor) # plot the canonical correlation between precip and evi
plot_cor_xy(total[[8]], total[[8]]$cor) # plot the canonical correlation between combined precip and temp vs evi 

# analysis split up by zone 
par(mfrow=c(2,3))
plot_cor_xy(total_zone1[[4]], total_zone1[[4]]$scores$corr.X.yscores[,1])
plot_cor_yx(total_zone1[[4]], total_zone1[[4]]$scores$corr.Y.xscores[,1])
plot_cor_xy(total_zone1[[6]], total_zone1[[6]]$scores$corr.X.yscores[,1])
plot_cor_yx(total_zone1[[6]], total_zone1[[6]]$scores$corr.Y.xscores[,1])
plot_cor_xy(total_zone1[[8]], total_zone1[[8]]$scores$corr.X.yscores[1:196,1])
plot_cor_yx(total_zone1[[8]], total_zone1[[8]]$scores$corr.Y.xscores[1:196,1])

par(mfrow=c(1,3))
plot_cor_xy(total_zone1[[4]], total_zone1[[5]]$XYcor[1:196,1])
plot_cor_xy(total_zone1[[6]], total_zone1[[7]]$XYcor[1:196,1])
plot_cor_xy(total_zone1[[8]], total_zone1[[9]]$XYcor[1:196,1])

#zone 2
par(mfrow=c(2,3))
plot_cor_xy(total_zone2[[4]], total_zone2[[4]]$scores$corr.X.yscores[,1])
plot_cor_yx(total_zone2[[4]], total_zone2[[4]]$scores$corr.Y.xscores[,1])
plot_cor_xy(total_zone2[[6]], total_zone2[[6]]$scores$corr.X.yscores[,1])
plot_cor_yx(total_zone2[[6]], total_zone2[[6]]$scores$corr.Y.xscores[,1])
plot_cor_xy(total_zone2[[8]], total_zone2[[8]]$scores$corr.X.yscores[1:250,1])
plot_cor_yx(total_zone2[[8]], total_zone2[[8]]$scores$corr.Y.xscores[1:250,1])

par(mfrow=c(1,3))
plot_cor_xy(total_zone2[[4]], total_zone2[[5]]$XYcor[1:250,1])
plot_cor_xy(total_zone2[[6]], total_zone2[[7]]$XYcor[1:250,1])
plot_cor_xy(total_zone2[[8]], total_zone2[[9]]$XYcor[1:250,1])

#zone 3
par(mfrow=c(2,3))
plot_cor_xy(total_zone3[[4]], total_zone3[[4]]$scores$corr.X.yscores[,1])
plot_cor_yx(total_zone3[[4]], total_zone3[[4]]$scores$corr.Y.xscores[,1])
plot_cor_xy(total_zone3[[6]], total_zone3[[6]]$scores$corr.X.yscores[,1])
plot_cor_yx(total_zone3[[6]], total_zone3[[6]]$scores$corr.Y.xscores[,1])
plot_cor_xy(total_zone3[[8]], total_zone3[[8]]$scores$corr.X.yscores[1:65,1])
plot_cor_yx(total_zone3[[8]], total_zone3[[8]]$scores$corr.Y.xscores[1:65,1])

par(mfrow=c(1,3))
plot_cor_xy(total_zone3[[4]], total_zone3[[5]]$XYcor[1:65,1])
plot_cor_xy(total_zone3[[6]], total_zone3[[7]]$XYcor[1:65,1])
plot_cor_xy(total_zone3[[8]], total_zone3[[9]]$XYcor[1:65,1])


# average grid point correlation coefficients 
temp <- c(mean(total[[4]]$cor), mean(total_zone1[[4]]$cor),mean(total_zone2[[4]]$cor),mean(total_zone3[[4]]$cor))
barplot(temp, names.arg=c("total", "zone1", "zone2", "zone3"), main="Temp")

precip <- c(mean(total[[6]]$cor), mean(total_zone1[[6]]$cor), mean(total_zone2[[6]]$cor), mean(total_zone3[[6]]$cor))
barplot(precip, names.arg=c("total", "zone1", "zone2", "zone3"), main="Precip")

climate <- c(mean(total[[8]]$cor), mean(total_zone1[[8]]$cor), mean(total_zone2[[8]]$cor),mean(total_zone3[[8]]$cor))
barplot(climate, names.arg=c("total", "zone1", "zone2", "zone3"), main="Climate")
