library(CCA)
library(raster)
library(maptools)
library(RColorBrewer)

# read in South America boundary 
data(wrld_simpl)
SA <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", 
        "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")
sa <- wrld_simpl[which(wrld_simpl@data$NAME %in% SA),]


# set command arguments for qsub 
library(argparse)
arg_parser <- ArgumentParser()
arg_parser$add_argument("-time", type="character", default="monthly") # set up time step - monthly, or seasonal 
arg_parser$add_argument("-type", type="character", default="vals") #set up type of data - observations or anomalies 
arg_parser$add_argument("-lag", type="character", default="lag4")
args <- arg_parser$parse_args()


# Read in climate data, evi and thermal regions
#precip_monthly_anomalies <- read.csv(file="/home/paulo/GE712/outputs/precip_monthly_anomalies.csv")
precip <- read.csv(file=sprintf("/projectnb/modislc/users/rkstan/GE712/outputs/precip_%s_%s.csv", args$time, args$type))
temp <- read.csv(file=sprintf("/projectnb/modislc/users/rkstan/GE712/outputs/temp_%s_%s.csv", args$time, args$type))
evi <- read.csv(file=sprintf("/projectnb/modislc/users/rkstan/GE712/outputs/EVI_%s_%s.csv", args$time, args$type))
thermal_regions <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/thermal_regions_vals.csv")

precip_spi <- read.csv(file=sprintf("/projectnb/modislc/users/rkstan/GE712/outputs/spi_%s_%s_%s.csv", args$time, args$type, args$lag))
precip_spi_t <- t(precip_spi)

# Merge and remove rows with at least one NA
total <- cbind(evi, precip, temp, thermal_regions)
filtered = na.omit(total)
total_filtered <- cbind(filtered[,1:156], precip_spi_t, filtered[,313:468], filtered[,469])
#total_filtered <- cbind(filtered[,1:156], filtered[,157:312] , filtered[,313:468], filtered[,469])
total_filtered_t <- t(total_filtered)

total_filtered_zone1 <- total_filtered[total_filtered[,469]==1,]
total_filtered_t_zone1 <- t(total_filtered_zone1)
total_filtered_zone2 <- total_filtered[total_filtered[,469]==2,]
total_filtered_t_zone2 <- t(total_filtered_zone2)
total_filtered_zone3 <- total_filtered[total_filtered[,469]==3,]
total_filtered_t_zone3 <- t(total_filtered_zone3)

setwd("/projectnb/modislc/users/rkstan/GE712/outputs")
run_canonical <- function(x, fname){
  total_month_EVI <- as.matrix(x[1:156,])
  total_month_precip <- as.matrix(x[157:312,])
  colnames(total_month_precip) <- paste("p", colnames(total_month_precip), sep="_")
  total_month_temp<- as.matrix(x[313:468,])
  colnames(total_month_temp) <- paste("t", colnames(total_month_temp), sep="_")
  total_month_other <- cbind(total_month_precip, total_month_temp)
  
  xy.cca.month.precip <- rcc(total_month_precip, total_month_EVI, 0.1982843, 0.1514255)
  xy.cor.month.precip <- matcor(total_month_precip, total_month_EVI)
  xy.cca.month.temp <- rcc(total_month_temp, total_month_EVI, 0.1982843, 0.1514255)
  xy.cor.month.temp <- matcor(total_month_temp, total_month_EVI)
  xy.cca.month.total <- rcc(total_month_other, total_month_EVI, 0.1982843, 0.1514255)
  xy.cor.month.total <- matcor(total_month_other, total_month_EVI)
    
  x.cf.month=as.matrix(total_month_precip)%*%xy.cca.month.precip$xcoef        # compute canonical factors on X
  y.cf.month=as.matrix(total_month_EVI)%*%xy.cca.month.precip$ycoef      # compute canonical factors on Y (only use 1st 3!)
  round(cor(x.cf.month,y.cf.month),2)                  # Ok?
  
  png(fname, width=1000, height = 1000)
  plot(seq(1,156), x.cf.month[,1], ylim=c(-10,10), type="l", xlab="Time", ylab="Canonical Factor", lwd=1.25)
  lines(seq(1,156), y.cf.month[,1], col="red", lty=2, lwd=1.25)
  legend("topright", 9.5, c("Precip", "EVI"), lty=c(1,2), lwd=c(1.25, 1.25), col=c("black", "red"))     
  dev.off()
  
  output_list <- list(total_month_precip, total_month_temp, total_month_other,
                      xy.cca.month.precip, xy.cor.month.precip,xy.cca.month.temp, 
                      xy.cor.month.temp,xy.cca.month.total, xy.cor.month.total)
  return(output_list)
  
}


total <- run_canonical(total_filtered_t, sprintf("canonical_factor_precip_%s_total.png", args$lag))
total_zone1 <- run_canonical(total_filtered_t_zone1, sprintf("canonical_factor_precip_%s_zone1.png", args$lag))
total_zone2 <- run_canonical(total_filtered_t_zone2, sprintf("canonical_factor_precip_%s_zone2.png", args$lag))
total_zone3 <- run_canonical(total_filtered_t_zone3, sprintf("canonical_factor_precip_%s_zone3.png", args$lag))


# # read in seasonal anomalies ------------------------------------------------------------
#   
#   precip_seas_anomalies <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_seas_anomalies.csv")
#   temp_seas_anomalies <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/temp_seas_anomalies.csv")
#   EVI_seas_anomalies <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_seas_anomalies.csv")
#   total_seas_original <- cbind(EVI_seas_anomalies, precip_seas_anomalies, temp_seas_anomalies)
#   total_seas_original_noNA <- na.omit(total_seas_original)
#   total_seas_original_noNA_t <- t(total_seas_original_noNA)
#   
#   total_seas_EVI <- as.data.frame(total_seas_original_noNA_t[1:52,])
#   total_seas_precip <- as.data.frame(total_seas_original_noNA_t[53:104,])
#   colnames(total_seas_precip) <- paste("p", colnames(total_seas_precip), sep="_")
#   total_seas_temp <- as.data.frame(total_seas_original_noNA_t[105:156,])
#   colnames(total_seas_temp) <- paste("t", colnames(total_seas_temp), sep="_")
#   total_seas_other <- cbind(total_seas_precip, total_seas_temp)
#   # 
#   # EVI_vector <- t(EVI_monthly_anomalies)
#   # total <- cbind(t(EVI_monthly_anomalies), t(precip_monthly_anomalies), t(temp_monthly_anomalies))
#   # ind <- apply(total, 2, function(total) all(is.na(total)))
#   # f <- total[,!ind]
#   # total_sample <- total[sample(1:nrow(total),5000,replace=FALSE),]
#   
#   )                  # Ok?
# 
# xy.cca.seas.precip <- rcc(total_seas_precip, total_seas_EVI, 0.1982843, 0.1514255)
# xy.cor.seas.precip <- matcor(total_seas_precip, total_seas_EVI)
# xy.cca.seas.temp <- rcc(total_seas_temp, total_seas_EVI, 0.1982843, 0.1514255)
# xy.cor.seas.temp <- matcor(total_seas_temp, total_seas_EVI)
# xy.cca.seas.total <- rcc(total_seas_other, total_seas_EVI, 0.1982843, 0.1514255)
# xy.cor.seas.total <- matcor(total_seas_other, total_seas_EVI)
# 
# }
# 
# x.cf.seas=as.matrix(total_seas_other)%*%xy.cca.seas.total$xcoef        # compute canonical factors on X
# y.cf.seas=as.matrix(total_seas_EVI)%*%xy.cca.seas.total$ycoef      # compute canonical factors on Y (only use 1st 3!)
# round(cor(x.cf.seas,y.cf.seas),2)                  # Ok?
# 
# plot(seq(1,52), x.cf.seas[,1], type="l", xlab="Time", ylab="Canonical Factor", lwd=1.25)
# lines(seq(1,52), x.cf.seas[,1], col="red", lty=2, lwd=1.25)
# legend("topright", 9.5, c("Climate", "EVI"), lty=c(1,2), lwd=c(1.25, 1.25), col=c("black", "red"))
# 
# 
# precip_seas_anomalies_lag1 <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_seas_anomalies_lag1.csv")
# temp_seas_anomalies_lag1 <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/temp_seas_anomalies_lag1.csv")
# EVI_seas_anomalies_lag1 <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_seas_anomalies_lag1.csv")
# total_seas_lag1 <- cbind(EVI_seas_anomalies_lag1, precip_seas_anomalies_lag1, temp_seas_anomalies_lag1)
# total_seas_lag1_noNA <- na.omit(total_seas_lag1)
# total_seas_lag1_noNA_t <- t(total_seas_lag1_noNA)
# 
# total_seas_EVI_lag1 <- as.data.frame(total_seas_lag1_noNA_t[1:51,])
# total_seas_precip_lag1 <- as.data.frame(total_seas_lag1_noNA_t[52:102,])
# colnames(total_seas_precip_lag1) <- paste("p", colnames(total_seas_precip_lag1), sep="_")
# total_seas_temp_lag1 <- as.data.frame(total_seas_lag1_noNA_t[103:153,])
# colnames(total_seas_temp_lag1) <- paste("t", colnames(total_seas_temp_lag1), sep="_")
# total_seas_other_lag1 <- cbind(total_seas_precip_lag1, total_seas_temp_lag1)
# 
# xy.cca.seas.precip.lag1 <- rcc(total_seas_precip_lag1, total_seas_EVI_lag1, 0.1982843, 0.1514255)
# xy.cor.seas.precip.lag1 <- matcor(total_seas_precip_lag1, total_seas_EVI_lag1)
# xy.cca.seas.temp.lag1 <- rcc(total_seas_temp_lag1, total_seas_EVI_lag1, 0.1982843, 0.1514255)
# xy.cor.seas.temp.lag1 <- matcor(total_seas_temp_lag1, total_seas_EVI_lag1)
# xy.cca.seas.total.lag1 <- rcc(total_seas_other_lag1, total_seas_EVI_lag1, 0.1982843, 0.1514255)
# xy.cor.seas.total.lag1 <- matcor(total_seas_other_lag1, total_seas_EVI_lag1)
# 
# x.cf.seas.lag1=as.matrix(total_seas_other_lag1)%*%xy.cca.seas.total.lag1$xcoef        # compute canonical factors on X
# y.cf.seas.lag1=as.matrix(total_seas_EVI_lag1)%*%xy.cca.seas.total.lag1$ycoef      # compute canonical factors on Y (only use 1st 3!)
# round(cor(x.cf.seas.lag1,y.cf.seas.lag1),2)                  # Ok?
# 

# read in yearly anomalies ------------------------------------------------------------

# precip_year_anomalies <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/precip_year_anomalies.csv")
# temp_year_anomalies <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/temp_year_anomalies.csv")
# EVI_year_anomalies <- read.csv(file="/projectnb/modislc/users/rkstan/GE712/outputs/EVI_year_anomalies.csv")
# total_year_original <- cbind(EVI_year_anomalies, precip_year_anomalies, temp_year_anomalies)
# total_year_original_noNA <- na.omit(total_year_original)
# total_year_original_noNA_t <- t(total_year_original_noNA)
# 
# total_year_EVI <- as.data.frame(total_year_original_noNA_t[1:13,])
# total_year_precip <- as.data.frame(total_year_original_noNA_t[14:26,])
# colnames(total_year_precip) <- paste("p", colnames(total_year_precip), sep="_")
# total_year_temp <- as.data.frame(total_year_original_noNA_t[27:39,])
# colnames(total_year_temp) <- paste("t", colnames(total_year_temp), sep="_")
# total_year_other <- cbind(total_year_precip, total_year_temp)
# 
# 
# xy.cca.year.precip <- rcc(total_year_precip, total_year_EVI, 0.1982843, 0.1514255)
# xy.cor.year.precip <- matcor(total_year_precip, total_year_EVI)
# xy.cca.year.temp <- rcc(total_year_temp, total_year_EVI, 0.1982843, 0.1514255)
# xy.cor.year.temp <- matcor(total_year_temp, total_year_EVI)
# xy.cca.year.total <- rcc(total_year_other, total_year_EVI, 0.1982843, 0.1514255)
# xy.cor.year.total <- matcor(total_year_other, total_year_EVI)
#   
# x.cf.year=as.matrix(total_year_other)%*%xy.cca.year.total$xcoef        # compute canonical factors on X
# y.cf.year=as.matrix(total_year_EVI)%*%xy.cca.year.total$ycoef      # compute canonical factors on Y (only use 1st 3!)
# round(cor(x.cf.year,y.cf.year),2)                  # Ok?
# 
# plot(x.cf.year[,1],y.cf.year[,1],xlab="First Canonical Factor in X",    # look at some plots!
#      ylab="First Canonical Factor in Y", cex.lab=1.25)
# lmfit <- lm(y.cf.year[,1]~x.cf.year[,1])
# 
# plot(seq(1,13), x.cf.year[,1], type="l", xlab="Time", ylab="Canonical Factor", lwd=1.25)
# lines(seq(1,13), x.cf.year[,1], col="red", lty=2, lwd=1.25)
# legend("topright", 9.5, c("Climate", "EVI"), lty=c(1,2), lwd=c(1.25, 1.25), col=c("black", "red"))

#plot -----------------------------------------

# make an empty raster with south america extent
south_am <- raster(nrows=138, ncols=94,crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",xmn=-81.5, xmx=-34.5, ymn=-56.5, ymx=12.5)


# plot function
plot_cor_xy <- function(x, score){
  south_am[as.numeric(x$names$Ynames)] <- score
  plot(south_am,col=rev(brewer.pal(6, "RdBu")),
       cex.axis=1, cex.lab=1.5, legend=FALSE, axes=F, box=F) 
  plot(sa, add=T,lty=1,lwd=0.5)
  plot(south_am, col=rev(brewer.pal(6, "RdBu")),legend.only=TRUE, legend.width=1, legend.shrink=0.85,legend.args=list("Correlation Coefficient (r)", side=4, line=2.5, cex=1.5))
  
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
plot_cor_xy(total[[4]], total[[4]]$scores$corr.X.yscores[,1])
plot_cor_yx(total[[4]], total[[4]]$scores$scores$corr.Y.xscores[,1])
plot_cor_xy(total[[6]], total[[6]]$scores$corr.X.yscores[,1])
plot_cor_yx(total[[6]], total[[6]]$scores$corr.Y.xscores[,1])
plot_cor_xy(total[[8]], total[[8]]$scores$corr.X.yscores[1:511,1])
plot_cor_yx(total[[8]], total[[8]]$scores$corr.Y.xscores[1:511,1])

par(mfrow=c(1,3))
plot_cor_xy(total[[4]], total[[5]]$XYcor[1:511,1])
plot_cor_xy(total[[6]], total[[7]]$XYcor[1:511,1])
plot_cor_xy(total[[8]], total[[9]]$XYcor[1:511,1])

# analysis split up by zone 
par(mfrow=c(2,3))
plot_cor_xy(total_zone1[[4]], total_zone1[[4]]$scores$corr.X.yscores[,1])
plot_cor_yx(total_zone1[[4]], total_zone1[[4]]$scores$scores$corr.Y.xscores[,1])
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
plot_cor_yx(total_zone2[[4]], total_zone2[[4]]$scores$scores$corr.Y.xscores[,1])
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
plot_cor_yx(total_zone3[[4]], total_zone3[[4]]$scores$scores$corr.Y.xscores[,1])
plot_cor_xy(total_zone3[[6]], total_zone3[[6]]$scores$corr.X.yscores[,1])
plot_cor_yx(total_zone3[[6]], total_zone3[[6]]$scores$corr.Y.xscores[,1])
plot_cor_xy(total_zone3[[8]], total_zone3[[8]]$scores$corr.X.yscores[1:65,1])
plot_cor_yx(total_zone3[[8]], total_zone3[[8]]$scores$corr.Y.xscores[1:65,1])

par(mfrow=c(1,3))
plot_cor_xy(total_zone3[[4]], total_zone3[[5]]$XYcor[1:65,1])
plot_cor_xy(total_zone3[[6]], total_zone3[[7]]$XYcor[1:65,1])
plot_cor_xy(total_zone3[[8]], total_zone3[[9]]$XYcor[1:65,1])

# # seasonal  
# par(mfrow=c(2,3))
# plot_cor_xy(xy.cca.seas.precip, xy.cca.seas.precip$scores$corr.X.yscores[,1])
# plot_cor_yx(xy.cca.seas.precip, xy.cca.seas.precip$scores$corr.Y.xscores[,1])
# plot_cor_xy(xy.cca.seas.temp, xy.cca.seas.temp$scores$corr.X.yscores[,1])
# plot_cor_yx(xy.cca.seas.temp, xy.cca.seas.temp$scores$corr.Y.xscores[,1])
# plot_cor_xy(xy.cca.seas.total, xy.cca.seas.total$scores$corr.X.yscores[1:2530,1])
# plot_cor_yx(xy.cca.seas.total, xy.cca.seas.total$scores$corr.Y.xscores[1:2530,1])
# 
# par(mfrow=c(1,3))
# plot_cor_xy(xy.cca.seas.precip, xy.cor.seas.precip$XYcor[1:2530,1])
# plot_cor_xy(xy.cca.seas.temp, xy.cor.seas.temp$XYcor[1:2530,1])
# plot_cor_xy(xy.cca.seas.total, xy.cor.seas.total$XYcor[1:2530,1])
# 
# # seasonal lagged!!  
# par(mfrow=c(2,3))
# plot_cor_xy(xy.cca.seas.precip.lag1, xy.cca.seas.precip.lag1$scores$corr.X.yscores[,1])
# plot_cor_yx(xy.cca.seas.precip.lag1, xy.cca.seas.precip.lag1$scores$corr.Y.xscores[,1])
# plot_cor_xy(xy.cca.seas.temp.lag1, xy.cca.seas.temp.lag1$scores$corr.X.yscores[,1])
# plot_cor_yx(xy.cca.seas.temp.lag1, xy.cca.seas.temp.lag1$scores$corr.Y.xscores[,1])
# plot_cor_xy(xy.cca.seas.total.lag1, xy.cca.seas.total.lag1$scores$corr.X.yscores[1:2530,1])
# plot_cor_yx(xy.cca.seas.total.lag1, xy.cca.seas.total.lag1$scores$corr.Y.xscores[1:2530,1])
# 
# par(mfrow=c(1,3))
# plot_cor_xy(xy.cca.seas.precip.lag1, xy.cor.seas.precip.lag1$XYcor[1:2530,1])
# plot_cor_xy(xy.cca.seas.temp.lag1, xy.cor.seas.temp.lag1$XYcor[1:2530,1])
# plot_cor_xy(xy.cca.seas.total.lag1, xy.cor.seas.total.lag1$XYcor[1:2530,1])
# 
# # # annual
# # par(mfrow=c(2,3))
# # plot_cor_xy(xy.cca.year.precip, xy.cca.year.precip$scores$corr.X.yscores[,1])
# # plot_cor_yx(xy.cca.year.precip, xy.cca.year.precip$scores$corr.Y.xscores[,1])
# # plot_cor_xy(xy.cca.year.temp, xy.cca.year.temp$scores$corr.X.yscores[,1])
# # plot_cor_yx(xy.cca.year.temp, xy.cca.year.temp$scores$corr.Y.xscores[,1])
# # plot_cor_xy(xy.cca.year.total, xy.cca.year.total$scores$corr.X.yscores[1:2553,1])
# # plot_cor_yx(xy.cca.year.total, xy.cca.year.total$scores$corr.Y.xscores[1:2553,1])
# # 
# # par(mfrow=c(1,3))
# # plot_cor_xy(xy.cca.year.precip, xy.cor.seas.precip$XYcor[1:2553,1])
# # plot_cor_xy(xy.cca.year.temp, xy.cor.seas.temp$XYcor[1:2553,1])
# # plot_cor_xy(xy.cca.year.total, xy.cor.seas.total$XYcor[1:2553,1])
# 

# extras ----------------------------------------
xy.cca.years <- cc(total_precip, t(f_1))
reg <- estim.regul(total_year_precip, total_year_EVI)
names(xy.cca)

xy.cca$cor
xy.cca$xcoef
xy.cca$ycoef

par(mfrow=c(2,1)) #plot the coefs for 1st canonical pair of canonical factors 
barplot(xy.cca$xcoef[,1], ylab="First climate loadings", cex.lab=1.25)
barplot(xy.cca$ycoef[,1], ylab="First EVI loadings", cex.lab=1.25)

plot(x.cf[,1],y.cf[,1],xlab="Second Canonical Factor in X",
     ylab="Second Canonical Factor in Y")
plot(x.cf.year[,1],x.cf.year[,2],xlab="Second Canonical Factor in X",    # look at some plots!
     ylab="First Canonical Factor in X")
