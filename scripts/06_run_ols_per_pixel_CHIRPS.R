library(raster)
library(rgdal)
library(RColorBrewer)
library(maptools)

source("/projectnb/modislc/users/rkstan/GE712/GE712/scripts/apply_stack_parallel.R")
setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")
data(wrld_simpl)
SA = c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Peru",
       "Suriname", "Uruguay", "Venezuela")
sa = wrld_simpl[which(wrld_simpl@data$NAME %in% SA),]

# Read in stacks of evi, precip and temp, 13 layers each
djf_raster_zone1 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/CHIRPSdjf2_zone1.envi")
mam_raster_zone1 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/CHIRPSmam2_zone1.envi")
jja_raster_zone1 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/CHIRPSjja2_zone1.envi")
son_raster_zone1 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/CHIRPSson2_zone1.envi")

djf_raster_zone2 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/CHIRPSdjf2_zone2.envi")
mam_raster_zone2 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/CHIRPSmam2_zone2.envi")
jja_raster_zone2 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/CHIRPSjja2_zone2.envi")
son_raster_zone2 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/CHIRPSson2_zone2.envi")

djf_raster_zone3 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/CHIRPSdjf2_zone3.envi")
mam_raster_zone3 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/CHIRPSmam2_zone3.envi")
jja_raster_zone3 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/CHIRPSjja2_zone3.envi")
son_raster_zone3 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/CHIRPSson2_zone3.envi")

#v <- raster::getValues(x)
#i = which(!apply(v, 1, anyNA))
#aux = apply(v, 1, get_stat, args.list=list(nl=12))
#apply(v[i,], 1, get_stat, args.list=list(nl=12)) 

get_stat <- function(x, args.list){
  
  res <- rep(NA, args.list$nl)
  
  if (anyNA(x))
    return(res)
  
  m1 <- lm(x[13:24] ~ x[1:12] +  I(x[1:12]^2) + x[25:36] +  I(x[25:36]^2) + x[37:48])
  m1$coefficients[m1$coefficients>20 | m1$coefficients < (-20)] <- NA
  sm1 <- summary(m1)
  
  
  
  res <- c(intercept               = m1$coefficients[1], # Why the equation is different from the otehrs lm(x[1:13] ~ x[14:26]) ???
           beta_precip             = m1$coefficients[2], 
           beta_precip2            = m1$coefficients[3],
           beta_precip_lag1        = m1$coefficients[4],
           beta_precip2_lag1       = m1$coefficients[5],
           beta_evi_lag1           = m1$coefficients[6],
           
           rsquared                = sm1$r.squared,
           
           signif_precip           = sm1$coefficients[2,4],
           signif_precip2          = sm1$coefficients[3,4],
           signif_precip_lag1      = sm1$coefficients[4,4],
           signif_precip2_lag1     = sm1$coefficients[5,4],
           signif_evi_lag1         = sm1$coefficients[6,4]
           
  )
  
  return(res)
  
}  

# Get ALL coefficients per season
djf_coefs_zone1 = apply_stack_parallel(djf_raster_zone1, fun = get_stat, nl = 12)
mam_coefs_zone1 = apply_stack_parallel(mam_raster_zone1, fun = get_stat, nl = 12)
jja_coefs_zone1 = apply_stack_parallel(jja_raster_zone1, fun = get_stat, nl = 12)
son_coefs_zone1 = apply_stack_parallel(son_raster_zone1, fun = get_stat, nl = 12)

djf_coefs_zone2 = apply_stack_parallel(djf_raster_zone2, fun = get_stat, nl = 12)
mam_coefs_zone2 = apply_stack_parallel(mam_raster_zone2, fun = get_stat, nl = 12)
jja_coefs_zone2 = apply_stack_parallel(jja_raster_zone2, fun = get_stat, nl = 12)
son_coefs_zone2 = apply_stack_parallel(son_raster_zone2, fun = get_stat, nl = 12)

djf_coefs_zone3 = apply_stack_parallel(djf_raster_zone3, fun = get_stat, nl = 12)
mam_coefs_zone3 = apply_stack_parallel(mam_raster_zone3, fun = get_stat, nl = 12)
jja_coefs_zone3 = apply_stack_parallel(jja_raster_zone3, fun = get_stat, nl = 12)
son_coefs_zone3 = apply_stack_parallel(son_raster_zone3, fun = get_stat, nl = 12)

#Function to save a single figure with the coefficients/optimum with all the four seasons
setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")
save_maps = function(w, x, y, z,index, location, fname, ...){
  png(fname, width=1000, height = 1000)
  
  par(mfrow=c(2,2), mar=c(3,3,3,3))
  
  plot(w[[index]], col=brewer.pal(6, "RdBu"), legend=F, main=substr(unlist(strsplit(deparse(substitute(w)),split="_"))[1], 1, 3))
  plot(location, add=T, lty=1, lwd=0.5)
  plot(w[[index]], col=brewer.pal(6, "RdBu"), legend.only=T, legend.width=1, legend.shrink=1, side=4, cex=1.25)
  
  plot(x[[index]], col=brewer.pal(6, "RdBu"), legend=F, , main=substr(unlist(strsplit(deparse(substitute(x)),split="_"))[1], 1, 3))
  plot(location, add=T, lty=1, lwd=0.5)
  plot(x[[index]], col=brewer.pal(6, "RdBu"), legend.only=T, legend.width=1, legend.shrink=1, side=4)
  
  plot(y[[index]], col=brewer.pal(6, "RdBu"), legend=F, , main=substr(unlist(strsplit(deparse(substitute(y)),split="_"))[1], 1, 3))
  plot(location, add=T, lty=1, lwd=0.5)
  plot(y[[index]], col=brewer.pal(6, "RdBu"), legend.only=T, legend.width=1, legend.shrink=1, side=4)
  
  plot(z[[index]],col=brewer.pal(6, "RdBu"), legend=F, , main=substr(unlist(strsplit(deparse(substitute(z)),split="_"))[1], 1, 3))
  plot(location, add=T, lty=1, lwd=0.5)
  plot(z[[index]], col=brewer.pal(6, "RdBu"), legend.only=T, legend.width=1, legend.shrink=1, side=4)
  
  dev.off()
}


# Save for comparison
save_maps(djf_coefs_zone1, mam_coefs_zone1, jja_coefs_zone1, son_coefs_zone1, 2, sa, "precip_zone1.png")
save_maps(djf_coefs_zone1, mam_coefs_zone1, jja_coefs_zone1, son_coefs_zone1, 3, sa, "precip2_zone1.png")
save_maps(djf_coefs_zone1, mam_coefs_zone1, jja_coefs_zone1, son_coefs_zone1, 4, sa, "precip_zone1_lagged.png")
save_maps(djf_coefs_zone1, mam_coefs_zone1, jja_coefs_zone1, son_coefs_zone1, 5, sa, "precip2_zone1_lagged.png")
save_maps(djf_coefs_zone1, mam_coefs_zone1, jja_coefs_zone1, son_coefs_zone1, 6, sa, "evi_zone1_lagged.png")


save_maps(djf_coefs_zone1, mam_coefs_zone1, jja_coefs_zone1, son_coefs_zone1, 7, sa, "rsquared_zone1.png")

save_maps(djf_coefs_zone2, mam_coefs_zone2, jja_coefs_zone2, son_coefs_zone2, 2, sa, "precip_zone2.png")
save_maps(djf_coefs_zone2, mam_coefs_zone2, jja_coefs_zone2, son_coefs_zone2, 3, sa, "precip2_zone2.png")
save_maps(djf_coefs_zone2, mam_coefs_zone2, jja_coefs_zone2, son_coefs_zone2, 4, sa, "precip_zone2_lagged.png")
save_maps(djf_coefs_zone2, mam_coefs_zone2, jja_coefs_zone2, son_coefs_zone2, 5, sa, "precip2_zone2_lagged.png")
save_maps(djf_coefs_zone2, mam_coefs_zone2, jja_coefs_zone2, son_coefs_zone2, 6, sa, "evi_zone2_lagged.png")

save_maps(djf_coefs_zone2, mam_coefs_zone2, jja_coefs_zone2, son_coefs_zone2, 7, sa, "rsquared_zone2.png")

save_maps(djf_coefs_zone3, mam_coefs_zone3, jja_coefs_zone3, son_coefs_zone3, 2, sa, "precip_zone3.png")
save_maps(djf_coefs_zone3, mam_coefs_zone3, jja_coefs_zone3, son_coefs_zone3, 3, sa, "precip2_zone3.png")
save_maps(djf_coefs_zone3, mam_coefs_zone3, jja_coefs_zone3, son_coefs_zone3, 4, sa, "precip_zone3_lagged.png")
save_maps(djf_coefs_zone3, mam_coefs_zone3, jja_coefs_zone3, son_coefs_zone3, 5, sa, "precip2_zone3_lagged.png")
save_maps(djf_coefs_zone3, mam_coefs_zone3, jja_coefs_zone3, son_coefs_zone3, 6, sa, "evi_zone3_lagged.png")

save_maps(djf_coefs_zone3, mam_coefs_zone3, jja_coefs_zone3, son_coefs_zone3, 7, sa, "rsquared_zone3.png")

# Filter them by significance
filter_coefs = function(season_coefs){
  # Create masks with threshold for significance
  filter_signif = function(x){ x[x > 0.05] = NA; return(x)}
  #filter_outliers = function(x){ x[x>20 | x < (-20)] <- NA; return(x)}
  precip_mask = calc(season_coefs[[8]], filter_signif)
  precip2_mask = calc(season_coefs[[9]], filter_signif)
  precip_mask_lagged = calc(season_coefs[[10]], filter_signif)
  precip2_mask_lagged = calc(season_coefs[[11]], filter_signif)
  evi_mask_lagged = calc(season_coefs[[12]], filter_signif)
  
  
  # Filter actual coefficients using the masks we created
  precip_masked = mask(season_coefs[[2]], precip_mask)
  precip2_masked = mask(season_coefs[[3]], precip2_mask)
  precip_masked_lagged = mask(season_coefs[[4]], precip_mask_lagged)
  precip2_masked_lagged = mask(season_coefs[[5]], precip2_mask_lagged)
  evi_masked_lagged = mask(season_coefs[[6]], evi_mask_lagged)
  
  # Filter outliers
  #precip_masked = calc(precip_masked, filter_outliers)
  #precip2_masked = calc(precip2_masked, filter_outliers)
  
  return(list(precip_masked, precip2_masked, precip_masked_lagged, precip2_masked_lagged, evi_masked_lagged))
}

# filter coefs
djf_filtered_zone1 = filter_coefs(djf_coefs_zone1)
mam_filtered_zone1 = filter_coefs(mam_coefs_zone1)
jja_filtered_zone1 = filter_coefs(jja_coefs_zone1)
son_filtered_zone1 = filter_coefs(son_coefs_zone1)

djf_filtered_zone2 = filter_coefs(djf_coefs_zone2)
mam_filtered_zone2 = filter_coefs(mam_coefs_zone2)
jja_filtered_zone2 = filter_coefs(jja_coefs_zone2)
son_filtered_zone2 = filter_coefs(son_coefs_zone2)

djf_filtered_zone3 = filter_coefs(djf_coefs_zone3)
mam_filtered_zone3 = filter_coefs(mam_coefs_zone3)
jja_filtered_zone3 = filter_coefs(jja_coefs_zone3)
son_filtered_zone3 = filter_coefs(son_coefs_zone3)

# Save those plots!
save_maps(djf_filtered_zone1, mam_filtered_zone1, jja_filtered_zone1, son_filtered_zone1, 1, sa, "precip_signif005_zone1.png")
save_maps(djf_filtered_zone1, mam_filtered_zone1, jja_filtered_zone1, son_filtered_zone1, 2, sa, "precip2_signif005_zone1.png")
save_maps(djf_filtered_zone1, mam_filtered_zone1, jja_filtered_zone1, son_filtered_zone1, 3, sa, "precip_signif005_zone1_lagged.png")
save_maps(djf_filtered_zone1, mam_filtered_zone1, jja_filtered_zone1, son_filtered_zone1, 4, sa, "precip2_signif005_zone1_lagged.png")
save_maps(djf_filtered_zone1, mam_filtered_zone1, jja_filtered_zone1, son_filtered_zone1, 5, sa, "evi_signif005_zone1_lagged.png")

save_maps(djf_filtered_zone2, mam_filtered_zone2, jja_filtered_zone2, son_filtered_zone2, 1, sa, "precip_signif005_zone2.png")
save_maps(djf_filtered_zone2, mam_filtered_zone2, jja_filtered_zone2, son_filtered_zone2, 2, sa, "precip2_signif005_zone2.png")
save_maps(djf_filtered_zone2, mam_filtered_zone2, jja_filtered_zone2, son_filtered_zone2, 3, sa, "precip_signif005_zone2_lagged.png")
save_maps(djf_filtered_zone2, mam_filtered_zone2, jja_filtered_zone2, son_filtered_zone2, 4, sa, "precip2_signif005_zone2_lagged.png")
save_maps(djf_filtered_zone2, mam_filtered_zone2, jja_filtered_zone2, son_filtered_zone2, 5, sa, "evi_signif005_zone2_lagged.png")

save_maps(djf_filtered_zone3, mam_filtered_zone3, jja_filtered_zone3, son_filtered_zone3, 1, sa, "precip_signif005_zone3.png")
save_maps(djf_filtered_zone3, mam_filtered_zone3, jja_filtered_zone3, son_filtered_zone3, 2, sa, "precip2_signif005_zone3.png")
save_maps(djf_filtered_zone3, mam_filtered_zone3, jja_filtered_zone3, son_filtered_zone3, 3, sa, "precip_signif005_zone3_lagged.png")
save_maps(djf_filtered_zone3, mam_filtered_zone3, jja_filtered_zone3, son_filtered_zone3, 4, sa, "precip2_signif005_zone3_lagged.png")
save_maps(djf_filtered_zone3, mam_filtered_zone3, jja_filtered_zone3, son_filtered_zone3, 5, sa, "evi_signif005_zone3_lagged.png")

