library(raster)
library(rgdal)
library(RColorBrewer)
library(maptools)

setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")
data(wrld_simpl)
SA = c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Peru",
       "Suriname", "Uruguay", "Venezuela")
sa = wrld_simpl[which(wrld_simpl@data$NAME %in% SA),]

# Read in stacks of evi, precip and temp, 13 layers each
djf_raster_zone1 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/djf_zone1.envi")
mam_raster_zone1 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/mam_zone1.envi")
jja_raster_zone1 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/jja_zone1.envi")
son_raster_zone1 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/son_zone1.envi")

djf_raster_zone2 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/djf_zone2.envi")
mam_raster_zone2 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/mam_zone2.envi")
jja_raster_zone2 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/jja_zone2.envi")
son_raster_zone2 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/son_zone2.envi")

djf_raster_zone3 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/djf_zone3.envi")
mam_raster_zone3 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/mam_zone3.envi")
jja_raster_zone3 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/jja_zone3.envi")
son_raster_zone3 <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/son_zone3.envi")

get_stat <- function(x, args.list){

  res <- rep(NA, args.list$nl)
 
  if (anyNA(x))
    return(res)
  
  m1 <- lm(x[1:13] ~ x[14:26] + x[27:39] + I(x[14:26]^2) + I(x[27:39]^2))
  m2 <- lm(x[1:13] ~ x[14:26] +  I(x[14:26]^2))
  m3 <- lm(x[1:13] ~ x[27:39] +  I(x[27:39]^2))

  sm1 <- summary(m1)
  sm2 <- summary(m2)
  sm3 <- summary(m3)

  res <- c(intercept          = m1$coefficients[1], # Why the equation is different from the otehrs lm(x[1:13] ~ x[14:26]) ???
           beta_precip        = m1$coefficients[2], 
           beta_temp          = m1$coefficients[3],
           beta_precip2       = m1$coefficients[4],
           beta_temp2         = m1$coefficients[5],
           rsquared           = sm1$r.squared, 
           signif_precip      = sm1[2,4],
           signif_temp        = sm1[3,4],
           signif_precip2     = sm1[4,4],
           signif_temp2       = sm1[4,4], # Why this is the same as signif_precip2
           beta_precip_opt    = m2[2],
           beta_precip2_opt   = m2[3],
           beta_temp_opt      = m3[2],
           beta_temp2_opt     = m3[3],
           signif_precip_opt  = sm2[2,4],
           signif_precip2_opt = sm2[3,4],
           signif_temp_opt    = sm3[2,4],
           signif_temp2_opt   = sm3[3,4]
  )

  return(res)

}  

# Get ALL coefficients per season
djf_coefs_zone1 = pply_stack_parallel(djf_zone1, fun = get_stat, nl = 18)
mam_coefs_zone1 = pply_stack_parallel(mam_zone1, fun = get_stat, nl = 18)
jja_coefs_zone1 = pply_stack_parallel(jja_zone1, fun = get_stat, nl = 18)
son_coefs_zone1 = pply_stack_parallel(son_zone1, fun = get_stat, nl = 18)

djf_coefs_zone2 = pply_stack_parallel(djf_zone2, fun = get_stat, nl = 18)
mam_coefs_zone2 = pply_stack_parallel(mam_zone2, fun = get_stat, nl = 18)
jja_coefs_zone2 = pply_stack_parallel(jja_zone2, fun = get_stat, nl = 18)
son_coefs_zone2 = pply_stack_parallel(son_zone2, fun = get_stat, nl = 18)

djf_coefs_zone3 = pply_stack_parallel(djf_zone3, fun = get_stat, nl = 18)
mam_coefs_zone3 = pply_stack_parallel(mam_zone3, fun = get_stat, nl = 18)
jja_coefs_zone3 = pply_stack_parallel(jja_zone3, fun = get_stat, nl = 18)
son_coefs_zone3 = pply_stack_parallel(son_zone3, fun = get_stat, nl = 18)


#Function to save a single figure with the coefficients/optimum with all the four seasons
save_maps = function(djf_list, mam_list, jja_list, son_list, index, fname){
  setwd("/projectnb/modislc/users/rkstan/GE712/outputs/")
  png(fname, width=1000, height = 1000)
  
  par(mfrow=c(2,2), mar=c(3,3,3,3))
  
  plot(djf_list[[index]], col=brewer.pal(6, "RdBu"), legend=F, main="DJF")
  plot(sa, add=T, lty=1, lwd=0.5)
  plot(djf_list[[index]], col=brewer.pal(6, "RdBu"), legend.only=T, legend.width=1, legend.shrink=1, side=4)
  
  plot(mam_list[[index]], col=brewer.pal(6, "RdBu"), legend=F, , main="MAM")
  plot(sa, add=T, lty=1, lwd=0.5)
  plot(mam_list[[index]], col=brewer.pal(6, "RdBu"), legend.only=T, legend.width=1, legend.shrink=1, side=4)
  
  plot(jja_list[[index]], col=brewer.pal(6, "RdBu"), legend=F, , main="JJA")
  plot(sa, add=T, lty=1, lwd=0.5)
  plot(jja_list[[index]], col=brewer.pal(6, "RdBu"), legend.only=T, legend.width=1, legend.shrink=1, side=4)
  
  plot(son_list[[index]],col=brewer.pal(6, "RdBu"), legend=F, , main="SON")
  plot(sa, add=T, lty=1, lwd=0.5)
  plot(son_list[[index]], col=brewer.pal(6, "RdBu"), legend.only=T, legend.width=1, legend.shrink=1, side=4)
 
  dev.off()
}


# Save for comparison
save_maps(djf_coefs_zone1, mam_coefs_zone1, jja_coefs_zone1, son_coefs_zone1, 2, "precip_zone1.png")
save_maps(djf_coefs_zone1, mam_coefs_zone1, jja_coefs_zone1, son_coefs_zone1, 3, "temp_zone1.png")
save_maps(djf_coefs_zone1, mam_coefs_zone1, jja_coefs_zone1, son_coefs_zone1, 4, "precip2_zone1.png")
save_maps(djf_coefs_zone1, mam_coefs_zone1, jja_coefs_zone1, son_coefs_zone1, 5, "temp2_zone1.png")

save_maps(djf_coefs_zone2, mam_coefs_zone2, jja_coefs_zone2, son_coefs_zone2, 2, "precip_zone2.png")
save_maps(djf_coefs_zone2, mam_coefs_zone2, jja_coefs_zone2, son_coefs_zone2, 3, "temp_zone2.png")
save_maps(djf_coefs_zone2, mam_coefs_zone2, jja_coefs_zone2, son_coefs_zone2, 4, "precip2_zone2.png")
save_maps(djf_coefs_zone2, mam_coefs_zone2, jja_coefs_zone2, son_coefs_zone2, 5, "temp2_zone2.png")

save_maps(djf_coefs_zone3, mam_coefs_zone3, jja_coefs_zone3, son_coefs_zone3, 2, "precip_zone3.png")
save_maps(djf_coefs_zone3, mam_coefs_zone3, jja_coefs_zone3, son_coefs_zone3, 3, "temp_zone3.png")
save_maps(djf_coefs_zone3, mam_coefs_zone3, jja_coefs_zone3, son_coefs_zone3, 4, "precip2_zone3.png")
save_maps(djf_coefs_zone3, mam_coefs_zone3, jja_coefs_zone3, son_coefs_zone3, 5, "temp2_zone3.png")

# Filter them by significance
filter_coefs = function(season_coefs){
  # Create masks with threshold for significance
  filter_signif = function(x){ x[x > 0.05] = NA; return(x)}
  filter_outliers = function(x){ x[x>20 | x < (-20)] <- NA; return(x)}
  precip_mask = calc(season_coefs[[7]], filter_signif)
  temp_mask = calc(season_coefs[[8]], filter_signif)
  precip2_mask = calc(season_coefs[[9]], filter_signif)
  temp2_mask = calc(season_coefs[[10]], filter_signif)
  
  # Filter actual coefficients using the masks we created
  precip_masked = mask(season_coefs[[2]], precip_mask)
  temp_masked = mask(season_coefs[[3]], precip2_mask)
  precip2_masked = mask(season_coefs[[4]], temp_mask)
  temp2_masked = mask(season_coefs[[5]], temp2_mask)
  
  # Filter outliers
  precip_masked = calc(precip_masked, filter_outliers)
  precip2_masked = calc(precip2_masked, filter_outliers)
  
  return(list(precip_masked, precip2_masked, temp_masked, temp2_masked))
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
save_maps(djf_filtered_zone1, mam_filtered_zone1, jja_filtered_zone1, son_filtered_zone1, 1, "precip_signif005_zone1.png")
save_maps(djf_filtered_zone1, mam_filtered_zone1, jja_filtered_zone1, son_filtered_zone1, 2, "precip2_signif005_zone1.png")
save_maps(djf_filtered_zone1, mam_filtered_zone1, jja_filtered_zone1, son_filtered_zone1, 3, "temp_signif005_zone1.png")
save_maps(djf_filtered_zone1, mam_filtered_zone1, jja_filtered_zone1, son_filtered_zone1, 4, "temp2_signif005_zone1.png")

save_maps(djf_filtered_zone2, mam_filtered_zone2, jja_filtered_zone2, son_filtered_zone2, 1, "precip_signif005_zone2.png")
save_maps(djf_filtered_zone2, mam_filtered_zone2, jja_filtered_zone2, son_filtered_zone2, 2, "precip2_signif005_zone2.png")
save_maps(djf_filtered_zone2, mam_filtered_zone2, jja_filtered_zone2, son_filtered_zone2, 3, "temp_signif005_zone2.png")
save_maps(djf_filtered_zone2, mam_filtered_zone2, jja_filtered_zone2, son_filtered_zone2, 4, "temp2_signif005_zone2.png")

save_maps(djf_filtered_zone3, mam_filtered_zone3, jja_filtered_zone3, son_filtered_zone3, 1, "precip_signif005_zone3.png")
save_maps(djf_filtered_zone3, mam_filtered_zone3, jja_filtered_zone3, son_filtered_zone3, 2, "precip2_signif005_zone3.png")
save_maps(djf_filtered_zone3, mam_filtered_zone3, jja_filtered_zone3, son_filtered_zone3, 3, "temp_signif005_zone3.png")
save_maps(djf_filtered_zone3, mam_filtered_zone3, jja_filtered_zone3, son_filtered_zone3, 4, "temp2_signif005_zone3.png")

# Fnc to calculate optimum values
calculate_optimum = function(season_coefs){
  # Create masks with threshold for significance
  filter_signif = function(x){ x[x > 0.05] = NA; return(x)}
  precip_mask = calc(season_coefs[[15]], filter_signif)
  precip2_mask = calc(season_coefs[[16]], filter_signif)
  temp_mask = calc(season_coefs[[17]], filter_signif)
  temp2_mask = calc(season_coefs[[18]], filter_signif)
  
  # Filter actual coefficients using the masks we created
  precip_masked = mask(season_coefs[[11]], precip_mask)
  precip2_masked = mask(season_coefs[[12]], precip2_mask)
  temp_masked = mask(season_coefs[[13]], temp_mask)
  temp2_masked = mask(season_coefs[[14]], temp2_mask)
  
  # Get optimum values
  optimum = list()
  optimum[[1]] = 2*temp2_masked  # 2nd derivative temp
  optimum[[2]] = (-1 * temp_masked) / optimum[[1]] # temp optimum, -1 required to avoid error
  optimum[[3]] = 2*precip2_masked # 2nd derivative precip
  optimum[[4]] = (-1 * precip_masked) / optimum[[3]] # precip optimum, -1 required to avoid error
  
  # Create masks where second derivative is negative (local maximum)
  filter_optimum = function(x){ x[x >= 0] = NA; return(x)}
  optimum_temp_mask = calc(optimum[[1]],filter_optimum)
  optimum_precip_mask = calc(optimum[[3]],filter_optimum)
  
  # Filter optimum valuesusing those masks
  optimum_temp = mask(optimum[[2]], optimum_temp_mask)
  optimum_precip = mask(optimum[[4]], optimum_precip_mask)
  
  return(list(optimum_temp, optimum_precip))
}


# Calculate optimum values
djf_optimum_zone1 = calculate_optimum(djf_coefs_zone1)
mam_optimum_zone1 = calculate_optimum(mam_coefs_zone1)
jja_optimum_zone1 = calculate_optimum(jja_coefs_zone1)
son_optimum_zone1 = calculate_optimum(son_coefs_zone1)

djf_optimum_zone2 = calculate_optimum(djf_coefs_zone2)
mam_optimum_zone2 = calculate_optimum(mam_coefs_zone2)
jja_optimum_zone2 = calculate_optimum(jja_coefs_zone2)
son_optimum_zone2 = calculate_optimum(son_coefs_zone2)

djf_optimum_zone3 = calculate_optimum(djf_coefs_zone3)
mam_optimum_zone3 = calculate_optimum(mam_coefs_zone3)
jja_optimum_zone3 = calculate_optimum(jja_coefs_zone3)
son_optimum_zone3 = calculate_optimum(son_coefs_zone3)

# Plot optimum temp
save_maps(djf_optimum_zone1, mam_optimum_zone1, jja_optimum_zone1, son_optimum_zone1, 1, "filtered_optimum_temp_005_zone1.png")
save_maps(djf_optimum_zone1, mam_optimum_zone1, jja_optimum_zone1, son_optimum_zone1, 2, "filtered_optimum_precip_005_zone1.png")

save_maps(djf_optimum_zone2, mam_optimum_zone2, jja_optimum_zone2, son_optimum_zone2, 1, "filtered_optimum_temp_005_zone2.png")
save_maps(djf_optimum_zone2, mam_optimum_zone2, jja_optimum_zone2, son_optimum_zone2, 2, "filtered_optimum_precip_005_zone2.png")

save_maps(djf_optimum_zone3, mam_optimum_zone3, jja_optimum_zone3, son_optimum_zone3, 1, "filtered_optimum_temp_005_zone3.png")
save_maps(djf_optimum_zone3, mam_optimum_zone3, jja_optimum_zone3, son_optimum_zone3, 2, "filtered_optimum_precip_005_zone3.png")


## CALCULATE AND MAP ECOLOGICAL OPTIMUM
#equations
# evi = a + b1*temp + b2*(temp^2)
### take first derivative 
# evi = b1*(temp^0) + b2*2*(temp^1)
# evi = b1 + 2*b2*temp
### find critical values 
# 0 = b1 + 2*b2*temp
# temp = -b1/(2*b2)
### take second derivative 


