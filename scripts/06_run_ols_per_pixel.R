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
djf_raster <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/djf_raster.envi")
mam_raster <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/mam_raster.envi")
jja_raster <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/jja_raster.envi")
son_raster <- brick("/projectnb/modislc/users/rkstan/GE712/outputs/son_raster.envi")

## RUN PIXELWISE OLS AND MAP COEFFICIENTS

# Prepare stacks by masking them so that they have the same data/nodata areas
prep_stacks = function(rast){
  evi = subset(rast, 1:13)
  precip = subset(rast, 14:26)
  temp = subset(rast, 27:39)
  
  # Mask them to have the same areas of data/nodata
  precip_masked = mask(precip, temp)
  evi_masked = mask(evi, precip_masked)
  out_rast = stack(evi_masked, precip_masked, temp)
  return(out_rast)

}

djf = prep_stacks(djf_raster)
mam = prep_stacks(mam_raster)
jja = prep_stacks(jja_raster)
son = prep_stacks(son_raster)

#Plot

plot(djf[[14:26]])
dev.new()
plot(mam[[14:26]])


# Define functions to obtain each of the coefficients from the regression

# 1 to 6: Get intercepts and slopes from evi ~ precip + temp + precip^2 + temp^2. Get r squares too.
get_intercept=function(x) { if (is.na(x[1])){ NA } else { lm(x[1:13] ~ x[14:26])$coefficients[1] }}

get_beta_precip=function(x) { if (is.na(x[1])){ NA } else { lm(x[1:13] ~ x[14:26] + x[27:39] + I(x[14:26]^2) + I(x[27:39]^2))$coefficients[2] }}
get_beta_temp=function(x) { if (is.na(x[1])){ NA } else { lm(x[1:13] ~ x[14:26] + x[27:39] + I(x[14:26]^2) + I(x[27:39]^2))$coefficients[3] }}
get_beta_precip2=function(x) { if (is.na(x[1])){ NA } else { lm(x[1:13] ~ x[14:26] + x[27:39] + I(x[14:26]^2) + I(x[27:39]^2))$coefficients[4] }}
get_beta_temp2=function(x) { if (is.na(x[1])){ NA } else { lm(x[1:13] ~ x[14:26] + x[27:39] + I(x[14:26]^2) + I(x[27:39]^2))$coefficients[5] }}

get_rsquared=function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:13] ~ x[14:26] + x[27:39] + I(x[14:26]^2) + I(x[27:39]^2));summary(m)$r.squared }}

# 7 to 10:Get significance level for each of the coefficients
get_signif_precip=function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:13] ~ x[14:26] + x[27:39] + I(x[14:26]^2) + I(x[27:39]^2));summary(m)$coefficients[2,4]}}
get_signif_temp=function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:13] ~ x[14:26] + x[27:39] + I(x[14:26]^2) + I(x[27:39]^2));summary(m)$coefficients[3,4]}}
get_signif_precip2=function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:13] ~ x[14:26] + x[27:39] + I(x[14:26]^2) + I(x[27:39]^2));summary(m)$coefficients[4,4]}}
get_signif_temp2=function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:13] ~ x[14:26] + x[27:39] + I(x[14:26]^2) + I(x[27:39]^2));summary(m)$coefficients[4,4]}}

# 11 to 14: Implements functions to calculate the optimum
get_beta_precip_opt=function(x) { if (is.na(x[1])){ NA } else { lm(x[1:13] ~ x[14:26] +  I(x[14:26]^2))$coefficients[2] }}
get_beta_precip2_opt=function(x) { if (is.na(x[1])){ NA } else { lm(x[1:13] ~ x[14:26] +  I(x[14:26]^2))$coefficients[3] }}
get_beta_temp_opt=function(x) { if (is.na(x[1])){ NA } else { lm(x[1:13] ~ x[27:39] +  I(x[27:39]^2))$coefficients[2] }}
get_beta_temp2_opt=function(x) { if (is.na(x[1])){ NA } else { lm(x[1:13] ~ x[27:39] +  I(x[27:39]^2))$coefficients[3] }}

# 15 to 18: Get significance of those optimum coefficients
get_signif_precip_opt=function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:13] ~ x[14:26] +  I(x[14:26]^2));summary(m)$coefficients[2,4] }}
get_signif_precip2_opt=function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:13] ~ x[14:26] +  I(x[14:26]^2));summary(m)$coefficients[3,4] }}
get_signif_temp_opt=function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:13] ~ x[27:39] +  I(x[27:39]^2));summary(m)$coefficients[2,4] }}
get_signif_temp2_opt=function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:13] ~ x[27:39] +  I(x[27:39]^2));summary(m)$coefficients[3,4] }}


# Put all functions on a vector to make it easier to calculate them
fnc_list = c(get_intercept, get_beta_precip, get_beta_temp, get_beta_precip2, get_beta_temp2, get_rsquared, 
             get_signif_precip, get_signif_temp, get_signif_precip2, get_signif_temp2, 
             get_beta_precip_opt,get_beta_precip2_opt, get_beta_temp_opt, get_beta_temp2_opt, 
             get_signif_precip_opt, get_signif_precip2_opt, get_signif_temp_opt, get_signif_temp2_opt)


# Iterate over functions and calculate each of the coefficients per season 
calc_coefs = function(x){
  coef_list = list()
  for(i in 1:length(fnc_list)){
    coef_list[[i]] = calc(x, fnc_list[[i]])
  }  
  return(coef_list)
}

# Get ALL coefficients per season
djf_coefs = calc_coefs(djf)
mam_coefs = calc_coefs(mam)
jja_coefs = calc_coefs(jja)
son_coefs = calc_coefs(son)

#Function to save a single figure with the coefficients/optimum with all the four seasons
save_maps = function(djf_list, mam_list, jja_list, son_list, index, fname){
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
save_maps(djf_coefs, mam_coefs, jja_coefs, son_coefs, 2, "precip_all.png")
save_maps(djf_coefs, mam_coefs, jja_coefs, son_coefs, 3, "temp_all.png")
save_maps(djf_coefs, mam_coefs, jja_coefs, son_coefs, 4, "precip2_all.png")
save_maps(djf_coefs, mam_coefs, jja_coefs, son_coefs, 5, "temp2_all.png")

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
djf_filtered = filter_coefs(djf_coefs)
mam_filtered = filter_coefs(mam_coefs)
jja_filtered = filter_coefs(jja_coefs)
son_filtered = filter_coefs(son_coefs)


# Save those plots!
save_maps(djf_filtered, mam_filtered, jja_filtered, son_filtered, 1, "precip_signif005.png")
save_maps(djf_filtered, mam_filtered, jja_filtered, son_filtered, 2, "precip2_signif005.png")
save_maps(djf_filtered, mam_filtered, jja_filtered, son_filtered, 3, "temp_signif005.png")
save_maps(djf_filtered, mam_filtered, jja_filtered, son_filtered, 4, "temp2_signif005.png")


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
djf_optimum = calculate_optimum(djf_coefs)
mam_optimum = calculate_optimum(mam_coefs)
jja_optimum = calculate_optimum(jja_coefs)
son_optimum = calculate_optimum(son_coefs)

# Plot optimum temp
save_maps(djf_optimum, mam_optimum, jja_optimum, son_optimum, 1, "filtered_optimum_temp_005.png")
save_maps(djf_optimum, mam_optimum, jja_optimum, son_optimum, 2, "filtered_optimum_precip_005.png")



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


