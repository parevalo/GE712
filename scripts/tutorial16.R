# Tutorial 13 - Canonical Correlation
setwd("~/Documents/Classes/Spring2015/GE516/Tutorials")

# First Get some data
dat=read.table("evap.txt",header=T)
x=dat[,2:4]     # "soil" variables
y=dat[,5:11]		# "air" variables

# Let's look at relationships within x and y
pairs(x)
pairs(y)

#predict on pca gives you principal components 
# What happens if we do PCA on each?
x.pca=predict(princomp(x))                     # compute PCA 
y.pca=predict(princomp(y))
plot(x.pca[,1],y.pca[,1],pch=16,col="red",
     xlab="1st PC of X",ylab="1st PC of Y")    # plot 1st PC for x vs 1st PC for y

# Now do CCA
xy.cca=cancor(x,y)           # estimate model
names(xy.cca)                # what is in the result?
#pairs depends on smallest dimensionality - U and V have 3 columns - correlation between first column in U and first column in V
xy.cca$cor                   # correlation between pairs of canonical factors
xy.cca$xcoef                 # coefficients for X
xy.cca$ycoef                 # coefficients for Y (note: cols 4-7 are meaningless)

par(mfrow=c(2,1))            # plot the coefs for 1st canonical pair of CFs in a barplot
barplot(xy.cca$xcoef[,1])    #multiply by the x coef 
barplot(xy.cca$ycoef[,2])   #multiply by the y coef 

x.cf=as.matrix(x)%*%xy.cca$xcoef         # compute canonical factors on X
y.cf=as.matrix(y)%*%xy.cca$ycoef[,1:3]   # compute canonical factors on Y (only use 1st 3!)
round(cor(x.cf,y.cf),2)                  # Ok?
xy.cca$cor
plot(x.cf[,1],y.cf[,1],xlab="First Canonical Factor in X",    # look at some plots!
     ylab="First Canonical Factor in Y")
plot(x.cf[,2],x.cf[,1],xlab="First Canonical Factor in X",
     ylab="Second Canonical Factor in X")
plot(x.cf[,2],y.cf[,2],xlab="Second Canonical Factor in X",
     ylab="Second Canonical Factor in Y")

# Multiple correlation (from multiple regression)
r.yx=matrix(cor(y[,1],x),ncol=1)
Rxx=cor(x)
R2=t(r.yx)%*%solve(Rxx)%*%r.yx
summary(lm(y[,1]~as.matrix(x)))

#Do it manually
Syy=var(y)			# Covariance matrix for air vars
Sxx=var(x)			# Covariance matrix for soil vars
Sxy=var(x,y)		# Covariance of soil w/each air var
Syx=var(y,x)		# Covariance of air w/each soil var

# compute eigen values and vectors
CCAy=eigen(solve(Syy)%*%Syx%*%solve(Sxx)%*%Sxy)
CCAx=eigen(solve(Sxx)%*%Sxy%*%solve(Syy)%*%Syx)

# check out results
CCAy
CCAx

x.cf=as.matrix(x)%*%as.matrix(CCAx$vectors)         # compute canonical factors on X
y.cf=as.matrix(y)%*%as.matrix(CCAy$vectors)[,1:3]   # compute canonical factors on Y (only use 1st 3!)

par(mfrow=c(1,1))
plot(x.cf[,1],y.cf[,1],xlab="First Canonical Factor in X",    # look at some plots!
     ylab="First Canonical Factor in Y")
plot(x.cf[,2],x.cf[,1],xlab="First Canonical Factor in X",
     ylab="Second Canonical Factor in X")
plot(x.cf[,2],y.cf[,2],xlab="Second Canonical Factor in X",
     ylab="Second Canonical Factor in Y")

# Compare with bulit-in
b=cancor(x,y,xcenter=T,ycenter=T)$xcoef	# coefs for x
a=cancor(x,y,xcenter=T,ycenter=T)$ycoef	# coefs for y
U=as.matrix(x)%*%b							# linear combo of x's
V=as.matrix(y)%*%a							# linear combo of y's
cor(U)											# no correlation here
cor(V)											# no correlation here
cor(U,V)										# check out diagonal!
plot(data.frame(y.cf[,1],V[,1]),col="blue",pch=16)
plot(data.frame(x.cf[,1],U[,1]),col="blue",pch=16)

