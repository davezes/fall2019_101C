


options(stringsAsFactors=FALSE, width=350)



xbool.savePlots <- FALSE

############ simple 1D support

library(mvtnorm)

p <- 1

N <- 1000 ##### must be div by 4

set.seed(777)


x <-
c(
rnorm(3*N/4, -1, 1),
rnorm(1*N/4, 1, 2)
)

y <- c( rep(0, 3*N/4), rep(1, 1*N/4) )




####################

x0 <- 0 ##### value at x -- try different values

############## show true joint density

xdom <- seq( min(x)-1, max(x)+1, length=2000 ) ; xdom

fd <- c(dnorm(xdom, -1, 1) * (3/4), dnorm(xdom, 1, 2) * (1/4)) #### true density over xdom x {0,1}

yy <- c( rep(0, length(xdom)), rep(1, length(xdom)) )

#library(plot3D)

library(scatterplot3d)

if(xbool.savePlots) { png(file.path("~", "Desktop", "joint_d_1d.png"), width=2000, height=1000, pointsize=28) }
par(mfrow=c(1, 2))
scatterplot3d( rep(xdom, 2), yy, fd, angle=30, xlab="x", ylab="y", zlab="density", cex.symbols=0.3) ### , type="l"
xsp3 <- scatterplot3d( rep(xdom, 2), yy, fd, angle=30, xlab="x", ylab="y", zlab="density", cex.symbols=0.3) ### , type="l"
xsp3$points3d(x=c(x0, x0), y=c(0, 1), z=c(1, 1), type="h", lwd=3, col="#FF0000")
if(xbool.savePlots) { dev.off() }





#####################################


##################################### bayes' classifier true density

ylik.given.x <-  c(dnorm(x0, -1, 1) * (3/4), dnorm(x0, 1, 2) * (1/4))
ylik.given.x


########## prob that y=1 given x0

ylik.given.x[2] / sum(ylik.given.x)



########################### use bayes' classifier empirically

library(ks)


if(xbool.savePlots) { png(file.path("~", "Desktop", "hist_d_1d.png"), width=1000, height=1000, pointsize=21) }
par(mfrow=c(2, 1), mar=c(3, 3, 2, 1))
hist(x[ y == 1 ], freq=FALSE, xlim=range(x))
xxdy1 <- kde(x[ y == 1 ])
points(xxdy1$eval.points, xxdy1$estimate, type="l", col="#00CC00", lwd=4)

hist(x[ y == 0 ], freq=FALSE, xlim=range(x))
xxdy0 <- kde(x[ y == 0 ])
points(xxdy0$eval.points, xxdy0$estimate, type="l", col="#00CC00", lwd=4)
if(xbool.savePlots) { dev.off() }



############# predict

pi_0 <- sum(y==0) / N ; pi_0
pi_1 <- sum(y==1) / N ; pi_1

predict(xxdy1, x=x0) * pi_1 / ( pi_0 * predict(xxdy0, x=x0) + pi_1 * predict(xxdy1, x=x0) )




