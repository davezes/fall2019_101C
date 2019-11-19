


options(stringsAsFactors=FALSE, width=350)


xbool.savePlots <- TRUE


####### make sure in working directory
source("___f_funs.R")


############ 

library(mvtnorm)

p <- 1

n <- 3000

set.seed(777)


x <- rnorm(n)
errs <- rnorm(n, 0, 1/7)

y <- cos(2*x) + errs


if(xbool.savePlots) { png("~/Desktop/wiggles_01.png", height=1000, width=1000, pointsize=24) }
plot(x, y, lwd=1, xlim=c(-5, 5))
if(xbool.savePlots) { dev.off() }




xknots <- seq(-4, 4, by=1)

fhat <- h_natSplines(x=x, y=y, xknots=xknots)

xdom <- seq(-5, 5, length=500)

y0hat <- fhat(x0=xdom)

if(xbool.savePlots) { png("~/Desktop/wiggles_02.png", height=1000, width=1000, pointsize=24) }
plot(x, y, lwd=1, xlim=c(-5, 5))
points(xdom, y0hat, type="l", col="#00AA00", lwd=4)
abline(v=xknots, col="#AAAAAA", lwd=3)
if(xbool.savePlots) { dev.off() }





xknots <- seq(-4, 4, by=2)

fhat <- h_natSplines(x=x, y=y, xknots=xknots)

xdom <- seq(-5, 5, length=500)

y0hat <- fhat(x0=xdom)

if(xbool.savePlots) { png("~/Desktop/wiggles_03.png", height=1000, width=1000, pointsize=24) }
plot(x, y, lwd=1, xlim=c(-5, 5))
points(xdom, y0hat, type="l", col="#00AA00", lwd=4)
abline(v=xknots, col="#AAAAAA", lwd=3)
if(xbool.savePlots) { dev.off() }





xknots <- seq(-4, 4, by=4)

fhat <- h_natSplines(x=x, y=y, xknots=xknots)

xdom <- seq(-5, 5, length=500)

y0hat <- fhat(x0=xdom)

if(xbool.savePlots) { png("~/Desktop/wiggles_04.png", height=1000, width=1000, pointsize=24) }
plot(x, y, lwd=1, xlim=c(-5, 5))
points(xdom, y0hat, type="l", col="#00AA00", lwd=4)
abline(v=xknots, col="#AAAAAA", lwd=3)
if(xbool.savePlots) { dev.off() }




xknots <- seq(-4, 4, by=1/2)

fhat <- h_natSplines(x=x, y=y, xknots=xknots)

xdom <- seq(-5, 5, length=500)

y0hat <- fhat(x0=xdom)

if(xbool.savePlots) { png("~/Desktop/wiggles_05.png", height=1000, width=1000, pointsize=24) }
plot(x, y, lwd=1, xlim=c(-5, 5))
points(xdom, y0hat, type="l", col="#00AA00", lwd=4)
abline(v=xknots, col="#AAAAAA", lwd=3)
if(xbool.savePlots) { dev.off() }




## hh <- apply( ( mxH %*% solve( mxHH ) ) * mxH, 1, sum )
## sum(hh)

