


options(stringsAsFactors=FALSE, width=350)


xbool.savePlots <- TRUE


####### make sure in working directory
source("___f_funs.R")


############ 

p <- 1



set.seed(777)


x <- I(1:12)
#x <- (1:8)

n <- length(x)

errs <- rnorm(n, 0, 1/700)

y <- cos(2*x) + errs
y <- cos(x) + errs
y <- 1 + cos(x/2) + errs


if(xbool.savePlots) { png("~/Desktop/simpleCos_01.png", height=1000, width=1000, pointsize=24) }
plot(x, y, lwd=1)
if(xbool.savePlots) { dev.off() }




xknots <- c(3.5, 6.5, 9.5)
#xknots <- c(2.5, 4.5, 6.5)

if(xbool.savePlots) { png("~/Desktop/simpleCos_02knots.png", height=1000, width=1000, pointsize=24) }
plot(x, y, lwd=1, main="Cubic Splines Micro Example")
abline(v=xknots, col="#AAAAAA", lwd=3)
if(xbool.savePlots) { dev.off() }


mxH <- f_basisCubicSplines(x, xknots, xintercept=TRUE)


bhat <- solve(crossprod(mxH)) %*% t(mxH) %*% y






xdom <- seq(min(x), max(x), length=500)

mxHdom <- f_basisCubicSplines(x=xdom, xknots)

y0dom <- mxHdom %*% bhat


if(xbool.savePlots) { png("~/Desktop/simpleCos_02knots_fit.png", height=1000, width=1000, pointsize=24) }
plot(x, y, lwd=1, main="Cubic Splines Micro Example")
points(xdom, y0dom, type="l", col="#00AA00", lwd=4)
abline(v=xknots, col="#AAAAAA", lwd=3)
if(xbool.savePlots) { dev.off() }




