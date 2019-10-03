

###

###

options(stringsAsFactors=FALSE, width=350)



xbool.savePlots <- FALSE


####### make sure in working directory
source("___f_funs.R")





############ prepair dataset for bias variance

N <- 50

set.seed(777)

x <- sort(runif(N, -1, 1))

xdom <- seq(-1, 1, length=300)

f.true <- 100 + 40 * x
f.true <- 5 * sin( 3 * x )

xsig <- 1/1


errs <- rnorm(N) * xsig

y <- f.true + errs





if(xbool.savePlots) { png(file.path("~", "Desktop", "biasVariance.png"), height=1000, width=1000, pointsize=24) }

plot(x, y)

xlwd <- 4


f.hat <- h.poly(x=x, y=y, porder=0)
yhat_1 <- f.hat(x=xdom)
points(xdom, yhat_1, type="l", col="#000000", lwd=xlwd)



f.hat <- h.poly(x=x, y=y, porder=1)
yhat_1 <- f.hat(x=xdom)
points(xdom, yhat_1, type="l", col=rainbow(10)[1], lwd=xlwd)




f.hat <- h.poly(x=x, y=y, porder=2)
yhat_2 <- f.hat(x=xdom)
points(xdom, yhat_2, type="l", col=rainbow(10)[2], lwd=xlwd)




f.hat <- h.poly(x=x, y=y, porder=5)
yhat_3 <- f.hat(x=xdom)
points(xdom, yhat_3, type="l", col=rainbow(10)[3], lwd=xlwd)





f.hat <- h.poly(x=x, y=y, porder=8)
yhat_4 <- f.hat(x=xdom)
points(xdom, yhat_4, type="l", col=rainbow(10)[4], lwd=xlwd)




f.hat <- h.poly(x=x, y=y, porder=20)
yhat_5 <- f.hat(x=xdom)
points(xdom, yhat_5, type="l", col=rainbow(10)[6], lwd=xlwd)

if(xbool.savePlots) { dev.off() }








########## function bias, function variance, total function predition variance


nn <- 10000

mxyhats0 <- matrix(NA, N, nn)
mxyhats1 <- matrix(NA, N, nn)
mxyhats2 <- matrix(NA, N, nn)
mxyhats3 <- matrix(NA, N, nn)
mxyhats4 <- matrix(NA, N, nn)
mxyhats5 <- matrix(NA, N, nn)

mxyy <- matrix(NA, N, nn)


for(ii in 1:nn) {
    
    
    
    errs <- rnorm(N) * xsig ##### create new y -- keep x the same.
    
    y <- f.true + errs
    
    f.hat <- h.poly(x=x, y=y, porder=0)
    mxyhats0[ , ii ] <- f.hat(x=x)
    
    f.hat <- h.poly(x=x, y=y, porder=1)
    mxyhats1[ , ii ] <- f.hat(x=x)
    
    f.hat <- h.poly(x=x, y=y, porder=2)
    mxyhats2[ , ii ] <- f.hat(x=x)
    
    f.hat <- h.poly(x=x, y=y, porder=5)
    mxyhats3[ , ii ] <- f.hat(x=x)
    
    f.hat <- h.poly(x=x, y=y, porder=8)
    mxyhats4[ , ii ] <- f.hat(x=x)
    
    f.hat <- h.poly(x=x, y=y, porder=20)
    mxyhats5[ , ii ] <- f.hat(x=x)
    
    ynew <- f.true + rnorm(N) * xsig
    
    mxyy[ , ii ] <- ynew
    
}



bias0 <- mean( (apply( mxyhats0, 1, mean ) - f.true)^2 ) ; bias0
varfhat0 <- mean( apply( mxyhats0, 1, var ) ) ; varfhat0
totpvar0 <- mean( (mxyy - mxyhats0)^2 ) ; totpvar0


bias1 <- mean( (apply( mxyhats1, 1, mean ) - f.true)^2 ) ; bias1
varfhat1 <- mean( apply( mxyhats1, 1, var ) ) ; varfhat1
totpvar1 <- mean( (mxyy - mxyhats1)^2 ) ; totpvar1


bias2 <- mean( (apply( mxyhats2, 1, mean ) - f.true)^2 ) ; bias2
varfhat2 <- mean( apply( mxyhats2, 1, var ) ) ; varfhat2
totpvar2 <- mean( (mxyy - mxyhats2)^2 ) ; totpvar2


bias3 <- mean( (apply( mxyhats3, 1, mean ) - f.true)^2 ) ; bias3
varfhat3 <- mean( apply( mxyhats3, 1, var ) ) ; varfhat3
totpvar3 <- mean( (mxyy - mxyhats3)^2 ) ; totpvar3


bias4 <- mean( (apply( mxyhats4, 1, mean ) - f.true)^2 ) ; bias4
varfhat4 <- mean( apply( mxyhats4, 1, var ) ) ; varfhat4
totpvar4 <- mean( (mxyy - mxyhats4)^2 ) ; totpvar4


bias5 <- mean( (apply( mxyhats5, 1, mean ) - f.true)^2 ) ; bias5
varfhat5 <- mean( apply( mxyhats5, 1, var ) ) ; varfhat5
totpvar5 <- mean( (mxyy - mxyhats5)^2 ) ; totpvar5



xords <- c(0, 1, 2, 5, 8, 20)
ybiases <- c(bias0, bias1, bias2, bias3, bias4, bias5) ; ybiases
yvarfhats <- c(varfhat0, varfhat1, varfhat2, varfhat3, varfhat4, varfhat5) ; yvarfhats
totpvars <- c(totpvar0, totpvar1, totpvar2, totpvar3, totpvar4, totpvar5) ; totpvars


if(xbool.savePlots) { png(file.path("~", "Desktop", "biasVariance2.png"), height=1000, width=1000, pointsize=24) }

plot(xords, sqrt(totpvars), type="l", col="#000000", ylim=c(0, max(sqrt(totpvars))), lwd=xlwd)
points(xords, sqrt(ybiases), type="l", col="#AA0000", lwd=xlwd)
points(xords, sqrt(yvarfhats), type="l", col="#0000AA", lwd=xlwd)

if(xbool.savePlots) { dev.off() }

