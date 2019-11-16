


options(stringsAsFactors=FALSE, width=350)


xbool.savePlots <- TRUE


####### make sure in working directory
source("___f_funs.R")


############ prepair dataset for bias variance

library(mvtnorm)

library(scatterplot3d)



n <- 5000

set.seed(777)

f_true <- function(x1, x2) {
    f <- sin(x1)*sin(x2)
    return(f)
}


x1 <- rnorm(n)
x2 <- rnorm(n)


ftrue <- f_true(x1=x1, x2=x2)

errs <- rnorm(n, 0, 1/7)

y <- ftrue + errs





if(xbool.savePlots) { png(file.path("~", "Desktop", "wiggles_sim_f_3d.png"), width=1000, height=1000, pointsize=28) }
par(mfrow=c(1, 1))
scatterplot3d( x1, x2, y, angle=30, xlab="x1", ylab="x2", zlab="y", cex.symbols=0.3) ### , type="l"
#xsp3 <- scatterplot3d( rep(xdom, 2), yy, fd, angle=30, xlab="x", ylab="y", zlab="density", cex.symbols=0.3) ### , type="l"
#xsp3$points3d(x=c(x0, x0), y=c(0, 1), z=c(1, 1), type="h", lwd=3, col="#FF0000")
if(xbool.savePlots) { dev.off() }



#if(xbool.savePlots) { png("~/Desktop/wiggles_x1_x2_01.png", height=1000, width=2000, pointsize=24) }
#par(mfrow=c(1, 2))
#plot(x1, y, lwd=1, xlim=c(-5, 5))
#plot(x2, y, lwd=1, xlim=c(-5, 5))
#if(xbool.savePlots) { dev.off() }





x1dom <- seq(-4, 4, length=70)
x2dom <- seq(-4, 4, length=70)
Xdom <- as.matrix( expand.grid( "x1"=x1dom, "x2"=x2dom ) )

fdom <- f_true(x1=Xdom[ , "x1" ], x2=Xdom[ , "x2" ])
mx.f.dom <- matrix(fdom, length(x1dom), length(x2dom))

## image(mx.f.dom)


if(xbool.savePlots) { png(file.path("~", "Desktop", "wireframe_true_01.png"), width=2000, height=1000, pointsize=28) }
par(mfrow=c(1,2))
persp(x=x1dom, y=x2dom, z=mx.f.dom, zlab="y", phi=30, theta=60)
persp(x=x1dom, y=x2dom, z=mx.f.dom, zlab="y", phi=30, theta=150)
if(xbool.savePlots) { dev.off() }




x1dom <- seq(-4, 4, length=300)
x2dom <- seq(-4, 4, length=300)
Xdom <- as.matrix( expand.grid( "x1"=x1dom, "x2"=x2dom ) )

fdom <- f_true(x1=Xdom[ , "x1" ], x2=Xdom[ , "x2" ])
mx.f.dom <- matrix(fdom, length(x1dom), length(x2dom))

if(xbool.savePlots) { png(file.path("~", "Desktop", "heat_true_01.png"), width=1000, height=1000, pointsize=28) }
par(mfrow=c(1,1))
image(mx.f.dom,  col = hcl.colors(300, "YlOrRd", rev = TRUE), xlab="x2", ylab="x1")
if(xbool.savePlots) { dev.off() }




########################### as GAM

xknots <- seq(-4, 4, by=1)



mxH1 <- f_basisNatSplines(x=x1, xknots=xknots)
mxH2 <- f_basisNatSplines(x=x2, xknots=xknots)

mxX <- cbind(mxH1, mxH2[ , -1])

bhat <- solve( crossprod(mxX) ) %*% ( t(mxX) %*% y )

yhat <- mxX %*% bhat

xrmse <- sqrt( mean( (y - yhat)^2 ) )
xrmse


if(xbool.savePlots) { png(file.path("~", "Desktop", "wiggles_simfitGAM_f_3d.png"), width=1000, height=1000, pointsize=18) }
par(mfrow=c(1, 1))
#scatterplot3d( x1, x2, y, angle=30, xlab="x1", ylab="x2", zlab="y", cex.symbols=0.3) ### , type="l"
xsp3 <- scatterplot3d( x1, x2, y, angle=30, xlab="x1", ylab="x2", zlab="y", cex.symbols=0.3, main=paste0("GAM Spines. RMSE: ", round(xrmse, 4))) ### , type="l"
xsp3$points3d(x=x1, y=x2, z=yhat, col="#FF0000")
if(xbool.savePlots) { dev.off() }





####################### interacted


xxeg <- expand.grid( "ndx1"=I(1:ncol(mxH1)), "ndx2"=I(1:(ncol(mxH1)-1)) )


mxX <- mxH1[ , xxeg[ , "ndx1"] ] * mxH2[ , -1][ , xxeg[ , "ndx2"] ]

dim(mxX)

bhatcross <- solve( crossprod(mxX) + diag(1, ncol(mxX)) ) %*% ( t(mxX) %*% y )

yhatcross <- mxX %*% bhatcross

xrmse <- sqrt( mean( (y - yhatcross)^2 ) )
xrmse

if(xbool.savePlots) { png(file.path("~", "Desktop", "wiggles_simfitGAM_finteract_3d.png"), width=1000, height=1000, pointsize=18) }
par(mfrow=c(1, 1))
#scatterplot3d( x1, x2, y, angle=30, xlab="x1", ylab="x2", zlab="y", cex.symbols=0.3) ### , type="l"
xsp3 <- scatterplot3d( x1, x2, y, angle=30, xlab="x1", ylab="x2", zlab="y", cex.symbols=0.3, main=paste0("Interacted Spines w/some Ridge!  RMSE: ", round(xrmse, 4))) ### , type="l"
xsp3$points3d(x=x1, y=x2, z=yhatcross, col="#FF0000")
if(xbool.savePlots) { dev.off() }





