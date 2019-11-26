


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




##############################

library(tree)


dfxy <- data.frame("y"=y, "x1"=x1, "x2"=x2)

xtree <- tree(y ~ x1 + x2, data=dfxy, minsize=14, mindev=0)

summary(xtree)

plot(xtree)




set.seed(333)

x1_0 <- x1 ## rnorm(n)
x2_0 <- x2 ## rnorm(n)
ftrue0 <- f_true(x1=x1_0, x2=x2_0)
errs0 <- rnorm(n, 0, 1/7)
y0 <- ftrue0 + errs0



## dfxy0 <- data.frame("y"=y0, "x1"=x1_0, "x2"=x2_0)
dfxy0 <- data.frame("y"=rep(NA, n), "x1"=x1_0, "x2"=x2_0)

y0hat <- predict(xtree, newdata=dfxy0)

xrmse <- sqrt( mean( (y0 - y0hat)^2 ) ) ; xrmse




########################### boosting



if(!dir.exists(file.path("~", "Desktop", "anim02"))) {
    dir.create(file.path("~", "Desktop", "anim02"))
} else {
    unlink(file.path("~", "Desktop", "anim02", "*"))
}


## f_best_split(x=x1, y=y, rtries=10, mincellsize=5)


ftreeq <- rep(0, n)

xls_fhat <- list()

xlambda <- 1/50
B <- 4000

rr <- y


for(iib in 1:B) {
    
    #dfxy <- data.frame("y"=rr, "x1"=x1, "x2"=x2)
    #xtree <- tree(y ~ x1 + x2, data=dfxy, minsize=2000, mincut=3, mindev=0)
    #summary(xtree)
    #this_fhat <- predict(xtree)
    
    xhood <- runif(1, 0.3, 1.0)
    this_fhat <- f_best_hood(X=cbind(x1, x2), y=rr, rtries=30, xhood=xhood)
    
    ftreeq <- ftreeq + xlambda * this_fhat
    
    rr <- rr - xlambda * this_fhat
    
    cat(mean(rr^2), "\n")
    
    if(TRUE) {
        if(xbool.savePlots) { png(file.path("~", "Desktop", "anim02", paste0("frame_", sprintf("%06d", iib), ".png")), width=800, height=800, pointsize=28) }
        par(mfrow=c(1,1), mar=c(1,1,1,1), bg="#000000")
        yhat_unit <- (ftreeq + 1.1) / 2.2
        xxcols <- hcl.colors(301, "YlOrRd", rev = FALSE)[ round(yhat_unit * 300)+1 ]
        plot(x2,  x1, col = xxcols, xlab="x2", ylab="x1", pch=19, main=iib, col.main="#FFFFFF", cex=0.7)
        if(xbool.savePlots) { dev.off() }
    }
    
}


if(xbool.savePlots) {
    xxsysstr <-
    paste0(
    "ffmpeg  -y  -loglevel error -r 30   " ,
    "  -i  ",
    file.path("~", "Desktop", "anim02", paste0("frame_", "%06d.png")),
    "  -codec:v libx264   -pix_fmt yuv420p  -r 15  -preset slow  ",
    file.path("~", "Desktop", "mov_boost_01.mp4")
    )
    
    system(xxsysstr)
}



xrmse <- sqrt( mean( (y - ftreeq)^2 ) ) ; xrmse
plot(ftreeq, y)




xrmse <- sqrt( mean( (y0 - ftreeq)^2 ) )
xrmse





if(xbool.savePlots) { png(file.path("~", "Desktop", "wiggles_funkyHoodBoost_3d.png"), width=1000, height=1000, pointsize=18) }
par(mfrow=c(1, 1))
#scatterplot3d( x1, x2, y, angle=30, xlab="x1", ylab="x2", zlab="y", cex.symbols=0.3) ### , type="l"
xsp3 <- scatterplot3d( x1, x2, y0, angle=30, xlab="x1", ylab="x2", zlab="y", cex.symbols=0.3, main=paste0("Funky Hood Boost, Prediction  RMSE: ", round(xrmse, 4))) ### , type="l"
xsp3$points3d(x=x1, y=x2, z=fhat0sum, col="#FF0000")
if(xbool.savePlots) { dev.off() }



####################### interacted natural splines


xknots <- seq(-4, 4, by=1)

mxH1 <- f_basisNatSplines(x=x1, xknots=xknots)
mxH2 <- f_basisNatSplines(x=x2, xknots=xknots)

xxeg <- expand.grid( "ndx1"=I(1:ncol(mxH1)), "ndx2"=I(1:(ncol(mxH1)-1)) )

mxX <- mxH1[ , xxeg[ , "ndx1"] ] * mxH2[ , -1][ , xxeg[ , "ndx2"] ]
dim(mxX)

bhatcross <- solve( crossprod(mxX) + diag(1, ncol(mxX)) ) %*% ( t(mxX) %*% y )


mxH1_0 <- f_basisNatSplines(x=x1_0, xknots=xknots)
mxH2_0 <- f_basisNatSplines(x=x2_0, xknots=xknots)
mxX0 <- mxH1_0[ , xxeg[ , "ndx1"] ] * mxH2_0[ , -1][ , xxeg[ , "ndx2"] ]

yhatcross0 <- mxX0 %*% bhatcross

xrmse <- sqrt( mean( (y0 - yhatcross0)^2 ) )
xrmse

#if(xbool.savePlots) { png(file.path("~", "Desktop", "wiggles_simfitGAM_finteract_3d.png"), width=1000, height=1000, pointsize=18) }
#par(mfrow=c(1, 1))
##scatterplot3d( x1, x2, y, angle=30, xlab="x1", ylab="x2", zlab="y", cex.symbols=0.3) ### , type="l"
#xsp3 <- scatterplot3d( x1, x2, y, angle=30, xlab="x1", ylab="x2", zlab="y", cex.symbols=0.3, main=paste0("Interacted Spines w/some Ridge!  RMSE: ", round(xrmse, 4))) ### , type="l"
#xsp3$points3d(x=x1, y=x2, z=yhatcross, col="#FF0000")
#if(xbool.savePlots) { dev.off() }





