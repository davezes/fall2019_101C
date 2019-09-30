


options(stringsAsFactors=FALSE, width=350)



xbool.savePlots <- FALSE

############ simple 1D support

N <- 10000

library(mvtnorm)

p <- 3

X <- cbind( rep(1, N), rmvnorm(N, rep(0, p), diag(1, p)) )

b <- c(1, -1, 1, 2)


f.true <- 1 / ( 1 + exp(- X %*% b) ) ; ## f.true

y <- rbinom(N, size=1, prob=f.true)

xdf <- data.frame("y"=y, X)

xglm <- glm(y ~ . -1, data=xdf, family=binomial(link="logit"))

summary(xglm)






######################################## derivative descent

b.hat <- rep(0, ncol(X))

reg <- diag(0/2, ncol(X))

xbool.keepGoing <- TRUE

mx.out <- rbind(NULL, b.hat)

while( xbool.keepGoing ) {
    phat <- as.vector(  1 / (1+ exp(- X %*% b.hat))  )
    ww <- phat*(1-phat)
    b.prev <- b.hat
    b.hat <- b.hat + solve( t(X * ww) %*% X   + reg ) %*% ( t(X) %*% (y-phat) )
    if( sum( (b.hat - b.prev)^2 ) < 10^(-16) ) { xbool.keepGoing <- FALSE }
    mx.out <- rbind(mx.out, t(b.hat))
    cat(b.hat, "\n")
}





##################### grid search for beta_hat



###### assume we know b0

xgrid <- as.matrix(expand.grid("b0"=1, "b1"=seq(-2, 3, length=50), "b2"=seq(-2, 3, length=50), "b3"=seq(-2, 3, length=50)))

tail(xgrid)

xcost <- rep(NA, nrow(xgrid))
for(ii in 1:nrow(xgrid)) {
    
    b.try <- xgrid[ii, ]
    uu <- X %*% b.try
    phat <- 1 / (1 + exp(-uu))
    
    xcost[ ii ] <- f.logistic.cost(y=y, p.hat=phat)
    if(ii %% 1000 == 0) {
    cat(ii, "\n")
    }
}


xndx <- which.min(xcost)

xgrid[ xndx, ]



library(scatterplot3d)

xxrnk <- ceiling( rank( -xcost) * 499 / length(xcost) )

xcols <- rainbow(1000)[ 500:1000 ][xxrnk ]
length(xcols)

if(xbool.savePlots) { png(file.path("~", "Desktop", "gridSearchLogistic_d_1d.png"), width=2000, height=1000, pointsize=28) }
par(mfrow=c(1, 2))
xsp3 <- scatterplot3d( xgrid[ ,2], xgrid[ ,3], xgrid[ ,4], angle=40, xlab="b1", ylab="b2", zlab="b3", cex.symbols=xxrnk/10000, color=xcols) ### , type="l"
xsp3$points3d(x=b[2], y=b[3], z=b[4], lwd=3, col="#000000", type="h")
xsp3$points3d(x=mx.out[ ,2], y=mx.out[ ,3], z=mx.out[ ,4], lwd=5, col="#AAAAAA", type="l")

xsp3 <- scatterplot3d( xgrid[ ,2], xgrid[ ,3], xgrid[ ,4], angle=130, xlab="b1", ylab="b2", zlab="b3", cex.symbols=xxrnk/10000, color=xcols) ### , type="l"
xsp3$points3d(x=b[2], y=b[3], z=b[4], lwd=3, col="#000000", type="h")
xsp3$points3d(x=mx.out[ ,2], y=mx.out[ ,3], z=mx.out[ ,4], lwd=5, col="#AAAAAA", type="l")
if(xbool.savePlots) { dev.off() }












