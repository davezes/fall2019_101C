

###

###

options(stringsAsFactors=FALSE, width=350)

xbool.savePlots <- FALSE


####### make sure in working directory
source("___f_funs.R")


############ prepair dataset for bias variance

library(mvtnorm)

library(plot3D)

p <- 2

N <- 2000 ##### must be even

set.seed(777)


X <- rbind(
rmvnorm(N, c(0,0), diag(0.5, p) + 0.5)
)

##########################################
xphi <- c(1, 3) ### must be p long


fun.true <- function(X, xphi) {
    return(apply( cos( t(X) * xphi ), 2, sum ))
}

f.true <- fun.true(X=X, xphi=xphi)


errs <- rnorm(N, 0, 1/20)

y <- f.true + errs


################### plot f true wireframe
x1dom <- seq(-2.5, 2.5, length=50)
x2dom <- seq(-2.5, 2.5, length=70)

Xdom <- expand.grid(x1dom, x2dom)
f.true.grid <- matrix( fun.true(X=Xdom, xphi=xphi), length(x1dom), length(x2dom) )

if(xbool.savePlots) { png(file.path("~", "Desktop", "KNN_rgr_wf_2d.png"), width=2000, height=1000, pointsize=28) }
par(mfrow=c(1, 2))
persp(x1dom, x2dom, f.true.grid, phi=15, theta=30, xlab="x1", ylab="x2", zlab="y")
persp(x1dom, x2dom, f.true.grid, phi=15, theta=120, xlab="x1", ylab="x2", zlab="y")
if(xbool.savePlots) { dev.off() }


################### plot data
if(xbool.savePlots) { png(file.path("~", "Desktop", "KNN_rgr_2d.png"), width=2000, height=1000, pointsize=28) }
par(mfrow=c(1, 2))
scatter3D(X[,1], X[,2], y, phi=15, theta=30, xlab="x1", ylab="x2", zlab="y")
scatter3D(X[,1], X[,2], y, phi=15, theta=120, xlab="x1", ylab="x2", zlab="y")
if(xbool.savePlots) { dev.off() }

################### bivar plots w/ y as color
if(xbool.savePlots) { png(file.path("~", "Desktop", "KNN_rgr_bivar_2d.png"), width=2000, height=1000, pointsize=28) }
par(mfrow=c(1, 3))
plot( X[, 1], y, col=rainbow(N*2)[1:N][ rank(y) ], pch=19)
plot( X[, 2], y, col=rainbow(N*2)[1:N][ rank(y) ], pch=19)
plot( X[,1], X[,2], col=rainbow(N*2)[1:N][ rank(y) ], pch=19)
if(xbool.savePlots) { dev.off() }




k <- 1


iindx <- sample( N, size=round(N/5) )
X.train <- X[ -iindx, ]
y.train <- y[ -iindx ]

######## from funs file
fhat <- h.knn.rgr(X=X.train, y=y.train, k=k)

X.valid <- X[ iindx, ]
y.valid <- y[ iindx ]

y0 <- fhat(x=X.valid)

xrmse <- sqrt( mean( (y.valid - y0)^2 ) ) ; xrmse



####################################################### LM



X.train <- X[ -iindx, ]
y.train <- y[ -iindx ]

############## if xphi is 1,1 , try 4,4 for porders

##### from funs file
fhat <- h.poly.multi(X=X.train, y=y.train, porders=c(4, 8), int=TRUE)

X.valid <- X[ iindx, ]
y.valid <- y[ iindx ]

y0 <- fhat(X=X.valid)

xrmse <- sqrt( mean( (y.valid - y0)^2 ) ) ; xrmse




