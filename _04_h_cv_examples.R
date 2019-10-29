


options(stringsAsFactors=FALSE, width=350)


xbool.savePlots <- FALSE


####### make sure in working directory
source("___f_funs.R")


############ prepair dataset for bias variance

library(mvtnorm)

p <- 1

N <- 300


set.seed(777)


x <- rnorm(N, 4, 1)
ftrue <- 3*sin(abs(x)*x/7)
ftrue <- 3*sin(x/2)
x <- x - mean(x)

errs <- rnorm(N, 1/7)

y <- ftrue + errs


if(xbool.savePlots) { png(file.path("~", "Desktop", "example_simple_01.png"), width=1000, height=1000, pointsize=28) }
par(mfrow=c(1,1))
plot(x=x, y=y)
if(xbool.savePlots) { dev.off() }





#######################

k.fold <- 30

iicos <- seq(0, N, length=k.fold+1)

ls.ndx.train <- list()
ls.ndx.valid <- list()

for(ii in 1:k.fold) {
    
    xndx.valid <- I( (iicos[ ii ] + 1):iicos[ ii + 1 ] ) ; xndx.valid
    xndx.train <- setdiff( I(1:N), xndx.valid )
    
    ls.ndx.train[[ ii ]] <- xndx.train
    ls.ndx.valid[[ ii ]] <- xndx.valid
  
}




############################ R^2 in place
xdom <- seq(min(x), max(x), length=500)

fhat <- h.poly(x=x, y=y, porder=1)
R2_1 <- 1 - mean( (y - fhat(x=x) )^2 ) / var(y) ; R2_1
y1fit <- fhat(x=xdom)

fhat <- h.poly(x=x, y=y, porder=3)
R2_3 <- 1 - mean( (y - fhat(x=x) )^2 ) / var(y) ; R2_3
y3fit <- fhat(x=xdom)

fhat <- h.poly(x=x, y=y, porder=4)
R2_4 <- 1 - mean( (y - fhat(x=x) )^2 ) / var(y) ; R2_4
y4fit <- fhat(x=xdom)

fhat <- h.poly(x=x, y=y, porder=8)
R2_8 <- 1 - mean( (y - fhat(x=x) )^2 ) / var(y) ; R2_8
y8fit <- fhat(x=xdom)

fhat <- h.poly(x=x, y=y, porder=12)
R2_12 <- 1 - mean( (y - fhat(x=x) )^2 ) / var(y) ; R2_12
y12fit <- fhat(x=xdom)



if(xbool.savePlots) { png(file.path("~", "Desktop", "example_simple_fits_01.png"), width=1000, height=1000, pointsize=28) }
par(mfrow=c(1,1))
plot(x=x, y=y)
points(xdom, y1fit, col=rainbow(15)[1], type="l", lwd=5)
points(xdom, y3fit, col=rainbow(15)[3], type="l", lwd=5)
points(xdom, y4fit, col=rainbow(15)[5], type="l", lwd=5)
points(xdom, y8fit, col=rainbow(15)[9], type="l", lwd=5)
points(xdom, y12fit, col=rainbow(15)[11], type="l", lwd=5)
if(xbool.savePlots) { dev.off() }



#####################

y.hat.poly1 <- rep(NA, N)
y.hat.poly3 <- rep(NA, N)
y.hat.poly4 <- rep(NA, N)
y.hat.poly8 <- rep(NA, N)
y.hat.poly12 <- rep(NA, N)

for(ii in 1:k.fold) {
    
    xndx.valid <- ls.ndx.valid[[ ii ]] ; xndx.valid
    xndx.train <- ls.ndx.train[[ ii ]]
    
    fhat <- h.poly(x=x[ xndx.train ], y=y[ xndx.train ], porder=1)
    y.hat.poly1[ xndx.valid ] <- fhat(x=x[ xndx.valid ])
    
    fhat <- h.poly(x=x[ xndx.train ], y=y[ xndx.train ], porder=3)
    y.hat.poly3[ xndx.valid ] <- fhat(x=x[ xndx.valid ])
    
    fhat <- h.poly(x=x[ xndx.train ], y=y[ xndx.train ], porder=4)
    y.hat.poly4[ xndx.valid ] <- fhat(x=x[ xndx.valid ])
    
    fhat <- h.poly(x=x[ xndx.train ], y=y[ xndx.train ], porder=8)
    y.hat.poly8[ xndx.valid ] <- fhat(x=x[ xndx.valid ])
    
    fhat <- h.poly(x=x[ xndx.train ], y=y[ xndx.train ], porder=12)
    y.hat.poly12[ xndx.valid ] <- fhat(x=x[ xndx.valid ])
    
}


sqrt( mean( (y - y.hat.poly1)^2 ) )

sqrt( mean( (y - y.hat.poly3)^2 ) )

sqrt( mean( (y - y.hat.poly4)^2 ) )

sqrt( mean( (y - y.hat.poly8)^2 ) )

sqrt( mean( (y - y.hat.poly12)^2 ) )



CVR2_1 <- 1 - mean( (y - y.hat.poly1)^2 ) / var(y) ; CVR2_1
CVR2_3 <- 1 - mean( (y - y.hat.poly3)^2 ) / var(y) ; CVR2_3
CVR2_4 <- 1 - mean( (y - y.hat.poly4)^2 ) / var(y) ; CVR2_4
CVR2_8 <- 1 - mean( (y - y.hat.poly8)^2 ) / var(y) ; CVR2_8
CVR2_12 <- 1 - mean( (y - y.hat.poly12)^2 ) / var(y) ; CVR2_12


xxxr2 <- c(R2_1, R2_3, R2_4, R2_8, R2_12)

xxxcvr2 <- c(CVR2_1, CVR2_3, CVR2_4, CVR2_8, CVR2_12)


xdf <- data.frame("R2"=xxxr2, "CV R2"=xxxcvr2)

rownames(xdf) <- c("poly 1", "poly 3", "poly 4", "poly 8", "poly 12")

xdf

print(xdf)


