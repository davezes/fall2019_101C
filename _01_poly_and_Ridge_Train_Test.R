

###

### THESE ARE DATA ESTIMATES, EMPIRIC ESTIMATES OF MSE

rm(list=ls())

options(stringsAsFactors=FALSE, width=350)


xbool.savePlots <- FALSE


############################### poly example
############################### poly example
############################### poly example




####### make sure in working directory
source("___f_funs.R")


############ prepair dataset for bias variance



N <- 500

set.seed(777)

x <- sort(runif(N, -1, 1))

## xdom <- seq(-1, 1, length=300)

f.true <- 100 + 40 * x
f.true <- 5 * sin( 3 * x )

xsig <- 1/1




errs <- rnorm(N) * xsig

y <- f.true + errs

plot(x, y)



n.valid <- 100

xpords <- seq(0, 20, by=1)


nn <- 1000

mx.train.mse <- matrix(NA, length(xpords), nn)
mx.valid.mse <- matrix(NA, length(xpords), nn)

for(iibig in 1:nn) {
    
    ndx.valid <- sample(N, size=n.valid)
    ndx.train <- setdiff( I(1:N), ndx.valid )
    
    x.train <- x[ ndx.train ]
    y.train <- y[ ndx.train ]
    
    x.valid <- x[ ndx.valid ]
    y.valid <- y[ ndx.valid ]
    
    train.mse <- rep(NA, length(xpords))
    valid.mse <- rep(NA, length(xpords))
    
    ii <- 1
    
    for(ii in 1:length(xpords)) {
        
        f.hat <- h.poly(x=x.train, y=y.train, porder=xpords[ii])
        
        y.hat.train <- f.hat(x=x.train)
        
        y.hat.valid <- f.hat(x=x.valid)
        
        train.mse[ii] <- mean( (y.train - y.hat.train)^2 )
        
        valid.mse[ii] <- mean( (y.valid - y.hat.valid)^2 )
        
    }
    
    mx.train.mse[ , iibig ] <- train.mse
    mx.valid.mse[ , iibig ] <- valid.mse
    
    cat(iibig, "\n")
}

train.mse.big <- apply(mx.train.mse, 1, mean)
valid.mse.big <- apply(mx.valid.mse, 1, mean)


if(xbool.savePlots) { png(file.path("~", "Desktop", "polyTrainTest_01.png"), height=1000, width=2000, pointsize=24) }
#png("~/Desktop/polyTrainTest_01.png", height=1000, width=2000, pointsize=24)

par(mfrow=c(1, 2))

plot(xpords, train.mse.big, ylim=range(c(train.mse.big,valid.mse.big)), type="l", col="#AAAAAA", lwd=4, xlab="polynomial order", ylab="MSE")
points(xpords, valid.mse.big, type="l", col="#AA0000", lwd=4)

plot(xpords, train.mse.big, ylim=c(0.5, 1.5), type="l", col="#AAAAAA", lwd=4, xlab="polynomial order", ylab="MSE")
points(xpords, valid.mse.big, type="l", col="#AA0000", lwd=4)

#dev.off()
if(xbool.savePlots) { dev.off() }



##################################### ridge example
##################################### ridge example
##################################### ridge example
##################################### ridge example



############# sim data

N <- 5000

p <- 200

X <- cbind(rep(1, N), matrix(rnorm(N * p, 0, 1), N, p))

b.true <- c(1, seq(1, 3, length=p))

y <- X %*% b.true + rnorm(N, 0, 50)





xlambdas <- seq(0, 1, length=20) / 5

n.valid <- 500

nn <- 20

mx.train.mse <- matrix(NA, length(xlambdas), nn)
mx.valid.mse <- matrix(NA, length(xlambdas), nn)

for(iibig in 1:nn) {
    
    ndx.valid <- sample(N, size=n.valid)
    ndx.train <- setdiff( I(1:N), ndx.valid )
    
    
    X.train <- X[ ndx.train, ]
    y.train <- y[ ndx.train ]
    
    
    X.valid <- X[ ndx.valid, ]
    y.valid <- y[ ndx.valid ]
    
    train.mse <- rep(NA, length(xlambdas))
    valid.mse <- rep(NA, length(xlambdas))
    
    ii <- 1
    
    for(ii in 1:length(xlambdas)) {
        
        ###### from funs file
        f.hat <- h.ridge(X=X.train, y=y.train, lambda=xlambdas[ii] * N)
        
        y.hat.train <- f.hat(x=X.train)
        
        y.hat.valid <- f.hat(x=X.valid)
        
        
        train.mse[ii] <- mean( (y.train - y.hat.train)^2 )
        
        valid.mse[ii] <- mean( (y.valid - y.hat.valid)^2 )
        
    }
    
    mx.train.mse[ , iibig ] <- train.mse
    mx.valid.mse[ , iibig ] <- valid.mse
    
    cat(iibig, "\n")
}

train.mse.big <- apply(mx.train.mse, 1, mean)
valid.mse.big <- apply(mx.valid.mse, 1, mean)


if(xbool.savePlots) { png(file.path("~", "Desktop", "ridgeTrainTest_01.png"), height=1000, width=1000, pointsize=24) }

par(mfrow=c(1, 1))

plot(xlambdas, train.mse.big, ylim=range(c(train.mse.big, valid.mse.big)), type="l", col="#AAAAAA", lwd=4, ylab="MSE", xlab="lambda ( <--- flexibility )")
points(xlambdas, valid.mse.big, type="l", col="#AA0000", lwd=4)

if(xbool.savePlots) { dev.off() }

