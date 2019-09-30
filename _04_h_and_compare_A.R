


options(stringsAsFactors=FALSE, width=350)


xbool.savePlots <- FALSE


####### make sure in working directory
source("___f_funs.R")


############ prepair dataset for bias variance

library(mvtnorm)

p <- 2

N <- 1000



xSig <- diag(0.5, p) + 0.5
X <- cbind( rep(1, N), rmvnorm(N, c(0, 0), xSig) )

b <- c(1, 2, -1)

uu <- X %*% b

p.true <- 1 / ( 1 + exp(-uu) )



#################### plot true
x1dom <- seq(-4, 4, length=100)
x2dom <- seq(-4, 4, length=100)
Xdom <- as.matrix( expand.grid( "x1"=x1dom, "x2"=x2dom ) )

uu <- cbind(rep(1, N), Xdom) %*% b
p.dom <- 1 / ( 1 + exp(-uu) )


plot(x=Xdom[ , 1], y=Xdom[, 2], cex=p.dom)


mx.p.dom <- matrix(p.dom, length(x1dom), length(x2dom))

if(xbool.savePlots) { png(file.path("~", "Desktop", "logisticExample_compare_true.png"), width=2000, height=1000, pointsize=28) }
par(mfrow=c(1,2))
persp(x=x1dom, y=x2dom, z=mx.p.dom, zlab="True Prob", phi=30, theta=60)
persp(x=x1dom, y=x2dom, z=mx.p.dom, zlab="True Prob", phi=30, theta=150)
if(xbool.savePlots) { dev.off() }



#################

y <- rbinom(N, size=1, prob=p.true)



#######################

k.fold <- 10

iicos <- seq(0, N, length=k.fold+1)

ii <- 1

mx.yhat.knn <- matrix(NA, N, 10)
kks <- I(1:10) * 2 - 1
colnames(mx.yhat.knn) <- kks

for(ii in 1:k.fold) {
    
    xndx.valid <- I( (iicos[ ii ] + 1):iicos[ ii + 1 ] ) ; xndx.valid
    xndx.train <- setdiff( I(1:N), xndx.valid )
    
    for(jj in 1:ncol(mx.yhat.knn)) {
        fhat <- h.knn(X=X[ xndx.train, ], y=y[ xndx.train ], k=kks[jj])
        mx.yhat.knn[ xndx.valid, jj] <- fhat(x=X[ xndx.valid, ])[ , "1" ]
    }
    
}


######### now for something completely unexpected ...
######### convert category counts to "probability" using rank percentile


mx.p.hat.knn <- ( t( mx.yhat.knn ) + 1/2 ) / ( kks + 1 )

apply(mx.p.hat.knn, 1, function(x, ytrue) { return(f.logistic.cost( y=ytrue, p.hat=x)) }, ytrue=y)





################# no need to run -- try and find best k for KNN -- takes a long time
if(FALSE) {
    
    k.fold <- 10
    
    iicos <- seq(0, N, length=k.fold+1)
    
    kks <- I(1:10) * 4 - 3 ; kks
    
    kks <- c(7, 21, 35, 63)
    
    nn <- 20
    
    mx.bigOut <- matrix(NA, length(kks), nn)
    
    for(iinn in 1:nn) {
        
        y <- rbinom(N, size=1, prob=p.true)
        
        mx.yhat.knn <- matrix(NA, N, length(kks))
        
        colnames(mx.yhat.knn) <- kks
        
        ii <- 1
        
        for(ii in 1:k.fold) {
            
            xndx.valid <- I( (iicos[ ii ] + 1):iicos[ ii + 1 ] ) ; xndx.valid
            xndx.train <- setdiff( I(1:N), xndx.valid )
            
            for(jj in 1:ncol(mx.yhat.knn)) {
                fhat <- h.knn(X=X[ xndx.train, ], y=y[ xndx.train ], k=kks[ jj ])
                mx.yhat.knn[ xndx.valid, jj ] <- fhat(x=X[ xndx.valid, ])[ , "1" ]
            }
            
        }
        
        mx.p.hat.knn <- ( t( mx.yhat.knn ) + 1/2 ) / ( kks + 1 )
        
        mx.bigOut[ , iinn ] <- apply(mx.p.hat.knn, 1, function(x, ytrue) { return(f.logistic.cost(y=ytrue, p.hat=x)) }, ytrue=y)
        
        cat(iinn, "\n")
    }
    
    
    xlogit.cost <- log(apply( exp(mx.bigOut), 1, mean )) ; xlogit.cost
    
    plot(kks, xlogit.cost, type="l")
    
}


##################### logistic



y.hat.logistic <- rep(NA, N)

for(ii in 1:k.fold) {
    
    xndx.valid <- I( (iicos[ ii ] + 1):iicos[ ii + 1 ] ) ; xndx.valid
    xndx.train <- setdiff( I(1:N), xndx.valid )
    
    fhat.logistic <- h.logistic(X=X[ xndx.train, , drop=FALSE ], y=y[ xndx.train ])
    
    y.hat.logistic[ xndx.valid ] <- fhat.logistic(x=X[ xndx.valid, , drop=FALSE ])
    
}

f.logistic.cost(y=y, p.hat=y.hat.logistic)





####################### like in book


k.fold <- 10

iicos <- seq(0, N, length=k.fold+1)

kk <- 28

nn <- 50

xcost.out.knn <- rep(NA, nn)
xcost.out.logistic <- rep(NA, nn)

for(iinn in 1:nn) {
    
    y <- rbinom(N, size=1, prob=p.true)
    
    yhat.knn <- rep(NA, N)
    y.hat.logistic <- rep(NA, N)
    
    ii <- 1
    
    for(ii in 1:k.fold) {
        
        xndx.valid <- I( (iicos[ ii ] + 1):iicos[ ii + 1 ] ) ; xndx.valid
        xndx.train <- setdiff( I(1:N), xndx.valid )
        
        ###### KNN
        fhat <- h.knn(X=X[ xndx.train, ], y=y[ xndx.train ], k=kk)
        yhat.knn[ xndx.valid ] <- fhat(x=X[ xndx.valid, ])[ , "1" ]
        
        ###### Logistic
        fhat.logistic <- h.logistic(X=X[ xndx.train, , drop=FALSE ], y=y[ xndx.train ])
        y.hat.logistic[ xndx.valid ] <- fhat.logistic(x=X[ xndx.valid, , drop=FALSE ])
        
        
    }
    
    p.hat.knn <- ( yhat.knn + 1/2 ) / ( kk + 1 )
    
    xcost.out.knn[ iinn ] <- f.logistic.cost(y=y, p.hat=p.hat.knn)
    
    xcost.out.logistic[ iinn ] <- f.logistic.cost(y=y, p.hat=y.hat.logistic)
    
    cat(iinn, "\n")
}


if(xbool.savePlots) { png(file.path("~", "Desktop", "logisticExample_compare_knn.png"), width=1000, height=1000, pointsize=28) }
par(mfrow=c(1, 1))
boxplot(x=list("KNN-28"=xcost.out.knn, "Logistic"=xcost.out.logistic))
if(xbool.savePlots) { dev.off() }




