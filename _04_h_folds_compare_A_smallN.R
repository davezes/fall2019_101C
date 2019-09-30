


options(stringsAsFactors=FALSE, width=350)


xbool.savePlots <- FALSE


####### make sure in working directory
source("___f_funs.R")


############ prepair dataset for bias variance

library(mvtnorm)

p <- 2

N <- 100



xSig <- diag(0.5, p) + 0.5
X <- cbind( rep(1, N), rmvnorm(N, c(0, 0), xSig) )

b <- c(1, 2, -1)

uu <- X %*% b

p.true <- 1 / ( 1 + exp(-uu) )





#######################

kk <- 28

nn <- 50

kk.folds <- c(5, 10, 50, 100)

xls.xcost.out.knn <- list()
xls.xcost.out.logistic <- list()


for(kkfs in 1:length(kk.folds)) {
    
    k.fold <- kk.folds[ kkfs ]
    
    iicos <- seq(0, N, length=k.fold+1)
    

    
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
            yhat.knn[ xndx.valid ] <- fhat(x=X[ xndx.valid, , drop=FALSE])[ , "1" ]
            
            ###### Logistic
            fhat.logistic <- h.logistic(X=X[ xndx.train, , drop=FALSE ], y=y[ xndx.train ])
            y.hat.logistic[ xndx.valid ] <- fhat.logistic(x=X[ xndx.valid, , drop=FALSE ])
            
            
        }
        
        p.hat.knn <- ( yhat.knn + 1/2 ) / ( kk + 1 )
        
        xcost.out.knn[ iinn ] <- f.logistic.cost(y=y, p.hat=p.hat.knn)
        
        xcost.out.logistic[ iinn ] <- f.logistic.cost(y=y, p.hat=y.hat.logistic)
        
        cat(iinn, "\n")
    }
    
    xls.xcost.out.knn[[ kkfs ]] <- xcost.out.knn
    xls.xcost.out.logistic[[ kkfs ]] <- xcost.out.logistic
    
}


xall.costs <- c(unlist(xls.xcost.out.knn), unlist(xls.xcost.out.logistic))

if(xbool.savePlots) { png(file.path("~", "Desktop", "logisticExample_compare_kfolds_smN.png"), width=2000, height=1000, pointsize=28) }
par(mfrow=c(1, 2))
boxplot(x=xls.xcost.out.knn, names=kk.folds, xlab="k-folds", main="KNN-28", ylim=range(xall.costs))
boxplot(x=xls.xcost.out.logistic, names=kk.folds, xlab="k-folds", main="Logistic Rgr", ylim=range(xall.costs))
if(xbool.savePlots) { dev.off() }



t.test(xls.xcost.out.logistic[[1]], xls.xcost.out.logistic[[4]])

t.test(xls.xcost.out.knn[[1]], xls.xcost.out.knn[[4]])



t.test(xls.xcost.out.logistic[[4]], xls.xcost.out.knn[[4]])


