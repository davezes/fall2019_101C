


options(stringsAsFactors=FALSE, width=350)


xbool.savePlots <- FALSE


####### make sure in working directory
source("___f_funs.R")


############ prepair dataset for bias variance

library(mvtnorm)

p <- 250

N <- 300

## set.seed(777)


xSig <- diag(2, p)
offdiags <- runif(p*(p-1)/2, -0.1, 0.1)

xSig[ lower.tri(xSig, diag = FALSE) ] <- offdiags
xSig <- t(xSig)
xSig[ lower.tri(xSig, diag = FALSE) ] <- offdiags





nn <- 100


k.fold <- N/10 ##### if you set this to N, it'll be LOOCV
iicos <- seq(0, N, length=k.fold+1)



xlambdas.vec <- exp( seq(-9, 1, length=25) ) ; xlambdas.vec



xvalid.mse <- rep(NA, nn)
xtest.mse <- rep(NA, nn)


for(iinn in 1:nn) {
    
    X <- rmvnorm(N, rep(0, p), xSig)
    
    ##################### deliberate sabotage !
    
    f.true <- X %*% eigen(xSig)[[ "vectors" ]][ , (p-19):p ] %*% (1 * rep(c(-1, 1), 10))
    
    var(f.true)
    
    y <- f.true + rnorm(N)
    ##xlm <- lm(y~X)
    ##summary(xlm)
    
    
    ########################### fit using full design matrix
    
    xlm <- lm(y~X)
    
    summary(xlm)
    
    
    ####################################### lambda search
    

    
    MSE.valid.best <- Inf
    
    for(iilam in 1:length(xlambdas.vec)) {
        
        y.hat <- rep(NA, N)
        
        for(ii in 1:k.fold) {
            
            xndx.valid <- I( (iicos[ ii ] + 1):iicos[ ii + 1 ] ) ; xndx.valid
            xndx.train <- setdiff( I(1:N), xndx.valid )
            
            ######
            xlambda <- xlambdas.vec[ iilam ]
            
            f.hat <- h.ridge.scale(X=X[ xndx.train, , drop=FALSE ], y=y[ xndx.train ], lambda=xlambda)
            y.hat[ xndx.valid ] <- f.hat(x=X[ xndx.valid, , drop=FALSE ])
            
        }
        
        MSE.valid <- mean( (y - y.hat)^2 )
        sqrt( MSE.valid )
        
        R2 <- 1 - MSE.valid / mean( (y - mean(y))^2 )
        R2
        
        cat(iilam, "::", xlambda, " -- ", R2, " -- ", MSE.valid, "\n")
        
        if(MSE.valid < MSE.valid.best) {
            MSE.valid.best <- MSE.valid
            iil.best <- iilam
        }
        
    }
    
    xvalid.mse[ iinn ] <- MSE.valid.best
    
    #############################
    
    
    X.ext <- rmvnorm(N, rep(0, p), xSig)
    
    f.true.ext <- X.ext %*% eigen(xSig)[[ "vectors" ]][ , (p-19):p ] %*% (1 * rep(c(-1, 1), 10))
    
    y.ext <- f.true.ext + rnorm(N)
    ##xlm <- lm(y~X)
    ##summary(xlm)
    
    xlambda <- xlambdas.vec[ iil.best ] ; xlambda
    
    f.hat <- h.ridge.scale(X=X, y=y, lambda=xlambda)
    y.hat.ext <- f.hat(x=X.ext)
    
    
    MSE.test <- mean( (y.ext - y.hat.ext)^2 )
    MSE.test
    
    xtest.mse[ iinn ] <- MSE.test
    
    cat("\n\n\n", iinn, "::", MSE.test, "\n\n\n")
    
}




cbind( xvalid.mse, xtest.mse )


t.test(xvalid.mse, xtest.mse)


plot(xvalid.mse, xtest.mse)

cor(xvalid.mse, xtest.mse)


nn.perm <- 10000

xcor.rnd <- rep(NA, nn.perm)

for(ii.rnd in 1:nn.perm) {
    
    yy.rnd <- sample(xtest.mse)
    
    xcor.rnd[ ii.rnd ] <- cor( xvalid.mse, yy.rnd )
}

hist(xcor.rnd)


