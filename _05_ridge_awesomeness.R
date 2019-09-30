


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



X <- rmvnorm(N, rep(0, p), xSig)


##################### deliberate sabotage !

f.true <- X %*% eigen(xSig)[[ "vectors" ]][ , (p-3):p ] %*% (10 * c(1, -1, 2, -2))

f.true <- X %*% eigen(xSig)[[ "vectors" ]][ , (p-19):p ] %*% (1 * rep(c(-1, 1), 10))

var(f.true)

y <- f.true + rnorm(N)
##xlm <- lm(y~X)
##summary(xlm)


########################### fit using full design matrix

xlm <- lm(y~X)

summary(xlm)





########################### no ridge


k.fold <- 30
k.fold <- N/1 ##### if you set this to N, it'll be LOOCV

iicos <- seq(0, N, length=k.fold+1)

y.hat <- rep(NA, N)

for(ii in 1:k.fold) {
    
    xndx.valid <- I( (iicos[ ii ] + 1):iicos[ ii + 1 ] ) ; xndx.valid
    xndx.train <- setdiff( I(1:N), xndx.valid )
    
    ###### shrink = 0
    xlambda <- 0/2
    f.hat <- h.ridge.scale(X=X[ xndx.train, , drop=FALSE ], y=y[ xndx.train ], lambda=xlambda)
    y.hat[ xndx.valid ] <- f.hat(x=X[ xndx.valid, , drop=FALSE ])
    
    ###### or use this
    #xdf <- data.frame("X"=X[ xndx.train, , drop=FALSE ], "y"=y[ xndx.train ])
    #ylm <- lm(y~., data=xdf)
    #y.hat[ xndx.valid ] <- predict(ylm, newdata=data.frame("X"=X[ xndx.valid, , drop=FALSE ], "y"=rep(NA, length(xndx.valid))))
    
}

MSE.test <- mean( (y - y.hat)^2 )
MSE.test

R2 <- 1 - MSE.test / mean( (y - mean(y))^2 )
R2

#### R2 <- var(y.hat) / var(y) ; R2


plot(y.hat, y)




####################################### lambda search

k.fold <- N/2 ##### if you set this to N, it'll be LOOCV
iicos <- seq(0, N, length=k.fold+1)


xlambdas.vec <- exp( seq(-10, 2, length=28) ) ; xlambdas.vec

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
    
    MSE.test <- mean( (y - y.hat)^2 )
    sqrt( MSE.test )
    
    R2 <- 1 - MSE.test / mean( (y - mean(y))^2 )
    R2
    
    cat(iilam, "::", xlambda, " -- ", R2, " -- ", MSE.test, "\n")
    
}




############

########################################### lasso using glmnet
########################################### lasso using glmnet
########################################### lasso using glmnet

library(glmnet)

library(dplyr)


k.fold <- N/1 ##### if you set this to N, it'll be LOOCV
iicos <- seq(0, N, length=k.fold+1)


xlambdas.vec <- exp( seq(-10, 3, length=30) ) ; xlambdas.vec

for(iilam in 1:length(xlambdas.vec)) {
    
    y.hat <- rep(NA, N)
    
    for(ii in 1:k.fold) {
        
        xndx.valid <- I( (iicos[ ii ] + 1):iicos[ ii + 1 ] ) ; xndx.valid
        xndx.train <- setdiff( I(1:N), xndx.valid )
        
        ######
        xlambda <- xlambdas.vec[ iilam ]
        
        xdf <- data.frame("X"=X[ xndx.train, , drop=FALSE ], "y"=y[ xndx.train ])
 
        xx <- model.matrix(y~., xdf)[ , -1]
        
        xglmnet <- glmnet(x=xx, y=y[ xndx.train ], family = "gaussian", alpha = 1, lambda = xlambda)
        
        x.test <- model.matrix(y~., data.frame("X"=X[ xndx.valid, , drop=FALSE ], "y"=y[ xndx.valid ]))[ , -1, drop=FALSE]
        y.hat[ xndx.valid ] <- xglmnet %>% predict(newx = x.test)

    }
    
    MSE.test <- mean( (y - y.hat)^2 )
    sqrt( MSE.test )
    
    R2 <- 1 - MSE.test / mean( (y - mean(y))^2 )
    R2
    
    
    cat(iilam, "::", xlambda, " -- ", R2, " -- ", MSE.test, "\n")
    
}



xlambda <- 0.004016914

xlambda <- 0.1

xdf <- data.frame("X"=X, "y"=y)

xx <- model.matrix(y~., xdf)[ , -1]

xglmnet <- glmnet(x=xx, y=y, family = "gaussian", alpha = 1, lambda = xlambda)

coef(xglmnet)



#############################


X.ext <- rmvnorm(N, rep(0, p), xSig)

f.true.ext <- X.ext %*% eigen(xSig)[[ "vectors" ]][ , (p-19):p ] %*% (1 * rep(c(-1, 1), 10))

y.ext <- f.true.ext + rnorm(N)
##xlm <- lm(y~X)
##summary(xlm)

xlambda <- xlambdas.vec[ 13 ] ; xlambda

f.hat <- h.ridge.scale(X=X, y=y, lambda=xlambda)
y.hat.ext <- f.hat(x=X.ext)


MSE.test <- mean( (y.ext - y.hat.ext)^2 )
MSE.test


