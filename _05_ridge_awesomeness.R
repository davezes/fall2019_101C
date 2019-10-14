


options(stringsAsFactors=FALSE, width=350)


xbool.savePlots <- TRUE


####### make sure in working directory
source("___f_funs.R")


############ prepair dataset for bias variance

library(mvtnorm)

p <- 250

N <- 300

set.seed(777)


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




####################################### lambda search

k.fold <- N/2 ##### if you set this to N, it'll be LOOCV
iicos <- seq(0, N, length=k.fold+1)


## xlambdas.vec <- c(0, exp( seq(-10, 2, length=28) )) ; xlambdas.vec

xlambdas.vec <- seq(0, 0.03, length=50) ; xlambdas.vec

R2vec <- NULL
MSEtestvec <- NULL

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
    
    R2vec[iilam] <- R2
    MSEtestvec[iilam] <- MSE.test
    
    cat(iilam, "::", xlambda, " -- ", R2, " -- ", MSE.test, "\n")
    
}


if(xbool.savePlots) { png("~/Desktop/ridgeAwesomeness_01.png", height=1000, width=1000, pointsize=24) }
plot(xlambdas.vec, R2vec, type="l", col="#00AA00", lwd=3)
if(xbool.savePlots) { dev.off() }


############

########################################### lasso using glmnet
########################################### lasso using glmnet
########################################### lasso using glmnet

library(glmnet)

library(dplyr)


k.fold <- N/2 ##### if you set this to N, it'll be LOOCV
iicos <- seq(0, N, length=k.fold+1)


## xlambdas.vec <- exp( seq(-10, 3, length=30) ) ; xlambdas.vec

xlambdas.vec <- seq(0, 0.03, length=50) ; xlambdas.vec

R2vecLass <- NULL
MSEtestvecLass <- NULL

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
    
    R2vecLass[ iilam ] <- R2
    MSEtestvecLass[ iilam ] <- MSE.test
    
    cat(iilam, "::", xlambda, " -- ", R2, " -- ", MSE.test, "\n")
    
}



xlambda <- 0.004016914

xlambda <- 0.1

xdf <- data.frame("X"=X, "y"=y)

xx <- model.matrix(y~., xdf)[ , -1]

xglmnet <- glmnet(x=xx, y=y, family = "gaussian", alpha = 1, lambda = xlambda)

coef(xglmnet)

if(xbool.savePlots) { png("~/Desktop/ridgeAwesomeness_02.png", height=1000, width=1000, pointsize=24) }
plot(xlambdas.vec, R2vec, type="l", col="#00AA00", lwd=5, ylab="CV Test R2")
points(xlambdas.vec, R2vecLass, type="l", col="#0000AA", lwd=5)
legend(0.023, 0.24, legend=c("Ridge", "Lasso"), bty="n", col=c("#00AA00", "#0000AA"), lwd=5)
if(xbool.savePlots) { dev.off() }





