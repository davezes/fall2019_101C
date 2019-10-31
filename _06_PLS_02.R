


options(stringsAsFactors=FALSE, width=350)


xbool.savePlots <- TRUE


####### make sure in working directory
source("___f_funs.R")


############ prepair dataset for bias variance

library(mvtnorm)

p <- 4

n <- 500

set.seed(777)


mxX <- rmvnorm(n, rep(0, p), diag(1, p))

head(mxX)



btrue <- c(2, 1, 0.1, 0 )


ftrue <- mxX %*% btrue

yerr <- rnorm(n, 0, 2)

y <- ftrue + yerr



##############

mxXnoi <- mxX

xmeans <- apply(mxXnoi, 2, mean)
xsds <- apply(mxXnoi, 2, sd)

ymean <- mean(y)
ysd <- sd(y)

############

mxXstnd <- t( ( t(mxXnoi) - xmeans ) / xsds )

ystnd <- (y - ymean) / ysd


xlm <- lm(ystnd~mxXnoi)
summary(xlm)


phis1 <- cor(mxXstnd, ystnd) ; phis1

Z1 <- apply( mxXnoi %*% phis1, 1, sum )

Z1 <- matrix(Z1)


#### Zi <- Z1 ; mxX <- mxXstnd

ff.PLSresids <- function(Zi, mxX) {
    
    p <- ncol(mxX)
    
    mxOut <- matrix(NA, nrow(mxX), ncol(mxX))
    colnames(mxOut) <- colnames(mxX)
    rownames(mxOut) <- rownames(mxX)
    
    Zi <- Zi - mean(Zi)
    
    for(j in 1:p) {
        Zihat <- mxX[ , j, drop=FALSE ] %*% solve(crossprod(mxX[ , j, drop=FALSE ])) %*% (t(mxX[ , j, drop=FALSE ]) %*% Zi)
        Zres <- Zi - Zihat
        mxOut[ , j] <- Zres
    }
    
    ## mxOut <- t( t(mxOut) / apply(mxOut, 2, sd) )
    
    return(mxOut)
    
}



mxResids1 <- ff.PLSresids(Zi=Z1, mxXstnd)
phis2 <- cor( mxResids1, ystnd ) ; phis2
Z2 <- apply( mxResids1 %*% phis2, 1, sum ) ; # Z2




xlm <- lm(ystnd ~ Z1)
summary(xlm)


xlm <- lm(ystnd ~ Z1 + Z2)
summary(xlm)



mxResids2 <- ff.PLSresids(Zi=Z2, mxResids1)
#mxResids2 <- ff.PLSresids(Zi=Z2, mxXstnd)
phis3 <- cor( mxResids2, ystnd ) ; phis3
Z3 <- apply( mxResids2 %*% phis3, 1, sum ) ; # Z3


xlm <- lm(ystnd ~ Z1 + Z2 + Z3)
summary(xlm)




xlm <- lm(ystnd~mxXnoi)
summary(xlm)




mxResids3 <- ff.PLSresids(Zi=Z3, mxResids2)
#mxResids3 <- ff.PLSresids(Zi=Z3, mxXstnd)
phis4 <- cor( mxResids3, ystnd ) ; phis4
Z4 <- apply( mxResids3 %*% phis4, 1, sum ) ; # Z3


xlm <- lm(ystnd ~ Z1 + Z2 + Z3 + Z4)
summary(xlm)







xlm <- lm(ystnd ~ Z1)
summary(xlm)

xlm <- lm(ystnd ~ Z2)
summary(xlm)

xlm <- lm(ystnd ~ Z3)
summary(xlm)

xlm <- lm(ystnd ~ Z4)
summary(xlm)




library(pls)


dfx <- data.frame("y"=y, mxX)


xpls <- plsr(y ~ ., data=dfx, x=TRUE)

names(xpls)


summary(xpls)


xpls$x

plot(Z1, xpls$x[ , "X1"])

plot(Z1, xpls$x[ , "X2"])

