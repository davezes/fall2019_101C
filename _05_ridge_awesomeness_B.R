


options(stringsAsFactors=FALSE, width=350)


xbool.savePlots <- TRUE


####### make sure in working directory
source("___f_funs.R")


############ prepair dataset for bias variance

library(mvtnorm)


n <- 500

set.seed(777)

x1 <- sample( c("Cat", "Dog", "Unicorn"), size= n, replace=TRUE )
x2 <- sample( c("black", "brown", "purple"), size= n, replace=TRUE )

x3 <- rnorm(n, 5, 10)

library(dummies)

xx1 <- dummy(x1)
xx2 <- dummy(x2)

mxX <- cbind( "int"=rep(1, n), "x"=x3, xx1, xx2 )

head(mxX)


btrue <- rep( c(1, 2, -1), length.out=ncol(mxX) )

ftrue <- mxX %*% btrue

yerr <- rnorm(n)

y <- ftrue + yerr


xdf <- data.frame("y"=y, mxX)

xlm <- lm(y~.-1, data=xdf)

summary(xlm)

## coefficients(summary(xlm))

cbind(btrue, coefficients(xlm))



mxXy <- cbind(mxX, "y"=y)
colnames(mxXy) <- c(colnames(mxX), "y")
head(mxXy)


Sxyxy <- crossprod( mxXy )
Sxyxy






###########

library(corpcor)

Lxyxy <- cov2cor(Sxyxy)

Lxx <- Lxyxy[ colnames(mxX), colnames(mxX) ]
Lxy <- Lxyxy[ colnames(mxX), "y" ]

xmxreg <- diag(0.0001, ncol(Lxx))
#xmxreg[1,1] <- 0

bhat_stnd <- solve( Lxx + xmxreg ) %*% Lxy
## bhat_stnd <- pseudoinverse( Lxx ) %*% Lxy ; rownames(bhat_stnd) <- colnames(mxX)
#bhat

xsd <- apply(mxX, 2, sd)
ysd <- sd(y)

bhat <- bhat_stnd / (xsd / ysd)
bhat[ "int", ] <- mean(y) / ysd

cbind(btrue, bhat)



################## un standardized


Sxx <- Sxyxy[ colnames(mxX), colnames(mxX) ]
Sxy <- Sxyxy[ colnames(mxX), "y" ]

bhat <- solve( Sxx + diag(1, ncol(Sxx)) ) %*% Sxy

cbind(btrue, bhat)












