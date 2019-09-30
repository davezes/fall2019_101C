


options(stringsAsFactors=FALSE, width=350)


xbool.savePlots <- FALSE


####### make sure in working directory
#### source("___f_funs.R")


############ prepair dataset for bias variance

library(mvtnorm)

p <- 20

N <- 2000

set.seed(777)


xSig <- diag(1, p)
offdiags <- runif(p*(p-1)/2, -0.2, 0.2)

xSig[ lower.tri(xSig, diag = FALSE) ] <- offdiags
xSig <- t(xSig)
xSig[ lower.tri(xSig, diag = FALSE) ] <- offdiags



X <- rmvnorm(N, rep(0, p), xSig)


##################### deliberate sabotage !
y <- X %*% eigen(xSig)[[ "vectors" ]][ , (p-3):p ] %*% c(1, -1, 2, -2) + rnorm(N)
##xlm <- lm(y~X)
##summary(xlm)


########################### fit using full design matrix

xlm <- lm(y~X)

summary(xlm)



########################### grab the 10 "highest variance" compenents of our design matrix, X

xeig <- eigen( crossprod(X) )

Xsub <- X %*% xeig[[ "vectors" ]][ , I(1:10) ]

xlm <- lm(y~Xsub)

summary(xlm)

############ whaaaaa ???????


