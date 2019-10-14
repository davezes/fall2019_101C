

###

### THESE ARE DATA ESTIMATES, EMPIRIC ESTIMATES OF MSE

options(stringsAsFactors=FALSE, width=350)

xbool.savePlots <- FALSE


N <- 50



########################## mux 0

library(mvtnorm)

p <- 5

X <- rmvnorm(N, rep(0, p), diag(1, p))

X[1, ] <- rep(3, p) #### unusual -- try changing this number and see what happens

H <- X %*% solve( crossprod(X) )  %*% t(X)


hii <- H[1, 1] ; hii


diag(H)



if(xbool.savePlots) { png("~/Desktop/leverage_mv.png", height=1000, width=1000, pointsize=28) }

par(mfrow=c(1, 1))

hist(diag(H))

if(xbool.savePlots) { dev.off() }





