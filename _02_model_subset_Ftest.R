

###

###

options(stringsAsFactors=FALSE, width=350)


xbool.savePlots <- FALSE



##################################### is full model significantly better than some feature subset model ?
##################################### is full model significantly better than some feature subset model ?
##################################### is full model significantly better than some feature subset model ?





############# sim data

N <- 2000

p <- 100

firstm <- 2

q <- p - firstm ; q


b.true <- c(1, seq(1, 3, length=firstm), rep(0, p-firstm))



X <- cbind(rep(1, N), matrix(rnorm(N * p, 0, 1), N, p))

y <- X %*% b.true + rnorm(N, 0, 50)


####



####### make sure in working directory
source("___f_funs.R")


f.hat <- h.ridge(X=X, y=y, lambda=0)
y.hat <- f.hat(x=X)

xRSS <- sum( (y - y.hat)^2 ) ; xRSS




########## smaller model
X.sub <- X[ , I(1:(1+firstm)), drop=FALSE]
f.hat <- h.ridge(X=X.sub, y=y, lambda=0)
y.hat <- f.hat(x=X.sub)

xRSS0 <- sum( (y - y.hat)^2 ) ; xRSS0



############ this is our F-stat on q over (n-p-1) degrees of freedom
F <- ( ( xRSS0 - xRSS ) / q ) / ( xRSS / ( N - p - 1 ) ) ; F

pf(F, q, N - p - 1, lower.tail=FALSE)




################## having fun!

nn <- 5000

pout.vec <- rep(NA, nn)
fout.vec <- rep(NA, nn)


for(ii in 1:nn) {
    
    y <- X %*% b.true + rnorm(N, 0, 50)
    
    f.hat <- h.ridge(X=X, y=y, lambda=0)
    y.hat <- f.hat(x=X)
    
    xRSS <- sum( (y - y.hat)^2 ) ; xRSS
    
    
    ########## smaller model
    X.sub <- X[ , I(1:(1+firstm)), drop=FALSE]
    f.hat <- h.ridge(X=X.sub, y=y, lambda=0)
    y.hat <- f.hat(x=X.sub)
    
    xRSS0 <- sum( (y - y.hat)^2 ) ; xRSS0

    
    ############ this is our F-stat on q over (n-p-1) degrees of freedom
    F <- ( ( xRSS0 - xRSS ) / q ) / ( xRSS / ( N - p - 1 ) ) ; F
    
    fout.vec[ii] <- F
    pout.vec[ii] <- pf(F, q, N - p - 1, lower.tail=FALSE)

    cat(ii, " ")
}


fdom <- seq( min(fout.vec), max(fout.vec), length=500 )
fdist <- df(fdom, q, N - p - 1)

if(xbool.savePlots) { png("~/Desktop/modelSubset_Ftest_underTrueNull_01.png", height=1000, width=2000, pointsize=24) }

par(mfrow=c(1, 2))
hist(fout.vec, main="F-stat under TRUE Null", freq=FALSE)
points( fdom, fdist, type="l", lwd=4, col="#00AA00" )
hist(pout.vec, main="F-test p-vals under TRUE Null")

if(xbool.savePlots) { dev.off() }

###### there are no term(s) in full model that are not in small model that drive response

################################ now, where null is false -- other predictors in q actually drive response



b.true <- c(1, seq(1, 3, length=firstm), rep(0.5, p-firstm))

y <- X %*% b.true + rnorm(N, 0, 50)


####





f.hat <- h.ridge(X=X, y=y, lambda=0)
y.hat <- f.hat(x=X)

xRSS <- sum( (y - y.hat)^2 ) ; xRSS




########## smaller model
X.sub <- X[ , I(1:(1+firstm)), drop=FALSE]
f.hat <- h.ridge(X=X.sub, y=y, lambda=0)
y.hat <- f.hat(x=X.sub)

xRSS0 <- sum( (y - y.hat)^2 ) ; xRSS0



############ this is our F-stat on q over (n-p-1) degrees of freedom
F <- ( ( xRSS0 - xRSS ) / q ) / ( xRSS / ( N - p - 1 ) ) ; F

pf(F, q, N - p - 1, lower.tail=FALSE)




################## having fun!

nn <- 5000

pout.vec <- rep(NA, nn)
fout.vec <- rep(NA, nn)


for(ii in 1:nn) {
    
    y <- X %*% b.true + rnorm(N, 0, 50)
    
    f.hat <- h.ridge(X=X, y=y, lambda=0)
    y.hat <- f.hat(x=X)
    
    xRSS <- sum( (y - y.hat)^2 ) ; xRSS
    
    
    ########## smaller model
    X.sub <- X[ , I(1:(1+firstm)), drop=FALSE]
    f.hat <- h.ridge(X=X.sub, y=y, lambda=0)
    y.hat <- f.hat(x=X.sub)
    
    xRSS0 <- sum( (y - y.hat)^2 ) ; xRSS0
    
    
    ############ this is our F-stat on q over (n-p-1) degrees of freedom
    F <- ( ( xRSS0 - xRSS ) / q ) / ( xRSS / ( N - p - 1 ) ) ; F
    
    fout.vec[ii] <- F
    pout.vec[ii] <- pf(F, q, N - p - 1, lower.tail=FALSE)
    
    cat(ii, " ")
}

fdom <- seq( min(fout.vec), max(fout.vec), length=500 )
fdist <- df(fdom, q, N - p - 1)



if(xbool.savePlots) { png("~/Desktop/modelSubset_Ftest_underFalseNull_01.png", height=1000, width=2000, pointsize=24) }

par(mfrow=c(1, 2))
hist(fout.vec, main="F-stat under FALSE Null", freq=FALSE)
points( fdom, fdist, type="l", lwd=4, col="#00AA00" )
hist(pout.vec, main="F-test p-vals under FALSE Null")

if(xbool.savePlots) { dev.off() }

###### there are term(s) in full model that are not in small model that drive response



