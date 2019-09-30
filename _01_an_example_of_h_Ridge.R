

###

###

options(stringsAsFactors=FALSE, width=350)


####### make sure in working directory
source("___f_funs.R")



############# sim data

N <- 10000

p <- 4

X <- cbind(rep(1, N), matrix(runif(N * p, -1, 1), N, p))

b.true <- c(1, 2, 3, 4, 5)

y <- X %*% b.true + rnorm(N)

########### see funs file
f.hat <- h.ridge(X=X, y=y, lambda=N/100)




########## predict out from new point

n0 <- 20
x0 <- cbind(rep(1, n0), matrix(runif(n0 * p, -1, 1), n0, p))

y0 <- x0 %*% b.true + rnorm(n0)

y.hat <- f.hat(x=x0)

sqrt( mean( (y0 - y.hat)^2 ) )

