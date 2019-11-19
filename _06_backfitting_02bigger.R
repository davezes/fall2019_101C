

######### this is an illustration of Q11 from Ch 7

options(stringsAsFactors=FALSE, width=350)


xbool.savePlots <- FALSE


####### make sure in working directory
source("___f_funs.R")


library(mvtnorm)



n <- 70

p <- 7

b0 <- 5

bb <- c(-1, 1, -2, 2, -3, 3, -4)

X <- rmvnorm(n, rep(0, p), diag(1, p))


f_true <- b0 + X %*% bb

errs <- rnorm(n, 0, 10)

y <- f_true + errs



bbhat <- rep(0, p)

nn <- 100

for(ii in 1:nn) {
    
    for(jj in 1:p) {
    yadj <- y -  X[ , -jj ] %*% bbhat[ -jj ] ; yadj
    xx <- X[ , jj ]
    bbhat[ jj ] <- lm(yadj ~ xx)$coef[2]
    }
    
    cat(bbhat, "\n")
    
}


###### compare with:

xlm <- lm(y ~ X)

summary(xlm)



############################# HUGE


n <- 1000

p <- 30

b0 <- 5

bb <- rep_len(c(-2, -1, 1, 2), p)

X <- rmvnorm(n, rep(0, p), diag(1, p))


f_true <- b0 + X %*% bb

errs <- rnorm(n, 0, 7)

y <- f_true + errs





bbhat <- rep(0, p)

nn <- 100

for(ii in 1:nn) {
    
    for(jj in 1:p) {
    yadj <- y -  X[ , -jj ] %*% bbhat[ -jj ] ; yadj
    xx <- X[ , jj ]
    bbhat[ jj ] <- lm(yadj ~ xx)$coef[2]
    }
    
    cat(bbhat, "\n")
    
}


###### compare with:

xlm <- lm(y ~ X)

summary(xlm)


