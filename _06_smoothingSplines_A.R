


options(stringsAsFactors=FALSE, width=350)


xbool.savePlots <- FALSE


####### make sure in working directory
source("___f_funs.R")


############ prepair dataset for bias variance

library(mvtnorm)

p <- 1

n <- 400

set.seed(777)


x <- rnorm(n)
x <- runif(n, -2.1, 2.1)
errs <- rnorm(n, 0, 1/7)

y <- cos(5*x) + errs


if(xbool.savePlots) { png("~/Desktop/mr_wiggles_01.png", height=1000, width=1000, pointsize=24) }
plot(x, y, lwd=1, xlim=c(-5, 5))
if(xbool.savePlots) { dev.off() }




############## use full standardization -- notice excessive wiggliness at right

xknots <- sort(x)

mxC <- f_basisNatSplines(x=x, xknots=xknots)[ , -1]

cnames <- paste0("X", I(1:ncol(mxC)))

colnames(mxC) <- cnames

cmeans <- apply(mxC, 2, mean)
csds <- apply(mxC, 2, sd)

mxCstnd <- t( ( t(mxC) - cmeans ) / csds )

ymean <- mean(y)
ysd <- sd(y)

mxCy <- cbind(mxC, "y"=y)

Lcycy <- cor(mxCy)
Lcc <- Lcycy[ cnames, cnames ]
Lcy <- Lcycy[ cnames, "y" ]




xdom <- seq(min(x), max(x), length=1000)
x0 <- f_basisNatSplines(x=xdom, xknots=xknots)[ , -1]
x0c <- t( t(x0) - cmeans  ) ##### centered
x0p <- t( ( t(x0) - cmeans) / csds ) ##### standardized





############### full standardize


xlambda <- 1/1000000
xreg <- xlambda


invLccReg <- solve( Lcc +  diag(xreg, ncol(Lcc)) )

bhats <- invLccReg %*% Lcy

mxHat <- mxCstnd %*% invLccReg %*% t(mxCstnd) / n

sum( diag(mxHat) )

y0p <- x0p %*% bhats

y0 <- y0p * ysd + ymean

if(xbool.savePlots) { png("~/Desktop/mr_wiggles_fit_01.png", height=1000, width=1000, pointsize=24) }
plot(x, y, lwd=1, xlim=c(-5, 5))
points(xdom, y0, type="l", lwd=4, col="#00AA00")
if(xbool.savePlots) { dev.off() }





################### only center

mxCy_c <- t( t(mxCy) - c(cmeans, ymean) )

Scycy <- crossprod(mxCy_c)
Scc <- Scycy[ cnames, cnames ]
Scy <- Scycy[ cnames, "y" ]


xlambda <- 1/10000

xreg <- xlambda

invSccReg <- solve( Scc +  diag(xreg, ncol(Scc)) )

bhats <- invSccReg %*% Scy

mxHat <- mxCy_c[ , cnames] %*% invSccReg %*% t(mxCy_c[ , cnames])

sum( diag(mxHat) ) #### increasing lambda decreases effective df (eff df the model consumes)

y0c <- x0c %*% bhats
y0 <- y0c + ymean

if(xbool.savePlots) { png("~/Desktop/mr_wiggles_fit_02.png", height=1000, width=1000, pointsize=24) }
plot(x, y, lwd=1, xlim=c(-5, 5))
points(xdom, y0, type="l", lwd=4, col="#00AA00")
if(xbool.savePlots) { dev.off() }


yhatc <- mxCy_c[ , cnames ] %*% bhats
yhat <- yhatc + ymean

rss <- sum( (y - yhat)^2 ) ; rss

rsscv <- sum( ( (y - yhat) / (1 - diag(mxHat)) )^2  ) ; rsscv








fhat <- h_SS_I(x=x, y=y, xlambda=xlambda)
xhobj <- fhat(x0=x)
rss <- sum( (y - xhobj[[ "y0hat" ]])^2 ) ; rss
rsscv <- sum( ( (y - xhobj[[ "y0hat" ]]) / (1 - diag( xhobj[[ "mxHat" ]] ) ) )^2  ) ; rsscv

sum(diag( xhobj[[ "mxHat" ]] ))




########## giant LOOCV


xlambda <- 1/10000 ###


cv_yhat <- rep(NA, n)
for(ii in 1:n) {
    fhat <- h_SS_I(x=x[ -ii ], y=y[ -ii ], xlambda=xlambda, boolHatMx=FALSE)
    xhobj <- fhat(x0=x[ ii ])
    cv_yhat[ ii ] <- xhobj[[ "y0hat" ]]
    cat(ii, "\n")
}

rss <- sum( (y - cv_yhat)^2 ) ; rss

rss_true_ridge <- rss

cat("True CV with Ridge:", rss_true_ridge, "\n")



###################

fit <- smooth.spline(x=x, y=y, df=24)

fit$lambda

pssobj <- predict(fit, x=x)

names(fit)


rss <- sum( (y - pssobj$y)^2 ) ; rss
rsscv <- sum( ( (y - pssobj$y) / (1 - fit$lev ) )^2  ) ; rsscv

fit2 <- smooth.spline(x=x, y=y, cv=TRUE)
fit2$df


cv_yhat_ss <- rep(NA, n)
for(ii in 1:n) {
    fit <- smooth.spline(x=x[ -ii ], y=y[ -ii] , df=fit2$df)
    pssobj <- predict(fit, x=x[ ii ])
    cv_yhat_ss[ ii ] <- pssobj[[ "y" ]]
    cat(ii, "\n")
}

rss <- sum( (y - cv_yhat_ss)^2 ) ; rss

rss_true_ss <- rss

cat("True CV with Smooth Spline:", rss_true_ss, "\n")

