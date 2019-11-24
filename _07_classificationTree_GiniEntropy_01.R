


options(stringsAsFactors=FALSE, width=350)


xbool.savePlots <- TRUE


####### make sure in working directory
source("___f_funs.R")


############ prepair dataset for bias variance

library(mvtnorm)

n <- 32

set.seed(777)

x1 <- c( runif(n/2, -1, 0.7), runif(n/2, -0.7, 1) )
x2 <- c( runif(n/2, 0, 1), runif(n/2, 0, 1) )

x1 <- c( rnorm(n/2, -0.2, 0.5), rnorm(n/2, 0.2, 0.5) )
x2 <- c( runif(n/2, 0, 1), runif(n/2, 0, 1) )

y <- c(rep("A", n/2), rep("B", n/2))



if(xbool.savePlots) { png("~/Desktop/tree_classification_cost_01.png", height=1000, width=1000, pointsize=24) }
plot(x1, x2, lwd=1, xlim=c(-1.1, 1.1), ylim=c(-0.1, 1.1))
text(x1, x2, labels=y, cex=1.4)
if(xbool.savePlots) { dev.off() }




mx <- f_table(x=x1, xcut=0, y=y) ; mx



f_gini(mx=mx)

xcuts <- seq(-0.6, 0.6, length=10) ; xcuts

xvec_gini <- NULL
xvec_entr <- NULL

for(ii in 1:length(xcuts)) {
    
    mx <- f_table(x=x1, xcut=xcuts[ii], y=y) ; mx
    xgini <- f_gini(mx=mx)
    xvec_gini[ ii ] <- sum(xgini)
    xentr <- f_classEntropy(mx=mx)
    xvec_entr[ ii ] <- sum(xentr)
    cat(xcuts[ii], sum(xgini), "\n")
}

if(xbool.savePlots) { png("~/Desktop/tree_classification_cost_02.png", height=1000, width=1000, pointsize=24) }
par(mfrow=c(2, 1), mar=c(2, 4, 1, 1))
plot(x1, x2, lwd=1, xlim=c(-1.1, 1.1), ylim=c(-0.1, 1.1), type="n")
text(x1, x2, labels=y, cex=1.4)
abline(v=xcuts)

plot(xcuts, xvec_gini, xlim=c(-0.8, 0.8), ylim=range(c(xvec_gini, xvec_entr)), type="l", ylab="Gini / Entropy", lwd=5, col="#00AA00")
points(xcuts, xvec_entr, type="l", ylab="Gini", lwd=5, col="#AA00AA")
if(xbool.savePlots) { dev.off() }



#################### obvious

set.seed(777)

x1 <- c( runif(n/2, -0.7, 0.0), runif(n/2, 0.0, 0.7) )
x2 <- c( runif(n/2, 0, 1), runif(n/2, 0, 1) )

y <- c(rep("A", n/2), rep("B", n/2))

xcuts <- seq(-0.6, 0.6, length=13) ; xcuts


xvec_gini <- NULL
xvec_entr <- NULL

for(ii in 1:length(xcuts)) {
    
    mx <- f_table(x=x1, xcut=xcuts[ii], y=y) ; mx
    xgini <- f_gini(mx=mx)
    xvec_gini[ ii ] <- sum(xgini)
    xentr <- f_classEntropy(mx=mx)
    xvec_entr[ ii ] <- sum(xentr)
    cat(xcuts[ii], sum(xgini), "\n")
}

if(xbool.savePlots) { png("~/Desktop/tree_classification_cost_03.png", height=1000, width=1000, pointsize=24) }
par(mfrow=c(2, 1), mar=c(2, 4, 1, 1))
plot(x1, x2, lwd=1, xlim=c(-0.8, 0.8), ylim=c(-0.1, 1.1), type="n")
text(x1, x2, labels=y, cex=1.4)
abline(v=xcuts)

plot(xcuts, xvec_gini, xlim=c(-0.8, 0.8), type="l", ylab="Gini / Entropy", lwd=5, col="#00AA00")
points(xcuts, xvec_entr, type="l", ylab="Gini", lwd=5, col="#AA00AA")
if(xbool.savePlots) { dev.off() }


