


options(stringsAsFactors=FALSE, width=350)


xbool.savePlots <- FALSE


####### make sure in working directory
## source("___f_funs.R")


############ prepair dataset for bias variance

## library(mvtnorm)


n <- 100


############ bootstrap example, 75th percentile, Cauchy

q3 <- qcauchy(0.75) ; q3 ### true value

nn <- 100

mx.CI <- matrix(NA, nn, 2)

for(iinn in 1:nn) {
    
    x <- rcauchy(n=n)
    
    q3hat <- quantile(x, c(0.75))
    
    q3hat.boot.vec <- rep(NA, 1000)
    for(jboot in 1:length(q3hat.boot.vec)) {
        xxs <- sample(x, size=length(x), replace=TRUE)
        q3hat.boot.vec[jboot] <- quantile(xxs, c(0.75))
    }
    ### hist(q3hat.boot.vec)
    
    this.CI <- quantile(q3hat.boot.vec, c(0.025, 0.975)) ; this.CI
    
    mx.CI[ iinn, ] <- this.CI
    
    cat(iinn, "\n")
}

xmissed <- mx.CI[ , 1] > q3 | mx.CI[ , 2] < q3


if(xbool.savePlots) { png(file.path("~", "Desktop", "bootstrapExample.png"), width=2000, height=800, pointsize=28) }
par(mfrow=c(1, 1))

plot(x=I(1:nn), mx.CI[ ,1], ylim=range(mx.CI), type="n", xlab="Trial", ylab="x", main=paste0("An Example of ", nn, " Trials"))

segments(x0=I(1:nn), y0=mx.CI[ , 1], x1=I(1:nn), y1=mx.CI[ , 2], lwd=2)

segments(x0=I(1:nn)[xmissed], y0=mx.CI[ xmissed, 1], x1=I(1:nn)[xmissed], y1=mx.CI[ xmissed, 2], lwd=5, col="#DD0000")

points(x=I(1:nn), mx.CI[ , 1], pch=19, cex=0.5)
points(x=I(1:nn), mx.CI[ , 2], pch=19, cex=0.5)
abline(h=q3, col="#00BB00", lwd=3)

if(xbool.savePlots) { dev.off() }


