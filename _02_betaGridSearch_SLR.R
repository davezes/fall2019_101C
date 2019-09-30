

###

### THESE ARE DATA ESTIMATES, EMPIRIC ESTIMATES OF MSE

options(stringsAsFactors=FALSE, width=350)

xbool.savePlots <- TRUE


N <- 1000



########################## mux 0

mux <- 0
sigx2 <- 1

sige2 <- 1


b0 <- 1
b1 <- 1



x <- rnorm(N, mux, sqrt(sigx2))

f.true <- b0 + b1 * x

errs <- rnorm(N, 0, sqrt(sige2))

y <- f.true + errs




b0dom <- seq(b0 - 1, b0 + 1, length=500)

b1dom <- seq(b1 - 1, b1 + 1, length=500)

mx.rmse <- matrix(NA, length(b0dom), length(b1dom))



for(iig in 1:length(b0dom)) {
    
    
    for(jjg in 1:length(b1dom)) {
        
        
        yhatg <- b0dom[ iig ] + b1dom[ jjg ] * x
        
        this.rmse <- sqrt( mean( ( y - yhatg )^2 ) )
        
        mx.rmse[ iig, jjg ] <- this.rmse
        
    }
    
}


if(xbool.savePlots) { png("~/Desktop/gridSearch01.png", width=1000, height=1000, pointsize=24) }

image( x=b0dom, y=b1dom, mx.rmse, col = hcl.colors(100, "YlOrRd", rev = TRUE), main="Grid Search of Params" )

points(b0, b1)

bhat1LS <- sum( (x - mean(x)) * (y - mean(y)) ) / sum( (x - mean(x))^2 ) ; bhat1LS
bhat0LS <-  mean(y) - bhat1LS * mean(x) ; bhat0LS

points(bhat0LS, bhat1LS, pch=19, col="#FF0000")

if(xbool.savePlots) { dev.off() }

