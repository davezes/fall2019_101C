


options(stringsAsFactors=FALSE, width=350)



xbool.savePlots <- FALSE

############ simple 1D support


xdom <- seq(-4, 4, length=1000)

N <- 1000 ##### must be div by 4


b0 <- 0
b1 <- 1
u <- b0 + b1 * xdom
y <- 1 / (1 + exp(-u))


if(xbool.savePlots) { png(file.path("~", "Desktop", "sigmoid_simple_A.png"), width=2000, height=500, pointsize=28) }
par(mfrow=c(1, 1), mar=c(3,3,1,1), mgp=c(2,1,0))
plot(xdom, y, type="l", lwd=4, xlab="X", ylab="Pr[Y]", main="b0 = 0, b1 = 1")
if(xbool.savePlots) { dev.off() }




b0 <- 1
b1 <- 1
u <- b0 + b1 * xdom
y <- 1 / (1 + exp(-u))


if(xbool.savePlots) { png(file.path("~", "Desktop", "sigmoid_simple_B.png"), width=2000, height=500, pointsize=28) }
par(mfrow=c(1, 1), mar=c(3,3,1,1), mgp=c(2,1,0))
plot(xdom, y, type="l", lwd=4, xlab="X", ylab="Pr[Y]", main="b0 = 1, b1 = 1")
if(xbool.savePlots) { dev.off() }




b0 <- 0
b1 <- 0
u <- b0 + b1 * xdom
y <- 1 / (1 + exp(-u))


if(xbool.savePlots) { png(file.path("~", "Desktop", "sigmoid_simple_C.png"), width=2000, height=500, pointsize=28) }
par(mfrow=c(1, 1), mar=c(3,3,1,1), mgp=c(2,1,0))
plot(xdom, y, type="l", lwd=4, xlab="X", ylab="Pr[Y]", main="b0 = 0, b1 = 0")
if(xbool.savePlots) { dev.off() }






