


options(stringsAsFactors=FALSE, width=350)


xbool.savePlots <- FALSE


####### make sure in working directory
source("___f_funs.R")


############ prepair dataset for bias variance

library(mvtnorm)

p <- 2

N <- 1000 ##### must be even

set.seed(777)


X <- rbind(
rmvnorm(N/2, c(2, 0), diag(0.5, p) + 0.5),
rmvnorm(N/2, c(0, 2), diag(1.5, p) - 0.5)
)

y <- c( rep("A", N/2), rep("B", N/2) )




xcols <- rep(NA, N)
xcols[ y == "A" ] <- "#0000BB"
xcols[ y == "B" ] <- "#BBBB00"

if(xbool.savePlots) { png(file.path("~", "Desktop", "QDA_01.png"), width=2000, height=1000, pointsize=28) }
par(mfrow=c(1, 2))
plot(X[ , 1], X[ , 2], type="n", xlim=range(X[ ,1]), ylim=range(X[ ,2]), xlab="x1", ylab="x2")
text(labels=y, X[ , 1], X[ , 2], col=xcols )
##text(labels=y.train, X.train[ , 1], X.train[ , 2], col=c("#FF0000", "#00FF00")[ match( y.train, c("A", "B") ) ] )







########### from funs file
fhat <- h.qda(X=X, y=y)

dd.hat <- fhat(x=X)

### hist(dd.hat)


y.cat.hat <- colnames(dd.hat)[ apply(dd.hat, 1, which.max) ]



xcols <- rep(NA, N)
xcols[ y.cat.hat == "A" ] <- "#0000BB55"
xcols[ y.cat.hat == "B" ] <- "#BBBB0077"
plot(X[ , 1], X[ , 2], col=xcols, pch="O", cex=2, xlab="x1", ylab="x2" )

if(xbool.savePlots) { dev.off() }





exp(dd.hat) / apply(exp(dd.hat), 1, sum)

################# region grid

Xdom <- as.matrix( expand.grid("x1"=seq(min(X[ ,1]), max(X[ ,1]), length=100), "x2"=seq(min(X[ ,2]), max(X[ ,2]), length=100)) )

dd.dom.hat <- fhat(x=Xdom)
ydom.cat.hat <- colnames(dd.dom.hat)[ apply(dd.dom.hat, 1, which.max) ]

#p.dom.hat <- exp(dd.dom.hat) / apply(exp(dd.dom.hat), 1, sum)
#ydom.cat.hat <- colnames(p.dom.hat)[ apply(p.dom.hat, 1, which.max) ]


xcols <- rep(NA, N)
xcols[ y == "A" ] <- "#0000BB"
xcols[ y == "B" ] <- "#BBBB00"


if(xbool.savePlots) { png(file.path("~", "Desktop", "QDA_02.png"), width=1000, height=1000, pointsize=28) }
par(mfrow=c(1, 1))

plot(X[ , 1], X[ , 2], type="n", xlim=range(X[ ,1]), ylim=range(X[ ,2]), xlab="x1", ylab="x2")
text(labels=y, X[ , 1], X[ , 2], col=xcols )

xcols <- rep(NA, N)
xcols[ ydom.cat.hat == "A" ] <- "#0000BB88"
xcols[ ydom.cat.hat == "B" ] <- "#BBBB0088"


points(Xdom[,1], Xdom[,2], col=xcols, pch=19, cex=0.5)

if(xbool.savePlots) { dev.off() }


#### dd.dom.hat[ Xdom[ , 1 ] > 4.7 & Xdom[ , 2] < -3.2 , ]

