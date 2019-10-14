

###

### KNN, with validation

### color cats in plots show validation miscat
### grey are simply the training data



options(stringsAsFactors=FALSE, width=350)


xbool.savePlots <- FALSE


####### make sure in working directory
source("___f_funs.R")




############ sim data

library(mvtnorm)

p <- 2

N <- 1000 ##### must be even

set.seed(777)


X <- rbind(
rmvnorm(N/2, c(0,0), diag(0.5, p) + 0.5),
rmvnorm(N/2, c(1,0), diag(0.5, p) + 0.5)
)

y <- c( rep("A", N/2), rep("B", N/2) )


k <- 9


iindx <- sample( N, size=round(N/5) )
X.train <- X[ -iindx, ]
y.train <- y[ -iindx ]

fhat <- h.knn(X=X.train, y=y.train, k=k)

X.valid <- X[ iindx, ]
y.valid <- y[ iindx ]

y0 <- fhat(x=X.valid)

xlab <- "A"
phat <- y0[ , xlab ] / k

y.hard <- rep( "B", length(iindx) )
y.hard[ phat > 0.5 ] <- xlab



table( y.valid, y.hard )

plot(X.train[ , 1], X.train[ , 2], type="n", xlim=range(X[ ,1]), ylim=range(X[ ,2]), xlab="x1", ylab="x2")
text(labels=y.train, X.train[ , 1], X.train[ , 2], col="#555555" )
##text(labels=y.train, X.train[ , 1], X.train[ , 2], col=c("#FF0000", "#00FF00")[ match( y.train, c("A", "B") ) ] )


xcols.valid <- rep("#FF0000", length(iindx))
xcols.valid[ y.valid == y.hard ] <- "#00FF00"
text(labels=y.valid, X.valid[ , 1], X.valid[ , 2], col=xcols.valid, cex=1.2 )
##text(labels=y.train, X.train[ , 1], X.train[ , 2], col=c("#FF0000", "#00FF00")[ match( y.train, c("A", "B") ) ] )


############################################### loop over different joint distributions



xmus <- c(0, 0.5, 1, 2, 4, 8)



if(xbool.savePlots) { png(file.path("~", "Desktop", "cat_01.png"), height=1000, width=1500, pointsize=24) }


par(mfrow=c(2, 3), mar=c(2, 2, 3, 1))


for(jj in 1:length(xmus)) {
    
    
    X <- rbind(
    rmvnorm(N/2, c(0,0), diag(0.5, p) + 0.5),
    rmvnorm(N/2, c(xmus[jj],0), diag(0.5, p) + 0.5)
    )
    
    y <- c( rep("A", N/2), rep("B", N/2) )
    
    
    k <- 1
    
    
    iindx <- sample( N, size=round(N/5) )
    X.train <- X[ -iindx, ]
    y.train <- y[ -iindx ]
    
    fhat <- h.knn(X=X.train, y=y.train, k=k)
    
    X.valid <- X[ iindx, ]
    y.valid <- y[ iindx ]
    
    y0 <- fhat(x=X.valid)
    
    xlab <- "A"
    phat <- y0[ , xlab ] / k
    
    y.hard <- rep( "B", length(iindx) )
    
    y.hard[ phat > 0.5 ] <- xlab
    
    cat( "delta x1 x2 mus:", xmus[jj], "\n")
    print(
    table( y.valid, y.hard )
    )
    cat( "\n\n\n")
    
    plot(X.train[ , 1], X.train[ , 2], type="n", xlim=range(X[ ,1]), ylim=range(X[ ,2]), xlab="x1", ylab="x2")
    text(labels=y.train, X.train[ , 1], X.train[ , 2], col="#555555" )
    ##text(labels=y.train, X.train[ , 1], X.train[ , 2], col=c("#FF0000", "#00FF00")[ match( y.train, c("A", "B") ) ] )
    
    
    xcols.valid <- rep("#FF0000", length(iindx))
    xcols.valid[ y.valid == y.hard ] <- "#00CC00"
    text(labels=y.valid, X.valid[ , 1], X.valid[ , 2], col=xcols.valid, cex=1.2 )
    ##text(labels=y.train, X.train[ , 1], X.train[ , 2], col=c("#FF0000", "#00FF00")[ match( y.train, c("A", "B") ) ] )

}


if(xbool.savePlots) { dev.off() }

