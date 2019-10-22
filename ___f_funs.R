






### lambda <- 1/10 ; x <- X[ xndx.valid, , drop=FALSE ] ; X <- X[ xndx.train, , drop=FALSE ] ; y <- y[ xndx.train ]

h.ridge.scale <- function(X, y, lambda) {
    
    p <- ncol(X)
    n <- nrow(X)
    
    x.mean <- apply(X, 2, mean)
    #x.stddev <- apply( X, 2, function(x) { return(sqrt( mean( (x - mean(x))^2 ) ) ) } )
    x.stddev <- apply( X, 2, sd )
    
    y.mean <- mean(y) ; y.mean
    
    X.stnd <- t( (t(X) - x.mean) / x.stddev )
    
    y.center <- y - y.mean
    
    X.stnd <- cbind(rep(1, n), X.stnd)
    
    Lxx <- crossprod(X.stnd) / n
    Lxy <- ( t(X.stnd) %*% y.center ) / n
    
    ##bhat <- solve( Lxx + diag(lambda, p) ) %*% Lxy
    bhat <- solve( Lxx + diag(lambda, p+1) ) %*% Lxy
    
    fhat <- function(x) {
        
        x.stnd <- t( (t(x) - x.mean) / x.stddev )
        x.stnd <- cbind(rep(1, nrow(x.stnd)), x.stnd)
        
        yhat <- x.stnd %*% bhat + y.mean
        return(yhat)
    }
    
    ##### note that bhat gets bound to the scope of fhat
    
    return(fhat)
    
}



#################### logit cost function

f.logistic.cost <- function(y, p.hat) {
    
    xf <- c( log(p.hat[ y == 1 ]), log(1 - p.hat[ y == 0 ]) )
    
    return( -2 * mean(xf) )
    
}




#### x <- X[1:3, , drop=FALSE]

#### k must be positive integer

h.logistic <- function(X, y) {
   
    N <- nrow(X)
    p <- ncol(X)
    
    b.hat <- rep(0, ncol(X))
    
    xbool.keepGoing <- TRUE
    
    #mx.out <- rbind(NULL, b.hat)
    
    while( xbool.keepGoing ) {
        phat <- as.vector(  1 / (1+ exp(- X %*% b.hat))  )
        ww <- phat*(1-phat)
        b.prev <- b.hat
        b.hat <- b.hat + solve( t(X * ww) %*% X ) %*% ( t(X) %*% (y-phat) )
        if( sum( (b.hat - b.prev)^2 ) < 10^(-16) ) { xbool.keepGoing <- FALSE }
        #mx.out <- rbind(mx.out, t(b.hat))
        #### cat(b.hat, "\n")
    }

    
    fhat <- function(x) {
        
        if( p != ncol(x) ) { stop("x must have the same number of columns as data design matrix.") }
        
        uu <- x %*% b.hat
        
        y.hat <- 1 / (1 + exp(-uu))
        
        return(y.hat)
   
    }
    
    ##### note that X, y, p, N, ycats get bound to the scope of fhat
    
    return(fhat)
    
}



#### x <- X[1:3, , drop=FALSE]

#### k must be positive integer

h.knn <- function(X, y, k) {
    
    if( k < 1 ) { stop("arg k must be > 0") }
    
    N <- nrow(X)
    p <- ncol(X)
    
    ycats <- sort(unique(y)) ; ycats
    
    
    
    fhat <- function(x) {
        
        if( p != ncol(x) ) { stop("x must have the same number of columns as data design matrix.") }
        
        mx.y0 <- matrix(0, nrow(x), length(ycats))
        colnames(mx.y0) <- ycats
        
        for(i in 1:nrow(x)) {
            xdist <- sqrt( apply((t(X) - x[i, ])^2, 2, sum) )
            xperm <- order(xdist) ; xperm
            y0 <- y[ xperm[ 1:k ] ] ; y0
            
            xxy <- as.matrix(table(y0)) ; xxy
            
            mx.y0[ i, ] <- xxy[ match(ycats, rownames(xxy)), 1 ]
            
        }
        mx.y0[ is.na(mx.y0) ] <- 0
        return(mx.y0)
    }
    
    ##### note that X, y, p, N, ycats get bound to the scope of fhat
    
    return(fhat)
    
}




h.poly <- function(x, y, porder) {
    
    if( porder < 0 ) { stop("arg porder must be >= 0") }
    
    N <- length(x)
    X <- matrix(NA, N, porder+1)
    for(j in 1:(porder+1)) {
        X[ , j ] <- x^(j-1)
    }
    bhat <- solve( crossprod(X) ) %*% ( t(X) %*% y )
    
    
    fhat <- function(x) {
        
        N <- length(x)
        X <- matrix(NA, N, porder+1)
        for(j in 1:(porder+1)) {
            X[ , j ] <- x^(j-1)
        }
        
        yhat <- X %*% bhat
        return(yhat)
    }
    
    ##### note that bhat gets bound to the scope of fhat
    ##### attr(fhat, "bhat") <- bhat
    
    return(fhat)
    
    
}





##### X <- X ; porders <- c(2, 3) ; int <- TRUE

h.poly.multi <- function(X, y, porders, int=TRUE) {
    
    if( length(porders) != ncol(X) ) { stop("length of porders must equal number of columns of X.") }
    
    N <- nrow(X)
    tcols <- ifelse(int, sum(porders)+1, sum(porders))
    kk <- ifelse(int, 1, 0)
    mxX <- matrix(1, N, tcols)
    for(jj in 1:length(porders)) {
        for(j in 1:(porders[jj])) {
            kk <- kk + 1
            mxX[ , kk ] <- X[ , jj ]^( j )
        }
    }
    bhat <- solve( crossprod(mxX) ) %*% ( t(mxX) %*% y )
    
    
    fhat <- function(X) {
        
        N <- nrow(X)
        tcols <- ifelse(int, sum(porders)+1, sum(porders))
        kk <- ifelse(int, 1, 0)
        mxX <- matrix(1, N, tcols)
        for(jj in 1:length(porders)) {
            for(j in 1:(porders[jj])) {
                kk <- kk + 1
                mxX[ , kk ] <- X[ , jj ]^( j )
            }
        }
        
        cat(bhat, "\n")
        
        yhat <- mxX %*% bhat
        return(yhat)
    }
    
    ##### note that bhat gets bound to the scope of fhat
    ##### attr(fhat, "bhat") <- bhat
    
    return(fhat)
    
}




h.knn.rgr <- function(X, y, k) {
    
    if( k < 1 ) { stop("arg k must be > 0") }
    
    N <- nrow(X)
    p <- ncol(X)
    
    ycats <- sort(unique(y)) ; ycats
    
    
    
    fhat <- function(x) {
        
        if( p != ncol(x) ) { stop("x must have the same number of columns as data design matrix.") }
        
        y0 <- rep(NA, nrow(x))
        
        for(i in 1:nrow(x)) {
            xdist <- sqrt( apply((t(X) - x[i, ])^2, 2, sum) )
            xperm <- order(xdist) ; xperm
            y0[ i ] <- mean(y[ xperm[ 1:k ] ]) ; y0
            
        }
        return(y0)
    }
    
    ##### note that X, y, p, N, ycats get bound to the scope of fhat
    
    return(fhat)
    
}




h.ridge <- function(X, y, lambda) {
    
    p <- ncol(X)
    
    bhat <- solve( crossprod(X) + diag(lambda, p) ) %*% ( t(X) %*% y )
    
    fhat <- function(x) {
        yhat <- x %*% bhat
        return(yhat)
    }
    
    ##### note that bhat gets bound to the scope of fhat
    
    return(fhat)
    
}




h.qda <- function(X, y) {
    
    
    N <- nrow(X)
    p <- ncol(X)
    
    ycats <- sort(unique(y)) ; ycats
    
    ls.base.p <- list()
    ls.Sig.hats <- list()
    ls.mu.hats <- list()
    ls.invSig.hats <- list()
    ls.detSig.hats <- list()
    
    ik <- 1
    for(ik in 1:length(ycats)) {
        ythis.cat <- ycats[ik] ; ythis.cat
        ls.mu.hats[[ ythis.cat ]] <- apply(X[ y == ythis.cat, ], 2, mean)
        ls.Sig.hats[[ ythis.cat ]] <- var(X[ y == ythis.cat, ])
        ls.invSig.hats[[ ythis.cat ]] <- solve( ls.Sig.hats[[ ythis.cat ]] )
        ls.detSig.hats[[ ythis.cat ]] <- det(ls.Sig.hats[[ ythis.cat ]])
        ls.base.p[[ ythis.cat ]] <- sum( y %in% ythis.cat ) / N
    }
    
    ##### x <- X[ 1:3, , drop=FALSE ]
    
    fhat <- function(x) {
        
        if( p != ncol(x) ) { stop("x must have the same number of columns as data design matrix.") }
        
        mx.ddl <- matrix(0, nrow(x), length(ycats))
        colnames(mx.ddl) <- ycats
        
        i <- 1
        for(i in 1:nrow(x)) {
            
            this.x <- x[ i, , drop=FALSE] ; this.x
            
            j <- 1
            for(j in 1:length(ycats)) {
                
                ythis.cat <- ycats[j] ; ythis.cat
                
                mx.ddl[ i, j ] <- -1/2 * this.x %*% ls.invSig.hats[[ ythis.cat ]] %*% t(this.x) +
                this.x %*% ls.invSig.hats[[ ythis.cat ]] %*% ls.mu.hats[[ ythis.cat ]] -
                1/2 * t(ls.mu.hats[[ ythis.cat ]]) %*% ls.invSig.hats[[ ythis.cat ]] %*% ls.mu.hats[[ ythis.cat ]] -
                1/2 * log(ls.detSig.hats[[ ythis.cat ]]) +
                log( ls.base.p[[ ythis.cat ]] )
                
            }
            
        }
        return(mx.ddl)
    }
    
    ##### note that X, y, p, N, ycats, ls.base.p, etc get bound to the scope of fhat
    
    return(fhat)
    
}



