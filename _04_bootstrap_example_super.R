


options(stringsAsFactors=FALSE, width=350)


xbool.savePlots <- TRUE


####### make sure in working directory
## source("___f_funs.R")


############ prepair dataset for bias variance

## library(mvtnorm)


n.super <- 100

xbig.missed <- rep(NA, n.super)

for(iisup in 1:n.super) {
    
    source("_04_bootstrap_example.R")
    xbig.missed[ iisup ] <- sum(xmissed)
    
    cat("\n\n\n", iisup, "\n\n\n")
}

mean(xbig.missed)

table(xbig.missed)


