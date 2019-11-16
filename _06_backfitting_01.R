

######### this is an illustration of Q11 from Ch 7

options(stringsAsFactors=FALSE, width=350)


xbool.savePlots <- FALSE


####### make sure in working directory
source("___f_funs.R")


n <- 100

b0 <- 5
b1 <- 2
b2 <- -1

x1 <- rnorm(n)
x2 <- rnorm(n)

f_true <- b0 + b1*x1 + b2*x2

errs <- rnorm(n)

y <- f_true + errs


library(plot3D)


if(xbool.savePlots) { png( file.path("~", "Desktop", "scatter_sim_01.png"), height=1000, width=1000, pointsize=24) }
scatter3D(x1, x2, y, xlab="x1", ylab="x2", zlab="y", phi=20, theta=40, lwd=3)
if(xbool.savePlots) { dev.off() }



b1hat <- 0

nn <- 1000

for(ii in 1:nn) {
    
    yadj <- y - b1hat*x1 ; yadj
    b2hat <- lm(yadj ~ x2)$coef[2]
    
    yadj <- y - b2hat*x2 ; yadj
    b1hat <- lm(yadj ~ x1)$coef[2]
    
    cat(b1hat, b2hat, "\n")
    
}


###### compare with:

xlm <- lm(y ~ x1 + x2)

summary(xlm)

