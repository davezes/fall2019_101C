

###

### THESE ARE DATA ESTIMATES, EMPIRIC ESTIMATES OF MSE

options(stringsAsFactors=FALSE, width=350)



N <- 1000



########################## mux 0



sige2 <- 4


b0 <- 1
b1 <- 1
b2 <- 2


x <- runif(N, 1, 2)
x2 <- x^2

f.true <- b0 + b1 * x + b2 * x2

errs <- rnorm(N, 0, sqrt(sige2))

y <- f.true + errs


plot(x, y)



xlm <- lm(y ~ x + x2)

summary(xlm)


library(car)


vif(xlm)





############### recenter

x.mean <- mean(x)

x.c <- x - x.mean

x.c2 <- x.c^2




plot(x.c, y)



xlm.c <- lm(y ~ x.c + x.c2)

summary(xlm.c)


vif(xlm.c)


#png("~/Desktop/vif_example_02.png", height=1000, width=2000, pointsize=28)

par(mfrow=c(1, 2))
plot(x, y)
plot(x.c, y)

#dev.off()





