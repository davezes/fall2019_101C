

### empirically show that f_hat and e may not be uncorrelated.

### this is an empirical demonstation and is not a substitute for a mathematical proof

options(stringsAsFactors=FALSE, width=350)


N <- 10000

x <- sort(runif(N, -1, 1))


f.true <- sign(x)

errs <- rnorm(N, 0, 1/2)

y <- f.true + errs


par(mfrow=c(1,2))

################# f_hat is a function of the errors
plot(x, y)

f.hat <- f.true + 0.5 * errs ###

points(x, f.true, col="#00FF00", type="l")
points(x, f.hat, col="#FF0000", type="l")


cor(f.true, errs)
cor(f.hat, errs)


mean( (y - f.hat)^2 )
mean( (f.true - f.hat)^2 ) + var(errs)



################# f_hat is a function of the errors
plot(x, y)

f.hat <- 0.5 * f.true ###

points(x, f.true, col="#00FF00", type="l")
points(x, f.hat, col="#FF0000", type="l")


cor(f.true, errs)
cor(f.hat, errs)


mean( (y - f.hat)^2 )
mean( (f.true - f.hat)^2 ) + var(errs)



