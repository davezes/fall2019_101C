

###

### THESE ARE DATA ESTIMATES, EMPIRIC ESTIMATES OF MSE

options(stringsAsFactors=FALSE, width=350)

xbool.savePlots <- TRUE

if(xbool.savePlots) { unlink(file.path("~", "Desktop", "animStuff", "*")) }

N <- 100



########################## mux 0

mux <- 0
sigx2 <- 1

sige2 <- 1


b0 <- 1
b1 <- 1


nn <- 100

k <- 0

while(k < 100) {
    
    k <- k + 1
    
    x <- rnorm(N, mux, sqrt(sigx2))
    
    f.true <- b0 + b1 * x
    
    errs <- rnorm(N, 0, sqrt(sige2))
    
    y <- f.true + errs
    
    bhat1 <- sum( (x - mean(x)) * (y - mean(y)) ) / sum( (x - mean(x))^2 ) ; bhat1
    bhat0 <-  mean(y) - bhat1 * mean(x) ; bhat0
    
    if(xbool.savePlots) { png( file.path("~", "Desktop", "animStuff", paste0("frame_", k, ".png")), width=1000, height=1000, pointsize=24 ) }
    plot(x, y, xlim=c(-6,6), ylim=c(-6,6), main=paste0("mu_x = ", mux))
    abline(v=0)
    abline(a=bhat0, b=bhat1, col="#00DD00")
    points(0, bhat0, col="#DD0000", pch=19, cex=1.4)
    if(xbool.savePlots) { dev.off() } else { Sys.sleep(0.3) }
    
    
}


if(xbool.savePlots) {
    xxsysstr <-
    paste0(
    "ffmpeg  -y  -loglevel error -r 15   " ,
    "  -i ",
    file.path("~", "Desktop", "animStuff", paste0("frame_", "%d.png")),
    "  -codec:v libx264   -pix_fmt yuv420p  -r 15  -preset slow  ",
    file.path("~", "Desktop", "mov_01.mp4")
    )
    
    system(xxsysstr)
}







########################## mux 2

if(xbool.savePlots) { unlink(file.path("~", "Desktop", "animStuff2", "*")) }

mux <- 4
sigx2 <- 1

sige2 <- 1


b0 <- 1
b1 <- 1


nn <- 100

k <- 0

while(k < 100) {
    
    k <- k + 1
    
    x <- rnorm(N, mux, sqrt(sigx2))
    
    f.true <- b0 + b1 * x
    
    errs <- rnorm(N, 0, sqrt(sige2))
    
    y <- f.true + errs
    
    bhat1 <- sum( (x - mean(x)) * (y - mean(y)) ) / sum( (x - mean(x))^2 ) ; bhat1
    bhat0 <-  mean(y) - bhat1 * mean(x) ; bhat0
    
    if(xbool.savePlots) { png( file.path("~", "Desktop", "animStuff2", paste0("frame_", k, ".png")), width=1000, height=1000, pointsize=24 ) }
    plot(x, y, xlim=c(-2, 10), ylim=c(-2, 10), main=paste0("mu_x = ", mux))
    abline(v=0)
    abline(a=bhat0, b=bhat1, col="#00DD00")
    points(0, bhat0, col="#DD0000", pch=19, cex=1.4)
    if(xbool.savePlots) { dev.off() } else { Sys.sleep(0.3) }
    
    
}


if(xbool.savePlots) {
    xxsysstr <-
    paste0(
    "ffmpeg  -y  -loglevel error -r 15   " ,
    "  -i ",
    file.path("~", "Desktop", "animStuff2", paste0("frame_", "%d.png")),
    "  -codec:v libx264   -pix_fmt yuv420p  -r 15  -preset slow  ",
    file.path("~", "Desktop", "mov_02.mp4")
    )
    
    system(xxsysstr)
}




