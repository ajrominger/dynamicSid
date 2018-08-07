dwormst1 <- function(n, r, k) {
    r * (1 - n / k)
}

dwormst2 <- function(n, r, k) {
    n / (1 + n^2)
}

dworms <- function(n, r, k) {
     n * (dwormst1(n, r, k) - dwormst2(n, r, k))
}


solworms <- function(r, k) {
    # browser()
    x <- seq(0.01, k, length.out = 1000)
    s <- dwormst1(x, r, k) - dwormst2(x, r, k)
    p <- diff(s < 0)
    
    solPoints <- which(p != 0)
    solTypes <- p[solPoints]
    
    roots <- sapply(solPoints, function(i) {
        uniroot(interval = x[c(i - 1, i + 1)], 
                f = function(n) dwormst1(n, r, k) - dwormst2(n, r, k))$root
    })
    
    return(list(roots = roots, types = solTypes))
}

eqworms <- png::readPNG('worms_eq.png')

npar <- 100
rr <- seq(0.1, 0.65, length.out = npar)
k <- 8

for(i in 1:npar) {
    sol <- solworms(rr[i], k)
    
    png(paste0('temp/frame_', paste0(rep(0, nchar(as.character(npar)) - nchar(as.character(i))), collapse = ''), 
              i, '.png', collapse = ''))
    
    layout(matrix(1:3, nrow = 3), heights = c(1, 3, 3))
    
    par(oma = c(3, 0.5, 0, 0), mar = c(0.5, 1.5, 0.75, 0.5), cex.lab = 1.5, cex.axis = 1.6, 
        mgp = c(2, 1, 0), tck = -0.03)
    
    plot(1, xlim = 0:1, ylim = 0:1, xaxs = 'i', yaxs = 'i', type = 'n', axes = FALSE)
    rasterImage(eqworms, 0.2, 0, 0.8, 1)
    
    par(mar = c(2.5, 2.5, 0.5, 0.5))
    
    curve(dwormst1(x, rr[i], k), from = 0, to = 8, col = 'red', 
          xlab = '', ylab = '', ylim = c(0, 0.65))
    curve(dwormst2(x, rr[i], k), col = 'blue', add = TRUE)
    points(sol$roots, dwormst1(sol$roots, rr[i], k), pch = c(16, 21)[(sol$types == -1) + 1], bg = 'white', cex = 2)
    
    curve(dworms(x, rr[i], k), from = 0, to = 8, xlab = '', ylab = '', 
          ylim = c(-0.5, 0.5))
    abline(h = 0, lty = 2)
    
    points(sol$roots, dworms(sol$roots, rr[i], k), pch = c(16, 21)[(sol$types == -1) + 1], bg = 'white', cex = 2)
    
    mtext('Population size', side = 1, outer = TRUE, line = 1)
    
    dev.off()
}

## make gif using ImageMagick
system('convert temp/*.png -delay 3 -loop 0 budworm_dynamics.gif')
