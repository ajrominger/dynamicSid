# part one of eq, linear in n
dwormst1 <- function(n, r, k) {
    r * (1 - n / k)
}

# part two of eq, funky in n
dwormst2 <- function(n, r, k) {
    n / (1 + n^2)
}

# all of diff eq
dworms <- function(n, r, k) {
     n * (dwormst1(n, r, k) - dwormst2(n, r, k))
}

# function to find the stable solution(s)
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


# second deriv wrt n
d2worms <- function(n, r, k) {
    t1 <- 1 + n^2
    t2 <- r * (1 - n/k) - n/t1
    
    t2 - n * (r * (1/k) + (1/t1 - n * (2 * n)/t1^2))
}

# function to find bifurcation points near `ninit` and `rinit`
bifurworms <- function(ninit, rinit, k) {
    f <- function(p) {
        c(dworms(p[1], p[2], k)^2, d2worms(p[1], p[2], k)^2)
    }
    
    s <- nleqslv::nleqslv(c(ninit, rinit), f)
    
    return(s$x)
}

bifurworms(4, 0.466, k)

# read in latex-generated eqs
eqworms <- png::readPNG('lecture_01/worms_eq.png')
eqK <- png::readPNG('lecture_01/k.png')
eqr <- png::readPNG('lecture_01/r.png')

# control parameters for computation
npar <- 200
rr <- seq(0.3, 0.7, length.out = npar)
k <- 8

# object to hold solutions across values of `rr`
nstar <- lapply(rr, function(r) {
    s <- solworms(r, k)
    
    return(cbind(r, s[[1]], s[[2]]))
})

nstar <- cbind(do.call(rbind, nstar), 3)
nstar[nstar[, 3] == 1, 4] <- kmeans(nstar[nstar[, 3] == 1, 2], 2)$cluster
colnames(nstar) <- c('r', 'n', 'type', 'group')


# loop over values of `rr` plotting parts 1 and 2, the whole diff eq
# and the bifurcation plot

for(i in 1:npar) {
    # extract this solution
    sol <- nstar[nstar[, 1] == rr[i], 2:3, drop = FALSE]
    sol <- list(roots = sol[, 1], types = sol[, 2])
    
    png(paste0('lecture_01/temp/frame_', paste0(rep(0, nchar(as.character(npar)) -
                                             nchar(as.character(i))),
                                     collapse = ''),
              i, '.png', collapse = ''),
        width = 5.8, height = 4.8, units = 'in', res = 280)
    
    layout(matrix(c(1, 1, 2, 2, 3, 3, 4, 5, 0, 6, 6, 0), ncol = 2), 
           heights = c(1, 1, 3, 3, 3, 3))
    
    par(oma = c(3, 0.5, 0, 0), mar = c(0, 2.5, 0.1, 2.5), cex.axis = 1.4, 
        mgp = c(2, 0.75, 0), tck = -0.03)
    
    # disply the diff eq
    plot(1, xlim = 0:1, ylim = 0:1, xaxs = 'i', yaxs = 'i', type = 'n', axes = FALSE, 
         xlab = '', ylab = '')
    rasterImage(eqworms, 0, 0.05, 1, 0.7)
    
    # plot the 2 eq parts
    par(mar = c(0.5, 2.5, 0.5, 2.5))
    
    curve(dwormst1(x, rr[i], k), from = 0, to = 8, col = 'red', 
          xlab = '', ylab = '', ylim = c(0, 0.65), xaxt = 'n', yaxt = 'n', 
          lwd = 2)
    axis(2, at = (0:3) * 2/10, cex.axis = 1.4)
    
    curve(dwormst2(x, rr[i], k), col = 'blue', add = TRUE, lwd = 2)
    points(sol$roots, dwormst1(sol$roots, rr[i], k), pch = c(16, 21)[(sol$types == -1) + 1], 
           bg = 'white', cex = 2)
    
    # plot the whole diff eq
    curve(dworms(x, rr[i], k), from = 0, to = 8, xlab = '', ylab = '', 
          ylim = c(-0.5, 0.5), yaxt = 'n', lwd = 2, 
          panel.first = abline(h = 0, col = 'gray'))
    axis(2, at = c(-0.4, 0, 0.4), cex.axis = 1.4)
    
    points(sol$roots, dworms(sol$roots, rr[i], k), pch = c(16, 21)[(sol$types == -1) + 1], 
           bg = 'white', cex = 2)
    
    mtext('Population size (N)', side = 1, line = 2.25, cex = 1.1)
    
    # show the parameter values
    par(mar = c(0, 0, 0, 0.5))
    
    plot(1, type = 'n', axes = FALSE, xlim = c(-0.05, 0.64), ylim = 0:1)
    rasterImage(eqK, -0.05, 0, 0.05, 0.4)
    
    plot(1, xlim = c(0.187, 0.695), ylim = 0:1, axes = FALSE)
    rasterImage(eqr, 0.19 + 0.005, 0.55, 0.235 + 0.005, 0.8)
    
    axis(1, at = pretty(rr), line = -1.5, tck = -0.2)
    
    y0 <- par('usr')[3] + 1.5 * par('cin')[2] * par('cex') * par('lheight') * 
        diff(grconvertY(0:1, 'inches', 'user'))
    segments(x0 = min(rr), x1 = max(rr), y0 = y0, lwd = 5, col = 'gray90')
    segments(x0 = min(rr), x1 = rr[i], y0 = y0, lwd = 5, col = 'black')
    points(rr[i], y0, pch = 21, bg = 'gray', cex = 2, lwd = 2)
    
    # abline(v = c(0.3, 0.7), col = 'blue')
    
    # the bifurcation plot
    par(mar = c(0.5, 4.5, 0.5, 0.5))
    plot(nstar[, 1:2], type = 'n', xlab = '', ylab = '')
    mtext('r', side = 1, line = 2.5, cex = 1.1)
    mtext(expression(N^'*'), side = 2, line = 2.5, cex = 1.1)
    
    lines(nstar[nstar[, 1] <= rr[i] & nstar[, 4] == 1, 1:2], lwd = 2)
    points(nstar[nstar[, 1] == rr[i] & nstar[, 4] == 1, 1:2, drop = FALSE], 
           pch = 16, cex = 2)
    
    lines(nstar[nstar[, 1] <= rr[i] & nstar[, 4] == 3, 1:2], lty = 3, lwd = 2)
    points(nstar[nstar[, 1] == rr[i] & nstar[, 4] == 3, 1:2, drop = FALSE], 
           pch = 21, bg = 'white', cex = 2)
    
    lines(nstar[nstar[, 1] <= rr[i] & nstar[, 4] == 2, 1:2], lwd = 2)
    points(nstar[nstar[, 1] == rr[i] & nstar[, 4] == 2, 1:2, drop = FALSE], 
           pch = 16, cex = 2)
    
    # par(xpd = NA)
    # abline(v = c(0.3, 0.7), col = 'red')
    
    dev.off()
}

## make gif using ImageMagick
system('convert lecture_01/temp/*.png -delay 3 -loop 0 lecture_01/budworm_dynamics.gif')
