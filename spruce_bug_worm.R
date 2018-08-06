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

solworms(0.5, 8)

r <- 0.5
k <- 8

par(mfrow = 2:1, mar = c(1, 3, 0, 0) + 0.5, oma = c(2, 0, 0, 0), mgp = c(2, 0.5, 0))

curve(dwormst1(x, r, k), from = 0, to = 8, col = 'red', frame.plot = FALSE, 
      xlab = '', ylab = '')
curve(dwormst2(x, r, k), col = 'blue', add = TRUE)

curve(dworms(x, 0.5, 8), from = 0, to = 8, xlab = 'Population size', ylab = expression(dN/dt), 
      frame.plot = FALSE, ylim = c(-1, 0.5))
abline(h = 0, lty = 2)

mtext('Population size', side = 1, outer = TRUE, line = 0.5)
