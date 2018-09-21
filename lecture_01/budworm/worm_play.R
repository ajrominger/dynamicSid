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

worms <- function(n0, r, k, t, dt) {
    nt <- round(t / dt)
    
    n <- c(n0, numeric(nt))
    
    if(length(r) == 1) r <- rep(r, nt)
    
    for(i in 1:nt) {
        n[i + 1] <- n[i] + dworms(n[i], r[i], k) * dt
        if(n[i + 1] <= 0) {
            n[i + 1] <- 0
            break
        }
    }
    
    return(n)
}

tmax <- 50
dt <- 0.01
nt <- tmax / dt

x <- seq(1, 0.25, length.out = nt)
rr <- x + (-0.5 + 1 / (1 + exp(-rnorm(nt, 0, 1))))
rr <- c(rep(0.6, nt / 2), rep(0.2, nt / 2)) + rnorm(nt, 0, 0.5)
rr <- cumsum(rnorm(nt, 0, 0.01))

k <- 8

plot(rr, type = 'l')
foo <- worms(6, rr, k, 50, 0.01)

plot(foo, type = 'l')

plot(rr, foo[-1])
