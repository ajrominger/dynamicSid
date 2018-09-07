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
rr <- cumsum(rnorm(tmax / dt, 0, 0.01)) + 0.3
plot(rr)
foo <- worms(1, rr, 8, 50, 0.01)

plot(foo, type = 'l')

