R.utils::sourceDirectory('R')

# first deriv
df <- function(x, b) {
    sd <- 0.75
    b - 0.3 * x - 2 * x^2 - 10 * dnorm(x, sd = sd)
}

# second deriv
ddf <- function(x) {
    sd <- 0.75
    x * (10 / sd^2) * dnorm(x, sd = sd) - 4 * x - 0.3
}

# range of control parameter `b`
nframe <- 80
bb <- seq(5.5, 3.8, length.out = nframe)


# the points where ddf is 0
dd0 <- c(uniroot(ddf, c(-1.1, -0.9))$root, 
         uniroot(ddf, c(-0.1, 0.1))$root, 
         uniroot(ddf, c(0.9, 1.1))$root)

# bifurcation points
bbifur <- sapply(dd0, function(x) {
    foo <- uniroot(function(b) {df(x, b)}, range(bb))$root
})

# add bifurcation points
bb <- sort(unique(c(bb, bbifur)), decreasing = TRUE)
nframe <- length(bb)

# fixed points
fp <- lapply(bb, function(b) {
    ints <- c(-2, dd0, 2)
    
    sol <- rep(NA, 4)
    for(i in 1:(length(ints) - 1)) {
        if(sum(sign(df(ints[i + (0:1)], b))) == 0) {
            sol[i] <- uniroot(df, ints[i + (0:1)], b = b)$root
        }
    }
    
    return(sol)
})

# clean up
fp <- cbind(rep(bb, each = 4), unlist(fp), rep(c(-1, 1, -1, 1), length(bb)))
fp <- fp[!is.na(fp[, 2]), ]
colnames(fp) <- c('b', 'x', 'type')

# add bifurcation points to fp
fp <- rbind(fp, cbind(b = bbifur, x = dd0, type = 0))
fp <- fp[order(fp[, 1], decreasing = TRUE), ]

# add f(x)
fp <- cbind(fp, f = 0)

# make folder to temporarily hold frames until processed into gif
if(!file.exists('lecture_01/fixedPointEvol/temp')) system('mkdir lecture_01/fixedPointEvol/temp')

# loop over bb
for(i in 1:nframe) {
    png(paste0('lecture_01/fixedPointEvol/temp/frame_', indexExpand(i, 0, nframe, 1), '.png'), 
        width = 4.8, height = 4.8, units = 'in', res = 280)
    
    par(mar = c(3, 3, 0, 0) + 0.5, mgp = c(1.75, 0.5, 0))
    
    curve(df(x, bb[i]), from = -2, to = 2, ylim = c(-1.5, 1.75), 
          panel.first = abline(h = 0, col = 'gray'), lwd = 2, 
          xlab = 'x', ylab = 'f(x)', cex.lab = 1.4, yaxt = 'n')
    axis(2, at = c(-1, 0, 1))
    
    theseFP <- fp[fp[, 1] == bb[i], , drop = FALSE]
    
    addFixedPoints(theseFP[, 'x'], theseFP[, 'f'], theseFP[, 'type'])
    # points(fp[fp[, 1] == bb[i], c('x', 'f'), drop = FALSE])
    dev.off()
}

animate('lecture_01/fixedPointEvol', 'fp_evol', cleanUp = TRUE)
