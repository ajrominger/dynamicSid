addFixedPoints <- function(x, y, types, r = 1) {
    r <- r * rep(0.025 * diff(par('usr')[1:2]), length(x))
    
    if(length(x) > 0) {
        symbols(x, y, circles = r, inches = FALSE, add = TRUE, 
                fg = c('black', 'transparent')[(types == 0) + 1], # bifurcations transparent
                bg = c('black', 'white')[(types <= 0) + 1])       # stable is black
        
        if(any(types == 0)) {
            for(i in 1:sum(types == 0)) {
                plotrix::floating.pie(x[types == 0][i], y[types == 0][i], 
                                      radius = r[types == 0][i], x = c(1, 1), 
                                      startpos = 0, col = c('black', 'white'))
            }
        }
    }
}

