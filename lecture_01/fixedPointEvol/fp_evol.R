curve(10 * cos(x) - 1.5 * cos(4 * x), from = -1, to = 1)

curve(15 -10 * x^2 + 30*dnorm(x, 2, 0.5), from = -2, to = 4, ylim = c(0, 15))
curve(10*dnorm(x, 4, 0.5), add = TRUE)



curve(6 * dnorm(x, 0, 0.8) + 2 * dnorm(x, 3, 0.4) - 0.04 * (x - 1)^4, from = -2, to = 4)
