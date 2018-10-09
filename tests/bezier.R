
library(gridBezier)
library(grid)

x <- c(0, .1, .2, .3) + .5
y <- c(0, .2, .2, 0) + .5

grid.newpage()
grid.lines(x, y, gp=gpar(col="grey"))
grid.circle(x, y, r=unit(1, "mm"), gp=gpar(col=NA, fill="grey"))
grid.Bezier(x, y)
grid.bezier(x, y, gp=gpar(col="red"))

x <- c(0, .1, .2) + .5
y <- c(0, .2, .2) + .5

grid.newpage()
grid.lines(x, y, gp=gpar(col="grey"))
grid.circle(x, y, r=unit(1, "mm"), gp=gpar(col=NA, fill="grey"))
grid.Bezier(x, y, open=FALSE)

x <- c(0, .1, .2, .3, .4, .5, .6) + .3
y <- c(0, .2, .2, .1, 0, .2, 0) + .3

grid.newpage()
grid.lines(x, y, gp=gpar(col="grey"))
grid.circle(x, y, r=unit(1, "mm"), gp=gpar(col=NA, fill="grey"))
grid.Bezier(x, y)

x <- c(0, .1, .2, .3, .4, .5) + .3
y <- c(0, .2, .2, .1, 0, .2) + .3

grid.newpage()
grid.lines(x, y, gp=gpar(col="grey"))
grid.circle(x, y, r=unit(1, "mm"), gp=gpar(col=NA, fill="grey"))
grid.Bezier(x, y, open=FALSE)


