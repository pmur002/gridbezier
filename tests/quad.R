
library(gridBezier)

x <- c(0, .1, .2) + .5
y <- c(0, .2, 0) + .5

grid.newpage()
grid.lines(x, y, gp=gpar(col="grey"))
grid.circle(x, y, r=unit(1, "mm"), gp=gpar(col=NA, fill="grey"))
grid.quad(x, y)

x <- c(0, .1, .2, .3) + .5
y <- c(0, .2, 0, .2) + .5

grid.newpage()
grid.lines(x, y, gp=gpar(col="grey"))
grid.circle(x, y, r=unit(1, "mm"), gp=gpar(col=NA, fill="grey"))
grid.quad(x, y, open=FALSE)

x <- c(0, .1, .2, .3, .4) + .5
y <- c(0, .2, 0, -.2, 0) + .5

grid.newpage()
grid.lines(x, y, gp=gpar(col="grey"))
grid.circle(x, y, r=unit(1, "mm"), gp=gpar(col=NA, fill="grey"))
grid.quad(x, y)

x <- c(0, .1, .2, .3) + .5
y <- c(0, .2, 0, -.2) + .5

grid.newpage()
grid.lines(x, y, gp=gpar(col="grey"))
grid.circle(x, y, r=unit(1, "mm"), gp=gpar(col=NA, fill="grey"))
grid.quad(x, y, open=FALSE)


