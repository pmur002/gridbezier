
library(grid)

x <- c(0, .1, .2, .3)
y <- c(0, .2, .2, 0)

grid.newpage()
grid.lines(x, y, gp=gpar(col="grey"))
grid.circle(x, y, r=unit(1, "mm"), gp=gpar(col=NA, fill="grey"))
pts <- bezCurvePts(x, y)
grid.lines(pts[,1], pts[,2])
grid.bezier(x, y, gp=gpar(col="red"))

x <- c(0, .1, .2, .3, .4, .5, .6)
y <- c(0, .2, .2, .1, 0, .2, 0)

grid.newpage()
grid.lines(x, y, gp=gpar(col="grey"))
grid.circle(x, y, r=unit(1, "mm"), gp=gpar(col=NA, fill="grey"))
pts <- bezSplinePts(x, y)
grid.lines(pts[,1], pts[,2])

system.time({ for (i in 1:1000) pts <- bezSplinePts(x, y) })

