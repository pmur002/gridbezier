
library(gridBezier)

normDiagram <- function(x, y, open=TRUE) {
    grid.newpage()
    grid.lines(x, y, gp=gpar(col="grey"))
    grid.circle(x, y, r=unit(1, "mm"), gp=gpar(col=NA, fill="grey"))
    bg <- BezierGrob(x, y, open=open)
    grid.draw(bg)
    pts <- BezierPoints(bg)
    norm <- BezierNormal(bg)
    grid.segments(pts$x, pts$y,
                  pts$x + norm$x, pts$y + norm$y,
                  default.units="in",
                  gp=gpar(col=rgb(0, 1, 0, .2)))
}

x <- c(0, .1, .2, .3) + .5
y <- c(0, .2, .2, 0) + .5
normDiagram(x, y)

x <- c(0, .1, .2) + .5
y <- c(0, .2, .2) + .5
normDiagram(x, y, open=FALSE)

x <- c(0, .1, .2, .3, .4, .5, .6) + .3
y <- c(0, .2, .2, .1, 0, .2, 0) + .3
normDiagram(x, y)

x <- c(0, .1, .2, .3, .4, .5) + .3
y <- c(0, .2, .2, .1, 0, .2) + .3
normDiagram(x, y, open=FALSE)


quadNormDiagram <- function(x, y, open=TRUE) {
    grid.newpage()
    grid.lines(x, y, gp=gpar(col="grey"))
    grid.circle(x, y, r=unit(1, "mm"), gp=gpar(col=NA, fill="grey"))
    bg <- quadGrob(x, y, open=open)
    grid.draw(bg)
    pts <- quadPoints(bg)
    norm <- quadNormal(bg)
    grid.segments(pts$x, pts$y,
                  pts$x + norm$x, pts$y + norm$y,
                  default.units="in",
                  gp=gpar(col=rgb(0, 1, 0, .2)))
}

x <- c(0, .1, .2) + .5
y <- c(0, .2, 0) + .5
quadNormDiagram(x, y)

x <- c(0, .1, .2, .3) + .5
y <- c(0, .2, 0, .2) + .5
quadNormDiagram(x, y, open=FALSE)

x <- c(0, .1, .2, .3, .4) + .5
y <- c(0, .2, 0, -.2, 0) + .5
quadNormDiagram(x, y)

x <- c(0, .1, .2, .3) + .5
y <- c(0, .2, 0, -.2) + .5
quadNormDiagram(x, y, open=FALSE)

