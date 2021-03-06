
library(gridBezier)

tanDiagram <- function(x, y, open=TRUE) {
    grid.newpage()
    grid.lines(x, y, gp=gpar(col="grey"))
    grid.circle(x, y, r=unit(1, "mm"), gp=gpar(col=NA, fill="grey"))
    bg <- BezierGrob(x, y, open=open)
    grid.draw(bg)
    pts <- BezierPoints(bg)
    tan <- BezierTangent(bg)
    grid.segments(pts$x, pts$y,
                  pts$x + tan$x, pts$y + tan$y,
                  default.units="in",
                  gp=gpar(col=rgb(1, 0, 0, .2)))
}

x <- c(0, .1, .2, .3) + .5
y <- c(0, .2, .2, 0) + .5
tanDiagram(x, y)

x <- c(0, .1, .2) + .5
y <- c(0, .2, .2) + .5
tanDiagram(x, y, open=FALSE)

x <- c(0, .1, .2, .3, .4, .5, .6) + .3
y <- c(0, .2, .2, .1, 0, .2, 0) + .3
tanDiagram(x, y)

x <- c(0, .1, .2, .3, .4, .5) + .3
y <- c(0, .2, .2, .1, 0, .2) + .3
tanDiagram(x, y, open=FALSE)

quadTanDiagram <- function(x, y, open=TRUE) {
    grid.newpage()
    grid.lines(x, y, gp=gpar(col="grey"))
    grid.circle(x, y, r=unit(1, "mm"), gp=gpar(col=NA, fill="grey"))
    bg <- quadGrob(x, y, open=open)
    grid.draw(bg)
    pts <- quadPoints(bg)
    tan <- quadTangent(bg)
    grid.segments(pts$x, pts$y,
                  pts$x + tan$x, pts$y + tan$y,
                  default.units="in",
                  gp=gpar(col=rgb(1, 0, 0, .2)))
}

x <- c(0, .1, .2) + .5
y <- c(0, .2, 0) + .5
quadTanDiagram(x, y)

x <- c(0, .1, .2, .3) + .5
y <- c(0, .2, 0, .2) + .5
quadTanDiagram(x, y, open=FALSE)

x <- c(0, .1, .2, .3, .4) + .5
y <- c(0, .2, 0, -.2, 0) + .5
quadTanDiagram(x, y)

x <- c(0, .1, .2, .3) + .5
y <- c(0, .2, 0, -.2) + .5
quadTanDiagram(x, y, open=FALSE)



