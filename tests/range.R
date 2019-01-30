
library(gridBezier)

## Generate points (or tangents or normals) for only a range of Bezier spline

x <- seq(.3, .7, length.out=10)
y <- rep(c(.1, .2, .2), length.out=10)

testRange <- function(r, offset) {
    bg <- BezierGrob(x, y + offset, stepFn=nSteps(10))
    grid.draw(bg)
    pts <- BezierPoints(bg, r)
    grid.circle(pts$x, pts$y, unit(.5, "mm"), "in", gp=gpar(fill="white"))
}

grid.newpage()
testRange(NULL, 0)
testRange(c(0, 1), .2)
testRange(c(-.5, 0), .4)
testRange(c(.5, 1.5), .6)
testRange(c(1, 2), .8)

grid.newpage()
testRange(c(1.5, 2.5), 0)
testRange(c(2, 3), .2)
testRange(c(3, 3.5), .4)

x <- seq(.3, .7, length.out=7)
y <- rep(c(.1, .2), length.out=7)

testQuadRange <- function(r, offset) {
    bg <- quadGrob(x, y + offset, stepFn=nSteps(10))
    grid.draw(bg)
    pts <- quadPoints(bg, r)
    grid.circle(pts$x, pts$y, unit(.5, "mm"), "in", gp=gpar(fill="white"))
}

grid.newpage()
testQuadRange(NULL, 0)
testQuadRange(c(0, 1), .2)
testQuadRange(c(-.5, 0), .4)
testQuadRange(c(.5, 1.5), .6)
testQuadRange(c(1, 2), .8)

grid.newpage()
testQuadRange(c(1.5, 2.5), 0)
testQuadRange(c(2, 3), .2)
testQuadRange(c(3, 3.5), .4)

