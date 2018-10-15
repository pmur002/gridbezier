

## Following ...
## https://pomax.github.io/bezierinfo/#matrix

bezMatrix <- rbind(c(1, 0, 0, 0),
                   c(-3, 3, 0, 0),
                   c(3, -6, 3, 0),
                   c(-1, 3, -3, 1))

derivMatrix <- rbind(c(-3, 3, 0, 0),
                     c(6, -12, 6, 0),
                     c(-3, 9, -9, 3))

## stepFn is given 'x' and 'y' (each length 4)
## for the control points;  it must return 't' between 0 and 1
nSteps <- function(n) {
    function(x, y) {
        seq(0, 1, length.out=n)
    }
}

bezCurveSlope <- function(x, y, stepFn=nSteps(100)) {
    t <- stepFn(x, y)
    t2 <- t*t
    tmatrix <- cbind(1, t, t2)
    mult <- tmatrix %*% derivMatrix
    dx <- mult %*% x
    dy <- mult %*% y
    cbind(x=dx, y=dy)
}

bezSplineSlope <- function(x, y, stepFn=nSteps(100)) {
    ncurves <- (length(x) - 1) %/% 3
    if (ncurves*3 + 1 != length(x)) 
        stop("Invalid number of control points")
    ## Expand 'x' and 'y' to sets of four control points
    index <- lapply(1:ncurves, function(i) ((i-1)*3 + 1):(i*3 + 1))
    slopes <- lapply(1:ncurves,
                     function(i) {
                         index <- ((i-1)*3 + 1):(i*3 + 1)
                         if (i == 1) {
                             bezCurveSlope(x[index], y[index], stepFn)
                         } else {
                             bezCurveSlope(x[index], y[index], stepFn)[-1,]
                         }
                     })
    list(x=unlist(lapply(slopes, function(c) c[,1])),
         y=unlist(lapply(slopes, function(c) c[,2])))
}

BezierTangent <- function(x) {
    xx <- convertX(x$x, "in", valueOnly=TRUE)
    yy <- convertY(x$y, "in", valueOnly=TRUE)
    nx <- length(xx)
    ny <- length(yy)
    n <- max(nx, ny)
    if (nx != ny) {
        xx <- rep(xx, length.out=n)
        yy <- rep(yy, length.out=n)
    }
    if (!x$open) {
        xx <- c(xx, xx[1])
        yy <- c(yy, yy[1])
    }
    slope <- bezSplineSlope(xx, yy, x$stepFn)
    len <- sqrt(slope$x^2 + slope$y^2)
    list(x=slope$x/len, y=slope$y/len)
}

BezierNormal <- function(x) {
    tangent <- BezierTangent(x)
    list(x=tangent$y, y=-tangent$x)
}

## Just start with single Bezier curve
## 'x' and 'y' must be length 4
bezCurvePts <- function(x, y, stepFn=nSteps(100)) {
    t <- stepFn(x, y)
    t2 <- t*t
    t3 <- t2*t
    tmatrix <- cbind(1, t, t2, t3)
    mult <- tmatrix %*% bezMatrix
    bx <- mult %*% x
    by <- mult %*% y
    cbind(x=bx, y=by)
}

## Expand to Bezier spline
## 'x' and 'y' must be length multiple-of-three-plus-one (and the same length)
bezSplinePts <- function(x, y, stepFn=nSteps(100)) {
    ncurves <- (length(x) - 1) %/% 3
    if (ncurves*3 + 1 != length(x)) 
        stop("Invalid number of control points")
    ## Expand 'x' and 'y' to sets of four control points
    index <- lapply(1:ncurves, function(i) ((i-1)*3 + 1):(i*3 + 1))
    curves <- lapply(1:ncurves,
                     function(i) {
                         index <- ((i-1)*3 + 1):(i*3 + 1)
                         if (i == 1) {
                             bezCurvePts(x[index], y[index], stepFn)
                         } else {
                             ## Drop first point to avoid duplication
                             bezCurvePts(x[index], y[index], stepFn)[-1,]
                         }
                     })
    list(x=unlist(lapply(curves, function(c) c[,1])),
         y=unlist(lapply(curves, function(c) c[,2])))
}

BezierPoints <- function(x) {
    xx <- convertX(x$x, "in", valueOnly=TRUE)
    yy <- convertY(x$y, "in", valueOnly=TRUE)
    nx <- length(xx)
    ny <- length(yy)
    n <- max(nx, ny)
    if (nx != ny) {
        xx <- rep(xx, length.out=n)
        yy <- rep(yy, length.out=n)
    }
    if (!x$open) {
        xx <- c(xx, xx[1])
        yy <- c(yy, yy[1])
    }
    bezSplinePts(xx, yy, x$stepFn)    
}

makeContent.Beziergrob <- function(x) {
    pts <- BezierPoints(x)
    if (x$open) {
        curve <- linesGrob(unit(pts$x, "in"),
                           unit(pts$y, "in"),
                           name="curve")
    } else {
        curve <- polygonGrob(unit(pts$x, "in"),
                             unit(pts$y, "in"),
                             name="curve")
    }
    setChildren(x, gList(curve))
}

BezierGrob <- function(x, y, default.units="npc",
                       open=TRUE,
                       stepFn=nSteps(100),
                       gp=gpar(), name=NULL) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    gTree(x=x, y=y, open=open, stepFn=stepFn, gp=gp, name=name,
          cl="Beziergrob")
}

grid.Bezier <- function(...) {
    grid.draw(BezierGrob(...))
}

