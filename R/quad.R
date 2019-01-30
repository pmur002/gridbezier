

## Following ...
## https://pomax.github.io/bezierinfo/#matrix

quadMatrix <- rbind(c(1, 0, 0),
                    c(-2, 2, 0),
                    c(1, -2, 1))

quadDerivMatrix <- rbind(c(-2, 2, 0),
                         c(2, -4, 2))

## stepFn is given 'x' and 'y' (each length 4) 
## for the control points and a range for 't'
nSteps <- function(n) {
    function(x, y, range=c(0, 1)) {
        seq(range[1], range[2],
            ## Minimum of 2 steps
            length.out=max(2, round(n*(abs(diff(range))))))
    }
}

## Just start with single Bezier curve
## 'x' and 'y' must be length 4
quadCurvePts <- function(x, y, stepFn=nSteps(100), range=c(0, 1)) {
    if (any(is.na(range))) {
        cbind(x=numeric(), y=numeric())
    } else {
        t <- stepFn(x, y, range)
        t2 <- t*t
        tmatrix <- cbind(1, t, t2)
        mult <- tmatrix %*% quadMatrix
        bx <- mult %*% x
        by <- mult %*% y
        cbind(x=bx, y=by)
    }
}

## Expand to Bezier spline
## 'x' and 'y' must be length multiple-of-two-plus-one (and the same length)
quadSplinePts <- function(x, y, ncurves, stepFn=nSteps(100), range=NULL) {
    ## Expand 'x' and 'y' to sets of four control points
    index <- lapply(1:ncurves, function(i) ((i-1)*2 + 1):(i*2 + 1))
    ## 'range' determines how much of the spline to generate
    ranges <- genRanges(range, ncurves)
    curves <- lapply(1:ncurves,
                     function(i) {
                         index <- ((i-1)*2 + 1):(i*2 + 1)
                         if (i == 1 || any(is.na(ranges[,i-1]))) {
                             quadCurvePts(x[index], y[index],
                                         stepFn, ranges[,i])
                         } else {
                             ## Drop first point to avoid duplication
                             quadCurvePts(x[index], y[index],
                                         stepFn, ranges[,i])[-1,]
                         }
                     })
    list(x=unlist(lapply(curves, function(c) c[,1])),
         y=unlist(lapply(curves, function(c) c[,2])))
}

quadPoints <- function(x, range=NULL) {
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
    quadSplinePts(xx, yy, x$ncurves, x$stepFn, range)    
}

quadCurveSlope <- function(x, y, stepFn=nSteps(100), range=c(0, 1)) {
    t <- stepFn(x, y, range)
    tmatrix <- cbind(1, t)
    mult <- tmatrix %*% quadDerivMatrix
    dx <- mult %*% x
    dy <- mult %*% y
    cbind(x=dx, y=dy)
}

quadSplineSlope <- function(x, y, ncurves, stepFn=nSteps(100), range=NULL) {
    ## Expand 'x' and 'y' to sets of four control points
    index <- lapply(1:ncurves, function(i) ((i-1)*2 + 1):(i*2 + 1))
    ## 'range' determines how much of the spline to generate
    ranges <- genRanges(range, ncurves)
    slopes <- lapply(1:ncurves,
                     function(i) {
                         index <- ((i-1)*2 + 1):(i*2 + 1)
                         if (i == 1 || any(is.na(ranges[,i-1]))) {
                             quadCurveSlope(x[index], y[index],
                                           stepFn, ranges[,i])
                         } else {
                             quadCurveSlope(x[index], y[index],
                                           stepFn, ranges[,i])[-1,]
                         }
                     })
    list(x=unlist(lapply(slopes, function(c) c[,1])),
         y=unlist(lapply(slopes, function(c) c[,2])))
}

quadTangent <- function(x, range=NULL) {
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
    slope <- quadSplineSlope(xx, yy, x$ncurves, x$stepFn, range)
    len <- sqrt(slope$x^2 + slope$y^2)
    list(x=slope$x/len, y=slope$y/len)
}

quadNormal <- function(x, range=NULL) {
    tangent <- quadTangent(x, range)
    list(x=tangent$y, y=-tangent$x)
}

makeContent.quadgrob <- function(x) {
    pts <- quadPoints(x)
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

checkQuadCP <- function(x, y, open) {
    n <- max(length(x), length(y))
    if (!open)
        n <- n + 1
    ncurves <- (n - 1) %/% 2
    if (ncurves*2 + 1 != n) 
        stop("Invalid number of control points")
    ncurves
}

quadGrob <- function(x, y, default.units="npc",
                     open=TRUE,
                     stepFn=nSteps(100),
                     gp=gpar(), name=NULL) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    gTree(x=x, y=y, ncurves=checkQuadCP(x, y, open),
          open=open, stepFn=stepFn, gp=gp, name=name,
          cl="quadgrob")
}

grid.quad <- function(...) {
    grid.draw(quadGrob(...))
}

