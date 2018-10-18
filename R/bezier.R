

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
## for the control points and a range for 't'
nSteps <- function(n) {
    function(x, y, range=c(0, 1)) {
        seq(range[1], range[2], length.out=round(n*(abs(diff(range)))))
    }
}

## Just start with single Bezier curve
## 'x' and 'y' must be length 4
bezCurvePts <- function(x, y, stepFn=nSteps(100), range=c(0, 1)) {
    if (any(is.na(range))) {
        cbind(x=numeric(), y=numeric())
    } else {
        t <- stepFn(x, y, range)
        t2 <- t*t
        t3 <- t2*t
        tmatrix <- cbind(1, t, t2, t3)
        mult <- tmatrix %*% bezMatrix
        bx <- mult %*% x
        by <- mult %*% y
        cbind(x=bx, y=by)
    }
}

genRanges <- function(range, ncurves) {
    if (is.null(range)) {
        ranges <- matrix(0:1, nrow=2, ncol=ncurves)
    } else {
        ranges <- matrix(NA, nrow=2, ncol=ncurves)
        for (i in 1:ncurves) {
            if (range[1] < i || i == ncurves) {
                if (i == 1 || range[1] > i - 1) {
                    ranges[1, i] <- range[1] - (i - 1)
                } else {
                    ranges[1, i] <- 0
                }
            }
            if (range[2] > i - 1 || i == 1) {
                if (i == ncurves || range[2] < i) {
                    ranges[2, i] <- range[2] - (i - 1)
                } else {
                    ranges[2, i] <- 1
                }
            }
        }
    }
    ranges
}

## Expand to Bezier spline
## 'x' and 'y' must be length multiple-of-three-plus-one (and the same length)
bezSplinePts <- function(x, y, stepFn=nSteps(100), range=NULL) {
    ncurves <- (length(x) - 1) %/% 3
    if (ncurves*3 + 1 != length(x)) 
        stop("Invalid number of control points")
    ## Expand 'x' and 'y' to sets of four control points
    index <- lapply(1:ncurves, function(i) ((i-1)*3 + 1):(i*3 + 1))
    ## 'range' determines how much of the spline to generate
    ranges <- genRanges(range, ncurves)
    curves <- lapply(1:ncurves,
                     function(i) {
                         index <- ((i-1)*3 + 1):(i*3 + 1)
                         if (i == 1 || any(is.na(ranges[,i-1]))) {
                             bezCurvePts(x[index], y[index],
                                         stepFn, ranges[,i])
                         } else {
                             ## Drop first point to avoid duplication
                             bezCurvePts(x[index], y[index],
                                         stepFn, ranges[,i])[-1,]
                         }
                     })
    list(x=unlist(lapply(curves, function(c) c[,1])),
         y=unlist(lapply(curves, function(c) c[,2])))
}

BezierPoints <- function(x, range=NULL) {
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
    bezSplinePts(xx, yy, x$stepFn, range)    
}

bezCurveSlope <- function(x, y, stepFn=nSteps(100), range=c(0, 1)) {
    t <- stepFn(x, y, range)
    t2 <- t*t
    tmatrix <- cbind(1, t, t2)
    mult <- tmatrix %*% derivMatrix
    dx <- mult %*% x
    dy <- mult %*% y
    cbind(x=dx, y=dy)
}

bezSplineSlope <- function(x, y, stepFn=nSteps(100), range=NULL) {
    ncurves <- (length(x) - 1) %/% 3
    if (ncurves*3 + 1 != length(x)) 
        stop("Invalid number of control points")
    ## Expand 'x' and 'y' to sets of four control points
    index <- lapply(1:ncurves, function(i) ((i-1)*3 + 1):(i*3 + 1))
    ## 'range' determines how much of the spline to generate
    ranges <- genRanges(range, ncurves)
    slopes <- lapply(1:ncurves,
                     function(i) {
                         index <- ((i-1)*3 + 1):(i*3 + 1)
                         if (i == 1 || any(is.na(ranges[,i-1]))) {
                             bezCurveSlope(x[index], y[index],
                                           stepFn, ranges[,i])
                         } else {
                             bezCurveSlope(x[index], y[index],
                                           stepFn, ranges[,i])[-1,]
                         }
                     })
    list(x=unlist(lapply(slopes, function(c) c[,1])),
         y=unlist(lapply(slopes, function(c) c[,2])))
}

BezierTangent <- function(x, range=NULL) {
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
    slope <- bezSplineSlope(xx, yy, x$stepFn, range)
    len <- sqrt(slope$x^2 + slope$y^2)
    list(x=slope$x/len, y=slope$y/len)
}

BezierNormal <- function(x, range=NULL) {
    tangent <- BezierTangent(x, range)
    list(x=tangent$y, y=-tangent$x)
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

