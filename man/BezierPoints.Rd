\name{BezierPoints}
\alias{BezierPoints}
\alias{BezierTangent}
\alias{BezierNormal}
\title{
  Calculate Points on a Bezier Curve
}
\description{
  Calculate points on a Bezier curve and/or tangents and/or normals
  to the curve at those points.
}
\usage{
BezierPoints(x, range = NULL)
BezierTangent(x, range = NULL)
BezierNormal(x, range = NULL)
}
\arguments{
  \item{x}{A \code{"BezierGrob"} object.
  }
  \item{range}{The range of \var{t} values within which to calculate
    points (or tangents or normals).  A numeric vector of length 2.}
}
\details{
  The tangents and normals are 1 inch in length.
}
\value{
  All functions return a list with components \code{x} and \code{y}.
  For \code{BezierPoints} these are locations on the curve.
  For \code{BezierTangent} and \code{BezierNormal}, these are the
  distances to the end
  points of tangent or normal line segments.
  All values are in inches.
}
\author{
  Paul Murrell
}
\examples{
x <- BezierGrob(c(.2, .2, .8, .8), c(.2, .8, .8, .2),
                stepFn=function(...) seq(0, 1, length.out=10))
grid.draw(x)
pts <- BezierPoints(x)
grid.circle(pts$x, pts$y, default.units="in", r=unit(.5, "mm"),
            gp=gpar(fill="black"))
tan <- BezierTangent(x)
grid.segments(pts$x, pts$y, pts$x + tan$x, pts$y + tan$y,
              default.units="in", gp=gpar(col="green"))
norm <- BezierNormal(x)
grid.segments(pts$x, pts$y, pts$x + norm$x, pts$y + norm$y,
              default.units="in", gp=gpar(col="red"))
}
\keyword{ dplot }% use one of  RShowDoc("KEYWORDS")
