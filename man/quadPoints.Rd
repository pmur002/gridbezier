\name{quadPoints}
\alias{quadPoints}
\alias{quadTangent}
\alias{quadNormal}
\title{
  Calculate Points on a Bezier Curve
}
\description{
  Calculate points on a Bezier curve and/or tangents and/or normals
  to the curve at those points.
}
\usage{
quadPoints(x, range = NULL)
quadTangent(x, range = NULL)
quadNormal(x, range = NULL)
}
\arguments{
  \item{x}{A \code{"quadGrob"} object.
  }
  \item{range}{The range of \var{t} values within which to calculate
    points (or tangents or normals).  A numeric vector of length 2.}
}
\details{
  The tangents and normals are 1 inch in length.
}
\value{
  All functions return a list with components \code{x} and \code{y}.
  For \code{quadPoints} these are locations on the curve.
  For \code{quadTangent} and \code{quadNormal}, these are the
  distances to the end
  points of tangent or normal line segments.
  All values are in inches.
}
\author{
  Paul Murrell
}
\examples{
x <- quadGrob(c(.2, .5, .8), c(.2, .8, .2),
                stepFn=function(...) seq(0, 1, length.out=10))
grid.draw(x)
pts <- quadPoints(x)
grid.circle(pts$x, pts$y, default.units="in", r=unit(.5, "mm"),
            gp=gpar(fill="black"))
tan <- quadTangent(x)
grid.segments(pts$x, pts$y, pts$x + tan$x, pts$y + tan$y,
              default.units="in", gp=gpar(col="green"))
norm <- quadNormal(x)
grid.segments(pts$x, pts$y, pts$x + norm$x, pts$y + norm$y,
              default.units="in", gp=gpar(col="red"))
}
\keyword{ dplot }% use one of  RShowDoc("KEYWORDS")
