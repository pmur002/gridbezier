\name{grid.quad}
\alias{grid.quad}
\alias{quadGrob}
\title{
  Draw a Quadratic Bezier Spline
}
\description{
  Draw a quadratic Bezier curve or quadratic Bezier spline
  (multiple quadratic Bezier curves
  strung together).
}
\usage{
grid.quad(...)
quadGrob(x, y, default.units="npc",
           open=TRUE, stepFn=nSteps(100), gp=gpar(), name=NULL)
}
\arguments{
  \item{x, y}{Locations of control points.  There should be three,
    or five (or four if not \code{open}), or seven (or eight), etc.
    Locations can be numeric or \pkg{grid} \code{"unit"} objects.
  }
  \item{default.units}{The coordinate system to use if control point
    locations are just numeric.}
  \item{open}{Whether to reuse the first control point as the last
    control point.  If closed, the shape may also be filled.}
  \item{stepFn}{A function to generate values of \var{t} at which the
    curve will be evaluated for drawing.  The default is 100 equal-sized
    steps from 0 to 1. This function is called for each Bezier curve
    within
    the Bezier spline, with arguments \code{x},
    \code{y} (the control points), and \code{range} (indicating the
    range of \var{t} to generate values for).}
  \item{gp}{A \pkg{grid} \code{"gpar"} object, as produced by
    \code{gpar()}, or \code{NULL}.}
  \item{name}{A name for the grob that is generated.}
  \item{\dots}{Arguments passed from \code{grid.quad()} to
    \code{quadGrob()}.}
}
\value{
  \code{quadGrob} produces a \code{"quadgrob"} object.
}
\author{
  Paul Murrell
}
\examples{
grid.quad(c(.2, .5, .8), c(.2, .8, .2))
}
\keyword{ dplot }% use one of  RShowDoc("KEYWORDS")
