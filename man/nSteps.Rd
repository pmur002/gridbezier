\name{nSteps}
\alias{nSteps}
\title{
  Calculate Steps for a Bezier Spline
}
\description{
  Calculate steps in \var{t} for drawing each Bezier curve within
  a Bezier spline.
}
\usage{
nSteps(n)
}
\arguments{
  \item{n}{The number of steps (assuming a range of \var{t} from 0 to 1).
  }
}
\details{
  This function generates a function that can be used as the
  \code{stepFn} argument to \code{\link{grid.Bezier}}.
  It will simply generate \code{n} values in the range 0 to 1,
  though if \code{range} is also provided, the number of steps
  is reduced (see the examples below).  A minimum of 2 steps will
  be generated.
}
\value{
  \code{BezierGrob} produces a \code{"BezierGrob"} object.
}
\author{
  Paul Murrell
}
\examples{
nSteps(100)
nSteps(100)(range=0:1)
nSteps(100)(range=0:1/2)
}
\keyword{ dplot }% use one of  RShowDoc("KEYWORDS")
