\name{set.breaks}
\alias{set.breaks}
\title{Set breaks for color palette. Internal function.}
\usage{
  set.breaks(mat, interval = 0.1)
}
\arguments{
  \item{mat}{data matrix or vector for which the breaks
  will be deterined}

  \item{interval}{interval of color breaks}
}
\value{
  A vector of breakpoints
}
\description{
  Set breaks for color palette. Internal function.
}
\examples{
# mat <- rbind(c(1,2,3,4,5), c(1, 3, 1), c(4,2,2)); pm <- PlotMatrix(mat, "twoway", midpoint = 3); PlotScale(pm$colors, pm$breaks)
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}
\references{
  See citation("sorvi")
}
\keyword{utilities}

