\name{korvaa.skandit}
\alias{korvaa.skandit}
\title{Replace special characters with standard ones.}
\usage{
  korvaa.skandit(s)
}
\arguments{
  \item{s}{string from which the special chars should be
  removed}
}
\value{
  string with special chars replaced by standard ones
}
\description{
  Replace special characters with standard ones.
}
\note{
  iconv function provides better tools for these purposes
  and is now the main tool This function is kept for
  compatibility with the older versions.
}
\examples{
# korvaa.skandit("my.string.here") # if no, special chars, the same string is returned
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}
\references{
  See citation("sorvi")
}
\keyword{utilities}

