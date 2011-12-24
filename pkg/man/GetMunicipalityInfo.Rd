\name{GetMunicipalityInfo}
\alias{get.municipality.info}
\alias{GetMunicipalityInfo}
\title{Get information of Finnish municipalities.}
\usage{
  GetMunicipalityInfo(url = "http://www.sral.fi/kilpailut/kunnatjamaakunnat.html")
}
\arguments{
  \item{url}{URL of the Wikipedia source}
}
\value{
  A data frame with municipality data
}
\description{
  Get information of Finnish municipalities.
}
\examples{
# tmp <- GetMunicipalityInfo("http://fi.wikipedia.org/wiki/V\%C3\%A4est\%C3\%B6tiheys")
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}
\references{
  See citation("sorvi")
}
\keyword{utilities}

