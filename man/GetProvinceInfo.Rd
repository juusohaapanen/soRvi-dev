\name{GetProvinceInfo}
\alias{get.province.info}
\alias{GetProvinceInfo}
\title{Get information of Finnish provinces.}
\usage{
  GetProvinceInfo(url = "http://fi.wikipedia.org/wiki/V\%C3\%A4est\%C3\%B6tiheys")
}
\arguments{
  \item{url}{URL of the Wikipedia source}
}
\value{
  A data frame. With the following entries: Maakunta:
  province; Pinta-ala: area; Vakiluku: population;
  Vaestotiheys: population density
}
\description{
  Get information of Finnish provinces.
}
\examples{
# tab <- GetProvinceInfo()
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}
\references{
  See citation("sorvi")
}
\keyword{utilities}

