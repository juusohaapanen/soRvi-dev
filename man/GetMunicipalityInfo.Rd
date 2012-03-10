\name{GetMunicipalityInfo}
\alias{GetMunicipalityInfo}
\alias{get.municipality.info}
\title{Get information of Finnish municipalities from Statistics Finland 2012
and Maanmittauslaitos (C) MML 2011. For details of MML data, see
help(GetShapeMML).}
\usage{
  GetMunicipalityInfo(url = "http://pxweb2.stat.fi/Database/Kuntien\%20perustiedot/Kuntien\%20perustiedot/Kuntaportaali.px")
}
\arguments{
  \item{url}{URL for Tilastokeskus municipality
  information}
}
\value{
  A data frame with municipality data
}
\description{
  Get information of Finnish municipalities from Statistics
  Finland 2012 and Maanmittauslaitos (C) MML 2011. For
  details of MML data, see help(GetShapeMML).
}
\examples{
# tmp <- GetMunicipalityInfo()
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}
\references{
  See citation("sorvi")
}
\keyword{utilities}

