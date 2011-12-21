\name{get.gadm}
\alias{get.gadm}
\title{Get map data in GADM format}
\usage{
  get.gadm(map = "FIN_adm", resolution = 4)
}
\arguments{
  \item{map}{Map identifier. Kun koko GADM-URL on esim.
  muotoa "http://gadm.org/data/rda/FIN_adm2.RData",
  map-muuttujaan sijoitetaan osa "FIN_adm". Aluekohtaisia
  osoitteita loytyy gadm:in verkkosivuilta
  http://gadm.org/}

  \item{resolution}{integer value of the resolution. Kun
  koko GADM-URL on esim. muotoa
  "http://gadm.org/data/rda/FIN_adm4.RData", taso-muutttuja
  on "4".}
}
\value{
  GADM object
}
\description{
  Get map data in GADM format
}
\note{
  Suomen osalta kuntatiedot (FIN_adm4) nayttaa olevan
  paivittamatta uusimpaan. Suositellaan MML:n karttoja, ks.
  help(MML).
}
\examples{
# Suomen kunnat: gadm <- get.gadm(map = "FIN_adm", resolution = 4)
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}
\references{
  See citation("sorvi")
}
\keyword{utilities}

