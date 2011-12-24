\name{FindProvince}
\alias{FindProvince}
\alias{municipality2province}
\title{Get information of Finnish municipalities.}
\usage{
  FindProvince(municipality.list = NULL,
    municipality.info = NULL)
}
\arguments{
  \item{municipality.list}{NULL}

  \item{municipality.info}{NULL}
}
\value{
  Mapping vector listing the province for each municipality
  in Finland.
}
\description{
  Get information of Finnish municipalities.
}
\examples{
# municipality.info <- GetMunicipalityInfo()
# my.municipalities <- as.character(municipality.info$Kunta) # list municipalities of interest
# m2p <- municipality2province(my.municipalities, municipality.info) # mapping between municipalities (kunta) and provinces (maakunta)
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}
\references{
  See citation("sorvi")
}
\keyword{utilities}

