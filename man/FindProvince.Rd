\name{FindProvince}
\alias{FindProvince}
\alias{municipality2province}
\title{List province for each municipality in Finland.}
\usage{
  FindProvince(municipalities = NULL,
    municipality.info = NULL)
}
\arguments{
  \item{municipalities}{NULL}

  \item{municipality.info}{NULL}
}
\value{
  Mapping vector listing the province for each municipality
  in Finland.
}
\description{
  List province for each municipality in Finland.
}
\examples{
# Info table for municipalities:
# municipality.info <- GetMunicipalityInfo()
# List all municipalities:
# all.municipalities <- as.character(municipality.info$Kunta)
# Pick province for given municipalities:
# m2p <- FindProvince(c("Helsinki", "Tampere", "Turku")) # mapping between municipalities (kunta) and provinces (maakunta)
# Speed up by providing predefined table of municipality info:
# m2p <- FindProvince(c("Helsinki", "Tampere", "Turku"), municipality.info)
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}
\references{
  See citation("sorvi")
}
\keyword{utilities}

