\name{PreprocessShapeMML}
\alias{Arguments:}
\alias{PreprocessShapeMML}
\alias{preprocess.shape.mml}
\title{Preprocessing function for MML data}
\usage{
  PreprocessShapeMML(sp)
}
\arguments{
  \item{sp}{Shape object (SpatialPolygonsDataFrame)

  Returns:}
}
\value{
  Shape object (from SpatialPolygonsDataFrame class)
}
\description{
  This script can be used to preprocess shape data obtained
  from Finnish geographical agency (Maanmittauslaitos, MML)
  The data copyright is on (C) MML 2011.
}
\details{
  The various Finland shape data files obtained from
  http://www.maanmittauslaitos.fi/aineistot-palvelut/digitaaliset-tuotteet/ilmaiset-aineistot/hankinta
  have been preprocessed with this function, and the
  preprocessed versions are included in soRvi package. Load
  the readily preprocessed data for use by typing in R:
  'data(MML)'. Alternatively, one can download the files
  from the above URL and apply this function.
}
\examples{
# data(MML); sp <- MML[[1]][[1]]; sp2 <- PreprocessShapeMML(sp)
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}
\references{
  See citation("sorvi")
}
\keyword{utilities}

