\name{GetShapeMML}
\alias{GetShapeMML}
\title{Shows how the MML Shape files have been converted into
the Rdata files included in soRvi package (load using data(MML)).}
\usage{
  GetShapeMML(input.data.dir = "./", verbose = TRUE)
}
\arguments{
  \item{input.data.dir}{Directory path where the original
  data can be accessed.}

  \item{verbose}{Print intermediate processing information

  Returns:}
}
\value{
  Returns a list of preprocessed shape files containing the
  public MML data sets.
}
\description{
  The various Finland shape data files obtained from
  http://www.maanmittauslaitos.fi/aineistot-palvelut/digitaaliset-tuotteet/ilmaiset-aineistot/hankinta
  have been preprocessed using this script, and the
  preprocessed versions are made available in soRvi
  package. Load the readily preprocessed data for use by
  typing in R: 'data(MML)'. Procedure for obtaining the
  data(MML) in soRvi package: 1) Download the MML shape
  files zip archives 1_milj_Shape_etrs_shape.zip and
  4_5_milj_shape_etrs-tm35fin.zip from
  http://www.maanmittauslaitos.fi/aineistot-palvelut/digitaaliset-tuotteet/ilmaiset-aineistot/hankinta
  and place them in temporary data directory
  (input.data.dir) 2) Run: GetShapeMML(input.data.dir) 3)
  Store the preprocessed data in the specified output file:
  save(MML, file = "MML.rda") 4) Compress the output file
  size require(tools) res <- resaveRdaFiles(MML.rda) 5)
  Store the data in soRvi data directory: pkg/data/MML.rda
}
\details{
  Arguments:
}
\examples{
# MML <- GetShapeMML(data.dir = "./")
# save(MML, file = "MML.rda")
# require(tools)
# res <- resaveRdaFiles(MML.rda)
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}
\references{
  See citation("sorvi")
}
\keyword{internal}

