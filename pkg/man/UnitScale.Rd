\name{UnitScale}
\alias{UnitScale}
\title{Scale data matrix columns to unit variance}
\usage{
  UnitScale(X, rm.na = TRUE, sd.value = NULL)
}
\arguments{
  \item{X}{data matrix}

  \item{rm.na}{logical: remove NAs}

  \item{sd.value}{standard deviation for columns (by
  default 1)}
}
\value{
  scaled data matrix
}
\description{
  Scale data matrix columns to unit variance
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}

