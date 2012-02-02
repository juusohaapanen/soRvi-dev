\name{CenterData}
\alias{CenterData}
\title{Shift the data matrix column means to a specified value}
\usage{
  CenterData(X, rm.na = TRUE, meanvalue = 0)
}
\arguments{
  \item{X}{data matrix}

  \item{rm.na}{logical: remove NAs}

  \item{meanvalue}{mean for columns (by default 0)}
}
\value{
  shifted data matrix
}
\description{
  Shift the data matrix column means to a specified value
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}

