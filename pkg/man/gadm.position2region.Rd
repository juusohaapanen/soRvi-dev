\name{gadm.position2region}
\alias{gadm.position2region}
\title{Map GADM location coordinates to region identifier}
\usage{
  gadm.position2region(x = c(24.9375, 24.0722),
    y = c(60.1783, 61.4639))
}
\arguments{
  \item{x}{X coordinate}

  \item{y}{Y coordinate}
}
\value{
  A data frame with coordinates, region, province, and
  municipality information
}
\description{
  Map GADM location coordinates to region identifier
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}
\references{
  See citation("sorvi")
}
\keyword{utilities}

