\name{Presidentti2012GetUserData}
\alias{Presidentti2012GetUserData}
\title{Get user answer data for HS vaalikone 2012}
\usage{
  Presidentti2012GetUserData(dates, API, per.page = 10000)
}
\arguments{
  \item{dates}{dates for which to retrieve data, for
  instance: c(paste("2011-11", 23:30, sep="-"),
  paste("2011-12", 1:31, sep="-"))}

  \item{API}{API key}

  \item{per.page}{maximum number of results to retrieve at
  one query in the for loop}
}
\value{
  dat.list list containing user answer data for the
  specified dates
}
\description{
  Get user answer data for HS vaalikone 2012
}
\note{
  A wrapper
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}

