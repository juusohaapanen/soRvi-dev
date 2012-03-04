\name{PreprocessWMS}
\alias{PreprocessWMS}
\title{Create a WMS object.}
\usage{
  PreprocessWMS(url, cache = "~")
}
\arguments{
  \item{url}{string describing the URL of the WMS}

  \item{cache}{string file system path controlling where
  the WMS cache is created (not implemented yet!)}
}
\value{
  WMS object corresponding to the provided URL
}
\description{
  WMS object fetches the relevant meta data from the WMS
  server (without actually getting the data). This helper
  function is a thin wrapper to create and return the
  appropriate WMS object.
}
\author{
  Joona Lehtomaki \email{joona.lehtomaki@gmail.org}
}
\seealso{
  listWMSurls
}

