\name{GetCapabilities}
\alias{GetCapabilities}
\title{Get WMS capabilities (meta data)}
\usage{
  GetCapabilities(url)
}
\arguments{
  \item{url}{a string url to target WMS}
}
\value{
  XMLRootNode
}
\description{
  Function intiates a GetCapabilities query to a specified
  URL and returns the response. Function will trigger an
  error if the response cannot be parsed and/or is not
  valid.
}
\note{
  Meant for package internal use only.
}
\author{
  Joona Lehtomaki \email{joona.lehtomaki@gmail.org}
}

