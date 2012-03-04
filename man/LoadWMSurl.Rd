\name{LoadWMSurl}
\alias{LoadWMSurl}
\title{Get the URL associated with one of the standard WMSs.}
\usage{
  LoadWMSurl(provider, service)
}
\arguments{
  \item{provider}{string describing the provider}

  \item{service}{string describing the WMS service
  requested}
}
\value{
  string URL
}
\description{
  Standard WMSs are a group of well-known WMSs provided as
  part of soRvi. All URLs have a provider (organization
  etc.) and a wmsname.
}
\author{
  Joona Lehtomaki \email{joona.lehtomaki@gmail.org}
}
\seealso{
  listWMSurls
}

