\name{get.maakuntatiedot}
\alias{get.maakuntatiedot}
\title{Maakuntien vaestontiheys}
\description{Maakuntien vaestontiheys}
\usage{get.maakuntatiedot(url)}
\arguments{
  \item{url}{Wikipedian url, josta tiedot haetaan}
}
\value{
  \item{Maakunta}{Maakunta}
  \item{Pinta-ala}{Maakunnan pinta-ala}
  \item{Vakiluku}{Maakunnan vakiluku}
  \item{Vaestotiheys}{Maakunnan vaestotiheys}
}
\examples{
# tmp <- get.maakuntatiedot("http://fi.wikipedia.org/wiki/V%C3%A4est%C3%B6tiheys")
}
\keyword{ utilities }



