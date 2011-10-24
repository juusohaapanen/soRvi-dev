\name{get.geocode.GoogleMaps}
\alias{get.geocode.GoogleMaps}
\title{Hae geokoodit GoogleMapsista}
\description{Hakee geokoodit GoogleMaps API:sta: http://code.google.com/apis/maps/documentation/staticmaps/index.html. HUOM! Vain 2500 hakua vuorokaudessa ja geokoodeja saa kayttaa vain GoogleMapsin kanssa: http://code.google.com/apis/maps/terms.html.}
\usage{get.geocode.GoogleMaps(str)}
\arguments{
  \item{str}{Katuosoite, esim. "Mannerheimintie, 00100, FI"}
}
\value{
  \item{geocode}{Geokoodi/koordinaatit vektorina (lat, lon)}
}
\author{Juuso Parkkinen \email{sorvi-commits@lists.r-forge.r-project.org}}
\examples{
# temp <- get.geocode.GoogleMaps("Mannerheimintie, 00100, FI")
}
\keyword{ utilities }



