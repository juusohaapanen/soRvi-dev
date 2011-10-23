\name{get.staticmap.GoogleMaps}
\alias{get.staticmap.GoogleMaps}
\title{Hae karttapohja GoogleMapsista}
\description{Hae kattapohja GoogleMapsista}
\usage{get.staticmap.GoogleMaps(center, zoom = 10, GRAYSCALE=FALSE, scale=2, maptype = 'map', destfile = 'TemporaryMap.png', n_pix = 640, format="png32")}
\arguments{
  \item{center}{Kartan keskipisteen koordinaatit}
  \item{zoom}{Zoom-taso}
  \item{GRAYSCALE}{Harmaasavy vai varit?}
  \item{scale}{Kartan tarkkuusskaala, 1: epatarkempi mutta nopeampi ladata, 2: tarkempi mutta huomattavasti hitaampi ladata}
  \item{maptype}{Karttatyyppi}
  \item{destfile}{Valiaikainen kuvatiedosto}
  \item{n_pix}{Kuvan koko (maksimi = 640)}
  \item{format}{Kuvan formaatti (png32 on paras)}
}
\value{
  \item{map}{Kartta}
}
\examples{
# center <- c(lon=24.90, lat = 60.20)
# hr.map <- get.staticmap.GoogleMaps(center = center, zoom = 10, GRAYSCALE=TRUE, maptype="map", scale=1)

}
\keyword{ utilities }



