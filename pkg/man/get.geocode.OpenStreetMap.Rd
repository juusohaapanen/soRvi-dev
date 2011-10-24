\name{get.geocode.OpenStreetMap}
\alias{get.geocode.OpenStreetMap}
\title{Hae geokoodit OpenStreetMapsista}
\description{Hakee geokoodit OpenStreetMaps Nominatim-palvelusta: http://wiki.openstreetmap.org/wiki/Nominatim. HUOM! Kaytossa rajoituksia, mm. 1 kysely per sekunti: http://wiki.openstreetmap.org/wiki/Nominatim_usage_policy.}
\usage{get.geocode.OpenStreetMap(query)}
\arguments{
  \item{query}{Osoite, esim. "Mannerheimintie+100,Helsinki", tai kohteen nimi, esim. "Eduskuntatalo"}
}
\value{
  \item{geocode}{Geokoodi/koordinaatit vektorina (lat, lon)}
}
\author{Juuso Parkkinen \email{sorvi-commits@lists.r-forge.r-project.org}}
\examples{
# temp <- get.geocode.OpenStreetMap("Eduskuntatalo")
}
\keyword{ utilities }



