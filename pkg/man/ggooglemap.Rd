\name{ggooglemap}
\alias{ggooglemap}
\title{Karttadatan lataus GoogleMapsista}
\description{Karttadatan lataus GoogleMapsista}
\usage{
ggooglemap(location, center, API = "", type = c("color", "bw")[1], rgbcoefs = c(0, 1, 0), zoom = 10, maptype = "map", destfile = "TemporaryMap.jpg", n_pix = 640)
}
\arguments{
  \item{location}{Sijainti.}
  \item{center}{Kartan keskusta.}
  \item{API}{GoogleMaps API-koodi.}
  \item{type}{Kuvan tietoja.}
  \item{rgbcoefs}{RGB-kertoimet.}
  \item{zoom}{Zoomaustaso.}
  \item{maptype}{Kartan tyyppi.}
  \item{destfile}{Tallennustiedosto karttakuvalle.}
  \item{n_pix}{Pikselien maara.}
}
%\details{}
\value{data.frame}
\references{Viitetiedot, ks. citation("sorvi")}
\author{Juuso Parkkinen \email{sorvi-commits@lists.r-forge.r-project.org}}
%\note{}
%\seealso{}
\examples{
cat("TBA\n")
}
\keyword{ utilities }

