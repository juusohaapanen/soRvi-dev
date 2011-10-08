\name{gadm.plot}
\alias{gadm.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
gadm-muotoisen karttadatan piirto
}
\description{
Varikuva gadm-muotoisen datan kartta-alueista.
}
\usage{
gadm.plot(url = "", taso = 2, main = "", col = NULL)
}

\arguments{
  \item{url}{Kartan osoite (ilman tasoa). Koko URL on muotoa "http://gadm.org/data/rda/FIN_adm2.RData"; gadm.url-muuttujaan sijoitetaan tasta vain alkuosa ilman tasoa ja paatetta, eli "http://gadm.org/data/rda/FIN_adm2". Aluekohtaisia osoitteita loytyy gadm:in verkkosivuilta http://gadm.org/}
  \item{taso}{Alueiden tarkkuusaste}
  \item{main}{Otsikko}
  \item{col}{Karttayksikoiden varit}
}
\details{
Karttadataa osoitteessa: http://gadm.org/data/rda
}
\value{Kaytetaan kuvan piirtoon.}
\references{Ks. citation("sorvi")}
\author{Leo Lahti \email{leo.lahti@iki.fi}}
%\note{}
%\seealso{}
\examples{
# Suomen kunnat
# gadm.plot(url = "http://gadm.org/data/rda/FIN_adm", taso = 2)
}
\keyword{ utilities }

