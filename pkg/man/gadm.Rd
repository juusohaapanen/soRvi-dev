\name{get.gadm}
\alias{get.gadm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{GADM-muotoisen karttadatan haku}
\description{GADM-muotoisen karttadatan haku.}
\usage{
get.gadm(alue = "FIN_adm", taso = 4)
}
\arguments{
  \item{alue}{Kartan tunnus. Kun koko GADM-URL on esim. muotoa "http://gadm.org/data/rda/FIN_adm2.RData", alue-muuttujaan sijoitetaan osa "FIN_adm". Aluekohtaisia osoitteita loytyy gadm:in verkkosivuilta http://gadm.org/}
  \item{taso}{Alueiden tarkkuusaste. Kun koko GADM-URL on esim. muotoa "http://gadm.org/data/rda/FIN_adm4.RData", taso-muutttuja on "4".}
}
\details{GADM-muotoisia karttadatoja voi selata osoitteessa: http://gadm.org/}
\value{GADM-muuttuja.}
\references{Ks. citation("sorvi")}
\author{Leo Lahti \email{sorvi-commits@lists.r-forge.r-project.org}}
\note{Suomen osalta kuntatiedot (FIN_adm4) nayttaa olevan
paivittamatta uusimpaan. Suositellaan MML:n karttoja, ks. help(MML).}
%\seealso{}
\examples{
# Suomen kunnat
# gadm <- get.gadm(alue = "FIN_adm", taso = 4)
}
\keyword{ utilities }

