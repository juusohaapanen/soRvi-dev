\name{hae.vaestorekisteri}
\alias{hae.vaestorekisteri}
\title{Suomen vaestorekisterin haku}
\description{Hakee Suomen kuntakohtaiset asutustiedot Vaestorekisterikeskuksen verkkosivulta}
\usage{hae.vaestorekisteri(url = "http://vrk.fi/default.aspx?docid=5127&site=3&id=0")}
\arguments{
  \item{url}{Vaestorekisterikeskuksen verkkosivujen URL.}
}
\details{Hakee taulukon ruudunraaputusmenetelmalla, eli parsimalla taulukon irti sivun lahdekoodista.}
\value{data.frame, joka sisaltaa muuttujat (lisaksi rivit on nimetty suomenkielisilla kunnan nimilla ilman skandeja): 
  \item{Koodi}{Kunnan tunnistekoodi}    
  \item{Kunta}{Kunnan suomenkielinen nimi}    
  \item{Kommun}{Kunnan ruotsinkielinen nimi}    
  \item{Miehet}{Miesten lukumaara}    
  \item{Naiset}{Naisten lukumaara}    
  \item{Yhteensa}{Ihmisten lukumaara}    
}
\references{Ks. citation("sorvi")}
\author{Leo Lahti \email{sorvi-commits@lists.r-forge.r-project.org}}
\note{Poistaa skandit data.framen rivinimista, jotta sujuva paritus muihin tietolahteisiin helpottuu.}
\examples{
# vrek <- hae.vaestorekisteri(url = "http://vrk.fi/default.aspx?docid=5127&site=3&id=0")
}
\keyword{ utilities }

