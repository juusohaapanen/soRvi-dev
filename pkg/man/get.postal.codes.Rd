\name{get.postal.codes}
\alias{get.postal.codes}
\title{Hae Suomen postinumerot}
\description{Hae Suomen postinumerot}
\usage{get.postal.codes(url = "http://fi.wikipedia.org/wiki/Luettelo_Suomen_postinumeroista_kunnittain")}
\details{Hakee Suomen postinumerot wikipediasta.}
\value{data.frame jossa objektit 
  \item{postal.code}{Postal code}
  \item{municipality}{Municipality name}
  \item{municipality.scandless}{Municipality name without special chars.}
}
\references{Viitetiedot, ks. citation("sorvi")}
\author{Juuso Parkkinen \email{sorvi-commits@lists.r-forge.r-project.org}}
%\note{}
%\seealso{}
\examples{
# pnro <- get.postal.codes()
}
\keyword{ utilities }

