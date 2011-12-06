\name{get.postal.codes}
\alias{get.postal.codes}
\title{Hae Suomen postinumerot}
\description{Hae Suomen postinumerot}
\usage{get.postal.codes()}
\details{Hakee Suomen postinumerot wikipediasta.}
\value{data.frame jossa objektit 
  \item{postinumero}{Postinumero}
  \item{kunta}{Kunnan nimi}
  \item{kunta.skanditon}{Kunnan nimi ilman skandeja}
}
\references{Viitetiedot, ks. citation("sorvi")}
\author{Juuso Parkkinen \email{sorvi-commits@lists.r-forge.r-project.org}}
%\note{}
%\seealso{}
\examples{
# pnro <- get.postal.codes()
}
\keyword{ utilities }

