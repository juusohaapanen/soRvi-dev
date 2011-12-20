\name{get.postal.codes}
\alias{get.postal.codes}
\title{Get Finnish postal codes vs. municipalities table from Wikipedia.}
\usage{
  get.postal.codes(url = "http://fi.wikipedia.org/wiki/Luettelo_Suomen_postinumeroista_kunnittain")
}
\arguments{
  \item{url}{URL for the HTML site where the postal codes
  are parsed.}
}
\value{
  A data frame with the following fields: postal.code:
  postal code; municipality: Name of the municipality
  (kunnan nimi); municipality.scandless: Municpality name
  without special chars.
}
\description{
  Get Finnish postal codes vs. municipalities table from
  Wikipedia.
}
\examples{
#postal.code.table <- get.postal.codes()
}
\author{
  Juuso Parkkinen and Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}
\references{
  See citation("sorvi")
}
\keyword{utilities}

