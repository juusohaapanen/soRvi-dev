\name{GetPopulationRegister}
\alias{GetPopulationRegister}
\title{Retrieve population register data}
\usage{
  GetPopulationRegister(url = "http://vrk.fi/default.aspx?docid=5127&site=3&id=0")
}
\arguments{
  \item{url}{String specifying the URL containing the
  population register data.}
}
\value{
  data.frame with municipality information.
}
\description{
  Retrieves municipality data from population register.
  TODO: merge with get.municipality.info function The url
  should be changed when there are updates:
}
\examples{
# df <- GetPopulationRegister()
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}
\references{
  See citation("sorvi")
}
\keyword{utilities}

