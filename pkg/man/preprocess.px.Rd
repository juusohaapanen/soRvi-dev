\name{preprocess.px}
\alias{preprocess.px}
\title{Preprocess PC Axis data.}
\usage{
  preprocess.px(px)
}
\arguments{
  \item{px}{PC Axis object.}
}
\value{
  PC Axis object.
}
\description{
  Preprocess PC Axis data.
}
\examples{
#
# px <- read.px("http://pxweb2.stat.fi/database/StatFin/vrm/synt/080_synt_tau_203_fi.px")
# px.polished <- preprocess.px(as.data.frame(px))
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}
\references{
  See citation("sorvi")
}
\keyword{utilities}

