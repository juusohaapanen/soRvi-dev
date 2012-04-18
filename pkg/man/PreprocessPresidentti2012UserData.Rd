\name{PreprocessPresidentti2012UserData}
\alias{PreprocessPresidentti2012UserData}
\title{Preprocess user answer data for HS vaalikone 2012}
\usage{
  PreprocessPresidentti2012UserData(dat.list, API = API)
}
\arguments{
  \item{dat.list}{Output from: dat.list <-
  Presidentti2012GetUserData(dates, API, per.page = 10000)}

  \item{API}{API key}
}
\value{
  data.frame with user answer data
}
\description{
  Preprocess user answer data for HS vaalikone 2012
}
\note{
  A wrapper
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}

