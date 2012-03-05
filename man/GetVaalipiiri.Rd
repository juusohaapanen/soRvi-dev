\name{GetVaalipiiri}
\alias{GetVaalipiiri}
\title{Load Vaalipiiri information
Useful for mapping election data to other municipality information}
\usage{
  GetVaalipiiri(url = "http://www.stat.fi/meta/luokitukset/vaalipiiri/001-2012/luokitusavain_kunta.html")
}
\arguments{
  \item{url}{URL for Tilastokeskus vaalipiirit.}
}
\value{
  data.frame listing election regions (Vaalipiiri), region
  IDs (Aluenumero) and municipalities (Alue)
}
\description{
  Load Vaalipiiri information Useful for mapping election
  data to other municipality information
}
\author{
  Juuso Parkkinen and Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}

