\name{Presidentti2012RateChoices}
\alias{Presidentti2012RateChoices}
\title{For Presidentti2012 answers, form numerical rating (in integers) for the
answer options (rougly corresponding to the index on conservative-liberal axis)}
\usage{
  Presidentti2012RateChoices(questions)
}
\arguments{
  \item{questions}{questions returned by
  GetPresidentti2012(category="questions", API=API)}
}
\value{
  list A list: each element corresponds to a question. For
  each question, the answer options are given an index,
  roughly corresponding to their position on
  conservative-liberal axis
}
\description{
  Load data from Presidentti2012 vaalikone Note! You need a
  personal API key to get the data!
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}

