\name{Presidentti2012RateChoices}
\alias{Presidentti2012RateChoices}
\title{For Presidentti2012 answers, form numerical rating (in integers) for the
answer options (rougly corresponding to the index on conservative-liberal axis)}
\usage{
  Presidentti2012RateChoices(questions, type = "rate")
}
\arguments{
  \item{questions}{questions returned by
  GetPresidentti2012(category="questions", API=API)}

  \item{type}{return the answer rating as integers
  ("integer") 0, 1, 2, ...  or as rates between [0,1].}
}
\value{
  list A list: each element corresponds to a question. For
  each question, the answer options are given an index,
  roughly corresponding to their position on
  conservative-liberal axis
}
\description{
  For Presidentti2012 answers, form numerical rating (in
  integers) for the answer options (rougly corresponding to
  the index on conservative-liberal axis)
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}

