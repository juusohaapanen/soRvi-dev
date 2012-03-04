\name{Presidentti2012CandidateAnswers2Numeric}
\alias{Presidentti2012CandidateAnswers2Numeric}
\title{For Presidentti2012 candidate answers, form numerical rating in [0, 1] for the
answer options (rougly corresponding to conservative-liberal axis)}
\usage{
  Presidentti2012CandidateAnswers2Numeric(candidates,
    questions, type = "rate")
}
\arguments{
  \item{candidates}{candidate information as given by
  candidates <- GetPresidentti2012(category="candidates",
  API=API)}

  \item{questions}{questions returned by
  GetPresidentti2012(category="questions", API=API)}

  \item{type}{return the answer rating as integers
  ("integer") 0, 1, 2, ...  or as rates between [0,1].}
}
\value{
  matrix A matrix: each row corresponds to a candidate. For
  each candidate, the answer options (columns) are rated
  within [0, 1]
}
\description{
  For Presidentti2012 candidate answers, form numerical
  rating in [0, 1] for the answer options (rougly
  corresponding to conservative-liberal axis)
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}

