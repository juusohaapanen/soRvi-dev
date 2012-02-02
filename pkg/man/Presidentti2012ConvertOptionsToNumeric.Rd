\name{Presidentti2012ConvertOptionsToNumeric}
\alias{Presidentti2012ConvertOptionsToNumeric}
\title{For Presidentti2012 answers, form numerical rating in [0, 1] for the
answer options (rougly corresponding to conservative-liberal axis)}
\usage{
  Presidentti2012ConvertOptionsToNumeric(df, questions,
    type = "rate")
}
\arguments{
  \item{df}{data.frame giving the merged table of subjects
  vs. background information + answers The example script
  for obtaining this was posted to Louhos.
  http://louhos.wordpress.com/2012/01/06/kenesta-seuraava-presidentti-ennusta-itse-hsn-vaalikonedatan-avulla/}

  \item{questions}{questions returned by
  GetPresidentti2012(category="questions", API=API)}

  \item{type}{return the answer rating as integers
  ("integer") 0, 1, 2, ...  or as rates between [0,1].}
}
\value{
  list A list with two data.frames: info (user information)
  and answer (user answers). The answer options are rated
  within [0, 1].  Each row corresponds to a user in each of
  the two data.frames.
}
\description{
  For Presidentti2012 answers, form numerical rating in [0,
  1] for the answer options (rougly corresponding to
  conservative-liberal axis)
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}

