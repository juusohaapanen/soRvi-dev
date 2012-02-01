\name{Presidentti2012ConvertOptionsToNumeric}
\alias{Presidentti2012ConvertOptionsToNumeric}
\title{For Presidentti2012 answers, form numerical rating in [0, 1] for the
answer options (rougly corresponding to conservative-liberal axis)}
\usage{
  Presidentti2012ConvertOptionsToNumeric(df, questions)
}
\arguments{
  \item{df}{data.frame giving the merged table of subjects
  vs. background information + answers The example script
  for obtaining this was posted to Louhos.
  http://louhos.wordpress.com/2012/01/06/kenesta-seuraava-presidentti-ennusta-itse-hsn-vaalikonedatan-avulla/}

  \item{questions}{questions returned by
  GetPresidentti2012(category="questions", API=API)}
}
\value{
  list A list: each element corresponds to a question. For
  each question, the answer options are rated within [0, 1]
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

