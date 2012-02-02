\name{Presidentti2012RetrieveAnswerText}
\alias{Presidentti2012RetrieveAnswerText}
\title{For Presidentti2012 answers, get answer IDs, text and rating
for the given question ID.}
\usage{
  Presidentti2012RetrieveAnswerText(question.id, questions)
}
\arguments{
  \item{question.id}{Question ID as in HS vaalikone (eg.
  numerical 80), or in soRvi e.g. character "Q80")}

  \item{questions}{questions returned by
  GetPresidentti2012(category="questions", API=API)}
}
\value{
  list A list with the fields question, answer id, answer
  text and answer rate for the given question.
}
\description{
  For Presidentti2012 answers, get answer IDs, text and
  rating for the given question ID.
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}

