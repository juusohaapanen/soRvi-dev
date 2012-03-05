\name{GetPresidentti2012}
\alias{GetPresidentti2012}
\title{Load Presidentti2012 data}
\usage{
  GetPresidentti2012(category = c("questions", "candidates", "useranswers"),
    API, ID = NULL, filter = NULL, page = 1,
    per_page = 500, show_total = "true")
}
\arguments{
  \item{category}{Data category ("questions", "candidates",
  "useranswers")}

  \item{API}{Personal api key, required}

  \item{ID}{id for the query, optional}

  \item{filter}{filter for the query, required for
  'useranswers'-category. ("question", "timerange",
  "topcandidate")}

  \item{page}{Pagenumber of results to get, optional}

  \item{per_page}{Number of answers to get (500-10000),
  optional}

  \item{show_total}{Show data information, optional}
}
\value{
  res List of data
}
\description{
  Load data from Presidentti2012 vaalikone Note! You need a
  personal API key to get the data!
}
\author{
  Juuso Parkkinen
  \email{sorvi-commits@lists.r-forge.r-project.org}
}

