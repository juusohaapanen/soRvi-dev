\name{getPresidentti2012Data}
\alias{getPresidentti2012Data}
\title{Load Presidentti2012 data}
\usage{
  getPresidentti2012Data(category = c("questions", "candidates", "useranswers"),
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

  \item{pagePagenumber}{of results to get, optional}

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
  Juuso Parkkinen \email{juuso.parkkinen@gmail.org}
}

