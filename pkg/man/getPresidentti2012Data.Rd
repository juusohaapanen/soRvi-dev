\name{getPresidentti2012Data}
\alias{getPresidentti2012Data}
\title{Hae Presidentti2012-vaalikoneen datoja}
\description{Hae Presidentti2012-vaalikoneen datoja. Haettavissa on vaalikoneen kysymykset, ehdokkaiden vastaukset seka aanestajien syottamat vastaukset. Lisaa dataa tulee koko ajan. Dokumentaatio: http://www2.hs.fi/extrat/hsnext/Vaalikone_API_20111207.pdf.}
\usage{getPresidentti2012Data(category=c("questions", "candidates", "useranswers"), API, ID=NULL, filter=NULL, page=1, per_page=500, show_total="true")}
\arguments{
  \item{category}{Datakategoria ("questions", "candidates", "useranswers")}
  \item{API}{Pakollinen api-avain, pyyda Esa Makiselta, at hs.fi}
  \item{ID}{Vapaaehtoinen, kysymyksen tai ehdokkaan id}
  \item{filter}{Pakollinen hakukriteeri datakategorialle 'useranswers', ("question", "timerange", "topcandidate")}
  \item{page}{Vapaaehtoinen, tulosten sivunumero}
  \item{per_page}{Vapaaehtoinen, vastauksien maara sivulla (500-10000)}
  \item{show_total}{Vapaaehtoinen, true/false, anna tuloksessa hakukriteeria vastaavien vastausten kokonaismaara ja sivujen kokonaismaara}
}
\value{
  List of data.
}
\examples{
# tmp <- getPresidentti2012Data(category="useranswers", API=API, filter="question:57:295", page=1, per_page=500, show_total="true")
}
\keyword{ utilities }



