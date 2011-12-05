get.province.info <- function (url) {

  require(XML)

  # Read tables from the website
  tables <- readHTMLTable("Http://fi.wikipedia.org/wiki/V%C3%A4est%C3%B6tiheys")

  # Maakuntien vaestotiheystaulukko
  tab <- tables[[1]]		

  tab$Maakunta <- iconv(tab$Maakunta, "latin-1", "UTF-8")

  names(tab) <- c("Maakunta", "Pinta-ala", "Vakiluku", "Vaestotiheys")

  tab

}


get.municipality.info <- function () {
  # Mapping between municipality-province (kunta-maakunta)
  temp <- readHTMLTable("http://www.sral.fi/kilpailut/kunnatjamaakunnat.html")
  kunnat.maakunnat <- temp[[7]]
  # maakunnat.kunnat <- temp[[8]]

  info <- kunnat.maakunnat[-1,]
  colnames(info) <- as.character(unlist(kunnat.maakunnat[1,]))
  rownames(info) <- info$Kunta

  info

}

municipality2province <- function (municipality.list = NULL, municipality.info = NULL) {

  # municipality.info <- get.municipality.info()

  if (is.null(municipality.info)) { 
    municipality.info <- get.municipality.info()
  }

  m2p <- as.character(municipality.info$Maakunta)
  names(m2p) <- as.character(municipality.info$Kunta)

  if (!is.null(municipality.list)) {
    m2p <- m2p[municipality.list]
  }

  m2p

}




# Localetocharset(as.character(tab$Maakunta[[5]]))