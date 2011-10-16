hae.maakuntatiedot <- function (url) {

  require(XML)

  # Read tables from the website
  tables <- readHTMLTable(url)

  # Maakuntien vaestotiheystaulukko
  tab <- tables[[1]]		

  names(tab) <- c("Maakunta", "Pinta-ala", "Vakiluku", "Vaestotiheys")

  tab

}