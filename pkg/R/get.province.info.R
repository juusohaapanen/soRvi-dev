# This file is a part of the soRvi program
# http://sorvi.r-forge.r-project.org

# Copyright (C) 2011 Leo Lahti <leo.lahti@iki.fi>. All rights reserved.

# This program is open source software; you can redistribute it and/or
# modify it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


#' Get information of Finnish provinces.
#'
#' @param url URL of the Wikipedia source 
#' @return A data frame. With the following entries: Maakunta: province; Pinta-ala: area; Vakiluku: population; Vaestotiheys: population density
#' @export 
#' @callGraphPrimitives
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # tmp <- get.province.info("http://fi.wikipedia.org/wiki/V%C3%A4est%C3%B6tiheys")
#' @keywords utilities

get.province.info <- function (url = "http://fi.wikipedia.org/wiki/V%C3%A4est%C3%B6tiheys") {

  require(XML)

  # Read tables from the website
  tables <- readHTMLTable(url)

  # Maakuntien vaestotiheystaulukko
  tab <- tables[[1]]		

  tab$Maakunta <- iconv(tab$Maakunta, "latin-1", "UTF-8")

  names(tab) <- c("Maakunta", "Pinta-ala", "Vakiluku", "Vaestotiheys")

  tab

}

#' Get information of Finnish municipalities.
#'
#' @param url URL of the Wikipedia source 
#' @return A data frame with municipality data
#' @export 
#' @callGraphPrimitives
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@lists.r-forge.r-project.org}
#' @examples # tmp <- get.municipality.info("http://fi.wikipedia.org/wiki/V%C3%A4est%C3%B6tiheys")
#' @keywords utilities


get.municipality.info <- function (url = "http://www.sral.fi/kilpailut/kunnatjamaakunnat.html") {
  # Mapping between municipality-province (kunta-maakunta)
  temp <- readHTMLTable(url)
  kunnat.maakunnat <- temp[[7]]
  # maakunnat.kunnat <- temp[[8]]

  info <- kunnat.maakunnat[-1,]
  colnames(info) <- as.character(unlist(kunnat.maakunnat[1,]))
  rownames(info) <- info$Kunta

  info

}





#' Get information of Finnish municipalities.
#'
#' @param municipality.list NULL 
#' @param municipality.info NULL 
#' @return Mapping vector listing the province for each municipality in Finland.
#' @export 
#' @callGraphPrimitives
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@lists.r-forge.r-project.org}
#' @examples 
#' # municipality.info <- get.municipality.info() 
#' # my.municipalities <- as.character(municipality.info$Kunta) # list municipalities of interest
#' # m2p <- municipality2province(my.municipalities, municipality.info) # mapping between municipalities (kunta) and provinces (maakunta)
#' @keywords utilities

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


#kunta2maakunta <- function (kunnat, map) {
#
#  # Mappaa kunnat maakuntiin
#  # map <- aluetaulukko.suomi()
#  # FIXME: hoitele skandit iconv-funktiolla
#  v <- korvaa.skandit(map$maakunta[match(kunnat, map$kunta)])
#  names(v) <- kunnat#
#
#  v
#}


#aluetaulukko.suomi <- function () {#
#
#  # FIXME: korvaa MML:n datoilla
#  # Hae mappays kunta-maakunta-laani
#  url.gadm <- "http://gadm.org/data/rda/" # URL for GADM R data
#  con <- url(paste(url.gadm, "FIN_adm", 4, ".RData", sep=""))
#  print(load(con))
#  close(con)#
#
#  data.frame(list(laani = gadm$NAME_1, maakunta = gadm$NAME_2, kunta = gadm$NAM#E_4))
#
#}

