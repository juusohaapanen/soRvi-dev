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
#' @aliases get.province.info
#' @param url URL of the Wikipedia source 
#' @return A data frame. With the following entries: Maakunta: province; Pinta-ala: area; Vakiluku: population; Vaestotiheys: population density
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # tmp <- GetProvinceInfo("http://fi.wikipedia.org/wiki/V%C3%A4est%C3%B6tiheys")
#' @keywords utilities

GetProvinceInfo <- function (url = "http://fi.wikipedia.org/wiki/V%C3%A4est%C3%B6tiheys") {

  # Was: get.province.info		  

  require(XML)

  # Read tables from the website
  tables <- readHTMLTable(url)

  # Population density in regions (maakunnat)
  tab <- tables[[1]]		

  tab$Maakunta <- iconv(tab$Maakunta, "latin-1", "UTF-8")

  names(tab) <- c("Maakunta", "Pinta-ala", "Vakiluku", "Vaestotiheys")

  tab

}

#' Get information of Finnish municipalities.
#'
#' @aliases get.municipality.info
#' @param url URL of the Wikipedia source 
#' @return A data frame with municipality data
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # tmp <- GetMunicipalityInfo("http://fi.wikipedia.org/wiki/V%C3%A4est%C3%B6tiheys")
#' @keywords utilities

GetMunicipalityInfo <- function (url = "http://www.sral.fi/kilpailut/kunnatjamaakunnat.html") {

  # Mapping between municipalities and provinces (kunta - maakunta)
  temp <- readHTMLTable(url)
  kunnat.maakunnat <- temp[[7]]
  # maakunnat.kunnat <- temp[[8]]

  info <- kunnat.maakunnat[-1,]
  colnames(info) <- as.character(unlist(kunnat.maakunnat[1,]))
  rownames(info) <- info$Kunta

  info

}


#' Get information of Finnish municipalities.
#' @aliases municipality2province
#' @param municipality.list NULL 
#' @param municipality.info NULL 
#' @return Mapping vector listing the province for each municipality in Finland.
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples 
#' # municipality.info <- GetMunicipalityInfo() 
#' # my.municipalities <- as.character(municipality.info$Kunta) # list municipalities of interest
#' # m2p <- municipality2province(my.municipalities, municipality.info) # mapping between municipalities (kunta) and provinces (maakunta)
#' @keywords utilities

FindProvince <- function (municipality.list = NULL, municipality.info = NULL) {

  # municipality.info <- get.municipality.info()

  if (is.null(municipality.info)) { 
    municipality.info <- GetMunicipalityInfo()
  }

  m2p <- as.character(municipality.info$Maakunta)
  names(m2p) <- as.character(municipality.info$Kunta)

  if (!is.null(municipality.list)) {
    m2p <- m2p[municipality.list]
  }

  m2p

}

