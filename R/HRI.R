# Copyright (C) 2011-2012 Juuso Parkkinen <juuso.parkkinen(at)gmail.com. All rights reserved.

# This program is open source software; you can redistribute it and/or
# modify it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# This file is a part of the soRvi program
# http://sorvi.r-forge.r-project.org

#' Preprocess 'aluejakokartat' from HRI
#'
#' Preprocess 'aluejakokartat' from Helsinki Region Infoshare
#' 
#' @author Juuso Parkkinen \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export
GetHRIaluejakokartat <- function() {
  
  message("Loading aluejakokartat from HRI...")
  # Need to install package rgdal
  # Mac users, see http://www.r-bloggers.com/installing-rgdal-on-mac-os-x-2/
  library(rgdal)
  library(gpclib)
  library(gdata)
  library(ggplot2)
  gpclibPermit()
  
  # Download KML files from http://www.hri.fi/fi/data/paakaupunkiseudun-aluejakokartat/
  # Substitute manually "xsd:sting" in <SimpleField type="xsd:string" name="KOKOTUN"> and other similar fields with "string" to read whole metadata
  pks.pienalue <- rgdal::readOGR(dsn="data/PKS_Kartta_Rajat_KML2011/PKS_pienalue2.kml", layer="pks_pienalue")
  
  pks.pienalue@data$id <- rownames(pks.pienalue@data) # Add IDs
  pks.points <- fortify.SpatialPolygonsDataFrame(pks.pienalue, region="id") # Get point data
  pks.points$group <- sub(".1", "", pks.points$group) # Regex PIEN to joinable format
  
  pks.df <- merge(pks.points, pks.pienalue@data, by.x="group", by.y = "id") # Put everything together
  pks.df <- pks.df[order(pks.df$order),] # sort DF so that polygons come out in the right order
  
  # Fix encoding
  pks.df$Nimi <- factor(iconv(pks.df$Nimi, from="ISO-8859-1", to="UTF-8"))
  pks.df$NIMI_ISO <- factor(iconv(pks.df$NIMI_ISO, from="ISO-8859-1", to="UTF-8"))
  pks.df$Name <- factor(iconv(pks.df$Name, from="ISO-8859-1", to="UTF-8"))
  pks.pienalue@data$Nimi <- factor(iconv(pks.pienalue@data$Nimi, from="ISO-8859-1", to="UTF-8"))
  pks.pienalue@data$NIMI_ISO <- factor(iconv(pks.pienalue@data$NIMI_ISO, from="ISO-8859-1", to="UTF-8"))
  pks.pienalue@data$Name <- factor(iconv(pks.pienalue@data$Name, from="ISO-8859-1", to="UTF-8"))
  
  message("DONE\n")
  return(list(pienalue=pks.pienalue, pienalue.df=pks.df))
}


#' Access Omakaupunki data
#'
#' Access Omakaupunki data through the API. Using the API requireds a password 
#' and an API key. For more details and API documentation see
#' http://blogit.hs.fi/hsnext/omakaupungin-meno-ja-palvelutiedot-avoimena-rajapintana
#' 
#' @param query API query, e.g. "event" or "directory" (services)
#' @param login Personal username (required)
#' @param password Personal password (required)
#' @param api_key Personal API key (required)
#' @param ... Additional parameters to the API (optional). See details from the API documentation
#' The data is licensed under CC BY-NC-SA 3.0. 
#'
#' @return List of results
#'
#' @author Juuso Parkkinen \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export
#' @examples # event.categories <- GetOmakaupunki("event/categories", LOGIN, PASSWORD, API)
GetOmakaupunki <- function(query, login, password, api_key, ...) {
  
  library(RCurl)
  library(rjson)
  
  api.url <- "http://api.omakaupunki.fi/v1/"
  query.url <- paste(api.url, query, sep="")
  curl <- getCurlHandle(cookiefile = "")
  params <- list(login=login, password=password, api_key=api_key, ...)
  val <- getForm(query.url, .params=params, curl=curl, binary=FALSE)
  res <- fromJSON(val)
  return(res)
}

#' Access Paakaupunkiseudun Palvelukartta data
#'
#' Access Paakaupunkiseudun Palvelukartta data from the it's API (version 2). 
#' Using the API is free. For more details and API documentation see
#' http://www.hel.fi/palvelukarttaws/rest/ver2.html.
#' For licensing terms pf the data see http://www.hel2.fi/palvelukartta/REST.html.
#' 
#' @param category API query category, e.g. "service"
#' @param ... Additional parameters to the API (optional). See details from the API documentation
#'
#' @return List of results
#'
#' @author Juuso Parkkinen \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export
#' @examples # pk.services <- GetPalvelukartta("service")
GetPalvelukartta <- function(category, ...) {
  
  library(RCurl)
  library(rjson)
  
  api.url <- paste("http://www.hel.fi/palvelukarttaws/rest/v2/", category, "/", sep="")
  curl <- getCurlHandle(cookiefile = "")
  params <- list(...)
  val <- getForm(api.url, .params=params, curl=curl)
  res <- fromJSON(val)
  return(res)
}
