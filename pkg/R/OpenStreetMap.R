# Copyright (C) 2011 Juuso Parkkinen <juuso.parkkinen(at)gmail.com. All rights reserved.

# This program is open source software; you can redistribute it and/or
# modify it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# This file is a part of the soRvi program
# http://sorvi.r-forge.r-project.org

#' Get geo code from OpenStreetMap
#'
#' Get gecode for given plave from OpenStreetMap Nominatim
#' See http://wiki.openstreetmap.org/wiki/Nominatim
#'
#' @query Either a street address, e.g. 'Mannerheimintie+100,Helsinki' or place, e.g. 'Eduskuntatalo'
#'
#' @return coordinates (lat, lon)
#' 
#' @author Juuso Parkkinen \email{juuso.parkkinen@@gmail.org}
#' @export
get.geocode.OpenStreetMap <- function(query) {
  
  u <- paste("http://nominatim.openstreetmap.org/search?q=",query,"&format=json", sep="")
  val <- getURI(u)
  res <- fromJSON(val)
  if (length(res)>0)
    return(as.numeric(c(res[[1]]$lat, res[[1]]$lon)))
  else # Geocode not found
    return(NULL)
}