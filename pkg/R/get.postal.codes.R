
# This file is a part of the soRvi program
# http://sorvi.r-forge.r-project.org

# Copyright (C) 2011 Leo Lahti <leo.lahti@iki.fi>. All rights reserved.

# This program is open source software; you can redistribute it and/or
# modify it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


#' Get Finnish postal codes vs. municipalities table from Wikipedia. 
#'
#' @param url URL for the HTML site where the postal codes are parsed. 
#'
#' @return A data frame with the following fields: postal.code: postal code; municipality: Name of the municipality (kunnan nimi); municipality.scandless: Municpality name without special chars. 
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Juuso Parkkinen and Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples 
#' #postal.code.table <- get.postal.codes() 
#' @keywords utilities

get.postal.codes <- function (url = "http://fi.wikipedia.org/wiki/Luettelo_Suomen_postinumeroista_kunnittain") {

  # Lue sivun lahdekoodi
  txt <- readLines(url)

  # Poimi lista 
  txt <- txt[grep("^<li>", txt)]

  # Eriyta municipalitynimet ja yksityiskohtaisemmat paikannimet / postinumerot
  cnt <- 0
  map <- list()
  for (i in 1:length(txt)) {
    li <- txt[[i]]
    if (substr(li, 1, 11) == "<li><a href") {            
      # Parsi kunnan nimi otsikkorivilta
      municipality <- unlist(strsplit(unlist(strsplit(li, ">"))[[3]], "<"))[[1]]
    } else {
      tmp1 <- unlist(strsplit(li, ">"))[[2]]      
      tmp0 <- unlist(strsplit(tmp1, "/"))
      postinro <- unlist(strsplit(tmp0[[1]], " "))[[1]] 
      cnt <- cnt + 1
      map[[cnt]] <- c(postinro, municipality)
    }
  }

  library(plyr)
  map <- ldply(map)
  colnames(map) <- c("postal.code", "municipality")
  map$municipality.scandless <- korvaa.skandit(map$municipality)

  # Poista viimeinen rivi
  map <- map[-nrow(map),]

  map
}
