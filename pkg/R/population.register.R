# This file is a part of the soRvi program
# http://sorvi.r-forge.r-project.org

# Copyright (C) 2011 Leo Lahti <leo.lahti@iki.fi>. All rights reserved.

# This program is open source software; you can redistribute it and/or
# modify it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


#' Retrieve population register data
#'
#' This script retrieves municipality data from population register. 
#' TODO: merge with get.municipality.info function
#' The url should be changed when there are updates:
#'
#' @param url String. Specify the URL containing the population register data.
#' @return data.frame with municipality information.
#' @export
#' @callGraphPrimitives
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@lists.r-forge.r-project.org}
#' @examples # df <- get.population.register()
#' @keywords utilities


get.population.register <- function (url = "http://vrk.fi/default.aspx?docid=5127&site=3&id=0") {

  require(XML)

  # Read tables from the website
  tables <- readHTMLTable(url)

  # Population is in table 4
  pop <- tables[[4]]

  # Preprocess the data
  pop <- pop[-c(1, (nrow(pop):(nrow(pop)-1))),] # Poista ruotsinkielinen otsikkorivi ja yhteenvetorivi

  # Valkkaa vain informatiiviset sarakkeet
  pop <- pop[, c(1, 4, 5, 6)]
  colnames(pop) <- c("Kunta", "Miehet", "Naiset", "Yhteensa")
  # Eriyta kuntakoodi ja suomen- ja ruotsinkieliset nimet
  parser <- function (x) { 
    x <- as.character(x)
    x <- unlist(strsplit(unlist(strsplit(x, "\r\n")), " - "))
    x <- unname(strstrip(x))  
  }
  nimet <- t(sapply(pop[[1]], function (x) {parser(x)}))
  # Convert counts to numeric
  pop <- data.frame(apply(data.frame(pop[, -1]), 2, function (x){as.numeric(as.character(x))}))
  # Talleta kaikki dataframeen
  df <- data.frame(list(nimet, pop))
  colnames(df)[1:3] <- c("Koodi", "Kunta", "Kommun")

  # Muuta skandit pois; FIXME: later using iconv function
  kuntanimet <- as.character(df$Kunta)
  kuntanimet <- gsub("\\xe4", "a", kuntanimet)
  kuntanimet <- gsub("\\xC4", "A", kuntanimet)
  kuntanimet <- gsub("\\xD6", "O", kuntanimet)
  kuntanimet <- gsub("\\xf6", "o", kuntanimet)
  kuntanimet <- gsub("\\xE5", "a", kuntanimet)
  rownames(df) <- kuntanimet

  df

}

