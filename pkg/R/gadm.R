# Copyright (C) 2011 Leo Lahti <leo.lahti@iki.fi>. All rights reserved.

# This program is open source software; you can redistribute it and/or
# modify it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# This file is a part of the soRvi program
# http://sorvi.r-forge.r-project.org

#' Get map data in GADM format
#'
#' @param alue Map identifier. Kun koko GADM-URL on esim. muotoa "http://gadm.org/data/rda/FIN_adm2.RData", alue-muuttujaan sijoitetaan osa "FIN_adm". Aluekohtaisia osoitteita loytyy gadm:in verkkosivuilta http://gadm.org/
#' @param resolution integer value of the resolution. Kun koko GADM-URL on esim. muotoa "http://gadm.org/data/rda/FIN_adm4.RData", taso-muutttuja on "4".
#'
#' @return GADM object
#' @export
#' @callGraphPrimitives
#' @note Suomen osalta kuntatiedot (FIN_adm4) nayttaa olevan paivittamatta uusimpaan. Suositellaan MML:n karttoja, ks. help(MML). 
#'
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@lists.r-forge.r-project.org}
#' @examples # Suomen kunnat: gadm <- get.gadm(alue = "FIN_adm", resolution = 4)
#' @keywords utilities

get.gadm <- function (resolution = "FIN_adm", taso = 4) {

  # see http://ryouready.wordpress.com/2009/11/16/infomaps-using-r-visualizing-german-unemployment-rates-by-color-on-a-map/   # http://r-spatial.sourceforge.net/gallery/ 
  # url <- "http://gadm.org/data/rda/FIN_adm"

  # Ladataan Suomen kartta, joka on jaettu kuntiin
  # FIXME: lisaa muut tasot myoh.
  if (taso == "laanit") {taso <- 1}
  if (taso == "maakunnat") {taso <- 2}
  if (taso == "kunnat") {taso <- 4}

  url.gadm <- "http://gadm.org/data/rda/" # URL for GADM R data
  con <- url(paste(url.gadm, resolution, taso, ".RData", sep=""))
  print(load(con))
  close(con)

  # Putsaa nimet
  if (taso == 4) {
    if (any(duplicated(gadm$NAME_4))) {
      warning("Poistetaan duplikaatit")
      gadm <- gadm[!duplicated(gadm$NAME_4),] # Poista duplikaatit
    }
    # FIXME: etsi tapa sisallyttaa skandit R-pakettiin
    warning("Poistetaan skandit")
    gadm.kunnat <- as.character(gadm$NAME_4)
    gadm.kunnat <- gsub("\xe4", "a", gadm.kunnat)
    gadm.kunnat <- gsub("\xf6", "o", gadm.kunnat)
    gadm.kunnat <- gsub("\U3e34633c", "A", gadm.kunnat)

    gadm$kunnat <- gadm.kunnat

  }

  gadm

}  

#' Map GADM location coordinates to region identifier
#'
#' @param x X coordinate 
#' @param y Y coordinate 
#'
#' @return A data frame with coordinates, region, province, and municipality information
#' @export
#' @callGraphPrimitives
#'
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@lists.r-forge.r-project.org}
#' @keywords utilities

gadm.position2region <- function (x = c(24.9375, 24.0722), y = c(60.1783, 61.4639)) {

  # Modified from http://www.r-ohjelmointi.org/?p=894
 
  # Muodostetaan ensin data frame, jossa ovat koordinaatit
  library(sp)
  
  dat<-data.frame(id=c("a", "b"), x=x, y=y)
  coordinates(dat) = ~x+y

  # Ladataan Suomen kartta, joka on jaettu laaneihin
  gadm <- get.gadm(resolution = "FIN_adm", taso = 1)
  province <- overlay(gadm, dat)

  # Ladataan Suomen kartta, joka on jaettu maakuntiin
  gadm <- get.gadm(resolution = "FIN_adm", taso = 2)
  region<-overlay(gadm, dat)

  # Ladataan Suomen kartta, joka on jaettu kuntiin
  gadm <- get.gadm(resolution = "FIN_adm", taso = 4)
  municipality <- overlay(gadm, dat)

  # Yhdistetaan tiedot yhdeksi data frameksi
  df <- data.frame(dat, province = province$VARNAME_1, region = region$VARNAME_2, municipality = municipality$NAME_4)

  df

}


