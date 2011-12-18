# This file is a part of the soRvi program
# http://sorvi.r-forge.r-project.org

# Copyright (C) 2011 Leo Lahti <leo.lahti@iki.fi>. All rights reserved.

# This program is open source software; you can redistribute it and/or
# modify it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.



#' Retrieve MML data 
#'
#' This script can be used for preprocessing of shape data from 
#' Finnish geographical agency (Maanmittauslaitos, MML)
#' The data copyright is on (C) MML 2011.
#'
#' @param sp Shape object (SpatialPolygonsDataFrame)
#'
#' @return Shape object (from SpatialPolygonsDataFrame class)
#' @export
#' @callGraphPrimitives
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@lists.r-forge.r-project.org}
#' @examples # sp <- preprocess.shape.mml(sp)
#' @keywords utilities


preprocess.shape.mml <- function (sp) {

  # TODO?: parseri, joka poimii vain oleelliset tiedot data.frameen
  # ja tekee tarpeelliset merkistomuunnokset. Tsekkaa mita sisaltavat:
  # names(sp)
  # "Suuralue"  "Suural_ni1" "Suural_ni2" 
  # "AVI"        "AVI_ni1"    "AVI_ni2"    
  # "Maakunta"   "Maaku_ni1" "Maaku_ni2"  
  # "Seutukunta" "Seutuk_ni1" "Seutuk_ni2" 
  # "Kunta"     "Kunta_ni1"  "Kunta_ni2" 
  # "Kieli_ni1"  "Kieli_ni2"  # Ruotsi/Suomi
  # "Kaupunki"   # (1/2/3) 
  # "SHAPE_Leng" "SHAPE_Area"

  nams <- colnames(sp@data)
  inds <- which(nams %in% c("AVI_ni1", "AVI_ni2", "Kieli_ni1", "Kieli_ni2", "TEXT1", "TEXT2", "TEXT3", "Suural_ni1", "Suural_ni2", "Maaku_ni1",  "Maaku_ni2", "Seutuk_ni1", "Seutuk_ni2", "Kunta_ni1", "Kunta_ni2"))
  dat <- sp@data

  # Convert encoding to UTF-8 for the text fields
  dat[, inds] <- apply(sp@data[, inds], 2, function (x) {iconv(x, from = "latin1", to = "UTF-8")})

  # Convert the text fields back into factors as in the original data
  for (k in inds) { dat[, k] <- factor(dat[,k]) }

  ###################################

  # The name (ni1) is always given with the main language (Kieli_ni1)
  # For compatibility with other data sources, add fields where all
  # names are systematically listed in Finnish, no matter what is the
  # main language

  if (!is.null(sp$AVI_ni1)) {
    dat$AVI.FI <- iconv(sp$AVI_ni1, from = "latin1", to = "UTF-8")
  }

  if (!is.null(sp$Kieli_ni1)) {
    dat$Kieli.FI <- dat$Kieli_ni1
  }

  if (!is.null(sp$Suural_ni1)) {
    dat$Suuralue.FI   <- iconv(dat$Suural_ni1, from = "latin1", to = "UTF-8") 
  }

  if (!is.null(sp$Maaku_ni1)) {
    dat$Maakunta.FI   <- iconv(dat$Maaku_ni1, from = "latin1", to = "UTF-8")  
  }

  if (!is.null(sp$Seutuku_ni1)) {
    dat$Seutukunta.FI <- iconv(dat$Seutuku_ni1, from = "latin1", to = "UTF-8")
  }

  if (!is.null(sp$Kunta_ni1)) {
    kunta <- as.character(sp$Kunta_ni1)
    inds <- sp$Kieli_ni1 == "Ruotsi" & !sp$Kunta_ni2 == "N_A"
    kunta[inds] <- as.character(sp$Kunta_ni2[inds])
    dat$Kunta.FI <- factor(iconv(kunta, from = "latin1", to = "UTF-8"))
  }

  ########################################################

  sp@data <- dat

  sp
}
