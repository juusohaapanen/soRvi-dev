# This file is a part of the soRvi program
# http://sorvi.r-forge.r-project.org

# Copyright (C) 2011 Leo Lahti <leo.lahti@iki.fi>. All rights reserved.

# This program is open source software; you can redistribute it and/or
# modify it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


#' Preprocessing function for MML data 
#'
#' This script can be used to preprocess shape data 
#' obtained from Finnish geographical agency (Maanmittauslaitos, MML)
#' The data copyright is on (C) MML 2011.
#'
#' @aliases preprocess.shape.mml
#'
#' Arguments:
#'   @param sp Shape object (SpatialPolygonsDataFrame)
#'
#' Returns:
#'   @return Shape object (from SpatialPolygonsDataFrame class)
#'
#' @details The various Finland shape data files obtained from http://www.maanmittauslaitos.fi/aineistot-palvelut/digitaaliset-tuotteet/ilmaiset-aineistot/hankinta have been preprocessed with this function, and the preprocessed versions are included in soRvi package. Load the readily preprocessed data for use by typing in R: 'data(MML)'. Alternatively, one can download the files from the above URL and apply this function.
#'
#' @export
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # data(MML); sp <- MML[[1]][[1]]; sp2 <- PreprocessShapeMML(sp)
#' @keywords utilities

PreprocessShapeMML <- function (sp) {

  # TODO: parseri, joka poimii vain oleelliset tiedot data.frameen
  # ja tekee tarpeelliset merkistomuunnokset. Tsekkaa mita sisaltavat:
  # names(sp)
  # "Suuralue"  "Suural_ni1" "Suural_ni2" 
  # "AVI"        "AVI_ni1"    "AVI_ni2"    
  # "Maakunta"   "Maaku_ni1" "Maaku_ni2"  
  # "Seutukunta" "Seutuk_ni1" "Seutuk_ni2" 
  # "Kunta"     "Kunta_ni1"  "Kunta_ni2" 
  # "Kieli_ni1"  "Kieli_ni2"  # Ruotsi/Suomi
  # "Kaupunki"   
  # "SHAPE_Leng" "SHAPE_Area"

  # Specify fields that need to converted into UTF-8
  nams <- colnames(sp@data)
  inds <- which(nams %in% c("AVI_ni1", "AVI_ni2", "Kieli_ni1", "Kieli_ni2", "TEXT1", "TEXT2", "TEXT3", "Suural_ni1", "Suural_ni2", "Maaku_ni1",  "Maaku_ni2", "Seutuk_ni1", "Seutuk_ni2", "Kunta_ni1", "Kunta_ni2"))
  dat <- sp@data

  # Convert encoding to UTF-8 for the text fields
  dat[, inds] <- apply(sp@data[, inds], 2, function (x) {iconv(x, from = "latin1", to = "UTF-8")})

  # Convert text fields back into factors as in the original data
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

  sp@data <- dat

  sp
}




#' Shows how the MML Shape files have been converted into 
#' the Rdata files included in soRvi package (load using data(MML)).
#'
#' The various Finland shape data files obtained from http://www.maanmittauslaitos.fi/aineistot-palvelut/digitaaliset-tuotteet/ilmaiset-aineistot/hankinta have been preprocessed using this script, and the preprocessed versions are made available in soRvi package. Load the readily preprocessed data for use by typing in R: 'data(MML)'. 

#' Procedure for obtaining the data(MML) in soRvi package:
#' 1) Download the MML shape files from 
#'    http://www.maanmittauslaitos.fi/aineistot-palvelut/digitaaliset-tuotteet/ilmaiset-aineistot/hankinta
#' 2) Run: GetShapeMML(data.dir = "./")
#' 3) Store the preprocessed data in the specified output file:
#'    save(MML, file = "MML.rda")
#' 4) Compress the output file size 
#'    require(tools)
#'    res <- resaveRdaFiles(MML.rda)
#'    #res <- resaveRdaFiles("../data/")
#' 5) Store the data in soRvi data directory: pkg/data/MML.rda

#'
#' Arguments:
#'   @param input.data.dir Directory path where the original data can be accessed. 
#'   @param verbose Print intermediate processing information
#'
#' Returns:
#'   @return Returns a list of preprocessed shape files containing the public MML data sets. 
#' 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # GetShapeMML(data.dir = "./", output.file = "MML.rda")
#' @keywords internal

GetShapeMML <- function (input.data.dir = "./", verbose = TRUE) {

  MML <- list()
  for (resolutions in c("1_milj_Shape_etrs_shape", "4_5_milj_shape_etrs-tm35fin")) {

    fs <- list.files(paste(input.data.dir, resolutions, sep = ""), full.names = TRUE, pattern = ".shp")

    MML[[resolutions]] <- list()

    for (f in fs) {
      if (verbose) { message(f) }
      file.id <- unlist(strsplit(unlist(strsplit(f, "/"))[[3]], "\\."))[[1]]
      MML[[resolutions]][[file.id]] <- PreprocessShapeMML(readShapePoly(f))
    }
  }

  # return the data object
  MML

}