# This file is a part of the soRvi program
# http://sorvi.r-forge.r-project.org

# Copyright (C) 2011-2012 Leo Lahti. All rights reserved.
# Contact: <sorvi-commits@@lists.r-forge.r-project.org>

# This program is open source software; you can redistribute it and/or
# modify it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Get PC Axis data with custom preprocessing for PC Axis 
#' files from Statistics Finland (Tilastokeskus) http://www.stat.fi/
#'
#' Arguments:
#'  @param px PC Axis object, or its URL.
#'
#' Returns:
#'  @return PC Axis object.
#'
#' @export
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples 
#' # px <- GetPXTilastokeskus("http://pxweb2.stat.fi/database/StatFin/vrm/synt/080_synt_tau_203_fi.px")
#' @keywords utilities

GetPXTilastokeskus <- function (px) {

  # If URL is given, read the data into PX object
  if (is.url(px)) {
    message(paste("Reading Tilastokeskus data from ", px))
    px <- read.px(px)	 
  }

  # Convert to data.frame 
  if (class(px) == "px") { 
    px <- as.data.frame(px) 
  }

  # Preprocess field names
  fields <- c("Alue", "Kunta")
  for (nam in intersect(fields, colnames(px))) {
    px[[nam]] <- sapply(px[[nam]], function (x) {strsplit(as.character(x), " - ")[[1]][[1]]})
  }    

  px

}

