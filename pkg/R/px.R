# This file is a part of the soRvi program
# http://sorvi.r-forge.r-project.org

# Copyright (C) 2011 Leo Lahti <leo.lahti@iki.fi>. All rights reserved.

# This program is open source software; you can redistribute it and/or
# modify it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Preprocess PC Axis data.
#'
#' @param px PC Axis object.
#'
#' @return PC Axis object.
#' @export
#' @callGraphPrimitives
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@lists.r-forge.r-project.org}
#' @examples # 
#' # px <- read.px("http://pxweb2.stat.fi/database/StatFin/vrm/synt/080_synt_tau_203_fi.px")	 
#' # px.polished <- preprocess.px(as.data.frame(px))
#' @keywords utilities

preprocess.px <- function (px) {

  # Preprocessing for PC Axis files 
  # ("px" or "data.frame" class)

  if (class(px) == "px") { px <- as.data.frame(px) }
  
  # Putsaa kuntanimet ym.
  fields <- c("Alue", "Kunta")
  for (nam in intersect(fields, colnames(px))) {
    px[[nam]] <- sapply(px[[nam]], function (x) {strsplit(as.character(x), " - ")[[1]][[1]]})
  }    

  px

}

