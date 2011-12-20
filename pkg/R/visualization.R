# This file is a part of the soRvi program
# http://sorvi.r-forge.r-project.org

# Copyright (C) 2011 Leo Lahti <leo.lahti@iki.fi>. All rights reserved.

# This program is open source software; you can redistribute it and/or
# modify it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Visualize the specified fields of a shape object on using 1- or 2-way color scale. 
#' 
#' This function is used for fast investigation of shape objects; standard visualization choices are made
#' automatically; fast and easy-to-use but does not necessarily provide optimal visualization.
#'
#' @param sp Shape object 
#' @param varname Variable name from the shape object sp to be visualized
#' @param type String. Specifies visualization type. Options: "oneway" (color scale ranges from white to dark red; the color can be changed if needed); "twoway" (color scale ranges from dark blue through white to dark red; colors can be changed if needed)
#' @param ncol Number of distinct colors shades
#' @param at Color transition points
#' @param palette Optional. Color palette.
#' @param main Optional. Title text.
#' @param colorkey Logical. Show color interpretation in a separate legend.
#' @param lwd Optional. Line width for shape polygon borders.
#' @param border.col Optional. Color for shape polygon borders.
#' @param col.regions Optional. Specify color for the shape object regions manually.
#' @return ggplot2 object
#' @export
#' @callGraphPrimitives
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # visualize.shape(sp, varname) 
#' @seealso \code{\link{get.hsy}}
#' @keywords utilities


visualize.shape <- function (sp, varname, type = "oneway", ncol = 10, at = NULL, palette = NULL, main = NULL, colorkey = TRUE, lwd = .4, border.col = "black", col.regions = NULL) {

  if (type == "oneway") { 
    # Define color palette
    if (is.null(palette)) {
      palette <- colorRampPalette(c("white", "red"), space = "rgb")
    }

    sp[[varname]] <- as.numeric(as.character(sp[[varname]]))

    if (is.null(at)) { 
      at <- seq(min(sp[[varname]]), max(sp[[varname]]), length = ncol) 
    }

    if (is.null(main)) {
      main <- varname
    }

    if (is.null(col.regions)) {
      col.regions <- palette(ncol)
    }

    q <- spplot(sp, varname,
            col.regions = col.regions,
	    main = main,
	    colorkey = colorkey,
	    lwd = lwd,
	    col = border.col,
	    at = at
           )
  }

  if (type == "twoway") { 

    # Plot palette around the data average
    # To highlight deviations in both directions

    # Define color palette
    if (is.null(palette)) {
      palette <- colorRampPalette(c("blue", "white", "red"), space = "rgb")
    }

    if (is.null(at)) { 
      # Linear color palette around the average
      #int <- (max(sp[[varname]]) - min(sp[[varname]]))/2
      #mid <- mean(sp[[varname]])
      min <- min(sp[[varname]])
      max <- max(sp[[varname]])
      at <- seq(min, max, length = ncol) 
      #at <- seq(mid - int, mid + int, length = ncol) 
    }
    print(at)

    if (is.null(main)) {
      main <- varname
    }

    if (is.null(col.regions)) {
      col.regions <- palette(ncol)
    }

    q <- spplot(sp, varname,
            col.regions = col.regions,
	    main = main,
	    colorkey = colorkey,
	    lwd = lwd,
	    col = border.col,
	    at = at
           )
  }


  print(q)
  q
}
