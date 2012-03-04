# This file is a part of the soRvi program
# http://sorvi.r-forge.r-project.org

# Copyright (C) 2011-2012 Leo Lahti 
# <sorvi-commits@lists.r-forge.r-project.org>
# All rights reserved.

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
#' @param type String. Specifies visualization type. Options: "oneway", "twoway", "qualitative", "custom". See details. 
#' @param ncol Number of distinct colors shades
#' @param at Color transition points
#' @param palette Optional. Color palette.
#' @param main Optional. Title text.
#' @param colorkey Logical. Show color interpretation in a separate legend.
#' @param lwd Optional. Line width for shape polygon borders.
#' @param border.col Optional. Color for shape polygon borders.
#' @param col.regions Optional. Specify color for the shape object regions manually.
#' @return ggplot2 object
#' @details Visualization types include: oneway/sequential (color scale ranges from white to dark red, or custom color given with the palette argument); twoway/bipolar/diverging (color scale ranges from dark blue through white to dark red; or custom colors); discrete/qualitative (discrete color scale; the colors are used to visually separate regions); and "custom" (specify colors with the col.regions argument)
#' @export
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # PlotShape(sp, varname) 
#' @seealso \code{\link{get.hsy}}
#' @keywords utilities


PlotShape <- function (sp, varname, type = "oneway", ncol = 10, at = NULL, palette = NULL, main = NULL, colorkey = TRUE, lwd = .4, border.col = "black", col.regions = NULL) {

  # type = "oneway"; ncol = 10; at = NULL; palette = NULL; main = NULL; colorkey = TRUE; lwd = .4; border.col = "black"; col.regions = NULL

  pic <- NULL

  if (is.null(main)) {
    main <- varname
  }

  if (is.factor(sp[[varname]]) && (!type %in% c("discrete", "qualitative", "custom"))) {
    warning("Discrete/custom color scale required for factors; resetting color type")
    type <- "qualitative"
  }

  if (type %in% c("oneway", "quantitative", "sequential")) {
    # Define color palette
    if (is.null(palette)) {
      palette <- colorRampPalette(c("white", "red"), space = "rgb")
    }

    sp[[varname]] <- as.numeric(as.character(sp[[varname]]))

    if (is.null(at)) { 
      at <- seq(min(sp[[varname]]), max(sp[[varname]]), length = ncol) 
    } else {
      # Override ncol if at is given
      ncol <- length(at)
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

  } else if (type %in% c("twoway", "bipolar", "diverging")) { 

    # Plot palette around the data average
    # To highlight deviations in both directions

    # Define color palette
    if (is.null(palette)) {
      palette <- colorRampPalette(c("blue", "white", "red"), space = "rgb")
    }

    if (is.null(at)) { 
      # Linear color palette around the average
      min <- min(sp[[varname]])
      max <- max(sp[[varname]])
      at <- seq(min, max, length = ncol) 
    } else {
      # Override ncol if at is given
      ncol <- length(at)
    }
    # message(at)

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
  } else if (type %in% c("qualitative", "discrete")) {

    vars <- factor(sp[[varname]])
    sp[[varname]] <- vars
    
    if (is.null(col.regions) && length(sp[[varname]]) == length(levels(sp[[varname]]))) {
      # Aims to find colors such that neighboring polygons have 
      # distinct colors
      cols <- GenerateMapColours(sp) # Generate color indices
      col.regions <- brewer.pal(max(cols), "Paired")[cols]
    } else if (is.null(col.regions)) {
      
      # Use ncol colors, loop them to fill all regions    
      col.regions <- rep(brewer.pal(ncol, "Paired"), ceiling(length(levels(vars))/ncol))[1:length(levels(vars))]
    }

    colorkey <- FALSE

    pic <- spplot(sp, varname, col.regions = col.regions, main = main, colorkey = colorkey, lwd = lwd, col = border.col)

  } else if (type == "custom") {

    # User-defined colors for each region  
    if (is.null(col.regions)) {  
      stop("Define region colors through the col.regions argument 
      		   in the custom mode!")
    }

  }

  if (is.null(col.regions)) {
    col.regions <- palette(ncol)
  }

  if (is.null(pic)) {
    pic <- spplot(sp, varname, col.regions = col.regions, main = main, colorkey = colorkey, lwd = lwd, col = border.col, at = at)
  }
	  
  print(pic)
  pic
}


#' Visualize a matrix with one or two-way color scale. 
#' TODO: one-way color scale
#' 
#' This function is used for fast investigation of matrix objects; standard visualization choices are made
#' automatically; fast and easy-to-use but does not necessarily provide optimal visualization.
#'
#' @param mat matrix
#' @param type String. Specifies visualization type. Options: "oneway" (color scale ranges from white to dark red; the color can be changed if needed); "twoway" (color scale ranges from dark blue through white to dark red; colors can be changed if needed)
#' @param midpoint middle point for the color plot: smaller values are shown with blue, larger are shown with red in type = "twoway"
#' @param palette Optional. Color palette.
#' @param colors Optional. Colors.
#' @param col.breaks breakpoints for the color palette
#' @param interval interval for palette color switches
#' @param plot.axes String. Indicates whether to plot x-axis ("x"), y-axis ("y"), or both ("both").
#' @param row.tick interval for plotting row axis texts
#' @param col.tick interval for plotting column axis texts
#' @param cex.xlab use this to specify distinct font size for the x axis
#' @param cex.ylab use this to specify distinct font size for the y axis
#' @param xlab optional x axis labels
#' @param ylab optional y axis labels
#' @param limit.trunc color scale limit breakpoint
#' @param mar image margins
#' @param ... optional parameters to be passed to function 'image', see help(image) for further details
#' @return A list with the color palette (colors), color breakpoints (breaks), and palette function (palette.function)
#' @export
#' @references See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # mat <- rbind(c(1,2,3,4,5), c(1, 3, 1), c(4,2,2)); PlotMatrix(mat, "twoway", midpoint = 3) 
#' @keywords utilities

PlotMatrix <- function (mat, type = "twoway", midpoint = 0, 
	      	        palette = NULL, colors = NULL, col.breaks = NULL, interval = .1, 
			plot.axes = "both",
			row.tick = 1, col.tick = 1, 
			cex.xlab = .9, cex.ylab = .9, 
			xlab = NULL, ylab = NULL,
			limit.trunc = 0, mar = c(5, 4, 4, 2), ...) {

  # Center the data and color breakpoints around the specified midpoint
  mat <- mat - midpoint

  if (length(col.breaks) == 0)  {
    m <- max(round(max(abs(mat)), limit.trunc) - interval, 0)
    mm <- m + interval/2
    vals <- seq(interval/2,mm,interval)
    # Set col.breaks evenly around zero
    col.breaks  <- c(-(m + 1e6), c(-rev(vals), vals), m+1e6)
  }
		  
  if (is.null(palette)) {
    my.palette <- colorRampPalette(c("blue", "white", "red"), space = "rgb")
  } else if (palette == "blue-black-red") {
    my.palette <- colorRampPalette(c("blue", "black", "red"), space = "rgb")
  } else if (palette == "blue-white-red") {
    my.palette <- colorRampPalette(c("blue", "white", "red"), space = "rgb")
  } else if (palette == "blue-white-yellow") {
    my.palette <- colorRampPalette(c("blue", "white", "yellow"), space = "rgb")
  } else if (palette == "blue-black-yellow") {
    my.palette <- colorRampPalette(c("blue", "black", "yellow"), space = "rgb")
  } else if (palette == "bw") {
    gray.palette <- function (int) {
      gray(seq(0,1,length=int))
    }
    my.palette <- gray.palette
  }

  # if mycolors is provided it overrides palette
  if (is.null(colors)) { colors <- my.palette(length(col.breaks) - 1) }
	   		      
  # transpose and revert row order to plot matrix in the same way it
  # appears in its numeric form
  par(mar = mar)
  image(t(mat[rev(seq(nrow(mat))),]), col = colors, xaxt = 'n', yaxt = 'n', zlim = range(col.breaks), breaks = col.breaks, ...)

  if (plot.axes == "both" || plot.axes == TRUE) {
    
    if (is.null(xlab)) {
      v <- seq(1, ncol(mat), col.tick) # take every nth index
      axis(1, at = seq(0,1,length = ncol(mat))[v], labels = colnames(mat)[v], cex.axis=cex.xlab, las=2, ...)    
    } else {
      axis(1, at = seq(0,1,length = ncol(mat)), labels = xlab, cex.axis=cex.xlab, las=2, ...)    
    }

    if (is.null(ylab)) {
      v <- seq(1, nrow(mat), row.tick) # take every nth index
      axis(2, at = seq(0,1,length = nrow(mat))[v], labels = rev(rownames(mat))[v], cex.axis=cex.ylab, las=2, ...)
    } else {  
      axis(2, at = seq(0,1,length = nrow(mat)), labels = ylab, cex.axis=cex.ylab, las=2, ...)
    }

  } else if (plot.axes == "x") {

    if (is.null(xlab)) {
      v <- seq(1, ncol(mat), col.tick) # take every nth index
      axis(1, at = seq(0,1,length = ncol(mat))[v], labels = colnames(mat)[v], cex.axis=cex.xlab, las=2)    
    } else {
      axis(1, at = seq(0,1,length = ncol(mat)), labels = xlab, cex.axis=cex.xlab, las=2)    
    }

  } else if (plot.axes == "y") {

    if (is.null(ylab)) {
      v <- seq(1, nrow(mat), row.tick) # take every nth index
      axis(2, at = seq(0, 1, length = nrow(mat))[v], labels = rev(rownames(mat))[v], cex.axis = cex.xlab, las = 2)
    } else {  
      axis(2, at = seq(0, 1, length = nrow(mat)), labels = ylab, cex.axis=cex.xlab, las=2)
    }
  }
  
  # Return default margins
  par(mar = c(5, 4, 4, 2) + 0.1)
 
  return(list(colors = colors, breaks = col.breaks + midpoint, palette.function = my.palette))
      	  
}


#' Visualize color scale for PlotMatrix output
#' NOTE: Experimental. To be tested thoroughly.
#' 
#' @param breaks breakpoints for colors
#' @param colors Optional. Colors.
#' @param m overrides breaks, mypalette and produces a plot that ranges (-m,m)
#' @param label.step step between label text plotting
#' @param interval interval for palette color switches
#' @param two.sided indicates one- or two-sided color palette
#' @param label.start start point for the labels
#' @param Nlab number of labels
#' @param palette.function palette color scale function
#' @param ndigits number of digits to plot
#' @param ... optional parameters to be passed to function 'axis', see help(axis) for further detai
#' @return A list with the color palette (palette), color breakpoints (breaks), and palette function (palette.function)
#' @export
#' @references See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # mat <- rbind(c(1,2,3,4,5), c(1, 3, 1), c(4,2,2)); pm <- PlotMatrix(mat, "twoway", midpoint = 3); PlotScale(pm$colors, pm$breaks)
#' @keywords utilitie

PlotScale <- function (breaks, colors = NULL, m = NULL, label.step = 2, interval=.1, two.sided = TRUE, label.start = 1.00, Nlab = 3, palette.function = NULL, ndigits = 2, ...) {

  if (two.sided) {
    
    if (length(m)>0) {
      breaks <- set.breaks(m, interval)
      image(t(as.matrix(seq(-mm, mm, length = 100))), col = colors, xaxt = 'n', yaxt = 'n', zlim = range(breaks), breaks=breaks)
    } else {
      image(t(as.matrix(breaks)), col = colors, xaxt = 'n',yaxt = 'n', zlim = range(breaks), breaks = breaks)
    }
  
    mm1 <- sort(breaks)[[2]]
    mm2 <- rev(sort(breaks))[[2]]
    
    tmp <- unlist(strsplit(as.character(mm1),"\\."))

    digit.step <-10^(-ndigits)
    labs <- round(seq(mm1, mm2, by = digit.step), ndigits)
    start.position <- which.min(abs(round(labs, ndigits) - (-label.start)))
    end.position <- length(labs) - 1 
    inds <- seq(start.position, end.position, length = Nlab)
      
    axis(2, at = seq(0, 1, length = Nlab), labels = labs[inds], las=2, ...)
  }

  if (!two.sided) {

    mm <- max(breaks) + 1e6 # infty
    m <- max(breaks)
 
    labs = seq(0,m,label.step)
    #inds = sapply(labs,function(lab){min(which(lab<=breaks))})
    start.position <- which.min(abs(round(labs, ndigits) - (-label.start)))
    end.position <- which.min(abs(round(labs, ndigits) - (label.start)))
    inds <- seq(start.position,end.position,length=Nlab)  

    image(t(as.matrix(seq(0, m, length = 100))), col = colors, xaxt='n',yaxt='n', zlim=range(breaks), breaks=breaks)
    
    axis(2, at = seq(0, 1, length=Nlab), labels=labs[inds], las=2, ...)
  }
  
}


#' Set breaks for color palette. Internal function.
#'
#' @param mat data matrix or vector for which the breaks will be deterined 
#' @param interval interval of color breaks
#' @return A vector of breakpoints
#' @references See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # mat <- rbind(c(1,2,3,4,5), c(1, 3, 1), c(4,2,2)); pm <- PlotMatrix(mat, "twoway", midpoint = 3); PlotScale(pm$colors, pm$breaks)
#' @keywords utilities

set.breaks <- function (mat, interval=.1) {
  if (max(abs(mat))>1) {
    m <- floor(max(abs(mat)))
  } else {
    m <- round(max(abs(mat)),nchar(1/interval)-1)
  }

  mm <- m + interval/2
  vals <- seq(interval/2,mm,interval)
  # Note: the first and last values mimic infinity
  mybreaks  <- c(-(m+1e6),c(-rev(vals),vals),m+1e6)
  mybreaks
}



#' Generate color indices for shape object with the aim to color 
#  neighboring objects with distinct colors.
#'
#' @param sp SpatialPolygonsDataFrame object
#' @return Color index vector
#' @references See citation("sorvi") 
#' @export
#' @author Modified from the code by Karl Ove Hufthammer from http://r-sig-geo.2731867.n2.nabble.com/Colouring-maps-so-that-adjacent-polygons-differ-in-colour-td6237661.html; modifications by Leo Lahti
#' @examples # col <- GenerateMapColours(sp)    
#' @keywords utilities


GenerateMapColours <- function(sp) {

  nb <- spdep::poly2nb(sp)   # Generate neighbours lists

  n <- length(sp)            # Number of polygons

  cols <- numeric(n)        # Initial colouring

  cols[1] <- 1              # Let the first polygon have colour 1

  cols1n <- 1:n             # Available colour indices

  for(i in 2:n)
    cols[i] <- which.min(cols1n %in% cols[nb[[i]]])

  cols

}

