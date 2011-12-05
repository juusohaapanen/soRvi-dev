visualize.shape <- function (shape.object, varname, type = "oneway", ncol = 10, at = NULL, palette = NULL, main = NULL, colorkey = TRUE, lwd = .4, border.col = "black", col.regions = NULL) {

  if (type == "oneway") { 
    # Define color palette
    if (is.null(palette)) {
      palette <- colorRampPalette(c("white", "red"), space = "rgb")
    }

    shape.object[[varname]] <- as.numeric(as.character(shape.object[[varname]]))

    if (is.null(at)) { 
      at <- seq(min(shape.object[[varname]]), max(shape.object[[varname]]), length = ncol) 
    }

    if (is.null(main)) {
      main <- varname
    }

    if (is.null(col.regions)) {
      col.regions <- palette(ncol)
    }

    q <- spplot(shape.object, varname,
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
      #int <- (max(shape.object[[varname]]) - min(shape.object[[varname]]))/2
      #mid <- mean(shape.object[[varname]])
      min <- min(shape.object[[varname]])
      max <- max(shape.object[[varname]])
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

    q <- spplot(shape.object, varname,
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
