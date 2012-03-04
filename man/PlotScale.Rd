\name{PlotScale}
\alias{PlotScale}
\title{Visualize color scale for PlotMatrix output
NOTE: Experimental. To be tested thoroughly.}
\usage{
  PlotScale(breaks, colors = NULL, m = NULL,
    label.step = 2, interval = 0.1, two.sided = TRUE,
    label.start = 1, Nlab = 3, palette.function = NULL,
    ndigits = 2, ...)
}
\arguments{
  \item{breaks}{breakpoints for colors}

  \item{colors}{Optional. Colors.}

  \item{m}{overrides breaks, mypalette and produces a plot
  that ranges (-m,m)}

  \item{label.step}{step between label text plotting}

  \item{interval}{interval for palette color switches}

  \item{two.sided}{indicates one- or two-sided color
  palette}

  \item{label.start}{start point for the labels}

  \item{Nlab}{number of labels}

  \item{palette.function}{palette color scale function}

  \item{ndigits}{number of digits to plot}

  \item{...}{optional parameters to be passed to function
  'axis', see help(axis) for further detai}
}
\value{
  A list with the color palette (palette), color
  breakpoints (breaks), and palette function
  (palette.function)
}
\description{
  Visualize color scale for PlotMatrix output NOTE:
  Experimental. To be tested thoroughly.
}
\examples{
# mat <- rbind(c(1,2,3,4,5), c(1, 3, 1), c(4,2,2)); pm <- PlotMatrix(mat, "twoway", midpoint = 3); PlotScale(pm$colors, pm$breaks)
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}
\references{
  See citation("sorvi")
}
\keyword{utilitie}

