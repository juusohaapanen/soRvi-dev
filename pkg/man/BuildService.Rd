\name{BuildService}
\alias{BuildService}
\title{Build a WMS service description for the GDAL WMS driver.}
\usage{
  BuildService(WMS, layer, extent, resolution)
}
\arguments{
  \item{WMS}{WMS-object containing the necessary service
  information}

  \item{layer}{the name of the layer to be fetched from the
  data source}

  \item{extent}{SpatialPolygonsDataFrame object to be used
  to define the extent}

  \item{resolution}{integer value of the resolution (CRS
  dependent)}
}
\value{
  character XML string
}
\description{
  WMS service description file is a XML file that describes
  required and optional information on how to retrieve an
  exisiting WMS raster over the web. The extent of the
  raster tile from the data source is defined by the extent
  of a SpatialPolygonsDataFrame object (no other ways of
  providing extent are implemented yet). Raster resolution
  (pixel size is also provided as a parameter (there seems
  to be no way to query the original resolution from the
  service.
}
\note{
  meant for package internal use only
}
\author{
  Joona Lehtomaki \email{joona.lehtomaki@gmail.org}
}
\references{
  \url{http://www.gdal.org/frmt_wms.html}
}

