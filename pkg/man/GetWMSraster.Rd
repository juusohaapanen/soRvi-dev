\name{GetWMSraster}
\alias{GetWMSraster}
\title{Get WMS raster.}
\usage{
  GetWMSraster(WMS, layer, extent, resolution)
}
\arguments{
  \item{WMS}{a WMS object containing the necessary service
  informations}

  \item{layer}{string name of the layer to be fetched from
  the data source}

  \item{extent}{SpatialPolygonsDataFrame object to be used
  to define the extent}

  \item{resolution}{integer value of the resolution (CRS
  dependent)}
}
\description{
  After the WMS object is set up, it can be queried in
  order the get the actual data (raster map). Function uses
  GDAL to read the remote WMS by first creating the service
  description.
}
\author{
  Joona Lehtomaki \email{joona.lehtomaki@gmail.org}
}
\seealso{
  buildServiceDesc
}

