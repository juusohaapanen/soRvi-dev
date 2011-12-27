#' Build a WMS service description for the GDAL WMS driver.
#'
#' WMS service description file is a XML file that describes required and
#' optional information on how to retrieve an exisiting WMS raster over the
#' web. The extent of the raster tile from the data source is defined by the
#' extent of a SpatialPolygonsDataFrame object (no other ways of
#' providing extent are implemented yet). Raster resolution (pixel size is
#' also provided as a parameter (there seems to be no way to query the original
#' resolution from the service.
#'
#' @param WMS WMS-object containing the necessary service information
#' @param layer the name of the layer to be fetched from the data source
#' @param extent SpatialPolygonsDataFrame object to be used to define the extent
#' @param resolution integer value of the resolution (CRS dependent)
#'
#' @return character XML string
#'
#' @note meant for package internal use only
#'
#' @references
#' \url{http://www.gdal.org/frmt_wms.html}
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.org}

BuildService <- function(WMS, layer, extent, resolution) {
  
  if (class(WMS) != 'WMS') {
    stop(paste('WMS unsupported type: ', class(WMS)))
  }
  
  # Extent is defined by the bounding box of the SpatialPolygonsObject provided
  # as extent parameter.
  # TODO: implement other ways of providing the extent
  if (class(extent) == 'SpatialPolygonsDataFrame') {
    bbox.extent <- bbox(extent)
    # Number of columns and rows (i.e. resolution) is defined by the real
    # width and height of the raster divided by the resolution parameter
    # (all this depends on the CRS, not very tested)
    ncols <- round((bbox.extent[1, 2] - bbox.extent[1, 1]) / resolution, 0)
    nrows <- round((bbox.extent[2, 2] - bbox.extent[2, 1]) / resolution, 0)
    # Set the extent corners
    ul.x <- bbox.extent[1, 1]
    ul.y <- bbox.extent[2, 2]
    lr.x <- bbox.extent[1, 2]
    lr.y <- bbox.extent[2, 1]
  } else {
    stop('Function only supports SpatialPolygonDataFrames')
  }
  
  # Create the XML structure based on the GDAL specification at
  # http://www.gdal.org/frmt_wms.html
  
  # Root level
  root <- newXMLNode('GDAL_WMS')
  # Service level
  service <- newXMLNode('Service', attrs=c(name='WMS'), parent=root)
  version <- newXMLNode('Version', text='1.1.1', parent=service)
  server.url <- newXMLNode('ServerUrl', text=WMS@base.url, parent=service)
  # TODO: CRS should not be hard coded
  srs <- newXMLNode('SRS', text='EPSG:3067', parent=service)
  # Not sure if really needed
  image.format <- newXMLNode('ImageFormat', text='image/tiff', parent=service)
  layers <- newXMLNode('Layers', text=layer, parent=service)
  # Style is needed even if empty
  style <- newXMLNode('Style', parent=service)
  
  # dw.node level
  dw.node <- newXMLNode('DataWindow', parent=root)
  # Note that the following notation is minX, maxY, maxX, minY
  ulx.node <- newXMLNode('UpperLeftX', text=ul.x, parent=dw.node)
  uly.node <- newXMLNode('UpperLeftY', text=ul.y, parent=dw.node)
  lrx.node <- newXMLNode('LowerRightX', text=lr.x, parent=dw.node)
  lry.node <- newXMLNode('LowerRightY', text=lr.y, parent=dw.node)
  # TODO: although size is set here, it is not completely clear how the
  # native raster resolution on the WMS server is related to resolution
  # requested
  sizex.node <- newXMLNode('SizeX', text=ncols, parent=dw.node)
  sizey.node <- newXMLNode('Sizey', text=nrows, parent=dw.node)
  originy.node <- newXMLNode('YOrigin', text='top', parent=dw.node)
  
  # Back to the root level
  projection.node <- newXMLNode('Projection', text='EPSG:3067', parent=root)
  # Optional, this is also the default. Seems to be required in case where the
  # the raster requested is 3-band RGB raster.
  bands.count.node <- newXMLNode('BandsCount', text='3', parent=root)
  # Optional, probably not needed here
  cache <- newXMLNode('Cache', parent=root)
  
  # Save the created XML object, not providing a file path converts the object
  # into string.
  return(saveXML(root))
}

#' Create a WMS object.
#'
#' WMS object fetches the relevant meta data from the WMS server (without
#' actually getting the data). This helper function is a thin wrapper to
#' create and return the appropriate WMS object.
#'
#' @param url string describing the URL of the WMS
#' @param cache string file system path controlling where the WMS cache is created (not implemented yet!)
#'
#' @return WMS object corresponding to the provided URL
#'
#' @seealso listWMSurls
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.org}
#' @export

PreprocessWMS <- function(url, cache='~') {
  wms <- new("WMS", base.url=url)
}

#' Get WMS capabilities (meta data)
#'
#' Function intiates a GetCapabilities query to a specified URL and returns
#' the response. Function will trigger an error if the response cannot be parsed
#' and/or is not valid.
#'
#' @param url a string url to target WMS
#'
#' @return XMLRootNode
#'
#' @note Meant for package internal use only.
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.org}

GetCapabilities <- function(url) {
  
  errormsg <- "Could not get capabilities for WMS"
  
  xmlroot <- tryCatch(xmlRoot(xmlTreeParse(url, isURL=TRUE)),
                      error=function(err) stop(paste(errormsg,err)))
  return(xmlroot)
}

#' Get WMS layers
#'
#' All WMS have a set of layers which correspond to the actual rasters. getLayers
#' list all layers associated with a specific WSM object.
#'
#' @param WMS object
#'
#' @note NOT IMPLEMENTED YET.
#'
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.org}
#' @export

GetWMSlayers <- function(WMS) {
  input.json <- GetURL("http://paikkatieto.ymparisto.fi/ArcGIS/rest/services/INSPIRE/SYKE_Maanpeite/MapServer/layers?f=json")
  json <- fromJSON(input.json)
  browser()
  stop("getLayers is not implemented yet!")
}

#' Get the URL associated with one of the standard WMSs.
#'
#' Standard WMSs are a group of well-known WMSs provided as part of soRvi. All
#' URLs have a provider (organization etc.) and a wmsname.
#'
#' @param provider string describing the provider
#' @param service string describing the WMS service requested
#'
#' @return string URL
#'
#' @seealso listWMSurls
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.org}
#' @export

LoadWMSurl <- function(provider, service) {
  
  errormsg <- "Could not load WMS definition XML, have you loaded soRvi?"
  
  xml.urls <- tryCatch(xmlRoot(xmlTreeParse(system.file("extdata/wms-urls.xml",
                                                        package="sorvi"))),
                       error = function(err) stop(paste(errormsg, err)))
  
  xpath.string <- paste("/services//provider[@name='", provider,
                        "']//service[@name='", service, "']//url[@type='WMS']",
                        sep="")
  url <- unlist(getNodeSet(xml.urls, xpath.string))
  if (length(url) > 0){
    # FIXME: this is just waiting to get broken...
    return(url[4][[1]])
  } else {
    stop(paste("Could not find provider", provider, "with wmsname", wms.name))
  }
}

#' Get WMS raster.
#'
#' After the WMS object is set up, it can be queried in order the get the
#' actual data (raster map). Function uses GDAL to read the remote WMS by first
#' creating the service description.
#'
#' @param WMS a WMS object containing the necessary service informations
#' @param layer string name of the layer to be fetched from the data source
#' @param extent SpatialPolygonsDataFrame object to be used to define the extent
#' @param resolution integer value of the resolution (CRS dependent)
#'
#' @seealso buildServiceDesc
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.org}
#' @export

GetWMSraster <- function(WMS, layer, extent, resolution) {
  
  # Use GDAL to read in the value, readGDAL returns a SpatialObject
  wms.description <- BuildService(WMS, layer, extent, resolution)
  wms.raster <- readGDAL(wms.description)
  return(wms.raster)
}

#' List all the URLs associated with one of the standard WMSs.
#'
#' Standard WMSs are a group of well-known WMSs provided as part of soRvi. All
#' URLs have a provider (organization etc.) and a WMS name and an URL.
#'
#' @return SpatialObject from the target WMS raster
#'
#' @seealso buildServiceDesc listWMSurls
#' @author Joona Lehtomaki \email{joona.lehtomaki@@gmail.org}
#' @export

ListWMSurls <- function() {
  
  errormsg <- "Could not load WMS definition XML, have you loaded soRvi?"
  
  xml.urls <- tryCatch(xmlRoot(xmlTreeParse(system.file("extdata/wms-urls.xml",
                                                        package="sorvi"))),
                       error = function(err) stop(paste(errormsg, err)))
  
  # FIXME: url node should not be hard coded
  # Loop through the XML structure and print out the provider name and WMS names
  # and URLs
  for (i in 1:length(xml.urls[[1]])) {
    provider <- ""
    if (xmlName(xml.urls[[1]]) == "provider") {
      provider  <- xmlAttrs(xml.urls[[1]])[[1]]
    }
    if (xmlName(xml.urls[[1]][[i]]) == "service") {
      cat("Provider: ", provider, "\n")
      cat("Service: ", xmlAttrs(xml.urls[[1]][[i]]), "\n")
      for (item in xmlChildren(xml.urls[[1]][[i]])) {
        if (length(xmlAttrs(item)) > 0 ) {
          cat("\t", xmlName(item), " (", xmlAttrs(item)[[1]], ")",
              ": ", xmlValue(item), "\n", sep="")
        } else {
          cat("\t", xmlName(item), ": ", xmlValue(item), "\n", sep="")
        }
      }
    }
  }
}