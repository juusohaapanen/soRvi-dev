# Modified from https://github.com/hadley/ggplot2/wiki/Crime-in-Downtown-Houston,-Texas-:-Combining-ggplot2-and-Google-Maps
get.staticmap.GoogleMaps <- function(center, zoom = 10, GRAYSCALE=FALSE, scale=1, maptype = 'map',
                                     destfile = 'TemporaryMap.png', n_pix = 640, format="png32") {

  # Get map with given scale
  if (scale==1) 
    GetMap(center = center[c('lat','lon')], GRAYSCALE=GRAYSCALE, size = c(n_pix, n_pix), 
           zoom = zoom, format = format, maptype = maptype, destfile=destfile)
  else if (scale==2)
    GetMap(center = center[c('lat','lon')], GRAYSCALE=GRAYSCALE, size = c(n_pix, n_pix),
           zoom = zoom, format = format, maptype = paste(maptype, "&scale=2", sep=""), destfile=destfile)
  else
    stop("Invalid scale-value!")
  
  map <- readPNG(destfile)
  n_pix <- n_pix*scale #Double number of pixels if scale==2
  
  # Deal with color
  if(GRAYSCALE == FALSE) {
    cat("Colours: yes\n")
    map <- apply(map, 1:2, function(v) rgb(v[1], v[2], v[3]))     
  } else {
    cat("Colours: no\n")
    nrow <- nrow(map)
    ncol <- ncol(map)
    map <- grey(rgb2grey(map, coefs = c(0, 1, 0)))
    map <- matrix(map, nrow = nrow, ncol = ncol)
  }

  # Reshape map for plotting
  m_map <- melt(map)
  names(m_map) <- c('x','y','fill')
  m_map <- within(m_map,{
    x <- x - n_pix/2 - 1
    y <- y - n_pix/2 - 1
  })     
  
  mapInfo <- list(lat = center['lat'], lon = center['lon'], zoom = zoom, map)
  XY_cent <- LatLon2XY.centered(mapInfo, center['lat'], center['lon'])
  
  # Geocode pixel references
  s <- (-n_pix/2) : (n_pix/2 - 1)  
  lat_wrapper <- function(x) XY2LatLon(mapInfo, -n_pix/2, x)[1]
  lats <- apply(data.frame(s), 1, lat_wrapper)  
  lon_wrapper <- function(y) XY2LatLon(mapInfo, y, -n_pix/2)[2]
  lons <- apply(data.frame(s), 1, lon_wrapper)
  
  # Merge colors to latlons and return
  df_xy   <- expand.grid(x = s, y = s)
  df_ll   <- expand.grid(lat = rev(lats), lon = lons)
  df_xyll <- data.frame(df_xy, df_ll)
  df <- suppressMessages(join(df_xyll, m_map, type = 'right'))
  df <- df[,c('lon','lat','fill')]
  return(df)
}

# Modified from http://www.r-chart.com/2010/07/maps-geocoding-and-r-user-conference.html
get.geocode.GoogleMaps <- function(str) {

  u <- paste('http://maps.google.com/maps/api/geocode/xml?sensor=false&address=',str)
  doc <- xmlTreeParse(u, useInternal=TRUE)
  lat <- sapply(getNodeSet(doc, "/GeocodeResponse/result/geometry/location/lat"), function(el) xmlValue(el))
  lon <- sapply(getNodeSet(doc, "/GeocodeResponse/result/geometry/location/lng"), function(el) xmlValue(el))
  return(c(lat,lon))
}





