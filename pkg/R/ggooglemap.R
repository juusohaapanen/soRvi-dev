ggooglemap <- function(location, center, API="", type = c("color","bw")[1],
                       rgbcoefs = c(0, 1, 0), zoom = 10, maptype = "map",
                       destfile = "TemporaryMap.jpg", n_pix = 640) {

  # This function is used to get map data from GoogleMaps 
  # using RGoogleMaps-package
  # Modified from 
  # https://github.com/hadley/ggplot2/wiki/Crime-in-Downtown-Houston,-Texas-:-Combining-ggplot2-and-Google-Maps

  # get map
  GetMap(API, 
  	 center = center[c("lat","lon")], 
	 size = c(n_pix, n_pix), 
	 zoom = zoom, 
	 format = "jpg", 
	 maptype = maptype, 
	 destfile = destfile)

  # load map and deal with color
  map <- read.jpeg(destfile)
  map <- apply(map, 1:2, function(v) rgb(v[1], v[2], v[3]))

  # reshape map for plotting
  m_map <- melt(map)
  names(m_map) <- c("x","y","fill")
  m_map <- within(m_map,{
    x <- x - n_pix/2 - 1
    y <- y - n_pix/2 - 1
  })

  mapInfo <- list(lat = center["lat"], lon = center["lon"], zoom = zoom, map)
  XY_cent <- LatLon2XY.centered(mapInfo, center["lat"], center["lon"])

  # geocode pixel references
  s <- (-n_pix/2) : (n_pix/2 - 1)
  lat_wrapper <- function(x) XY2LatLon(mapInfo, -n_pix/2, x)[1]
  lats <- apply(data.frame(s), 1, lat_wrapper)
  lon_wrapper <- function(y) XY2LatLon(mapInfo, y, -n_pix/2)[2]
  lons <- apply(data.frame(s), 1, lon_wrapper)

  # merge colors to latlons and return
  df_xy   <- expand.grid(x = s, y = s)
  df_ll   <- expand.grid(lat = rev(lats), lon = lons)
  df_xyll <- data.frame(df_xy, df_ll)
  df <- suppressMessages(join(df_xyll, m_map, type = "right"))
  df <- df[,c("lon","lat","fill")]
  df
}
