# Get geocode using OpenStreetMap
get.geocode.OpenStreetMap <- function(query) {
  
  u <- paste("http://nominatim.openstreetmap.org/search?q=",query,"&format=json", sep="")
  val <- getURI(u)
  res <- fromJSON(val)
  if (length(res)>0)
    return(as.numeric(c(res[[1]]$lat, res[[1]]$lon)))
  else # Geocode not found
    return(NULL)
}