# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# This file is a part of the soRvi program
# http://sorvi.r-forge.r-project.org

#' Preprocess 'lukio data
#'
#' Preprocess data about Finnish high school performance in year 2011
#' 
#' @author Juuso Parkkinen \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export
preprocess.PKS.lukiot <- function() {

  # Script for processing Finnish school data
  # License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses
  # Copyright 2011 Juuso Parkkinen, juuso.parkkinen@gmail.com. All rights reserved.
  
  library(XML)
  library(gdata)
  
  # Read data about high school performance (HS 31.5.2011)  
  u <- "http://www.hs.fi/kotimaa/artikkeli/1135266565425"
  tables <- readHTMLTable(u)
  lukiot <- tables[[1]]
  
  # Fix some fields and remove broken ones
  lukiot$Ylioppilaita <- as.numeric(as.vector(lukiot[[4]]))
  lukiot$Keskiarvo <- round(as.numeric(gsub(",", ".", as.vector(lukiot[[5]]))), digits=1)
  lukiot$Ranking <- paste(as.vector(lukiot[[1]]), ".", sep="")
  lukiot <- lukiot[-c(1, 4, 5)]
  
  # Get subset of Helsinki region schools, drop "aikuislukiot"
  hr.lukiot <- subset(lukiot, Kunta %in% c("Helsinki", "Espoo", "Vantaa", "Kauniainen"))
  hr.lukiot <- subset(hr.lukiot, Kunta %in% c("Helsinki", "Espoo", "Vantaa", "Kauniainen"))
  hr.lukiot <- hr.lukiot[-grep("aikuis", hr.lukiot$Koulu),]
  hr.lukiot <- drop.levels(hr.lukiot)
  
  # Query OSM with high school names
  hr.lukiot$lon <- hr.lukiot$lat <- NA
  lukio.names <- tolower(paste(gsub(" ", "+", hr.lukiot$Koulu), hr.lukiot$Kunta, sep=","))
  for (i in 1:length(lukio.names)) {
    Sys.sleep(1)
    latlon <- get.geocode.OpenStreetMap(lukio.names[i])
    if (!is.null(latlon)) {
      hr.lukiot$lat[i] <- latlon[1]
      hr.lukiot$lon[i] <- latlon[2]
    }
  }
  
  # Some coordinates are missing
  # hr.lukiot$Koulu[is.na(hr.lukiot$lon)]
  # Get mising coordinates manually with school addresses (should find a better way!)
  addresses <- c("Kevatkatu+2,Helsinki", "Kalevankatu+8,Helsinki", "Urheilukatu+10-12,Helsinki", 
                 "Kettutie+6,Helsinki", "Mantytie+14,Helsinki", "Laajalahdentie+21,Helsinki", 
                 "Sandelsgatan+3,Helsinki", "Unioninkatu+2,Helsinki", "Elevhemsvagen+23,Grankulla",
                 "Kasavuorentie+1,Kauniainen", "Pietari+Hannikaisen+tie+6,Helsinki", "Martinlaaksontie+36,Vantaa",
                 "Ylastontie+3,Vantaa", "Sotungintie+19,Vantaa", "Lucina+Hagmanin+polku+4,Helsinki",
                 "Moisiontie+3,Helsinki", "Makipellontie+19,Helsinki", "Louhentie+3,Helsinki",
                 "Arkadiankatu+26,Helsinki", "Rintinpolku+2,Helsinki", "Arentipolku+1+,Helsinki")
  
  # Query OSM with high school addresses
  lats <- lons <- rep(NA, length(addresses))
  for (i in 1:length(addresses)) {
    Sys.sleep(1)
    latlon <- get.geocode.OpenStreetMap(addresses[i])
    if (!is.null(latlon)) {
      lats[i] <- latlon[1]
      lons[i] <- latlon[2]
    }
  }
  
  # Add the rest of the coordinates
  stopifnot(length(which(is.na(hr.lukiot$lon))) == length(lons))
  hr.lukiot$lat[is.na(hr.lukiot$lat)] <- lats
  hr.lukiot$lon[is.na(hr.lukiot$lon)] <- lons
  
  # Convert to UTF-8
  hr.lukiot$Koulu <- factor(iconv(hr.lukiot$Koulu, from="ISO-8859-1", to="UTF-8"))
  
  # Save final data
  save(hr.lukiot, file="data/PKS_lukiot.rda")
}
