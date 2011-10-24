library(rgdal)
library(gpclib)
library(gdata)

preprocess.PKS.aluejakokartat <- function() {
# Script for processing PKS aluejakokartat data
# License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses
# Copyright 2011 Juuso Parkkinen, juuso.parkkinen@gmail.com. All rights reserved.

# Need to install package rgdal
# Mac users, see http://www.r-bloggers.com/installing-rgdal-on-mac-os-x-2/
gpclibPermit()

# Download KML files from http://www.hri.fi/fi/data/paakaupunkiseudun-aluejakokartat/
# Substitute manually "xsd:sting" in <SimpleField type="xsd:string" name="KOKOTUN"> and other similar fields with "string" to read whole metadata
pks.pienalue <- readOGR(dsn="data/PKS_Kartta_Rajat_KML2011/PKS_pienalue2.kml", layer="pks_pienalue")

pks.pienalue@data$id <- rownames(pks.pienalue@data) # Add IDs
pks.points <- fortify.SpatialPolygonsDataFrame(pks.pienalue, region="id") # Get point data
pks.points$group <- sub(".1", "", pks.points$group) # Regex PIEN to joinable format

pks.df <- merge(pks.points, pks.pienalue@data, by.x="group", by.y = "id") # Put everything together
pks.df <- pks.df[order(pks.df$order),] # sort DF so that polygons come out in the right order

# Fix encoding
pks.df$Nimi <- factor(iconv(pks.df$Nimi, from="ISO-8859-1", to="UTF-8"))
pks.df$NIMI_ISO <- factor(iconv(pks.df$NIMI_ISO, from="ISO-8859-1", to="UTF-8"))
pks.df$Name <- factor(iconv(pks.df$Name, from="ISO-8859-1", to="UTF-8"))
pks.pienalue@data$Nimi <- factor(iconv(pks.pienalue@data$Nimi, from="ISO-8859-1", to="UTF-8"))
pks.pienalue@data$NIMI_ISO <- factor(iconv(pks.pienalue@data$NIMI_ISO, from="ISO-8859-1", to="UTF-8"))
pks.pienalue@data$Name <- factor(iconv(pks.pienalue@data$Name, from="ISO-8859-1", to="UTF-8"))

# Save data
save(pks.pienalue, pks.df, file="data/PKS_aluejakokartat.rda")
}
