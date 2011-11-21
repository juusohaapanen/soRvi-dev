# soRvi-pakettia varten suositeltujen pakettien asennusskripti


if (!require(gdata)) {try(install.packages("gdata"))}
if (!require(ggplot2)) {try(install.packages("ggplot2"))}
if (!require(gpclib)) {try(install.packages("gpclib"))}
if (!require(mapproj)) {try(install.packages("mapproj"))}
if (!require(maps)) {try(install.packages("maps"))}
if (!require(maptools)) {try(install.packages("maptools"))}
if (!require(plyr)) {try(install.packages("plyr"))}
if (!require(png)) {try(install.packages("png"))}
if (!require(pxR)) {try(install.packages("pxR"))}
if (!require(ReadImages)) {try(install.packages("ReadImages"))}
if (!require(RgoogleMaps)) {try(install.packages("RgoogleMaps"))}
if (!require(rjson)) {try(install.packages("rjson"))}
if (!require(sp)) {try(install.packages("sp"))}
if (!require(XML)) {try(install.packages("XML"))}
if (!require(RCurl)) {try(install.packages("RCurl")})
if (!require(rgdal)) {try(install.packages("rgdal"))}
if (!require(rgeos)) {try(install.packages("rgeos"))}
if (!require(rgl)) {try(install.packages("rgl"))}
if (!require(rworldmap)) {try(install.packages("rworldmap"))}
if (!require(gridExtra)) {try(install.packages("gridExtra"))}
#if (!require()) {try(install.packages(""))}

install.packages("sorvi", repos="http://R-Forge.R-project.org", type = "source", dependencies = TRUE)

