# soRvi-pakettia varten suositeltujen pakettien asennusskripti

# Kayttoohjeet: 
# 1) Asenna uusin R-versio (r-project.org)
# 2) Kaynnista R
# 3) Aja tama skripti R:n komentorivilta kirjoittamalla
#    source("http://sorvi.r-forge.r-project.org/examples/sorvi.installation.R")
#    (pakettien asennus edellyttaa toimivaa verkkoyhteytta)

# Suppress error messages during installation
silent <- TRUE

# Install the packages
if (!require(gdata)) {try(install.packages("gdata"), silent = silent)}
if (!require(ggplot2)) {try(install.packages("ggplot2"), silent = silent)}
if (!require(gpclib)) {try(install.packages("gpclib"), silent = silent)}
if (!require(gridExtra)) {try(install.packages("gridExtra"), silent = silent)}
if (!require(mapproj)) {try(install.packages("mapproj"), silent = silent)}
if (!require(maps)) {try(install.packages("maps"), silent = silent)}
if (!require(maptools)) {try(install.packages("maptools"), silent = silent)}
if (!require(plyr)) {try(install.packages("plyr"), silent = silent)}
if (!require(png)) {try(install.packages("png"), silent = silent)}
if (!require(pxR)) {try(install.packages("pxR"), silent = silent)}
if (!require(RCurl)) {try(install.packages("RCurl"), silent = silent)}
if (!require(ReadImages)) {try(install.packages("ReadImages"), silent = silent)}
if (!require(rgdal)) {try(install.packages("rgdal"), silent = silent)}
if (!require(rgeos)) {try(install.packages("rgeos"), silent = silent)}
if (!require(rgl)) {try(install.packages("rgl"), silent = silent)}
if (!require(RgoogleMaps)) {try(install.packages("RgoogleMaps"), silent = silent)}
if (!require(rjson)) {try(install.packages("rjson"), silent = silent)}
if (!require(rworldmap)) {try(install.packages("rworldmap"), silent = silent)}
if (!require(sp)) {try(install.packages("sp"), silent = silent)}
if (!require(XML)) {try(install.packages("XML"), silent = silent)}
#if (!require()) {try(install.packages(""), silent = silent)}


#install.packages("sorvi", repos="http://R-Forge.R-project.org", type = "source", dependencies = TRUE)

# This is a temporary solution for Mac/Linux on 8.12.2011, 
# waiting for R-Forge maintenance break to be over.
download.file(“http://roihu.info/temp/sorvi/sorvi_latest.tar.gz”, destfile = “sorvi_latest.tar.gz”)
install.packages(“sorvi_latest.tar.gz”)
