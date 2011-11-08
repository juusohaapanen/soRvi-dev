#.First.lib <- function(lib, pkg)
#{
#   library.dynam('netresponse', pkg, lib)
#   #library.dynam('../src/netresponse', pkg, lib)
#   cat('netresponse loaded')
#}

.onLoad <- function(lib, pkg)
{
   #library.dynam('sorvi', pkg, lib) # for C libraries
  # Load dependent package
  library(gdata)
  library(RgoogleMaps)
  library(ReadImages)
  library(plyr)
  library(png)
  library(maps)
  library(maptools)
  library(XML)
  library(rjson)

  # Install packages if missing
  if (!try(require(RCurl, quietly = TRUE))) {
    install.packages("RCurl")
    require(RCurl)
  }

  if (!try(require(gpclib, quietly = TRUE))) {
    install.packages("gpclib")
    require(gpclib)
  }

  if (!try(require(rgdal, quietly = TRUE))) {
    install.packages("rgdal")
    require(rgdal)
  }

  cat('\nsorvi - avoimen datan tyokalupakki. Copyright (C) 2011 Leo Lahti ja Juuso Parkkinen. Tama on vapaa ohjelmisto, jota voi vapaasti kayttaa, muokata ja levittaa FreeBSD-lisenssilla.\n')
}
