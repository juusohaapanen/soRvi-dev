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
  library(png)
  library(maptools)
  library(XML)
  library(RCurl)
  library(rjson)
  cat('\nsorvi - avoimen datan tyokalupakki. Copyright (C) 2011 Leo Lahti ja Juuso Parkkinen. Tama on vapaa ohjelmisto, jota voi vapaasti kayttaa, muokata ja levittaa FreeBSD-lisenssilla.\n')
}
