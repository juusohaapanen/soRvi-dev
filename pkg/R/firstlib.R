#.First.lib <- function(lib, pkg)
#{
#   library.dynam('netresponse', pkg, lib)
#   #library.dynam('../src/netresponse', pkg, lib)
#   cat('netresponse loaded')
#}

.onLoad <- function(lib, pkg)
{
  #require.dynam('sorvi', pkg, lib) # for C libraries
  # Load dependent package
  #require(gdata, quietly = TRUE)
  #require(maps, quietly = TRUE)
  #require(maptools, quietly = TRUE)
  #require(plyr, quietly = TRUE)
  #require(png, quietly = TRUE)
  #require(RgoogleMaps, quietly = TRUE)
  #require(ReadImages, quietly = TRUE)
  #require(rjson, quietly = TRUE)
  #require(sp, quietly = TRUE)
  #require(XML, quietly = TRUE)

  packageStartupMessage('\nsoRvi - avoimen datan tyokalupakki. Copyright (C) 2011 Leo Lahti ja Juuso Parkkinen. Tama on vapaa ohjelmisto, jota voi vapaasti kayttaa, muokata ja levittaa FreeBSD-lisenssilla.\n')
}
