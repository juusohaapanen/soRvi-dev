get.postinumerot <- function () {

  # Palauttaa data.framen, jossa postinumerot ja suomen kunnat on mapatty
  # Manuaalista parsintaa wikipediasta

  url <- "http://fi.wikipedia.org/wiki/Luettelo_Suomen_postinumeroista_kunnittain"

  # Lue sivun lahdekoodi
  txt <- readLines(url)

  # Poimi lista 
  txt <- txt[grep("^<li>", txt)]

  # Eriyta kuntanimet ja yksityiskohtaisemmat paikannimet / postinumerot
  cnt <- 0
  map <- list()
  for (i in 1:length(txt)) {
    li <- txt[[i]]
    if (substr(li, 1, 11) == "<li><a href") {            
      # Parsi kunnan nimi otsikkorivilta
      kunta <- unlist(strsplit(unlist(strsplit(li, ">"))[[3]], "<"))[[1]]
    } else {
      tmp1 <- unlist(strsplit(li, ">"))[[2]]      
      tmp0 <- unlist(strsplit(tmp1, "/"))
      postinro <- unlist(strsplit(tmp0[[1]], " "))[[1]] 
      cnt <- cnt + 1
      map[[cnt]] <- c(postinro, kunta)
    }
  }

  library(plyr)
  map <- ldply(map)
  colnames(map) <- c("postinumero", "kunta")
  map$kunta.skanditon <- korvaa.skandit(map$kunta)

  # Poista viimeinen rivi
  map <- map[-nrow(map),]

  map
}
