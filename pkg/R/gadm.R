
kunta2maakunta <- function (kunnat, map) {

  # Mappaa kunnat maakuntiin
  # map <- aluetaulukko.suomi()
  v <- korvaa.skandit(map$maakunta[match(kunnat, map$kunta)])
  names(v) <- kunnat

  v
}


aluetaulukko.suomi <- function () {

  # Hae mappays kunta-maakunta-laani

  url.gadm <- "http://gadm.org/data/rda/" # URL for GADM R data
  con <- url(paste(url.gadm, "FIN_adm", 4, ".RData", sep=""))
  print(load(con))
  close(con)

  data.frame(list(laani = gadm$NAME_1, maakunta = gadm$NAME_2, kunta = gadm$NAME_4))

}


hae.gadm <- function (alue = "FIN_adm", taso = 4) {

  # see http://ryouready.wordpress.com/2009/11/16/infomaps-using-r-visualizing-german-unemployment-rates-by-color-on-a-map/   # http://r-spatial.sourceforge.net/gallery/ 
  # url <- "http://gadm.org/data/rda/FIN_adm"

  # Ladataan Suomen kartta, joka on jaettu kuntiin
  # FIXME: lisaa muut tasot myoh.
  if (taso == "laanit") {taso <- 1}
  if (taso == "maakunnat") {taso <- 2}
  if (taso == "kunnat") {taso <- 4}

  url.gadm <- "http://gadm.org/data/rda/" # URL for GADM R data
  con <- url(paste(url.gadm, alue, taso, ".RData", sep=""))
  print(load(con))
  close(con)

  # Putsaa nimet
  if (taso == 4) {
    if (any(duplicated(gadm$NAME_4))) {
      warning("Poistetaan duplikaatit")
      gadm <- gadm[!duplicated(gadm$NAME_4),] # Poista duplikaatit
    }
    # FIXME: etsi tapa sisallyttaa skandit R-pakettiin
    warning("Poistetaan skandit")
    gadm.kunnat <- as.character(gadm$NAME_4)
    gadm.kunnat <- gsub("\xe4", "a", gadm.kunnat)
    gadm.kunnat <- gsub("\xf6", "o", gadm.kunnat)
    gadm.kunnat <- gsub("\U3e34633c", "A", gadm.kunnat)

    gadm$kunnat <- gadm.kunnat

  }

  gadm

}  


gadm.position2region <- function (x = c(24.9375, 24.0722), y = c(60.1783, 61.4639)) {

  # Modified from http://www.r-ohjelmointi.org/?p=894
 
  # Muodostetaan ensin data frame, jossa ovat koordinaatit
  library(sp)
  
  dat<-data.frame(id=c("a", "b"), x=x, y=y)
  coordinates(dat) = ~x+y

  # Ladataan Suomen kartta, joka on jaettu laaneihin
  gadm <- hae.gadm(alue = "FIN_adm", taso = 1)
  province <- overlay(gadm, dat)

  # Ladataan Suomen kartta, joka on jaettu maakuntiin
  gadm <- hae.gadm(alue = "FIN_adm", taso = 2)
  region<-overlay(gadm, dat)

  # Ladataan Suomen kartta, joka on jaettu kuntiin
  gadm <- hae.gadm(alue = "FIN_adm", taso = 4)
  municipality <- overlay(gadm, dat)

  # Yhdistetaan tiedot yhdeksi data frameksi
  df <- data.frame(dat, province = province$VARNAME_1, region = region$VARNAME_2, municipality = municipality$NAME_4)

  df

}


