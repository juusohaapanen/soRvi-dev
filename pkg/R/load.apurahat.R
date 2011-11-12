load.apurahat <- function() {
  # Script for processing apurahat data
  # License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses
  # Copyright 2011 Juuso Parkkinen, juuso.parkkinen@gmail.com. All rights reserved.
 
  cat("Loading apurahat-data...")
  # Load the apuraha-data in csv-format from the HS Next page
  apurahat <- read.csv("http://www2.hs.fi/extrat/hsnext/apurahat-2005-2010.csv", sep=";", quote="", fileEncoding="ISO-8859-1")

  # Remove canceled application
  apurahat <- apurahat[-c(13226, 13227),]

  # There are several things to fix in the data, here some of them are taken care of
  apurahat$Myontosumma.EUR <- as.numeric(gsub("\\,", "\\.", gsub(" ", "", apurahat[[10]])))
  apurahat$Hakemusluokka[apurahat$Hakemusluokka %in% c("säveltajat", "esittävä sävelt")] <- "säveltaide"
  apurahat$Hakemusluokka[apurahat$Hakemusluokka == ""] <- c(rep("kuvataide", 5), "kirjallisuus")
  apurahat$Hakemusluokka <- droplevels(apurahat$Hakemusluokka)
  levels(apurahat$Hakemusluokka) <- toupper(levels(apurahat$Hakemusluokka))
  apurahat$Hakemusluokka <- reorder(apurahat$Hakemusluokka, apurahat$Myontosumma.EUR, sum)
  
  apurahat$Maakunta[apurahat$Maakunta=="PIRKANMAAN MAAKUNTA"] <- "PIRKANMAA"
  apurahat$Maakunta[apurahat$Maakunta=="HÄME"] <- "KANTA-HÄME"
  apurahat$Maakunta[apurahat$Maakunta=="Åland"] <- "AHVENANMAA"
  apurahat$Maakunta[apurahat$Maakunta %in% c("", " ")] <- "ULK"
  levels(apurahat$Maakunta)[24] <- "ULKOMAA"
  apurahat$Maakunta <- droplevels(apurahat$Maakunta)
  apurahat$Maakunta <- reorder(apurahat$Maakunta, apurahat$Myontosumma.EUR, sum)
  
  apurahat$Kotipaikka[apurahat$Kotipaikka=="HELSINGFORS"] <- "HELSINKI"
  apurahat$Kotipaikka <- droplevels(apurahat$Kotipaikka)
  apurahat$Sukupuoli[apurahat$Sukupuoli==""] <- c("M", "M", "M", NA, "M", "M", NA, "M", "N", "N", "N", "N", "M", "N", "N", "N", "N", "N", "M", "N", "M", "N")
  apurahat$Sukupuoli <- droplevels(apurahat$Sukupuoli)
  
  # Compute years of birth and construct age groups
  apurahat$Syntymavuosi <- as.numeric(as.vector(apurahat[[3]]))
  apurahat$Syntymavuosi[apurahat$Syntymavuosi==1902] <- 1962 # Jukka Tapio Manninen syntynyt 1962 eika 1902
  apurahat$Ika <- apurahat$Vuosi - apurahat$Syntymavuosi
  apurahat$Ikaryhma <- cut(apurahat$Ika, breaks=c(min(apurahat$Ika, na.rm=T), seq(20, 80, by=5), max(apurahat$Ika, na.rm=T)))
  cat("DONE\n")
  return(apurahat)
}