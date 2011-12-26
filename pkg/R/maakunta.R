# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# This file is a part of the soRvi program
# http://sorvi.r-forge.r-project.org

#' Get Information about Finnish province
#'
#' Preprocess data about Finnish high school performance in year 2011
#' 
#' @return maakuntakartta data frame with map and info of the provinces
#' 
#' @author Juuso Parkkinen \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export
GetMaakuntainfo <- function() {
  
  cat("Loading maakuntakartta-data...")
  library(sp)
  con <- url("http://gadm.org/data/rda/FIN_adm2.RData", encoding="UTF-8")
  load(con)
  close(con)
    
  # Fix province names
  gadm@data$Maakunnat <- factor(gsub("\xe4", "?", gadm@data$NAME_2))
  levels(gadm@data$Maakunnat) <- c("KESKI-SUOMI", "KESKI-POHJANMAA", "ITÄ-UUSIMAA", "VARSINAIS-SUOMI", "KAINUU", "KYMENLAAKSO", "LAPPI", "POHJOIS-KARJALA","POHJOIS-SAVO", "POHJOIS-POHJANMAA", "POHJANMAA", "PIRKANMAA", "PÄIJÄT-HÄME", "SATAKUNTA", "ETELÄ-KARJALA", "ETELÄ-POHJANMAA", "ETELÄ-SAVO", "KANTA-HÄME", "UUSIMAA")
  
  # Change to ggplot2-format with fortify
  library(ggplot2)
  if (!gpclibPermitStatus())
    gpclibPermit()
  maakuntakartta <- fortify(gadm, region="Maakunnat")
  
  # Remove ITÄ-UUSIMAA
  maakuntakartta$id[maakuntakartta$id=="ITÄ-UUSIMAA"] <- "UUSIMAA"
  
  # Add data of province population sizes
  # Data obtained manually from Tilastokeskus
  temp <- data.frame(maakunta=sort(unique(maakuntakartta$id)))
  temp$asukasluku <- c(133703, 193504, 154668, 82073, 174555,
                       68321, 273637, 182382, 183488, 487923,
                       177946, 165866, 394965, 247943, 201772,
                       227031, 1532309, 465183)
  maakuntakartta$asukasluku <- temp$asukasluku[match(maakuntakartta$id, temp$maakunta)]
  cat("DONE\n")
  return(maakuntakartta)
}
  
