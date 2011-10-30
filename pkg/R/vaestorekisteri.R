get.vaestorekisteri <- function (url = "http://vrk.fi/default.aspx?docid=5127&site=3&id=0") {

  # Hakee kuntien asukasluvut vaestotietojarjestelmasta
  # Osoite pitanee muuttaa paivitysten mukana:
  # "http://vrk.fi/default.aspx?docid=5127&site=3&id=0"

  require(XML)

  # Read tables from the website
  tables <- readHTMLTable(url)

  # Population is in table 4
  pop <- tables[[4]]

  # Putsaa data
  pop <- pop[-c(1, (nrow(pop):(nrow(pop)-1))),] # Poista ruotsinkielinen otsikkorivi ja yhteenvetorivi

  # Valkkaa vain informatiiviset sarakkeet
  pop <- pop[, c(1, 4, 5, 6)]
  colnames(pop) <- c("Kunta", "Miehet", "Naiset", "Yhteensa")
  # Eriyta kuntakoodi ja suomen- ja ruotsinkieliset nimet
  parser <- function (x) { 
    x <- as.character(x)
    x <- unlist(strsplit(unlist(strsplit(x, "\r\n")), " - "))
    x <- unname(strstrip(x))  
  }
  nimet <- t(sapply(pop[[1]], function (x) {parser(x)}))
  # Convert counts to numeric
  pop <- data.frame(apply(data.frame(pop[, -1]), 2, function (x){as.numeric(as.character(x))}))
  # Talleta kaikki dataframeen
  df <- data.frame(list(nimet, pop))
  colnames(df)[1:3] <- c("Koodi", "Kunta", "Kommun")
  # Muuta skandit pois
  kuntanimet <- as.character(df$Kunta)
  kuntanimet <- gsub("\\xe4", "a", kuntanimet)
  kuntanimet <- gsub("\\xC4", "A", kuntanimet)
  kuntanimet <- gsub("\\xD6", "O", kuntanimet)
  kuntanimet <- gsub("\\xf6", "o", kuntanimet)
  kuntanimet <- gsub("\\xE5", "a", kuntanimet)
  rownames(df) <- kuntanimet

  df

}

