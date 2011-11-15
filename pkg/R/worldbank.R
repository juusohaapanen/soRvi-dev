# Scripts for loading World Bank datasets
# License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses
# Copyright 2011 Juuso Parkkinen, juuso.parkkinen@gmail.com. All rights reserved.

load.migration.data <- function(countries) {
  
  library(gdata)
  
  # Load migration matrix from World Bank
  migration.matrix <- read.xls("http://siteresources.worldbank.org/INTPROSPECTS/Resources/334934-1110315015165/T1.Estimates_of_Migrant_Stocks_2010.xls")
  data.inds <- 2:214
  
  # Get migration flow in to and out from the given countries
  migration.dat <- list()
  for (i in 1:length(countries)) {
    cat("Loading migration data for:", countries[i])
    if (!any(migration.matrix[1,]==countries[i])) {
      cat("- Error: country not found!\n")
    } else {
      migration.in <- as.vector(migration.matrix[data.inds, migration.matrix[1,]==countries[i]])
      migration.in <- as.numeric(gsub(",", "", migration.in))
      migration.out <- as.vector(as.matrix(migration.matrix[migration.matrix[,1]==countries[i], data.inds]))
      migration.out <- as.numeric(gsub(",", "", migration.out))
      migration.dat[[i]] <- data.frame(In=migration.in, Out=migration.out)
      cat("- DONE\n")
    }
  }
  if (length(migration.dat) > 0)
    names(migration.dat) <- countries
  else
    stop("No data found - quitting\n")
  
  # Add country name information
  migration.dat$Country <- as.vector(migration.matrix[data.inds, 1])
  
  # Add manually alternative country names for some countries (based on names used in rworldmap)
  countries.alternative <- as.vector(migration.matrix[data.inds, 1])
  alt.names <- c("Bahamas", "Channel Islands", "Democratic Republic of the Congo", "Congo",
                 "Cote d'Ivoire", "Egypt", "Faroe Islands", "Gambia",
                 "Hong Kong", "Iran (Islamic Republic of)", "Korea, Democratic People's Republic of", "Korea, Republic of",
                 "Kosovo", "Kyrgyzstan", "Lao People's Democratic Republic", "Libyan Arab Jamahiriya",
                 "Macau", "The former Yugoslav Republic of Macedonia", 
                 "Micronesia, Federated States of", "Republic of Moldova",
                 "Burma", "Russia", "Sao Tome and Principe", "Slovakia",
                 "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "United Republic of Tanzania",
                 "Venezuela", "Viet Nam", "United States Virgin Islands", "Palestine",
                 "Yemen")
  countries.alternative[c(14, 39, 44, 45, 47, 57, 63, 69, 84, 89, 101, 102, 103, 
                          105, 106, 111, 115, 116, 128, 129, 135, 160, 164, 171, 
                          178, 179, 180, 188, 207, 208, 209, 210, 211)] <- alt.names
  migration.dat$CountryAlternative=countries.alternative
  return(migration.dat)
}
