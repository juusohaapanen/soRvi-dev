# This file is a part of the soRvi program
# http://sorvi.r-forge.r-project.org

# Copyright (C) 2011 Joona Lehtomaki <joona.lehtomaki@gmail.com>. All rights 
# reserved.

# This program is open source software; you can redistribute it and/or
# modify it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

.parse.df <- function(df) {
  # Create an empty list to hold the expanded rows as elements
  new.rows <- list()
  # Loop over the rows in the data frame
  for (i in 1:nrow(df)) {
    # Get the current row (2 elements)
    row <- df[i,]
    # Split the 1st elemnt by white space -> ressult in character vector with 
    # 3 or 4 elements (kuntaID, piiriID, piiri (A-part) [piiri (B-part)])
    cell.raw <- unlist(strsplit(row[[1]], "\\s"))
    # Created the expanded verision by joining elemnents 1 and 2 as they are
    # and concatenating elements 3 and 4 (A- and B-part) together if piiri 
    # constitutes of 2 parts
    cell.expanded <- c(cell.raw[1:2], paste(cell.raw[3:length(cell.raw)], 
                                            collapse=' '))
    # Create a new 4 item row by joining the expanded cell with aanestysaluekoodi
    # (also trim the leading white space)
    new.rows[[i]] <- c(cell.expanded, trim(row[[2]]))
  }
  # Merge the list of vectors (new rows) into a matrix
  new.df <- do.call("rbind", new.rows)
  # Create the header
  colnames(new.df) <- c("kuntaID", "piiriID", "piiri", "aanestysaluekoodi")
  # Return a data frame
  return(data.frame(new.df))
}
res <- .parse.df(df1)

#' Retrieve HKK data 
#'
#' This script retrieves data from Helsinki Real Estate Department (Helsingin 
#' kaupunki kiinteistovirasto HKK) through the HKK website
#' http://kartta.hel.fi/avoindata/index.html
#' For details, see the HKK website
#'
#' The data copyright is on Helsingin kaupunkimittausosasto (C)  2011.
#' 
#' @param which.data  A string. Specify the name of the HKK data set to retrieve. Currently available options: Aluejakokartat;Aanestysjakoalue;Seutukartta Rakennustietoruudukko; SeutuRAMAVA; key.KATAKER.
#' @param data.dir A string. Specify the path where to save the downloaded data. A new subdfolder "aanestysalueet" will be created.
#'
#' @author Joona Lehtomäki \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export
GetHKK <- function(which.data, data.dir) {
  # TODO: shold all the urls/paths be defined independently from the functions?
  data.url <- "http://kartta.hel.fi/avoindata/aineistot/"

  if (which.data == "Aanestysjakoalue") {
    if (!require(XLConnect)) {
      try(install.packages("XLConnect"))
    }
    
    # Remote zip that will be downloaded
    remote.zip <- "pk_seudun_aanestysalueet.zip"
    # Location and name of the zip file that will be saved on the local computer
    local.zip <- file.path(find.package("sorvi"), "extdata", remote.zip)
    # Create the web address from where to fetch the zip
    data.url <- paste(data.url, remote.zip, sep = "")
    message(paste("Dowloading HKK data from ", data.url, "in file", local.zip))
    download.file(data.url, destfile = local.zip)
    
    # Unzip the downloaded zip file
    data.dir <- file.path(data.dir, "aanestysalueet")
    unzip(local.zip, exdir=data.dir)
    # Extract the data from respective zips for each city
    city.zips <- list.files(path=data.dir, pattern="*[.zip]$", full.names=TRUE)
    for (city.zip in city.zips) {
      unzip(city.zip, exdir=data.dir)
    }
    
    # Construct the name of the Excel file containing additional data on voting
    # districts
    xls.file.name <- file.path(data.dir, "Aanestyaluekoodit.xls")
    
    xls.sheets <- list('Helsinki'=1, 'Espoo'=2, 'Vantaa'=3, 'Kauniainen'=4)
    aux.dfs  <- lapply(xls.sheets, 
                        function(x) readWorksheetFromFile(xls.file.name, 
                                                          sheet=x,
                                                          header = TRUE, 
                                                          startRow = 2))
    
    # Each auxilary data frame has 2 columns: Aanestysalue and aanestysaluekoodi
    # 1st cell of each row  contains 1) city ID, 2) district ID and 
    # 3) disrict name
    # Parse the 1st cell of each row into 3 individual elements
    aux.dfs <- lapply(aux.dfs, function(x) .parse.df(x))
    
  } else if (which.data == "Aluejakokartat") {
    stop("Not implemented yet; Try GetHRIaluejakokartat instead")
  } else if (which.data == "Seutukartta") {
    stop("Not implemented yet")
  } else {
    stop(paste(which.data, "is not a valid data set descriptor"))
  }
}