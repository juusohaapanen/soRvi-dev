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
#'
#' @author Joona Lehtomäki \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export
GetHKK <- function(which.data) {
  # TODO: shold all the urls/paths be defined independently from the functions?
  data.url <- "http://kartta.hel.fi/avoindata/aineistot/"

  if (which.data == "Aanestysjakoalue") {
    # Remote zip that will be downloaded
    remote.zip <- "pk_seudun_aanestysalueet.zip"
    # Location and name of the zip file that will be saved on the local computer
    local.zip <- file.path(find.package("sorvi"), "extdata", remote.zip)
    # Create the web address from where to fetch the zip
    data.url <- paste(data.url, remote.zip, sep = "")
    message(paste("Dowloading HKK data from ", data.url, "in file", local.zip))
    download.file(data.url, destfile = local.zip)
    
    # Construct the name of the Excel file containing additional data on voting
    # districts
    xls.file.name <- file.path(find.package("sorvi"), "extdata", 
                               "Aanestyaluekoodit.xls")
    
    xls.sheets <- list('Helsinki'=1, 'Espoo'=2, 'Kauniainen'=3, 'Vantaa'=4)
    aux.dfs  <- lapply(xls.sheets, function(x) read.xls(xls.file.name, sheet = x))
    
  } else if (which.data == "Aluejakokartat") {
    stop("Not implemented yet; Try GetHRIaluejakokartat instead")
  } else if (which.data == "Seutukartta") {
    stop("Not implemented yet")
  } else {
    stop(paste(which.data, "is not a valid data set descriptor"))
  }
}