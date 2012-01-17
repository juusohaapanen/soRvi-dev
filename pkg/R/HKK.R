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
#' @author Joona Lehtomäki \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export

GetHKK <- function(which.data) {
  data.url <- "http://kartta.hel.fi/avoindata/index.html"

  destfile <- paste(which.data, "_SHP.zip", sep = "")
  data.url <- paste(data.path, which.data, "_SHP.zip", sep = "")
  message(paste("Dowloading HSY data from ", data.url, " in file ", destfile))
  download.file(data.url, destfile = destfile)
    
  }
}