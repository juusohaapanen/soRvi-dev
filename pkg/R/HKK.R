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


.check.schema <- function(sp.object1, sp.object2) {
  
  # Check that both objects derive from Spatial
  if (!is(sp.object1) || !is(sp.object2)) {
    warning("Objects must derive from Spatial class")
    return(FALSE)
  }
  
  # First check that the attribute tables (data frames) have same number of cols
  if (length(sp.object1@data) != sp.object2@data) {
    warning("Spatial objects' data frames are different length")
    return(FALSE)
  } else {
    # Check the attribute tables have the same colnames
    if (names(sp.object1@data) == names(sp.object2@data)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

.pad.zeros <- function(x, target.length=4) {
  x <- as.character(x)
  char.diff <- target.length - nchar(x)
  if (char.diff < 0) {
    warning("The length of a padded string cannot be smaller than the original")
    return(x)
  }
  return(paste(paste(rep("0", char.diff), collapse=""), x, sep=""))
}

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

#' Retrieve HKK data 
#'
#' This script retrieves data from Helsinki Real Estate Department (Helsingin 
#' kaupunki kiinteistovirasto HKK) through the HKK website
#' http://kartta.hel.fi/avoindata/index.html
#' For details, see the HKK website
#'
#' The data copyright is on Helsingin kaupunkimittausosasto (C)  2011.
#' 
#' @param which.data  A string. Specify the name of the HKK data set to retrieve. Currently available options: Aluejakokartat;Aanestysaluejako;Seutukartta Rakennustietoruudukko; SeutuRAMAVA; key.KATAKER.
#' @param data.dir A string. Specify the path where to save the downloaded data. A new subdfolder "aanestysalueet" will be created.
#'
#' @return a list of Shape objects (from SpatialPolygonsDataFrame class)
#' @export
#' @references
#' See citation("sorvi") 
#' @author Joona Lehtomaki \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # sp <- GetHKK("Aanestysaluejako", data.dir="C:/data")

GetHKK <- function(which.data, data.dir) {
  # TODO: shold all the urls/paths be defined independently from the functions?
  data.url <- "http://kartta.hel.fi/avoindata/aineistot/"
  
  if (which.data == "Aanestysaluejako") {
    
    # Remote zip that will be downloaded
    remote.zip <- "pk_seudun_aanestysalueet.zip"
    # Location and name of the zip file that will be saved on the local computer
    local.zip <- file.path(data.dir, remote.zip)
    # Create the web address from where to fetch the zip
    data.url <- paste(data.url, remote.zip, sep = "")
    message(paste("Dowloading HKK data from ", data.url, "in file", local.zip))
    download.file(data.url, destfile = local.zip)
    # Unzip the downloaded zip file
    data.dir <- file.path(data.dir, "aanestysalueet")
    unzip(local.zip, exdir=data.dir)
    
    mapinfo.file <- file.path(data.dir, "PKS_aanestysalueet_kkj2.TAB")
    
    sp.cities <- rgdal::readOGR(mapinfo.file, 
                                 layer = rgdal::ogrListLayers(mapinfo.file))
    
    # Fix encoding
    sp.cities@data$TKNIMI <- factor(iconv(sp.cities@data$TKNIMI, 
                                          from="ISO-8859-1", to="UTF-8"))
    sp.cities@data$Nimi <- factor(iconv(sp.cities@data$Nimi, 
                                        from="ISO-8859-1", to="UTF-8"))
    
    return(sp.cities)
    
  } else if (which.data == "Aluejakokartat") {
    stop("Not implemented yet; Try GetHRIaluejakokartat instead")
  } else if (which.data == "Seutukartta") {
    stop("Not implemented yet")
  } else {
    stop(paste(which.data, "is not a valid data set descriptor"))
  }
}

#' Merge a list of Spatial*DataFrame objects into one Spatial*DataFrame
#'
#' Several independent Spatatial*DataFrame objects held in a list can be merged
#' into one object as long as all are of the same class. CRS 
#' projections will be performed if target CRS is provided. If CRS is not 
#' provided, the CRS of the first object will be used. If even one object is 
#' missing a CRS, no projections are performed and there is no guarantee that
#' merge will produce desired outcome.
#'
#' All schemas must match, a schema is checked on the basis of column names. An
#' optional FID string can be give to name for a field (column) in table schema
#' that will be used as FIDs. It's up to the user to check that FID values are
#' unique.
#'
#' @param sp.list  A list of Spatial*DataFrame objects to be merged
#' @param CRS A proj4string definign target CRS for the target Spatial*DataFrame object
#' @param FID A string that names the column used as FID
#'
#' @return a list of Shape objects (from SpatialPolygonsDataFrame class)
#' @export
#' @seealso spChFIDs, spRbind
#' @references
#' See citation("sorvi") 
#' @author Joona Lehtomaki \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # sp <- GetHKK("Aanestysaluejako", data.dir="C:/data")

MergeSpatial <- function(sp.list, CRS=NA, FID=NA) {
  
  allowed.classes <- c("SpatialPolygonsDataFrame", "SpatialPointsDataFrame",
                       "SpatialLinesDataFrame")
  
  # Test that all the objects in the list are of the same class
  # FIXME: what's the smartest way of testing whether an object is of a 
  # particular class?
  
  # Get the class of the first sp object in the list
  class.type <- class(sp.list[[1]])[[1]]
  
  # Only particular Spatial*DataFrame classes are allowed, here we check that the
  # class of the 1st object is allowed
  if (!class.type %in% allowed.classes) {
    stop(paste("All objects must be instances of one of the following classes: ",
               paste(allowed.classes, collapse=", ")))
  }
  
  # Check that all objects have the same class
  if(all(sapply(sp.list, function(x) is(x, class.type)))) {
    
    # Check that all objects have the same schema
    ref.schema <- sp.list[[1]]
    
    # Compare the schema of all the other objects
    # FIXME: now the first object is compared against itself, this is a bit 
    # stupid
    if(all(sapply(sp.list, function(x) .check.schema(ref.schema, x)))) {
      if (is.na(FID)) {
        # No FID field is provided, so create FID fields for all attribute
        # tables (data frames). FID values must be unique over all tables.
        row.fids <- 1:sum(sapply(sp.list, function(x) nrow(x)))
        for (sp.object in sp.list) {
          rows <- nrow(sp.object)
          FID <- row.fids[1:rows]
          sp.object@data <- spChFIDs(sp.object@data, FID)
          # Remove the used FIDs
          row.fids <- row.fids[rows:length(row.fids)]
        }
        return(do.call("spRbind", sp.list))
        
      } else {
        NULL
      }
    }
       
  } else {
    stop(paste("All objects must be instances of one of the following class: ",
               class.type))
  }
}

