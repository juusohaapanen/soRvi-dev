# To call in the statistician after the experiment is done may be no more
# than asking him to perform a post-mortem examination: he may be able to
# say what the experiment died of. ~ Sir Ronald Aylmer Fisher

# This file is a part of the soRvi program
# http://sorvi.r-forge.r-project.org

# Copyright (C) 2011 Leo Lahti <leo.lahti@iki.fi>. All rights reserved.

# This program is open source software; you can redistribute it and/or
# modify it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Sort data frame
#'
#' @param df data.frame to be sorted by the specified columns
#' @param sortvar variable/s according which the data.frame shall be sorted
#' @param ... Other arguments to pass
#' @return data.frame (sorted version)
#' @export
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # dfsort(df, x, -z) 
#' @keywords utilities


dfsort <- function(df, sortvar, ...) {
  # Korvaa skandit a/o versiolla
  attach(df)
  df <- df[with(df,order(sortvar,...)),]
  return(df)
  detach(df)

}


#' Replace special characters with standard ones.
#'
#' @param s string from which the special chars should be removed
#' @return string with special chars replaced by standard ones
#' @export
#' @note iconv function provides better tools for these purposes and is now the main tool
#' This function is kept for compatibility with the older versions.
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # korvaa.skandit("my.string.here") # if no, special chars, the same string is returned
#' @keywords utilities

korvaa.skandit <- function (s) {

  s <- gsub("\\xe4", "a", s)
  s <- gsub("\\xC4", "A", s)
  s <- gsub("\\xD6", "O", s)
  s <- gsub("\\xf6", "o", s)
  s <- gsub("\\xE5", "a", s)
  s <- gsub("\\U3e34633c", "A", s)

  s

}

#' Check if the given object is an url string
#'
#' Arguments:
#'  @param s input object to check
#'
#' Returns:
#'  @return TRUE/FALSE indicating whether the input string is a valid URL.
#'
#' @export
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # is.url("http://aa.px")
#' @keywords utilities
is.url <- function (s) {
  (class(s) == "character" && substr(s,1,7) == "http://")
}


#' Retrieve shape objects by their file names.
#'  
#' Takes list of shape file names (or IDs without the .shp ending).
#' Returns a corresponding list of shape objects from the working directory, 
#' or from the directory path specified as part of the file name.
#'
#' @param files vector of input files
#' @param proj4string projection information
#' @return shape object, or a list of shape objects, depending on the length of function argument (a single file name vs. multiple file names)
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # ReadShape(files)
#' @keywords utilities

ReadShape <- function (files, proj4string = NA) {

  ids <- unlist(sapply(files, function (x) {strsplit(x, "\\.")[[1]][[1]]}))
   
  shapedata <- list()

  for (id in ids) {
    print(id)
    shapedata[[id]] <- try(readShapePoly(id, 
                                         proj4string=CRS(as.character(proj4string))))
  }

  # If just one file converted, give directly the shape file as out put
  # (and not a list)
  if (length(files) == 1) {
    shapedata <- shapedata[[1]]
  }

  shapedata

}

#' Remove spaces from a string (single string or vector/list of strings).
#'
#' @param s string or vector/list of strings
#' @return string without spaces
#' @export
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # strstrip("a b") # returns "ab"
#' @keywords utilities


strstrip <- function (s) {

  if (length(s) == 1) {
    stripped <- strstrip.single(s)
  } else {
    stripped <- sapply(s, strstrip.single)
  }

  stripped
}


#' Remove spaces from a single string
#'
#' @param s string
#' @return string without spaces
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # strstrip.single("a b") # returns "ab"
#' @keywords utilities

strstrip.single <- function (s) {

  # Remove spaces from a string

  # (C) Leo Lahti 2008-2011
  # FreeBSD license (keep this notice)

  # Strip string i.e. remove spaces from the beginning and end
  while (substr(s,1,1)==" ") {
    s <- substr(s,2,nchar(s))
  }
  while (substr(s,nchar(s),nchar(s))==" ") {
    s <- substr(s,1,nchar(s)-1)
  }
  s
}
