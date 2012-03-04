# This file is a part of the soRvi program
# http://sorvi.r-forge.r-project.org

# Copyright (C) 2008-2012 Leo Lahti <leo.lahti@iki.fi>. All rights reserved.

# This program is open source software; you can redistribute it and/or
# modify it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#' Strip string i.e. remove spaces from the beginning and end
#' @param s string or character vector
#'
#' @return Stripped string
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples 
#' #s2 <- Strip(s) 
#' @keywords utilities
Strip <- function (s) {

  ss <- c()
  for (i in 1:length(s)) {
    si <- s[[i]]

    # Strip string i.e. remove spaces from the beginning and end
    while (substr(si,1,1)==" ") {
      si <- substr(si, 2, nchar(si))
    }
    while (substr(si, nchar(si), nchar(si))==" ") {
      si <- substr(si, 1, nchar(si) - 1)
    }
    ss[[i]] <- si
  }
  ss
}
