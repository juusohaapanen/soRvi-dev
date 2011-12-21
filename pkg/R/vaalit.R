# This file is a part of the soRvi program
# http://sorvi.r-forge.r-project.org

# Copyright (C) 2008-2011 Juuso Parkkinen <juuso.parkkinen@gmail.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# Function for reading the Presidentti2012 data
# For documentation, see http://www2.hs.fi/extrat/hsnext/Vaalikone_API_20111207.pdf

#' Load Presidentti2012 data
#'
#' Load data from Presidentti2012 vaalikone
#' Note! You need a personal API key to get the data!
#'
#' @param category Data category ("questions", "candidates", "useranswers")
#' @param API Personal api key, required
#' @param ID id for the query, optional
#' @param filter filter for the query, required for 'useranswers'-category. ("question", "timerange", "topcandidate")
#' @param page Pagenumber of results to get, optional
#' @param per_page Number of answers to get (500-10000), optional
#' @param show_total Show data information, optional
#'
#' @return res List of data
#' 
#' @author Juuso Parkkinen \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export
getPresidentti2012Data <- function(category=c("questions", "candidates", "useranswers"), 
                                   API, ID=NULL, filter=NULL, page=1, per_page=500, show_total="true") {
  
  library(RCurl)
  library(rjson)
  curl <- getCurlHandle(cookiefile="")
  vaalikone.url <- paste("http://api.vaalikone.fi/presidentti2012/v1/", category, sep="")
  
  # Define parameters based on category
  if (category == "questions") {
    params <- list(api_key=API, id=ID)
    if (!is.null(ID))
      cat("Note! Parameter 'id' doens't work with category 'questions'. Will return all questions.")
  }
  else if (category == "candidates")
    params <- list(api_key=API, id=ID)
  else if (category == "useranswers")
    params <- list(api_key=API, filter=filter, page=page, per_page=per_page, show_total=show_total)  
  else
    stop("Invalid 'category' given!")
  val <- getForm(uri=vaalikone.url, .params=params, curl=curl, .encoding="utf-8")
  res <- fromJSON(val)
  
  # Report error if given
  if (length(res$error) > 0)
    stop(paste("ERROR! code: ", res$error$code, ", message: ", res$error$message, sep=""))
  else
    return(res)
}