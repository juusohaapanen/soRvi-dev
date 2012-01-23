# This file is a part of the soRvi program
# http://sorvi.r-forge.r-project.org

# Copyright (C) 2008-2012 Juuso Parkkinen and Leo Lahti 
# <sorvi-commits@lists.r-forge.r-project.org>. 
# All rights reserved.

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# Function for reading the Presidentti2012 data
# For documentation, see
# http://www2.hs.fi/extrat/hsnext/Vaalikone_API_20111207.pdf

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
GetPresidentti2012 <- function(category=c("questions", "candidates", "useranswers"), 
                               API, ID=NULL, filter=NULL, page=1, per_page=500, 
			       show_total="true") {

#category=c("questions", "candidates", "useranswers"); ID=NULL; filter=NULL; page=1; per_page=500; show_total="true"

  library(RCurl)
  library(rjson)
  curl <- getCurlHandle(cookiefile="")
  vaalikone.url <- paste("http://api.vaalikone.fi/presidentti2012/v1/", category, sep="")
  
  # Define parameters based on category
  if (category == "questions") {
    params <- list(api_key=API, id=ID)
    if (!is.null(ID))
      cat("Note! Parameter 'id' doens't work with category 'questions'. Will return all questions.")
  } else if (category == "candidates") {
    params <- list(api_key=API, id=ID)
  } else if (category == "useranswers") {
    params <- list(api_key=API, filter=filter, page=page, per_page=per_page, show_total=show_total)  
  } else {
    stop("Invalid 'category' given!")
  }
  val <- getForm(uri = vaalikone.url, .params = params, curl = curl, .encoding = "utf-8")
  res <- fromJSON(val)
  
  # Report error if given
  if (length(res$error) > 0) {
    stop(paste("ERROR! code: ", res$error$code, ", message: ", res$error$message, sep=""))
  } else {
    return(res)
  }
}


#' For Presidentti2012 candidate answers, form numerical rating in [0, 1] for the 
#' answer options (rougly corresponding to conservative-liberal axis)
#'
#' @param candidates candidate information as given by 
#'                   candidates <- GetPresidentti2012(category="candidates", API=API)
#' @param questions questions returned by GetPresidentti2012(category="questions", API=API)
#'
#' @return matrix A matrix: each row corresponds to a candidate. For each candidate, 
#'              the answer options (columns) are rated within [0, 1]
#' 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export

Presidentti2012CandidateAnswers2Numeric <- function (candidates, questions) {

  # Question IDs
  qids <- paste("Q", sapply(questions[[1]], function(x){x$id}), sep = "")

  # Single-option questions
  single.qids <- qids[sapply(questions[[1]], function(x){x$maxSelect}) == 1]  

  # Pick candidate answers
  mat <- matrix(NA, length(candidates[[1]]), length(single.qids))
  rownames(mat) <- paste("C", as.character(sapply(candidates[[1]], function(x) {x$id})), sep = "")
  colnames(mat) <- single.qids

  for (cind in 1:length(candidates[[1]])) {

    cidx <- paste("C", candidates[[1]][[cind]]$id, sep = "")
    ans <- sapply(candidates[[1]][[cind]]$answers, function(x){x$choices})
    names(ans) <- paste("Q", sapply(candidates[[1]][[cind]]$answers, function(x){x$question}), sep = "")  
    # Convert list() to NAs
    ans[sapply(ans, length) == 0] <- NA
				 
    mat[cidx, single.qids] <- as.character(unlist(ans)[single.qids])

  }

  # Convert choiceIDs to numeric
  mat2 <- Presidentti2012ConvertOptionsToNumeric(mat, questions)

  mat2
}


#' For Presidentti2012 answers, form numerical rating in [0, 1] for the 
#' answer options (rougly corresponding to conservative-liberal axis)
#'
#' @param df data.frame giving the merged table of subjects vs. background information + answers
#'             The example script for obtaining this was posted to Louhos. 
#'             http://louhos.wordpress.com/2012/01/06/kenesta-seuraava-presidentti-ennusta-itse-hsn-vaalikonedatan-avulla/
#' @param questions questions returned by GetPresidentti2012(category="questions", API=API)
#'
#' @return list A list: each element corresponds to a question. For each question, 
#'              the answer options are rated within [0, 1]
#' 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export

Presidentti2012ConvertOptionsToNumeric <- function (df, questions) {

  # Convert matrices to data.frames
  if (is.matrix(df)) {df <- as.data.frame(df)}

  # Rate the choices
  choice.ratings <- Presidentti2012RateChoices(questions)

  # Replace selection IDs by corresponding selection rates (0...1)
  for (qid in names(choice.ratings)) {
    print(qid)
    df[, qid] <- as.numeric(choice.ratings[[qid]][as.character(df[, qid])])
  }
  df 
}


#' For Presidentti2012 answers, form numerical rating (in integers) for the 
#' answer options (rougly corresponding to the index on conservative-liberal axis)
#'
#' Load data from Presidentti2012 vaalikone
#' Note! You need a personal API key to get the data!
#'
#' @param questions questions returned by GetPresidentti2012(category="questions", API=API)
#'
#' @return list A list: each element corresponds to a question. For each question, 
#'              the answer options are given an index, roughly corresponding to their
#'              position on conservative-liberal axis
#' 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export

Presidentti2012RateChoices <- function (questions) {

  # Single-option questions
  qinds <- which(sapply(questions[[1]], function(x){x$maxSelect}) == 1)

  # Measure each choice with rate from 0...1 
  # and for each question (no multichoice questions).
  # list choiceID and the corresponding rate

  choice.ratings <- list()
  for (qind in qinds) {
    question.id <- paste("Q", questions[[1]][[qind]]$id, sep = "")

    choice.ids <- sapply(questions[[1]][[qind]]$choices, function (x) {x$id})

    choice.rate <- seq(0, 1, length = length(choice.ids))
    names(choice.rate) <- choice.ids 

    choice.ratings[[question.id]] <- choice.rate

  }

  choice.ratings
}


