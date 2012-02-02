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
#' @param type return the answer rating as integers ("integer") 0, 1, 2, ... 
#'        or as rates between [0,1].
#' @return matrix A matrix: each row corresponds to a candidate. For each candidate, 
#'              the answer options (columns) are rated within [0, 1]
#' 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export

Presidentti2012CandidateAnswers2Numeric <- function (candidates, questions, type = "rate") {

  # Question IDs
  qids <- paste("Q", sapply(questions[[1]], function(x){x$id}), sep = "")

  # Single-option questions
  single.qids <- qids[sapply(questions[[1]], function(x){x$maxSelect}) == 1]  

  # Pick candidate answers
  mat <- matrix(NA, length(candidates$data), length(single.qids))
  #rownames(mat) <- paste("C", as.character(sapply(candidates$data, function(x) {x$id})), sep = "")
  rownames(mat) <- as.character(sapply(candidates$data, function(x) {x$lastname}))

  colnames(mat) <- single.qids

  for (cind in 1:length(candidates$data)) {

    #cidx <- paste("C", candidates$data[[cind]]$id, sep = "")
    cidx <- candidates$data[[cind]]$lastname
    ans <- sapply(candidates$data[[cind]]$answers, function(x){x$choices})
    names(ans) <- paste("Q", sapply(candidates$data[[cind]]$answers, function(x){x$question}), sep = "")  
    # Convert list() to NAs
    ans[sapply(ans, length) == 0] <- NA
				 
    mat[cidx, single.qids] <- as.character(unlist(ans)[single.qids])

  }

  # Convert choiceIDs to numeric
  mat2 <- Presidentti2012ConvertOptionsToNumeric(mat, questions, type = type)
 
  mat2
}


#' For Presidentti2012 answers, form numerical rating in [0, 1] for the 
#' answer options (rougly corresponding to conservative-liberal axis)
#'
#' @param df data.frame giving the merged table of subjects vs. background information + answers
#'             The example script for obtaining this was posted to Louhos. 
#'             http://louhos.wordpress.com/2012/01/06/kenesta-seuraava-presidentti-ennusta-itse-hsn-vaalikonedatan-avulla/
#' @param questions questions returned by GetPresidentti2012(category="questions", API=API)
#' @param type return the answer rating as integers ("integer") 0, 1, 2, ... 
#'        or as rates between [0,1].
#'
#' @return list A list with two data.frames: info (user information) and 
#'         answer (user answers). The answer options are rated within [0, 1]. 
#'         Each row corresponds to a user in each of the two data.frames.
#' 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export

Presidentti2012ConvertOptionsToNumeric <- function (df, questions, type = "rate") {

  # Convert matrices to data.frames
  if (is.matrix(df)) { df <- as.data.frame(df) }

  # Rate the choices
  choice.ratings <- Presidentti2012RateChoices(questions, type = type)

  # Replace selection IDs by corresponding selection rates
  for (qid in names(choice.ratings)) {
    print(qid)
    df[, qid] <- as.numeric(choice.ratings[[qid]][as.character(df[, qid])])
  }

  user.info <- df[, 1:8] # do not include user ID (field 9)
  user.answers <- df[, -seq(1,9,1)]

  if ("Tulot" %in% colnames(user.info)) {
    # Luokittele tulotason mukaan ja jarjesta tasot
    Tuloluokka <- c("10000", "5000", "15000", "100000", "25000", "30000", "40000", "50000", "60000", "80000", "20000", NA)
    names(Tuloluokka) <- c("c(10000, 14999)", "c(5000, 9999)", "c(15000, 19999)", "c(1e+05, 999999)", "c(25000, 29999)", "c(30000, 39999)", "c(40000, 49999)",
"c(50000, 59999)", "c(60000, 79999)", "c(80000, 99999)",  "c(20000, 24999)", "NULL")
    user.info$Tuloluokka <- Tuloluokka[as.character(user.info$Tulot)]
    user.info$Tuloluokka[is.na(user.info$Tuloluokka)] <- "NULL"
    user.info$Tuloluokka <- factor(user.info$Tuloluokka, levels = c("NULL", "5000", "10000", "15000", "20000", "25000", "30000", "40000", "50000", "60000", "80000"))
  }

  if ("Ika" %in% colnames(user.info)) {
    user.info$Ika <- factor(user.info$Ika, levels =  c("NULL", "c(18, 19)", "c(20, 24)", "c(25, 29)", "c(30, 34)", "c(35, 39)", "c(40, 44)", "c(45, 49)", "c(50, 54)", "c(55, 59)", "c(60, 64)", "c(65, 69)", "c(70, 74)", "c(75, 79)", "c(80, 84)", "c(85, 89)", "c(90, 100)"))
  }

  list(info = user.info, answer = user.answers)

}


#' For Presidentti2012 answers, form numerical rating (in integers) for the 
#' answer options (rougly corresponding to the index on conservative-liberal axis)
#'
#' @param questions questions returned by GetPresidentti2012(category="questions", API=API)
#' @param type return the answer rating as integers ("integer") 0, 1, 2, ... 
#'        or as rates between [0,1].
#'
#' @return list A list: each element corresponds to a question. For each question, 
#'              the answer options are given an index, roughly corresponding to their
#'              position on conservative-liberal axis
#' 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export

Presidentti2012RateChoices <- function (questions, type = "rate") {

  # Single-option questions
  qinds <- which(sapply(questions[[1]], function(x){x$maxSelect}) == 1)

  # Measure each choice with rate from 0...1 
  # and for each question (no multichoice questions).
  # list choiceID and the corresponding rate

  choice.ratings <- list()

  for (qind in qinds) {
    question.id <- paste("Q", questions[[1]][[qind]]$id, sep = "")

    choice.ids <- sapply(questions[[1]][[qind]]$choices, function (x) {x$id})

    if (type == "rate") {
      choice.rate <- seq(0, 1, length = length(choice.ids))    
    } else if (type == "integer") {
      choice.rate <- (1:length(choice.ids)) - 1
    }
    names(choice.rate) <- choice.ids 

    choice.ratings[[question.id]] <- choice.rate

  }

  choice.ratings
}


#' For Presidentti2012 answers, get answer IDs, text and rating
#' for the given question ID.
#'
#' @param question.id Question ID as in HS vaalikone (eg. numerical 80), or in soRvi e.g. character "Q80") 
#' @param questions questions returned by GetPresidentti2012(category="questions", API=API)
#'
#' @return list A list with the fields question, answer id, answer text and answer rate for the given question.
#' 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export

Presidentti2012RetrieveAnswerText <- function (question.id, questions) { 
  qid <- as.numeric(gsub("Q", "", question.id))
  ans.text <- sapply(questions$data[[which(sapply(questions$data, function (x) {x$id}) == qid)]]$choices, function(x) {x$text})
  ans.id <- sapply(questions$data[[which(sapply(questions$data, function (x) {x$id}) == qid)]]$choices, function(x) {x$id})
  ans.rate <- seq(0, 1, length = length(ans.id))

  question <- questions$data[[which(sapply(questions$data, function (x) {x$id}) == qid)]]$text

  list(question = question, id = ans.id, text = ans.text, rate = ans.rate)
}
