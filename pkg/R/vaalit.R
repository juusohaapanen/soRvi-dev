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


#' Load Vaalipiiri information
#' Useful for mapping election data to other municipality information
#'
#' @param url URL for Tilastokeskus vaalipiirit.
#'
#' @return data.frame listing election regions (Vaalipiiri), region IDs (Aluenumero) and municipalities (Alue)
#' 
#' @author Juuso Parkkinen and Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export

GetVaalipiiri <- function (url = "http://www.stat.fi/meta/luokitukset/vaalipiiri/001-2012/luokitusavain_kunta.html") {

  message(paste("Reading Vaalipiiri information from ", url))

  # Read info of municipalities and election areas from Tilastoteskus
  require(XML)
  temp <- XML::readHTMLTable(url)

  # Extract info that we want
  municipalities <- temp[[1]][-1,]
  municipalities$Vaalipiiri <- paste(as.vector(municipalities[,1]), as.vector(municipalities[,2]))
  municipalities <- municipalities[3:5]
  names(municipalities) <- c("Aluenumero", "Alue", "Vaalipiiri")

  # Fill missing Vaalipiiri info
  current.piiri <- NA
  for (i in 1:nrow(municipalities)) {
    # If vaalipiiri given, save it as current
    if (!nchar(gsub(" ", "", municipalities[i,"Vaalipiiri"])) == 2) {
      current.piiri <- as.vector(municipalities[i,"Vaalipiiri"])
    } else { # Else add current vaalipiiri
      municipalities[i, "Vaalipiiri"] <- current.piiri
    }
  }

  municipalities

}



#' Load presidential election 2012 results from HS Next
#'
#' @param election.round Presidential election round (1/2)
#' @param level Optional. Pick results for particular region type. Options: "municipalities" (kunnat)
#' @return Votes table
#' 
#' @author Juuso Parkkinen and Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export

GetElectionResultsPresidentti2012 <- function (election.round, level = NULL) {

    # Read first Election round

    ## Read 1st presidential election round votes from HS Next
    votes.url <- "http://www2.hs.fi/extrat/hsnext/presidentti1-tulos.csv"
    message(paste("Reading Finnish presidential election result data from", votes.url))
    votes <- read.csv(votes.url, sep=";")

    # Fix column names ("osuus" and "aania" are mixed with each other)
    names(votes) <- gsub("osuus", "temp", names(votes))
    names(votes) <- gsub("aania", "osuus", names(votes))
    names(votes) <- gsub("temp", "aania", names(votes))
    # Field 5: Aania yhteensa:
    votes[[5]] <- as.numeric(as.vector(gsub("None", "0", votes[[5]])))
  
    # Refine variable names
    names(votes) <- gsub("\\.", " ", names(votes))
    names(votes)[3:39] <- paste("1.K", names(votes)[3:39], sep=" ")
    
    votes1 <- votes

  
    # Read 2nd round votes from HS Next
    votes.url <- "http://www2.hs.fi/extrat/hsnext/presidentti2.csv"
    message(paste("Reading Finnish presidential election result data from", votes.url))
    votes <- read.csv(votes.url, sep=";", fileEncoding="ISO-8859-15")
 
    # Here the names are ok, but ',' has been used as the decimal separator
    bad.cols <- c(3,4,7,9,11,13,15)
    votes[,bad.cols] <- apply(votes[,bad.cols], 2, function(x) as.numeric(gsub(",", ".", x)))

    # Rows in votes1 and votes match perfectly with one exception:
    # votes1 is missing row 1995: 499021 K\"oklot
    # As we are now not interested in it, we simply remove it from
    # votes to make merging these two easier
    votes <- droplevels(votes[-1995,])
    names(votes) <- gsub("\\.", " ", names(votes))
    names(votes)[3:15] <- paste("2.K", names(votes)[3:15], sep=" ")
   
    votes2 <- votes
	
  # Vuoden 2012 alusta Lansi-Turunmaan kaupunki otti nimekseen Parainen
  # ja palasi nain aiemman Paraisten kaupungin vanhaan nimeen.
  levels(votes$Alue)[korvaa.skandit(as.character(levels(votes$Alue))) == "Lansi-Turunmaa"] <- "Parainen"

  # Get list of election region codes from Tilastokeskus
  message("Loading election region data from Tilastokeskus")
  url <- "http://www.stat.fi/meta/luokitukset/vaalipiiri/001-2012/luokitusavain_kunta.html"
  vaalipiirit <- GetVaalipiiri(url)

  # Rename regions to match voting data
  levels(vaalipiirit$Alue)[levels(vaalipiirit$Alue)=="Maarianhamina - Mariehamn"] <- "Maarianhamina"

  if (level == "municipalities") {

    # Match vaalipiirit and first election round 
    municipality.rows <- votes1$Aluenumero %in% vaalipiirit$Aluenumero
    votes1 <- votes1[municipality.rows,]
    votes2 <- votes2[municipality.rows,]
    regions <- vaalipiirit[match(votes1$Aluenumero, vaalipiirit$Aluenumero),]

    # RegionIDs do not match exactly between election rounds 1/2 but
    # region names do, use them to match 1/2 rounds
    # Confirm that regions match on the first and second election round 
    if (!all(as.vector(votes1$Alue) == as.vector(votes2$Alue))) {
      stop("Election regions do not match between election rounds.")
    } 
  }

  if (election.round == 1) {
    votes <- votes1
  } else if (election.round == 2) {
    votes <- votes2
  }

  # Merge municipality IDs and names with first round election results
  votes <- droplevels(cbind(regions, votes[,3:ncol(votes)]))

  votes

}




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


#' Preprocess Presidentti2012 question data
#'
#' @param questions output from 
#'        questions <- GetPresidentti2012(category="questions", API=API)
#' @return list A list with the fields Questions and Choices
#' @note A wrapper 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export

PreprocessPresidentti2012 <- function (questions) {
  Questions <- data.frame(ID=sapply(questions$data, function(x) x$id))
  Questions$Text <- sapply(questions$data, function(x) x$text)
  Choices <- list(ID=lapply(questions$data, function(y) sapply(y$choices, function(x) x$id)))
  Choices$Text <- lapply(questions$data, function(y) sapply(y$choices, function(x) x$text))
  # Wrap texts for visualization
  Questions$TextWrapped <- lapply(Questions$Text, function(x) paste(strwrap(x, width=80), collapse="\n"))
  Choices$TextWrapped <- lapply(Choices$Text, function(x) sapply(x, function(y) paste(strwrap(y, width=40), collapse="\n")))
  list(Questions = Questions, Choices = Choices)
}


#' Get user answer data for HS vaalikone 2012 
#'
#' @param dates dates for which to retrieve data, for instance: c(paste("2011-11", 23:30, sep="-"), paste("2011-12", 1:31, sep="-"))
#' @param API API key
#' @param per.page maximum number of results to retrieve at one query in the for loop
#' @return dat.list list containing user answer data for the specified dates
#' @note A wrapper 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export

Presidentti2012GetUserData <- function (dates, API, per.page = 10000) {

  dat.list <- list()
  for (di in 1:length(dates)) {

    filter <- paste("timerange:",dates[di], sep="")
    message("\n",filter, ", page 1...", appendLF=FALSE)

    # Get results (can download only 10000 at a time)
    dat <- GetPresidentti2012(category = "useranswers", API = API, filter = filter, 
       				   page = 1, per_page = per.page, show_total = "true")

    # Check if more than per.page answers given
    ten.ks <- ceiling(dat$pagination$total / per.page)
    if (ten.ks > 1) { 
      # Get remaining results, per.page at a time
      for (t in 2:ten.ks) {
        message("page ", t, "... ", appendLF = FALSE)
        temp.dat <- GetPresidentti2012(category = "useranswers", API = API, filter = filter, 
	    			page = t, per_page = per.page, show_total = "true")
        dat$data <- c(dat$data, temp.dat$dat)
    }
  }
  dat.list[[di]] <- dat
  }
  names(dat.list) <- dates

  dat.list
}



#' Preprocess user answer data for HS vaalikone 2012 
#'
#' @param dat.list Output from: dat.list <- Presidentti2012GetUserData(dates, API, per.page = 10000)
#' @param API API key
#' @return data.frame with user answer data
#' @note A wrapper 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @export


PreprocessPresidentti2012UserData <- function (dat.list, API = API) {

  questions <- GetPresidentti2012(category="questions", API = API)
  Questions <- PreprocessPresidentti2012(questions)$Questions

  # Construct a data frame
  Presidentti2012.df <- c()
  for (di in 1:length(dat.list)) {
    message(paste("Collecting the data", 100*di/length(dat.list), " percent.."))

    # Get respondent information
    info <- unlist(lapply(dat.list[[di]]$data, function(x) as.character(x[1:9])))
    info.mat <- matrix(info, ncol = 9, byrow = T)
    colnames(info.mat) <- names(dat.list[[di]]$data[[1]])[1:9]

    # Accept only those users who have answered to all questions
    # Get answers (not for Q14/ID70, because it is a multiple choice question)
    missing <- which(sapply(dat.list[[di]]$data, function(x) length(x$answers)) < 25)

    answer.list <- lapply(dat.list[[di]]$data[-missing], function(x) matrix(as.character(unlist(x$answers[-14])), ncol=2, byrow=T)[,2])
    answer.mat <- matrix(unlist(answer.list), nrow=length(answer.list), ncol=24, byrow=T)
    colnames(answer.mat) <- paste("Q", Questions$ID[-14], sep = "")

    # Join the matrices
    date.df <- cbind(as.data.frame(info.mat[-missing,]), as.data.frame(answer.mat))
    Presidentti2012.df <- rbind(Presidentti2012.df, date.df)

  }

  # Translate variable names and fix some of them
  names(Presidentti2012.df)[1:8] <- c("Paivamaara", "Koulutustaso", "Sukupuoli", "Tulot", "Ykkosehdokas", "Puolue", "Ika", "Asuinpaikka")
  levels(Presidentti2012.df$Sukupuoli) <- c("NULL", "Nainen", "Mies")[match(levels(Presidentti2012.df$Sukupuoli), c("NULL", "f", "m"))]
  Presidentti2012.df$Paivamaara <- as.Date(Presidentti2012.df$Paivamaara)

  # Get candidate data
  candidates <- GetPresidentti2012(category = "candidates", API = API)

  # Match candidate IDs and names
  candidate <- sapply(candidates$data, function(x) x$lastname)  # candidate name
  names(candidate) <- sapply(candidates$data, function(x) x$id) # candidate ID
  levels(Presidentti2012.df$Ykkosehdokas) <- candidate[levels(Presidentti2012.df$Ykkosehdokas)]

  # Reorder factor levels, some by abundance, some in the natural way
  # 'attach' lets us use the factors without repeating the data frame name every time
  Presidentti2012.df$Koulutustaso <- reorder(Presidentti2012.df$Koulutustaso, id, length)
  Presidentti2012.df$Ykkosehdokas <- reorder(Presidentti2012.df$Ykkosehdokas, id, length)
  Presidentti2012.df$Puolue <- reorder(Presidentti2012.df$Puolue, id, length)
  Presidentti2012.df$Asuinpaikka <- reorder(Presidentti2012.df$Asuinpaikka, id, length)
  Presidentti2012.df$Tulot <- factor(Presidentti2012.df$Tulot, levels=levels(Presidentti2012.df$Tulot)[c(1,9,2,3,5:8,10:12,4)])

  Presidentti2012.df
  
}