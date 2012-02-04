# Script for loading and analyzing Finnish Presidential election
# data from year 2012
# Copyright (C) 2011-2012 Juuso Parkkinen and Leo Lahti
# <sorvi-commits@lists.r-forge.r-project.org>. All rights reserved.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License:
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# This script was implemented with soRvi version 0.1.44
# For more information, see http://sorvi.r-forge.r-project.org/
library(sorvi)

###################################
## USING THE PRESIDENTTI2012 API ##
###################################

# Ask for your own API key, instructions here:
# http://blogit.hs.fi/hsnext/helsingin-sanomat-julkaisee-vaalikoneen-tiedot-avoimena-rajapintana
API <- "xxxxxxxx"

# Get and process question and choice texts
questions <- GetPresidentti2012(category="questions", API = API)
Questions <- PreprocessPresidentti2012(questions)$Questions
Choices <- PreprocessPresidentti2012(questions)$Choices

# Get candidate data
candidates <- GetPresidentti2012(category = "candidates", API = API)

# Save background data for later use
# NOTE: create subfolder in the working directory if it does not exist yet
save(Questions, Choices, questions, candidates, file= "Presidentti2012_BackgroundData.RData")

##############################
## GET ALL USER ANSWER DATA ##
##############################

# We will use the useranswers -query with the timerange-filter
# to get all useranswer data recorded so far
# Note! This is slow, at least an hour
# Define dates: the first election round

dates <- c(paste("2011-11", 23:30, sep="-"), paste("2011-12", 1:31, sep="-"), paste("2012-1", 1:22, sep = "-"))
dat.list <- Presidentti2012GetUserData(dates, API, per.page = 10000)
save(dat.list, file = "Presidentti2012_RawData.RData")

# Preprocess user answer data and save 
Presidentti2012.df <- PreprocessPresidentti2012UserData(dat.list, API)
save(Presidentti2012.df, file = "Presidentti2012_Data.RData")

#####################################################

# Candidate data tables
candidate.data <- Presidentti2012CandidateAnswers2Numeric(candidates, questions)
candidate.data.int <- Presidentti2012CandidateAnswers2Numeric(candidates, questions, type = "integer")
candidate.info <- t(sapply(candidates$data, function (x) {as.vector(x[c("firstname", "lastname", "age", "number", "gender", "party", "id")])}))
rownames(candidate.info) <- candidate.info[, "lastname"]
candidate.info[, "party"] <- sapply(candidates$data, function (x) {as.vector(x$party$common)})
candidate.info <- as.data.frame(candidate.info)

# User data tables
user.data <- Presidentti2012ConvertOptionsToNumeric(Presidentti2012.df, questions, type = "rate")
user.data.int <- Presidentti2012ConvertOptionsToNumeric(Presidentti2012.df, questions, type = "integer")
keep <- apply(user.data$answer, 1, function(x) {!any(is.na(x))})
user.answ <- as.matrix(user.data$answer[keep,])
user.answ.int <- as.matrix(user.data.int$answer[keep,])
user.info <- as.data.frame(user.data$info[keep,])

# Save
save(candidate.data, candidate.data.int, candidate.info, 
     user.answ, user.answ.int, user.info, 
     file = "ratedata.RData")


# Use this to load necessary parts for analysis:
#resdir <- "Results-20120122"
#load(paste(resdir, "/ratedata.RData", sep = ""))
#load(paste(resdir, "/Presidentti2012_Data.RData", sep = ""))
#load(paste(resdir, "/Presidentti2012_BackgroundData.RData", sep = ""))
