

preprocess.Oikotie <- function() {
# Script for processing Oikotie data
# License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses
# Copyright 2011 Juuso Parkkinen, juuso.parkkinen@gmail.com. All rights reserved.
  
 library(gdata)
  # First download and unzip data from http://www2.hs.fi/extrat/hsnext/oikotie-data.zip
myynnit <- read.csv("data/myynnit.csv", sep=";", quote="", fileEncoding="ISO-8859-1")

# Fix formats, and remove lines with errors (additional ';'s)
myynnit$Size <- as.numeric(gsub(pattern=",", replacement=".", as.vector(myynnit$Size)))
myynnit$Price <- as.numeric(gsub(pattern=",", replacement=".", as.vector(myynnit$Price)))
myynnit$Floor <- as.numeric(as.vector(myynnit$Floor))
myynnit$Apartment.condition <- as.numeric(as.vector(myynnit$Apartment.condition))
myynnit <- myynnit[-unique(c(which(is.na(myynnit$Size)), which(is.na(myynnit$Floor)))),]

# Compute price per square meter and fix zip codes
myynnit$Price.per.square <- myynnit$Price / myynnit$Size
myynnit$Zip.code <- as.character(myynnit$Zip.code)
for (i in 2:4)
  myynnit$Zip.code[nchar(myynnit$Zip.code)==i] <- paste(paste(rep("0", 5-i), collapse=""), myynnit$Zip.code[nchar(myynnit$Zip.code)==i], sep="") 

# Filter data based on price and size
myynnit <- myynnit[-which(myynnit$Price <= 3),]
myynnit <- myynnit[-which(myynnit$Price > 1000000),] 
myynnit <- myynnit[-which(myynnit$Size < 10),]
myynnit <- myynnit[-which(myynnit$Size > 500),]
myynnit <- myynnit[-which(myynnit$Price.per.square < 500),]

# Extract street names
streets <- strsplit(as.vector(myynnit$Location), split=" ")
streets2 <- sapply(streets, function(x) paste(x[1:(length(x)-1)], collapse=" "))
lengths <- sapply(streets, length)
streets2[grep("Hennalankuja", streets2)] <- "Hennalankuja"
streets2[lengths %in% c(5,6)] <- sapply(streets[lengths %in% c(5,6)], function(x) x[1])
myynnit$Street <- streets2

# Take only Helsinki region data (zip code begins with 00, 01, 02)
zips <- unique(myynnit$Zip.code)
zips.beginnings <- sapply(strsplit(zips, split=""), function(x) paste(x[1:2], collapse=""))
zips.hr <- zips[zips.beginnings %in% c("00", "01", "02")]
hr.myynnit <- subset(myynnit, Zip.code %in% zips.hr)

# Fix encoding
myynnit$Location <- factor(iconv(myynnit$Location, from="ISO-8859-1", to="UTF-8"))
myynnit$Street <- factor(iconv(myynnit$Street, from="ISO-8859-1", to="UTF-8"))
myynnit$Room.configuration <- factor(iconv(myynnit$Room.configuration, from="ISO-8859-1", to="UTF-8"))
hr.myynnit$Location <- factor(iconv(hr.myynnit$Location, from="ISO-8859-1", to="UTF-8"))
hr.myynnit$Street <- factor(iconv(hr.myynnit$Street, from="ISO-8859-1", to="UTF-8"))
hr.myynnit$Room.configuration <- factor(iconv(hr.myynnit$Room.configuration, from="ISO-8859-1", to="UTF-8"))

# Save data
save(myynnit, hr.myynnit, file="data/Oikotie.rda")
}
