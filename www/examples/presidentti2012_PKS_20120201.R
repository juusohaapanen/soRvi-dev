# Script for analyzing Finnish Presidential election data from year 2012
# Copyright (C) 2011 Juuso Parkkinen <juuso.parkkinen@gmail.com>. All rights reserved.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License: 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# This script was implemented with soRvi version 0.1.45
library(sorvi)

## READ VOTING RESULTS FROM HS NEXT
votes.url <- "http://www2.hs.fi/extrat/hsnext/presidentti1-tulos.csv"
votes <- read.csv(votes.url, sep=";")

# Fix column names ("osuus" and "aania" are mixed with each other)
names(votes) <- gsub("osuus", "temp", names(votes))
names(votes) <- gsub("ääniä", "osuus", names(votes))
names(votes) <- gsub("temp", "ääniä", names(votes))

## READ VOTING AREA DATA FROM HKK
library(rgdal)
areas <- GetHKK(which.data="Aanestysaluejako", data.dir="TEMP")

# Create new Aluenumero code from TKTUNNUS for areas data (discard the first 2
# digits)
areas@data$Aluenumero <- sapply(areas@data$TKTUNNUS, function(x) substr(as.character(x), 3, nchar(as.character(x))))

# Check that Aluenumero is found also in the voting data
if(!all(areas@data$Aluenumero %in% votes$Aluenumero))
  stop("DAMN!")

# Merge voting data to area data based on Aluenumero
areas@data <- merge(areas@data, votes, by="Aluenumero")

# Set the projection right and reproject to WS84
areas@proj4string <- CRS("+init=epsg:2392")
areas <- spTransform(areas, CRS("+proj=longlat +datum=WGS84"))

# Map the cities from code to name
city.codes  <- list("091"="Helsinki", "049"="Espoo", "235"="Kauniainen", 
                    "092"="Vantaa")
areas[["Kuntanimi"]] <- sapply(as.character(areas[["KUNTA"]]), function(x) city.codes[[x]])
areas@data$Kuntanimi <- factor(areas@data$Kuntanimi)

# Split the spatial data into respective cities
areas.cities <- SplitSpatial(areas, "Kuntanimi")

# Note: first create vaalit directory
save(areas, file="vaalit/Presidentti2012_PKS_aanestysalueet_20120202.RData")

## PLOT with ssplot
## HOW TO JOIN CITIES?
library(ggplot2)
library(gridExtra)
load("vaalit/Presidentti2012_PKS_aanestysalueet_20120202.RData")
plots <- list()
varname <- "Pekka.Haavisto.osuus"
for (city in names(areas.cities))
  plots[[city]] <- spplot(areas.cities[[city]], zcol=varname, 
                          main=city, names.attr=gsub("\\.", " ", varname))
Haavisto.grob <- do.call(arrangeGrob, c(plots, list(ncol=1)))
pdf("vaalit/Presidentti2012_PKS_Pekka_Haavisto_osuus_spplot_20120202.pdf", width=6, height=15)
Haavisto.grob
dev.off()

## PLOT WITH ggplot2

areas.df <- fortify(areas, region="Aluenumero")

varname <- "Pekka.Haavisto.osuus"
areas.df[[varname]] <- votes[[varname]][match(areas.df$id, votes$Aluenumero)]
hp <- ggplot(areas.df, aes(x=long, y=lat)) + geom_polygon(aes(group=group, fill=Pekka.Haavisto.osuus))
ggsave("vaalit/Presidentti2012_PKS_Pekka_Haavisto_osuus_ggplot2_20120202.pdf", plot=hp)