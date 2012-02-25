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
names(votes) <- gsub("ההniה", "osuus", names(votes))
names(votes) <- gsub("temp", "ההniה", names(votes))

## READ VOTING AREA DATA FROM HKK
library(rgdal)
library(XLConnect)
areas <- GetHKK(which.data="Aanestysaluejako", data.dir="TEMP")


## MERGE DATA EACH FOUR CITIES
for (city in names(areas)) {
  
  # Unify area number format (Aluenumero)
  ifelse (city=="Helsinki", start.n <- 3, start.n <- 2)
  areas[[city]]@data$Aluenumero <- substr(areas[[city]]@data$aanestysaluekoodi, start=start.n, stop=1000)
  # Check that it's ok
  if(!all(areas[[city]]@data$Aluenumero %in% votes$Aluenumero))
    stop("DAMN!")
  # Merge voting data to area data based on Aluenumero
  areas[[city]]@data <- merge(areas[[city]]@data, votes, by="Aluenumero")
  # Transform map projection
  areas[[city]]@proj4string <- CRS("+init=epsg:2392")
  areas[[city]] <- spTransform(areas[[city]], CRS("+proj=longlat +datum=WGS84"))
}

save(areas, file="vaalit/Presidentti2012_PKS_aanestysalueet_20120202.RData")


## PLOT with ssplot
## HOW TO JOIN CITIES?
library(ggplot2)
library(gridExtra)
load("vaalit/Presidentti2012_PKS_aanestysalueet_20120202.RData")
plots <- list()
varname <- "Pekka.Haavisto.osuus"
for (city in names(areas))
  plots[[city]] <- spplot(areas[[city]], zcol=varname, 
                          main=city, names.attr=gsub("\\.", " ", varname))
Haavisto.grob <- do.call(arrangeGrob, c(plots, list(ncol=1)))
pdf("vaalit/Presidentti2012_PKS_Pekka_Haavisto_osuus_spplot_20120202.pdf", width=6, height=15)
Haavisto.grob
dev.off()


## PLOT WITH ggplot2
areas.df <- rbind(fortify(areas$Helsinki, region="Aluenumero"),
                  fortify(areas$Espoo, region="Aluenumero"),
                  fortify(areas$Kauniainen, region="Aluenumero"),
                  fortify(areas$Vantaa, region="Aluenumero"))

varname <- "Pekka.Haavisto.osuus"
areas.df[[varname]] <- votes[[varname]][match(areas.df$id, votes$Aluenumero)]
hp <- ggplot(areas.df, aes(x=long, y=lat)) + geom_polygon(aes(group=group, fill=Pekka.Haavisto.osuus))
ggsave("vaalit/Presidentti2012_PKS_Pekka_Haavisto_osuus_ggplot2_20120202.pdf", plot=hp)