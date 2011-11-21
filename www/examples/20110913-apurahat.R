# Code for analysing and visualising Finnish apurahat-data
# License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses
# Copyright 2011 Juuso Parkkinen, juuso.parkkinen@gmail.com.

# NOTE! This code has been udpated 12.11.2011 to use sorvi-package!

# Install soRvi package
# Instructions in http://sorvi.r-forge.r-project.org/asennus.html

library(sorvi)

# Load apurahat data
apurahat <- load.apurahat()

# Load maakuntakartta (need to permit the use of gpclib)
if (!gpclibPermitStatus())
    gpclibPermit()
maakuntakartta <- load.maakuntakartta()

##########################
## Basic visualizations ##
##########################

library(ggplot2)
library(gridExtra)

# Plot top20 artists (HS 19.5.2011)
henkilo.summat <- aggregate(apurahat$Myontosumma.EUR, by=list(apurahat$Hakijan.nimi), sum)
top20.nimet <- henkilo.summat[order(henkilo.summat$x, decreasing=T)[1:20],1]
top20 <- subset(henkilo.summat, henkilo.summat$Group.1 %in% top20.nimet)
names(top20) <- c("Hakijan.nimi", "Myontosumma.EUR")
top20$Hakijan.nimi <- reorder(factor(top20$Hakijan.nimi), top20$Myontosumma.EUR, sum)
p1 <- ggplot(top20, aes(Hakijan.nimi, Myontosumma.EUR)) + geom_bar(fill="red")
p1 <- p1 + coord_flip() + opts(axis.text.x=theme_text(angle=-90, hjust=0),title="Top 20 apurahan saaneet")
ggsave("top20.png", plot=p1)

# Plot by province and year
maakunta.summat <- aggregate(apurahat$Myontosumma.EUR, list(apurahat$Maakunta, apurahat$Vuosi), sum)
names(maakunta.summat) <- c("Maakunta", "Vuosi", "Myontosumma.EUR")
p2 <- ggplot(maakunta.summat, aes(x=Maakunta, y=Myontosumma.EUR, fill=factor(Vuosi))) + geom_bar(position="dodge")
p2 <- p2 + coord_flip() + opts(axis.text.x=theme_text(angle=-90, hjust=0),title="Apurahat maakunnittain summattuna")
p2 <- p2 + scale_y_continuous(formatter="comma")

# Plot by class and year
hakemusluokka.summat <- aggregate(apurahat$Myontosumma.EUR, list(apurahat$Hakemusluokka, apurahat$Vuosi), sum)
names(hakemusluokka.summat) <- c("Hakemusluokka", "Vuosi", "Myontosumma.EUR")
p3 <- ggplot(hakemusluokka.summat, aes(x=Hakemusluokka, y=Myontosumma.EUR, fill=factor(Vuosi))) + geom_bar(position="dodge")
p3 <- p3 + coord_flip() + opts(axis.text.x=theme_text(angle=-90, hjust=0),title="Apurahat hakemusluokittain summattuna")
p3 <- p3 + scale_y_continuous(formatter="comma")
ggsave("hakemusluokat.png", plot=p3)

# Plot by age and gender
ikaryhma.summat <- aggregate(apurahat$Myontosumma.EUR, list(apurahat$Ikaryhma, apurahat$Sukupuoli), sum)
names(ikaryhma.summat) <- c("Ikaryhma", "Sukupuoli", "Myontosumma.EUR")
p4 <- ggplot(ikaryhma.summat, aes(x=Ikaryhma, y=Myontosumma.EUR, fill=Sukupuoli)) + geom_bar(position="dodge")
p4 <- p4 + coord_flip() + opts(axis.text.x=theme_text(angle=-90, hjust=0),title="Apurahat ikaryhmittain")
p4 <- p4 + scale_y_continuous(formatter="comma")
ggsave("ika_sukupuoli.png", plot=p4)

# Plot age group vs. class
ika.vs.hakemus <- aggregate(apurahat$Myontosumma.EUR, list(apurahat$Hakemusluokka, apurahat$Ikaryhma), sum)
names(ika.vs.hakemus) <- c("Hakemusluokka", "Ikaryhma", "Myontosumma.EUR")
p5 <- ggplot(ika.vs.hakemus, aes(Hakemusluokka, Ikaryhma)) + geom_point(aes(size=Myontosumma.EUR))# + geom_jitter()
p5 <- p5 + coord_flip() + opts(axis.text.x=theme_text(angle=-90), title="Apurahat, ikaryhma vs. hakemusluokka")
p5 <- p5 + scale_area()
ggsave("ikaryhma_vs_hakemusluokka.png", plot=p5)

##############################
## Plot on a map of Finland ##
##############################

# Sum by province, remove "ITa-UUSIMAA"
apurahat$Maakunta[apurahat$Maakunta=="ITa-UUSIMAA"] <- "UUSIMAA"
apurahat$Maakunta <- droplevels(apurahat$Maakunta)
maakunta.summat <- aggregate(apurahat$Myontosumma.EUR, by=list(apurahat$Maakunta), sum)
maakuntakartta$summat <- maakunta.summat[match(toupper(maakuntakartta$id), maakunta.summat[,1]),2]

# Add size of population for each province (2010, obtained originally from Tilastokeskus), compute scholarschip per citizen
maakuntakartta$EUR.per.asukas <- maakuntakartta$summat / maakuntakartta$asukasluku

# Plot apurahat on a map by province
p1 <- ggplot(maakuntakartta, aes(x = long, y = lat))
p1 <- p1 + geom_polygon(aes(group=group, fill=EUR.per.asukas), colour="white")
p1 <- p1 + opts(title="Apurahat per asukas maakunnittain")
p1 <- p1 + coord_map(project="gilbert") + xlab(NULL) + ylab(NULL) + scale_colour_discrete(name = "EUR per asukas")

# Another plot without Uusimaa
p2 <- ggplot(subset(maakuntakartta, id != "UUSIMAA"), aes(x = long, y = lat))
p2 <- p2 + geom_polygon(aes(group=group, fill=EUR.per.asukas), colour="white")
p2 <- p2 + opts(title="Apurahat per asukas maakunnittain (ei Uusimaa)")
p2 <- p2 + coord_map(project="gilbert") + xlab(NULL) + ylab(NULL) + scale_colour_discrete(name = "EUR per asukas")

# Plot both maps side by side
ggsave("Apurahat_kartalla_maakunnittain.png", plot=arrangeGrob(p1, p2, ncol=2), width=12, height=8)
