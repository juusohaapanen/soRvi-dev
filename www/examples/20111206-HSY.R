# (C) Leo Lahti 2011 <leo.lahti@iki.fi>. All rights reserved.
# License: FreeBSD (keep this notice).
# http://en.wikipedia.org/wiki/BSD_licenses

# Example on utilizing Geographical information from Helsinki Region
# Environmental Services Authority (Helsingin seudun ymparistopalvelu
# HSY). The HSY data copyright: (C) HSY 2011.

# Implemented with sorvi_0.1.27

# Load soRvi open data toolkit (http://sorvi.r-forge.r-project.org)
library(sorvi)

# Download and preprocess Helsinki Region Environmental Services
# Authority data (C) Helsingin seudun ymparistopalvelu HSY 2011.  For
# licensing and data description, see
# http://www.hsy.fi/seututieto/kaupunki/paikkatiedot/Sivut/Avoindata.aspx
# http://www.hsy.fi/seututieto/Documents/Paikkatiedot/Tietokuvaukset_kaikki.pdf

###############################################

# Investigate population distribution in Helsinki

sp <- get.hsy("Vaestoruudukko") 

## Investigate contents of the map data
print(head(as.data.frame(sp)))

# Visualize population on Helsinki Map

at <-  c(seq(0, 2000, 250), Inf) # color palette breakpoints
q <- visualize.shape(sp, "ASUKKAITA", type = "oneway", at = at, ncol = length(at), main = "Helsingin asukasjakauma")
# See contents of this function to modify details

# Save image in PNG format
png("HSY.vaesto.png"); print(q); dev.off()

#####################################################

# Visualize the building year of the oldest building in 
# different regions in Helsinki

# Download and preprocess HSY SeutuRAMAVA data
# (C) HSY 2011; for data description see:
# http://www.hsy.fi/seututieto/Documents/Paikkatiedot/Tietokuvaukset_kaikki.pdf
sp <- get.hsy("SeutuRAMAVA")
sp$VANHINRAKE <- as.integer(sp$VANHINRAKE)
sp$VANHINRAKE[sp$VANHINRAKE == 999999999] <- NA
at <- seq(1800, 2020, 10)
palette <- colorRampPalette(c("blue", "gray", "red"), space = "rgb")
q <- visualize.shape(sp, "VANHINRAKE", type = "twoway", at = at, ncol = length(at), palette = palette, main = "Vanhimman rakennuksen rakennusvuosi")

png("HSY.vanhinrakennus.png"); print(q); dev.off()

###############################################

# Visualize the distribution of the building years for the oldest building
# across Helsinki regions

par(mar=c(4, 4, 4, 2), las = 1)
df <- dfsort(as.data.frame(sp), VANHINRAKE)
df <- df[apply(df, 1, function (x) {!any(is.na(x))}), c("NIMI", "VANHINRAKE")]
v <- df$VANHINRAKE
names(v) <- as.character(df$NIMI)
v <- rev(v)
plot(v, 1:length(v), type = "n", xlim = c(min(v) - 10, max(v) + 10), ylab = "Kaupunginosat", xlab = "Rakennusvuosi", main = "Vanhimman rakennuksen rakennusvuosi", yaxt = "n")
text(v, 1:length(v), labels = names(v), cex = 0.5)

png("HSY.ikajakauma.png"); print(q); dev.off()

###############################################

# Area which has been built or is currently being built

#library(RColorBrewer) 
df <- as.data.frame(sp)
df <- df[, c("RAKERA_AS", "RAKERA_MUU", "KARA_AS", "KARA_MUU")]
keep <- apply(df, 1, function (x) {!all(x == 0)}) & !is.na(as.data.frame(sp)$NIMI)
df <- df[keep,]
rownames(df) <- as.character(as.data.frame(sp)[keep, "NIMI"])
df <- rename(df, c(RAKERA_AS = "Rakenteilla (asuminen)", RAKERA_MUU = "Rakenteilla (muu)", KARA_AS = "Rakennettu (asuminen)", KARA_MUU = "Rakennettu (muu)"))
#df <- df/as.data.frame(sp)$YKSLKM
df <- df[rev(order(rowSums(df), decreasing = TRUE)[1:50]),]
df <- df[, c(4,3,2,1)]
# display.brewer.all()
FD.palette <- rev(c("orange", "darkgray", "blue", "black"))
options(scipen=2)
par(mar=c(6, 8, 3, 2), las = 1)
barplot(t(df), beside=F,col=FD.palette, border=FD.palette, space=1, legend=F, ylab = "", xlab="Neliometria", main="Rakenteilla oleva kerrosala", mgp=c(4.5,1,0), horiz = TRUE, cex.names = 0.7, xlim = c(0, 1.02*max(rowSums(df))))
legend("bottomright", legend=rev(rownames(t(df))), fill=rev(FD.palette))
box()

png("HSY.kerrosala.png"); print(q); dev.off()

###############################################

# Area which is currently being built

# Rakenteilla oleva kerrosala
df <- as.data.frame(sp)
keep <- !df$RAKERA_AS == 0 & !df$RAKERA_MUU == 0
df <- df[keep, c("RAKERA_AS", "RAKERA_MUU")]
rownames(df) <- as.data.frame(sp)[keep, "NIMI"]
df <- df[rev(order(rowSums(df), decreasing = TRUE)[1:50]),]
df <- rename(df, c(RAKERA_AS = "Asuminen", RAKERA_MUU = "Muu"))
df <- df[, c("Muu", "Asuminen")]
# display.brewer.all()
FD.palette <- c("darkgray", "orange")
options(scipen=2)
par(mar=c(4, 8, 3, 2), las = 1)
barplot(t(df), beside=F,col=FD.palette, border=FD.palette, space=1, legend=F, ylab = "", xlab="Neliometria", main="Rakenteilla oleva kerrosala", mgp=c(4.5,1,0), horiz = TRUE, cex.names = 0.7, xlim = c(0, 1.02*max(rowSums(df))))
legend("bottomright", legend=rev(rownames(t(df))), fill=rev(FD.palette))
box()

png("HSY.kerrosala2.png"); print(q); dev.off()

