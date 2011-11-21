# (C) 2011 Leo Lahti <leo.lahti@iki.fi> All rights reserved.
# License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses

# Tama esimerkki on testattu sorvi-paketin versiolla 0.1.23

# Esimerkki Suomen kuntatason vaestonkasvutilastojen (Tilastokeskus)
# visualisoinnista Maanmittauslaitoksen karttadatalla (vuonna 2010)

# Lataa kirjastoja
library(sorvi)

###############################################

# Lue Suomen kuntarajat SpatialPolygon-muodossa
# (C) Maanmittauslaitos 2011
# http://www.maanmittauslaitos.fi/aineistot-palvelut/digitaaliset-tuotteet/ilmaiset-aineistot/hankinta
data(MML)
sp <- MML[["1_milj_Shape_etrs_shape"]][["kunta1_p"]]

#################################################

# Lue kuntatason vaestonkasvutiedot tilastokeskuksen StatFin-tietokannasta
# http://www.stat.fi/tup/statfin/index.html
# PC Axis-muodossa
px <- read.px("http://pxweb2.stat.fi/database/StatFin/vrm/synt/080_synt_tau_203_fi.px")

# Poimi taulukosta halutut tiedot
pxs <- subset(as.data.frame(px), Väestönmuutos.ja.väkiluku == "Luonnollinen väestönlisäys" & Vuosi == 2010)
# Putsaa data
vaestonkasvu <- preprocess.px(pxs)

################################################

# Lisaa tiedot karttaobjektiin
sp@data$vaestonkasvu <- vaestonkasvu$dat[match(sp$Kunta.FI, vaestonkasvu$Alue)]
# Korvaa puuttuvat arvot nollalla
sp[["vaestonkasvu"]][is.na(sp[["vaestonkasvu"]])] <- 0

################################################

# Maarittele varipaletti
my.palette <- colorRampPalette(c("blue", "white", "red"), space = "rgb")
ncol <- 10 # Number of colors

#################################################

# Piirra kuva
varname <- "vaestonkasvu"
int <- max(abs(sp[[varname]]))
q <- spplot(sp, varname,
col.regions = my.palette(ncol),
main = "Väestönkasvu 2010",
colorkey = TRUE,
lwd = .4,
col = "black",
at = seq(0 - int, 0 + int, length = ncol)
)

png("vaestonkasvu.png")
print(q)
dev.off()

#################################################

# Toinen tapa ggplot2-paketilla
# see http://had.co.nz/ggplot2/coord_map.html
# Hitaampi ja vahemman viimeistelty kuin yo. esimerkki

# may require gpclibPermit()
if (!gpclibPermitStatus()) {gpclibPermit()}

# Convert SpatialPolygon to data.frame
finmap  <- fortify(sp, region = "Kunta.FI")

# Add information
finmap$vaestonkasvu <- as.numeric(sp$vaestonkasvu)[match(finmap$id, sp$Kunta.FI)]

# NOTE: color scale ends are mistaken here?
p <- ggplot(finmap, aes(x = long, y = lat)) + geom_polygon(aes(group=id, fill=vaestonkasvu, col=vaestonkasvu), colour="black") + opts(title="Vaestonkasvu")

#png("vaestonkasvu2.png")
print(p)
#dev.off()

