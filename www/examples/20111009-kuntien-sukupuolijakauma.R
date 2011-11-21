# Lataa sorvi-paketti

library(sorvi)

# hae suomen kartta ja kuntarajat gadm-muodossa
gadm <- get.gadm("FIN_adm", "kunnat")

# vaestorekisterin asukasluvut kunnittain
vrek <- get.vaestorekisteri("http://vrk.fi/default.aspx?docid=5127&site=3&id=0")

# Liita vaestorekisterin tiedot karttaobjektiin ja
# aseta nollaan asukasluku kunnissa joiden osalta se ei ole tiedossa
gadm$asukkaita <- log10(rowSums(vrek[gadm$kunnat, c("Miehet", "Naiset")]))
gadm$asukkaita[is.na(gadm$asukkaita)] <- 0
# Laske myos sukupuolten suhteellinen osuus
gadm$miehet.osuus <- vrek[gadm$kunnat, "Miehet"]/vrek[gadm$kunnat, "Yhteensa"]
gadm$naiset.osuus <- vrek[gadm$kunnat, "Naiset"]/vrek[gadm$kunnat, "Yhteensa"]
# Aseta arvoon 50% miesten/naisten osuus
# kunnissa joiden osalta vakiluku ei ole tiedossa
gadm$miehet.osuus[is.na(gadm$miehet.osuus)] <- 0.5
gadm$naiset.osuus[is.na(gadm$naiset.osuus)] <- 0.5

# Piirra Suomen kartta varitettyna miesten suhteellisen osuuden nojalla
varname <- "miehet.osuus" # tarkasteltava muuttuja
interval <- max(abs(gadm[[varname]] - 0.5)) # paletin rajat
ncol <- 100 # varien maara
my.palette <- colorRampPalette(c("red", "white", "blue"), space = "rgb")

# Form image object
p <- spplot(gadm, varname,
col.regions = my.palette(ncol),
main = "Suomen kuntien miehitys",
colorkey = TRUE,
lwd = .4,
col = "white",
at = seq(0.5 - interval, 0.5 + interval, length = ncol)
)

# Save the Figure into a file:
png("Suomen.kuntien.miehitys.png", width = 600, height = 600)
print(p)
dev.off()

