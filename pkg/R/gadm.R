
gadm.plot <- function (url = "", taso = 2, main = "", col = NULL) {

  # see http://ryouready.wordpress.com/2009/11/16/infomaps-using-r-visualizing-german-unemployment-rates-by-color-on-a-map/                                       # http://r-spatial.sourceforge.net/gallery/ 
  # url <- "http://gadm.org/data/rda/FIN_adm"

  require(sp)

  con <- url(paste(url,taso,".RData",sep=""))
  load(con) # gadm
  close(con)

  # plotataan kaikki sinisella
  if (is.null(col)) {col <- rep("blue", length(levels(gadm[[paste("NAME_",taso,sep="")]])))}

  spplot(gadm, paste("NAME_",taso,sep=""), 
	     col.regions=col, 
	     main=main,
	     colorkey = FALSE, 
	     lwd=.4, col="white")

}
