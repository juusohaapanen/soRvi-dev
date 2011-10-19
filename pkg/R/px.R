putsaa.px <- function (px) {

  # Preprocessing for PC Axis files 
  # ("px" or "data.frame" class)

  if (class(px) == "px") { px <- as.data.frame(px) }
  
  # Putsaa kuntanimet ym.
  fields <- c("Alue", "Kunta")
  for (nam in intersect(fields, colnames(px))) {
    px[[nam]] <- sapply(px[[nam]], function (x) {strsplit(as.character(x), " - ")[[1]][[1]]})
  }    

  px

}

