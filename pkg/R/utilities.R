#To call in the statistician after the experiment is done may be no more
#than asking him to perform a post-mortem examination: he may be able to
#say what the experiment died of.
#~ Sir Ronald Aylmer Fisher

#The plural of anecdote is not data.
#~ Roger Brinner

#The combination of some data and an aching desire for an answer does not
#ensure that a reasonable answer can be extracted from a given body of
#data.
#~ John Tukey



dfsort <- function(x, sortvar, ...) {
  #Sort data frame dd by columns like: esort(dd, -z, b)

  attach(x)
  x <- x[with(x,order(sortvar,...)),]
  return(x)
  detach(x)
}


korvaa.skandit <- function (s) {

  # Ks. myos iconv function

  # Korvaa skandit a/o versiolla
  s <- gsub("\\xe4", "a", s)
  s <- gsub("\\xC4", "A", s)
  s <- gsub("\\xD6", "O", s)
  s <- gsub("\\xf6", "o", s)
  s <- gsub("\\xE5", "a", s)

  s
}

shape2sorvi <- function (files) {

  ids <- unlist(sapply(files, function (x) {strsplit(x, "\\.")[[1]][[1]]}))
   
  # (C) Leo Lahti 2008-2011
  # FreeBSD license (keep this notice)

  # Take list of shape file names (or IDs without .shp ending)
  # and return a corresponding list of shape objects  

  shapedata <- list()

  for (id in ids) {
    print(id)
    shapedata[[id]] <- try(readShapePoly(id))
  }

  # If just one file converted, give directly the shape file as out put
  # (and not a list)
  if (length(files) == 1) {
    shapedata <- shapedata[[1]]
  }

  shapedata

}


strstrip <- function (mystr) {

  # (C) Leo Lahti 2008-2011
  # FreeBSD license (keep this notice)

  # Remove spaces from a string (single string or vector/list of strings)

  if (length(mystr) == 1) {
    stripped <- strstrip.single(mystr)
  } else {
    stripped <- sapply(mystr, strstrip.single)
  }

  stripped
}


strstrip.single <- function (mystr) {

  # Remove spaces from a string

  # (C) Leo Lahti 2008-2011
  # FreeBSD license (keep this notice)

  # Strip string i.e. remove spaces from the beginning and end
  while (substr(mystr,1,1)==" ") {
    mystr <- substr(mystr,2,nchar(mystr))
  }
  while (substr(mystr,nchar(mystr),nchar(mystr))==" ") {
    mystr <- substr(mystr,1,nchar(mystr)-1)
  }
  mystr
}
