




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
