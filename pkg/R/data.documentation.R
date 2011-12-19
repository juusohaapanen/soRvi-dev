#' MML data documentation 
#'
#' The MML data set contains preprocessed map data obtained from 
#' Maanmittauslaitos (MML; Finnish geographical agency). 
#' Copyright (C) MML 2011. 
#' The licenses allow modification and redistribution of the data.
#' 
#' @name MML
#' @docType data
#' @description (C) MML 2011.
#' @author Leo Lahti \email{sorvi-commits@lists.r-forge.r-project.org} 
#' is responsible of data preprocessing.
#' @references \url{http://www.maanmittauslaitos.fi/aineistot-palvelut/digitaaliset-tuotteet/ilmaiset-aineistot/hankinta lisenssiehtojen mukaisesti: http://www.maanmittauslaitos.fi/node/6417}
#' @usage data(MML)
#' @format list
#' @keywords data
NULL



#' translations data documentation 
#'
#' This data set contains translations for common terms for which
#' primary data is available in a non-Finnish language
#' 
#' @name translations
#' @docType data
#' @description Translations for common terms for which primary data is available in a non-Finnish language. 
#' @details Contents: fi.en.maakunnat: Province names Finnish-English
#' @author Leo Lahti \email{sorvi-commits@lists.r-forge.r-project.org} 
#' @references See cite(sorvi)
#' @usage data(translations)
#' @format list
#' @keywords data, misc
NULL



#' PKS.lukiot data documentation 
#'
#' This data set contains information about high schools in the Helsinki Region
#' 
#' @name PKS.lukiot
#' @docType data
#' @description Helsinki Region high school performance data from year 2011
#' @details Contents: hr.lukiot: Helsinki Region high school data
#' @author Juuso Parkkinen \email{sorvi-commits@lists.r-forge.r-project.org} 
#' @references See cite(sorvi)
#' @usage data(PKS.lukiot)
#' @format data.frame
#' @keywords data, misc
NULL



#' Oikotie data documentation 
#'
#' This data set contains information about apartment prices from Oikotie
#' 
#' @name Oikotie
#' @docType data
#' @description Helsinki Region apartment prices for years 2010-2011 from Oikotie
#' @details Contents: myynnit: National data, hr.myynnit: Helsinki redion data
#' @author Juuso Parkkinen \email{sorvi-commits@lists.r-forge.r-project.org} 
#' @references See cite(sorvi)
#' @usage data(Oikotie)
#' @format data.frame
#' @keywords data, misc
NULL



#' PKS.aluejakokartat data documentation 
#'
#' This data set contains information about area boundaries for Helsinki Region
#' 
#' @name PKS.aluejakokartat
#' @docType data
#' @description Helsinki Region maps from Helsinki Region Infoshare
#' @details Contents: pks.pienalue: Detailed area boundary data
#' @author Juuso Parkkinen \email{sorvi-commits@lists.r-forge.r-project.org} 
#' @references See cite(sorvi)
#' @usage data(PKS.aluejakokartat)
#' @format SpatialPolygonsDataFrame
#' @keywords data, misc
NULL

