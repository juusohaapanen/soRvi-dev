#' MML data documentation 
#'
#' The MML data set contains preprocessed map data obtained from 
#' Maanmittauslaitos (MML; Finnish geographical agency). 
#' Copyright (C) MML 2011. 
#' The licenses allow modification and redistribution of the data.
#' Preprocessed versions of most publicly available data sets are included. 
#' Certain shape data sets with preprocessing problems are currently ignored.
#'
#' @name MML
#' @docType data
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org} 
#' is responsible of data preprocessing.
#' @seealso Description of the data identifiers: MML[["1_milj_Shape_etrs_shape"]] http://www.maanmittauslaitos.fi/digituotteet/yleiskartta-11-000-000; MML[["4_5_milj_shape_etrs-tm35fin"]] http://www.maanmittauslaitos.fi/digituotteet/yleiskartta-14-500-000
#' @references \url{http://www.maanmittauslaitos.fi/aineistot-palvelut/digitaaliset-tuotteet/ilmaiset-aineistot/hankinta lisenssiehtojen mukaisesti: http://www.maanmittauslaitos.fi/node/6417}
#' @usage data(MML)
#' @format list
#' @keywords data
NULL


#' translations data documentation 
#'
#' This data set contains translations for common terms for which
#' primary data is available in a non-Finnish language
#' Contents: fi.en.maakunnat: Province names Finnish-English
#'
#' @name translations
#' @docType data
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org} 
#' @references See cite(sorvi)
#' @usage data(translations)
#' @format list
#' @keywords data misc
NULL


#' fi.en.maakunnat data documentation 
#'
#' Mappings between Finnish and English province (maakunta) names
#'
#' @name fi.en.maakunnat
#' @docType data
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org} 
#' @usage data(translations)
#' @format list
#' @keywords data
NULL



#' HRI.aluejakokartat data documentation 
#'
#' This data set contains information about area boundaries for Helsinki Region
#' Helsinki Region maps from Helsinki Region Infoshare
#' Contents: pks.pienalue: Detailed area boundary data
#' 
#' @name HRI.aluejakokartat
#' @docType data
#' @author Juuso Parkkinen \email{sorvi-commits@@lists.r-forge.r-project.org} 
#' @references See cite(sorvi)
#' @usage data(HRI.aluejakokartat)
#' @format list of SpatialPolygonsDataFrames
#' @keywords data misc
NULL

