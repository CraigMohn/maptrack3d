#' maptrack3d: A package for drawing maps as 3d surfaces and displaying
#'   GPS track data on the surfaces.
#'
#'
#'
#'
#' @section main entry call:
#'   \link{draw3dMap}
#'
#' @section read SRTM data saving rasters:
#'   \link{elevationsToRaster}
#'
#' @section read shapefiles and create feature rasters form elevation rasters:
#'   \link{featuresForElevations}
#'
#'
#' @import tidyverse 
#' @import raster sp rgdal sf velox cleangeo
#' @import tigris rmapshaper OpenStreetMap  
#' @import rgl htmlwidgets 
#' @import viridis scico
#' @importFrom grDevices colorRampPalette rgb
#' @importFrom methods as
#' @importFrom utils unzip
#' @importFrom stringr str_pad
#' @importFrom gplots col2hex
#' @importFrom rgeos gIntersection gUnaryUnion gBuffer gSimplify gIntersects gContainsProperly
#' @importFrom pals linearl gnuplot 
#'
#' @name maptrack3d
NULL

###  make the R checker happy
tedious <- utils::globalVariables(c("subsegment"))


