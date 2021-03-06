% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/featuresForElevations.R
\name{elevationsToRaster}
\alias{elevationsToRaster}
\title{Create and save a raster fileset specified}
\usage{
elevationsToRaster(
  rasterFileSetName = "default",
  rasterDir,
  mapDataDir,
  USStatevec = NULL,
  CAProvincevec = NULL,
  USParkvec = NULL,
  worldCountryvec = NULL,
  mapWindow = NULL,
  cropbox = NULL,
  parkDir = NULL,
  mapbuffer = 0,
  mapmergebuffer = 0,
  maxrastercells = 2.5e+08,
  workProj4 = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
  year = 2017,
  resstr = "_1arc_v3_bil"
)
}
\arguments{
\item{rasterFileSetName}{names of saved raster data files}

\item{rasterDir}{location to load and save raster files}

\item{mapDataDir}{directory where zipped SRTM data files reside}

\item{USStatevec}{vector of standard 2-letter abbreviations of US states, or
pre-defined regional aggregations of states for use in defining map or
finding features to include in the map}

\item{CAProvincevec}{vector of standard 2-letter abbreviations of Canadian
provinvces states, or pre-defined regional aggregations of states for 
use in defining map or finding features to include in the map}

\item{USParkvec}{vector of US National Park names}

\item{worldCountryvec}{vector of 3-letter ISO country abbreviations}

\item{mapWindow}{a vector of 4 numbers which describe the region drawn.
The format is c(lon_min, lon_max, lat_min, lat_max)}

\item{cropbox}{vector of 4 numbers for cropping the map defined above
The format is c(lon_min, lon_max, lat_min, lat_max).}

\item{parkDir}{location of downloaded US Park boundary shapefiles 
downloaded from, for example, https://irma.nps.gov/DataStore/}

\item{mapbuffer}{numeric value to expand defined map}

\item{mapmergebuffer}{numeric value to expand components of the 
map defined before merging, to eliminate gaps from minor
boundary inconsistencies}

\item{maxrastercells}{maximum number of cells in each written raster}

\item{workProj4}{coordinte reference projection string}

\item{year}{numeric year to use in calls for map boundaries/features}

\item{resstr}{suffix on SRTM data files after lon/lat info}
}
\value{
a rasterLayer containing the elevations
}
\description{
\code{elevationsToRaster} create a raster fileset of elevations
}
\details{
Create a raster file set containing elevation data from SRTM BIL 
  zip files stored in a local directory
}
