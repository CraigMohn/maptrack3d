#' render a 3-D map from elevation and shapefile data and 
#'    add a set of paths to the plot
#'
#' \code{draw3dMap} draw 3D map from elevation and feature data,
#'    and add a set of paths to the plot
#'
#' Create a map displaying the tracks specified.  Tracks may be drawn in one
#'    specified color, with each separate track drawn in a different color from
#'    a specified palette, or as a series of closely spaced dots (which may
#'    be prodded into appearing as a line through judicious choice of point
#'    size and alpha) with the color varying based on speed and the chosen
#'    palette
#'
#' @param paths a set of segments which are contained in a 
#'    data frame or tibble containing at least: lon, lat, (these may also have
#'    the names "position_lon.dd" and "position_lat.dd") and are both 
#'    numeric with decimal degrees, and segment(numeric) to identify 
#'    different paths or segments on a path.  If trackCurveElevFromRaster
#'    is FALSE, the data frame must include a column named altitude.m which 
#'    contains the elevation recorded by the GPS.  The optional character
#'    variable color specifies the color on track segments 
#' @param mapWindow a vector of 4 numbers which describe the region drawn.
#'    The format is c(lon_min, lon_max, lat_min, lat_max)
#'    
#' @param USStatevec vector of standard 2-letter abbreviations of US states, or
#'    pre-defined regional aggregations of states for use in defining map or
#'    finding features to include in the map
#' @param CAProvincevec vector of standard 2-letter abbreviations of Canadian
#'    provinvces states, or pre-defined regional aggregations of states for 
#'    use in defining map or finding features to include in the map
#' @param USParkvec vector of US National Park names.  If specified, map
#'    will include the specified Parks but not the listed US states
#' @param parkDir string location of downloaded US Park boundary shapefiles 
#'    downloaded from, for example, https://irma.nps.gov/DataStore/ 
#' @param worldCountryvec vector of 3-letter ISO country abbreviations 
#' @param mapbuffer numeric value to expand defined map
#' @param mapmergebuffer numeric value to expand components (states, provinces,
#'    countries) of the map defined before merging, to eliminate gaps from minor
#'    boundary inconsistencies
#' @param cropbox vector of 4 numbers for cropping the map defined above
#'    The format is c(lon_min, lon_max, lat_min, lat_max).
#' @param rectangularMap logical, draw the entire rectangle enclosing the specified
#'    map area, as long as all relevant state/province rasters have been loaded
#'
#' @param elevDataSource character, "SRTM" to load data from saved SRTM data, 
#'    "Raster" to load saved raster files
#' @param mapDataDir character, directory where zipped SRTM data files reside
#' @param resstr character, suffix on SRTM data files after lon/lat info
#' @param rasterFileSetNames vector of names of saved raster data files
#' 
#' @param featureDataSource character, "Shapefiles" to load saved shapefiles, 
#'    "TIGER" to fetch TIGER data for US states, "Raster" to load saved raster data
#'    from directory specified , "none" to show none
#' @param shapefileDir character location to load/save shapefiles
#' @param writeShapefiles logical, write/overwrite shapefiles if TIGER data is used
#' @param year numeric year to use in calls for map boundaries/features
#' @param includeAllRoads logical, include all roads in shapefile, not just highways
#' @param zeroBufferTowns logical, use zero buffer trick to repair town polygon shapefile
#' @param zeroBufferWater logical, use zero buffer trick to repair water polygon shapefile
#' 
#' @param useImageRaster logical, use the image raster from saved openStreetmap 
#'    colrings of the map surface
#' 
#' @param writeElevFile logical, save the elevation raster files 
#' @param writeFeatureFile logical, save the feature data raster files
#' @param writeImageFile logical, save the "bing","apple-iphoto","stamen-terrain"
#'    images specified in rglColorScheme as a raster file
#' @param imageFilename name of raster or tiff file containing map background    
#' @param rasterDir character location to load and save raster files
#' @param rasterFileSetWriteName name to use for the raster fileset written

#' @param drawRGL logical, open a window with the interactive 3D map
#' @param res3dplot numeric, used to scale resolution, the maximum size the square root
#'    of the output height times width.  aggregation done in integer multiples.
#' @param drawProj4 string containing the projection to use in drawing the map,
#'    "UTM" (zone based on center of map),"Lambert","Albers", or any Proj4 string
#' @param maxElev numeric, all elevations greater than this are colored the same
#' @param vScale numeric vertical scale parameter, try bigger values for smaller areas
#' @param townLevel numeric, display towns ranked this number or higher: 
#'    3=all towns     5=larger towns (in US >50k)
#' @param roadLevel numeric, display roads ranked this number or higher:
#'    2=Service Drive, Bike Path, etc      3=Local Street
#'    4=Secondary Hwy                      5=Primary Hwy/Transit
#' @param waterALevel numeric, display areal water ranked this number or higher:
#'    2=res/treatmentpond/pit/quarry       3=lake/pond/swamp/stream
#'    4=class 2 or 3 bigger than 1k ha     5=named lake/pond/swamp/stream
#'    6=large lake/pond/swamp/stream       7=Ocean/Bay/Est/Sound
#'    8=glacier
#' @param waterLLevel numeric, display linear water ranked this number or higher:
#'    2=canal/ditch                        3=braided stream
#'    4=stream/river                       5=named stream/river
#'    6=named stream/river containing the string "RIV"
#' @param rglColorScheme name of color scheme from 
#'     c("default","beach","viridis","plasma","terrain","oleron","snow","oslo",
#'       "desert","lajolla","niccoli","bright",
#'       "bing","maptoolkit-topo","nps","apple-iphoto")
#' @param mapColorDepth number of bits per color channel in map image
#' @param rglNAcolor character, color used to display NA elevations
#' @param rglNegcolor character, color used to display elevations below sea level
#' @param citycolor character, color used to display cities
#' @param watercolor character, color used to display water,
#' @param roadcolor character, color used to display roads
#' @param glaciercolor character, color used to display glaciers
#' @param rglShininess number controlling surface shininess in rgl rendering
#' @param rglSpecular character, light color for specular light
#' @param rglDiffuse character, light color for diffuse light
#' @param rglAmbient character, light color for ambient light
#' @param rglEmission character, color of emitted light 
#' @param rglSmooth logical, use Gouraud shading if TRUE
#' @param rglAlpha numeric alpha value for surface
#' @param rglAntiAlias logical, antialias points and lines when drawing surface
#' @param rglTheta numeric coordinate for light source
#' @param rglPhi numeric coordinate for light source
#' 
#' @param trackColor character, name of color used to draw tracks
#' @param trackCurve logical, draw the tracks as curves above the surface
#' @param trackWidth integer, add to minimal line width 
#' @param trackCurveElevFromRaster logical, get curve elevations from elevation raster
#' @param trackCurveHeight numeric, distance above surface to draw curve (meters)
#'
#' @param saveRGL logical, save the map to an html file
#' @param mapoutputdir character location for saved html map file
#' @param outputName name of saved html map
#' 
#' @param gapTooLong numeric (meters). Sequential track observations which are
#'    farther aopart than this are not connected
#' @param workProj4 coordinate reference projection string
#' @param maxrastercells maximum number of cells in each written raster
#' @param maxRasterize number of number of items for calls to velox$rasterize
#' @param polySimplify numeric, amount of polygon simplification, see help
#'    for rmapshaper::ms_simplify 
#' @param polyMethod simplification method either "vis" or "dp"
#' @param polyWeighting see help for rmapshaper::ms_simplify 
#' @param polySnapInt see help for rmapshaper::ms_simplify
#' 
#' @param silent logical, suppress most output
#' @param noisy logical, more output to track progress 
#' 
#' @return NULL
#' 
#' @examples 
#' \dontrun{
#' ##  draw a rectangular region from local SRTM data
#' ##    to download the data, start at https://earthexplorer.usgs.gov/ ,
#' ##    create a free account and order the data you need
#' ##    put it into mapDataDir
#' mapWindow <- c(23.3,27.4,34.7,36.0)  # Crete
#' draw3dMap(mapWindow=mapWindow,
#'           mapDataDir="c:/bda/Europe 1s",
#'           vScale=1.6,
#'           rglColorScheme="bing",
#'           drawProj4="UTM", 
#'           saveRGL=TRUE,mapoutputdir=mapoutputdir,
#'           outputName="Crete")
#' 
#' ##  draw US Yosemite National Park from local SRTM data
#' draw3dMap(USParkvec="YOSE",USStatevec="CA",
#'           parkDir=parkDir,mapbuffer=10000,
#'           mapDataDir=mapDataDir,
#'           shapefileDir=shapefileDir,
#'           featureDataSource="shapefiles",
#'           townLevel=3,roadLevel=2,waterALevel=4,waterLLevel=5,
#'           vScale=1.5,
#'           rglColorScheme="terrain",
#'           drawProj4="UTM",
#'           saveRGL=TRUE,mapoutputdir=mapoutputdir,
#'           outputName="Yosemite")
#' ##  draw US Yosemite National Park from CGIAR hosted SRTM data
#' draw3dMap(USParkvec="YOSE",USStatevec="CA",
#'           parkDir=parkDir,mapbuffer=10000,
#'           vScale=1.5,
#'           rglColorScheme="bing",
#'           drawProj4="UTM", 
#'           saveRGL=TRUE,mapoutputdir=mapoutputdir,
#'           outputName="Yosemite CGIAR bing")
#' ##  draw US Yosemite National Park from CGIAR hosted SRTM data and TIGER data
#' draw3dMap(USParkvec="YOSE",USStatevec="CA",
#'           parkDir=parkDir,mapbuffer=10000,
#'           shapefileDir=shapefileDir,
#'           featureDataSource="shapefiles",
#'           townLevel=3,roadLevel=2,waterALevel=4,waterLLevel=5,
#'           vScale=1.5,
#'           rglColorScheme="bing",
#'           drawProj4="UTM",
#'           saveRGL=TRUE,mapoutputdir=mapoutputdir,
#'           outputName="Yosemite CGIAR bing TIGER")
#' ## draw mainland Portugal from CGIAR hosted data
#' draw3dMap(worldCountryvec="PRT",
#'           cropbox=c(-10, 10, 36, 43), # remove islands
#'           mapbuffer=100, # expand boundary to speed masking
#'           vScale=1.4,    # increase scale with bing coloring
#'           rglColorScheme="bing",
#'           drawProj4="Albers", 
#'           saveRGL=TRUE,mapoutputdir=mapoutputdir,
#'           outputName="Portugal Albers bing")
#' }
#'
#' @export
draw3dMap <- function(paths=NULL,
                      # map shape parameters
                      mapWindow=NULL,
                      USStatevec=NULL,CAProvincevec=NULL,
                      USParkvec=NULL,parkDir=NULL,
                      worldCountryvec=NULL,
                      cropbox=NULL,mapbuffer=0,mapmergebuffer=0,
                      rectangularMap=TRUE,
                      #  elevation data parameters
                      elevDataSource="SRTM",
                      mapDataDir=NULL,resstr="_1arc_v3_bil",
                      rasterFileSetNames=NULL,
                      #  water/town/road features
                      featureDataSource="none",
                      shapefileDir=NULL,writeShapefiles=TRUE,
                      year=2017,includeAllRoads=FALSE,
                      zeroBufferTowns=FALSE,
                      zeroBufferWater=FALSE,
                      useImageRaster=FALSE,
                      #  raster save and location parameters
                      writeElevFile=FALSE,writeFeatureFile=FALSE,
                      writeImageFile=FALSE,imageFilename=NULL,
                      rasterDir=NULL,rasterFileSetWriteName=NULL,
                      #  plotting control
                      drawRGL=TRUE,
                      res3dplot=2500,drawProj4=NULL,
                      maxElev=3000,vScale=1.5,
                      townLevel=3,roadLevel=4,waterALevel=4,waterLLevel=5,
                      rglColorScheme="default",mapColorDepth=16,
                      rglNAcolor="Blue",rglNegcolor="Red",
                      citycolor="SlateGray",watercolor="Blue",
                      roadcolor="Black",glaciercolor="White",
                      rglShininess=0.02,
                      rglSpecular="black", rglDiffuse="white", 
                      rglAmbient="white", rglEmission="black",
                      rglSmooth=TRUE,
                      rglAlpha=1.0,
                      rglAntiAlias=TRUE,
                      rglTheta=0,
                      rglPhi=15,
                      trackColor="Magenta",
                      trackCurve=FALSE,
                      trackWidth=0,
                      trackCurveElevFromRaster=FALSE,
                      trackCurveHeight=15,
                      saveRGL=FALSE,mapoutputdir=NULL,outputName=NULL,
                      #  CRS, rasterization control
                      gapTooLong=100,
                      workProj4="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                      maxrastercells=250000000,maxRasterize=500000,
                      polySimplify=0.0,polyMethod="vis", 
                      polyWeighting=0.85,polySnapInt=0.0001,
                      silent=FALSE,noisy=FALSE) {

  elevDataSource <- argCaseFix(elevDataSource,c("Raster","SRTM"))
  featureDataSource <- argCaseFix(featureDataSource,c("Raster","Shapefiles","TIGER","none"))

  filterList <- list("spTown"=townLevel,
                     "spRoads"=roadLevel,
                     "spWaterA"=waterALevel,
                     "spWaterL"=waterLLevel)
  
  rewriteRasters <- elevDataSource=="Raster" & 
                   featureDataSource=="Raster" &
                   !is.null(rasterFileSetWriteName) &
                   (writeElevFile | writeFeatureFile)
  
  if (elevDataSource=="Raster" & writeElevFile &
           is.null(rasterFileSetWriteName)) {
    warning("will not overwrite elev rasterfileset that was source")
    writeElevFile <- FALSE
  }
  if (featureDataSource=="Raster" & writeFeatureFile &
           is.null(rasterFileSetWriteName)){
    warning("will not overwrite feature rasterfileset that was source")
    writeFeatureFile <- FALSE
  }
  
  statesInMap <- union(expandRegions(unique(toupper(USStatevec)),"US"),
                       expandRegions(unique(toupper(CAProvincevec)),"CANADA"))
  if (is.null(statesInMap) & 
      is.null(rasterFileSetNames) & 
      elevDataSource=="Raster") 
    stop(paste0("no state/province or dataset names specified for loading elevations"))
  if (is.null(statesInMap) & 
      is.null(rasterFileSetNames) & 
      featureDataSource=="Raster") 
    stop(paste0("no state/province or dataset names specified for loading features"))
 
  #############################################################################
  ####    set up masking map shape
  getMap <- !is.null(USStatevec) | !is.null(CAProvincevec) |
            !is.null(USParkvec) | !is.null(worldCountryvec) |
            !is.null(mapWindow) 
  if (getMap) {
    mapshape <- mapMask(USStatevec=USStatevec,
                        CAProvincevec=CAProvincevec,
                        USParkvec=USParkvec,
                        worldCountryvec=worldCountryvec,
                        mapWindow=mapWindow,
                        mapbuffer=mapbuffer,
                        mapmergebuffer=mapmergebuffer,
                        parkDir=parkDir,
                        workProj4=workProj4,
                        year=year)
    if (!is.null(cropbox)) {
      if (writeElevFile | writeFeatureFile | writeShapefiles) 
        warning("cropping map when saving raster/shapefiles.")
      CP <- as(raster::extent(cropbox), "SpatialPolygons")
      sp::proj4string(CP) <- CRS(sp::proj4string(mapshape))
      mapshape <- rgeos::gIntersection(mapshape, CP, byid=TRUE)
    }
    if (!silent) plot(mapshape) # which has CRS=workproj4
    if (noisy) print(mapshape)
  }
  #############################################################################
  ###    load or build elevations raster, crop it to mapshape
  if (elevDataSource=="Raster") {
    if (!is.null(rasterFileSetNames)) {
      savedRasterVec <- rasterFileSetNames
    } else {
      savedRasterVec <- statesInMap
    }
    elevations <- loadSavedElevData(savedRasterVec,rasterDir,
                                    noisy=noisy,silent=silent)
    if (!getMap) {
      # if map area wasn't specified above, use extent of raster
      mapshape <- as(raster::extent(elevations),"SpatialPolygons")
      if (!is.null(cropbox)) {
        CP <- as(raster::extent(cropbox), "SpatialPolygons")
        sp::proj4string(CP) <- CRS(sp::proj4string(mapshape))
        mapshape <- rgeos::gIntersection(mapshape, CP, byid=TRUE)
      }
    }
    if (!is.null(mapWindow) | !is.null(USParkvec) | !is.null(cropbox)) {
      if (!silent) print("cropping")
      temptime <- round(system.time(
        elevations <- quickmask(elevations,mapshape,rectangle=TRUE)
      )[[3]],digits=2)
      if (!silent) print(paste0("  ",temptime))
    }
  }  else {
    elevations <- loadMapElevData(mapshape=mapshape,mapDataDir=mapDataDir,
                                  resstr=resstr,noisy=noisy,silent=silent)
    #  raster is masked
  }
  
  ###   and write out the elevation raster if requested
  if (writeElevFile) {
    if (!is.null(rasterFileSetWriteName)) {
      elevfname <- rasterFileSetWriteName
    } else {
      elevfname <- paste0(statesInMap,collapse="")
    }
    writeElevRaster(elevations=elevations,
                    maxrastercells=maxrastercells,
                    rasterDir=rasterDir,
                    fname=elevfname,
                    noisy=noisy,silent=silent)
    if (writeImageFile) {
      imageForRasters(rasterFileSetName=elevfname,
                      rasterDir=rasterDir,
                      imageSource=rglColorScheme)   
    }
  } else {
    elevfname <- "error"
  }
  ###  load or create feature rasterStack, saving if necessary
  featureStack <- NULL
  if (featureDataSource=="Raster") {
    if (!is.null(rasterFileSetNames)) {
      savedNameVec <- rasterFileSetNames
    } else {
      savedNameVec <- statesInMap
    }
    featureStack <- loadSavedFeatureData(savedNameVec,rasterDir,
                                         noisy=noisy,silent=silent)
    if (!is.null(mapWindow) | !is.null(USParkvec) | !is.null(cropbox)) {
      if (!silent) print("cropping")
      temptime <- round(system.time(
        featureStack <- quickmask(featureStack,mapshape,rectangle=TRUE)
      )[[3]],digits=2)
      if (!silent) print(paste0("  ",temptime))
    }
    if (rewriteRasters) {
      if (!is.null(rasterFileSetWriteName)) {
        fname <- rasterFileSetWriteName
      } else {
        fname <- paste0(statesInMap,collapse="")
      }
      writeFeatureRaster(featureStack=featureStack,
                         maxrastercells=maxrastercells,
                         rasterDir=rasterDir,
                         fname=fname,
                         noisy=noisy,silent=silent)
    }
  } else if (featureDataSource %in% c("Shapefiles","TIGER")) {
    if (writeFeatureFile & writeElevFile) {
      featuresForElevations(rasterFileSetName=elevfname,
                            rasterDir=rasterDir,
                            shapefileDir=shapefileDir,
                            USStatevec=USStatevec,
                            CAProvincevec=CAProvincevec,
                            featureDataSource=featureDataSource,
                            writeShapefiles=writeShapefiles,
                            includeAllRoads=includeAllRoads,
                            zeroBufferWater=zeroBufferWater,
                            workProj4=workProj4,
                            maxRasterize=maxRasterize,
                            polySimplify=polySimplify,
                            polyMethod=polyMethod, 
                            polyWeighting=polyWeighting,
                            polySnapInt=polySnapInt,
                            noisy=noisy,silent=silent)
      featureStack <- loadSavedFeatureData(elevfname,rasterDir)
    } else {
      spList <- loadShapeFiles(USStatevec=USStatevec,
                            CAProvincevec=CAProvincevec,
                            mapshape=mapshape,
                            shapefileDir=shapefileDir,
                            writeShapefiles=writeShapefiles,
                            shapefileSource=featureDataSource,
                            includeAllRoads=includeAllRoads,
                            year=year,
                            zeroBufferTowns=zeroBufferTowns,
                            zeroBufferWater=zeroBufferWater)
      if (writeFeatureFile){
        featureStack <- buildFeatureStack(baseLayer=elevations,
                                          mapshape=mapshape,
                                          spList=spList,
                                          maxRasterize=maxRasterize,
                                          sliceFeatureBuffer=0,
                                          polySimplify=polySimplify,
                                          polyMethod=polyMethod, 
                                          polyWeighting=polyWeighting,
                                          polySnapInt=polySnapInt,
                                          silent=silent,noisy=noisy)
        if (!is.null(rasterFileSetWriteName)) {
          featfname <- rasterFileSetWriteName
        } else {
          featfname <- paste0(statesInMap,collapse="")
        }
        if (!silent) print(paste0("writing ",featfname,"features.grd"))
        writeRaster(featureStack,
                    filename=paste0(rasterDir,"/",featfname,"/",
                                    featfname,"features",
                                    ".grd"),
                    bylayer=TRUE,suffix="names",   
                    datatype="INT1S",overwrite=TRUE)   
      }
    }
  } else {
    spList <- NULL
  }
  if (!is.null(featureStack)) { 
    elevations <- raster::addLayer(elevations,featureStack)
    names(elevations[[1]]) <- "elevations"
  }
  ##  rasters are cropped but not necessarily masked by this point
  if (!silent) print(paste0(elevations@ncols," columns by ",elevations@nrows," rows"))
  sfact <- ceiling(sqrt((as.double(elevations@ncols)/res3dplot)*
                        (as.double(elevations@nrows)/res3dplot)))
  if (sfact > 1) {
    if (!silent) print(paste0("scaling raster down by a factor of ",sfact))
    temptime <- system.time(
      elevations <- raster::aggregate(elevations,fact=sfact,fun=max,
                                      expand=TRUE,na.rm=TRUE)
    )[[3]]
    if (!silent) print(paste0(" ",temptime))
    if (!silent) print(paste0(elevations@ncols," columns by ",elevations@nrows," rows"))
  }

  if ( featureDataSource %in% c("Shapefiles","TIGER") &
      # and we have not generated a raster for native Proj4 
       ( is.null(featureStack) | !is.null(drawProj4) ) ) {
    spList <- filterFeatures(spList,filterList)
  } else {
    spList <- NULL
  }
  ##  need to mask if not a rectangle - whole states/provinces/countries are masked already,
  ##     so only need to worry about Parks
  if (is.null(mapWindow) & !rectangularMap & !is.null(USParkvec)) {
    temptime <- round(system.time(
      elevations <- quickmask(elevations,mapshape,rectangle=FALSE)
    )[[3]],digits=2)
    if (!silent) print(paste0("  ",temptime," masking"))
  }
  if (useImageRaster) {
    imageRaster <- loadSavedImageData(savedNameVec=savedRasterVec,
                                      rasterDir=rasterDir,
                                      imageSource=rglColorScheme,
                                      noisy=noisy,silent=silent)
  } else {
    imageRaster <- NULL
  }
  if (drawRGL)
    draw3DMapTrack(mapRaster=elevations,trackdf=paths,spList=spList,
                   featureLevels=filterList, #towns,roads,waterA,waterL
                   maxElev=maxElev,vScale=vScale,drawProj4=drawProj4,
                   rglColorScheme=rglColorScheme,
                   mapColorDepth=mapColorDepth,
                   imageFilename=imageFilename,
                   imageRaster=imageRaster,
                   citycolor=citycolor,roadcolor=roadcolor,
                   watercolor=watercolor,glaciercolor=glaciercolor,
                   rglNAcolor=rglNAcolor,rglNegcolor=rglNegcolor,
                   rglShininess=rglShininess,rglSmooth=rglSmooth,
                   rglAlpha=rglAlpha,rglAntiAlias=rglAntiAlias,
                   rglSpecular=rglSpecular,rglDiffuse=rglDiffuse,
                   rglAmbient=rglAmbient,rglEmission=rglEmission,
                   rglTheta=rglTheta,rglPhi=rglPhi,saveRGL=saveRGL,
                   trackColor=trackColor,trackCurve=trackCurve,
                   trackWidth=trackWidth,
                   trackCurveElevFromRaster=trackCurveElevFromRaster,
                   trackCurveHeight=trackCurveHeight,
                   gapTooLong=gapTooLong,
                   mapoutputdir=mapoutputdir,outputName=outputName,
                   silent=silent,noisy=noisy) 
  return(NULL)
}
#' set up call to draw3dMap with colors for the tracks
#'
#' \code{drawTrackCurves} draw 3D map from elevation and feature data,
#'    and add a set of colored paths to the plot
#'
#' This is a wrapper for draw3dMap.
#'
#' @param paths dataframe see draw3dMap
#' @param colorMode character either "track" where each segment in the path has 
#'   a different color sequentially chosen from the palette, or "speed" where the 
#'   pieces of the path are colored based on the speed recorded.
#' @param drawPalette name of the palette to be used.  Options are "plasma",
#'   "magma","viridis","heat","rainbow","red-blue-green","red-blue","default"
#' @param ...  parameters passed to draw3dMap
#' 
#' @return NULL
#' @export
#' 
drawColorTracks <- function(paths,drawPalette="default",colorMode="track",...) {
                            #trackCurve=TRUE,...) {
    
  if (colorMode=="speed" & "speed.m.s" %in% names(paths)) {
    if (drawPalette=="plasma") {
      spdcolors <- rev(plasma(101))
    } else if (drawPalette=="magma") {
      spdcolors <- rev(magma(101))
    } else if (drawPalette=="viridis") {
      spdcolors <- rev(viridis(101))
    } else if (drawPalette=="heat") {
      spdcolors <- rev(heat.colors(101))
    } else if (drawPalette=="rainbow") {
      spdcolors <- (rainbow(101,start=0.15,end=1))
    } else if (drawPalette=="red-blue-green") {
      spdcolors <- colorRampPalette(c("red","blue","green"))(101)
    } else if (drawPalette=="red-blue") {
      spdcolors <- colorRampPalette(c("red","blue"))(101)
    } else {
      spdcolors <- colorRampPalette(c("red","orange","cornflowerblue",
                                      "dodgerblue","blue","darkorchid",
                                      "purple","magenta"))(101)
    }
    speed <- paths$speed.m.s*2.23694
    speed[speed>40] <- 40
    speed[speed<3] <- 3
    paths$color <- spdcolors[floor(100*(speed - 3)/37) + 1]
  } else {
    segs <- unique(paths$segment)
    if (drawPalette=="plasma") {
      mapcvec <- viridis::plasma(length(segs),begin=0.0,end=0.7)
    } else if (drawPalette=="magma") {
        mapcvec <- viridis::plasma(length(segs),begin=0.0,end=0.7)
    } else if (drawPalette=="viridis") {
      mapcvec <- viridis::viridis(length(segs),begin=0.1,end=0.9)
    } else if (drawPalette=="heat") {
      mapcvec <- heat.colors(length(segs))
    } else if (drawPalette=="rainbow") {
      mapcvec <- rainbow(length(segs),start=0.2,end=0.9)
    } else if (drawPalette=="red-blue-green") {
      mapcvec <- colorRampPalette(c("red","blue","green"))(length(segs))
    } else if (drawPalette=="red-blue") {
      mapcvec <- colorRampPalette(c("red","blue"))(length(segs))
    } else if (drawPalette=="default") {
      mapcvec <- colorRampPalette(c("red","orange","cornflowerblue",
                                    "dodgerblue","blue","darkorchid",
                                    "purple","magenta"))(length(segs))
    } else {
      mapcvec <- rep(drawPalette,length(segs))
    }
    paths$color <- mapcvec[match(paths$segment, segs)]
  }
  draw3dMap(paths=paths,...)
  return(NULL)
}
