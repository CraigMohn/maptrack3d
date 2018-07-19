draw3DMapTrack <- function(mapRaster,
                           trackdf=NULL,
                           spList=NULL,  # overrides feature layers in rasterStack 
                           featureLevels=NULL,
                           maxElev=3000,
                           vScale=1.5,
                           drawProj4=NULL,
                           rglColorScheme="default",
                           mapColorDepth=16,
                           imageFilename=NULL,
                           imageRaster=NULL,
                           citycolor="White",
                           roadcolor="Black",
                           watercolor="Blue",
                           glaciercolor="White",
                           rglNAcolor="Blue",
                           rglNegcolor=NA,
                           rglShininess=0,
                           rglSmooth=TRUE,
                           rglAlpha=1.0,
                           rglAntiAlias=TRUE,
                           rglSpecular="black", 
                           rglDiffuse="white",
                           rglAmbient="white", 
                           rglEmission="black",
                           rglTheta=0,rglPhi=15,
                           trackColor="Magenta",
                           trackCurve=FALSE,
                           trackCurveElevFromRaster=TRUE,
                           trackCurveHeight=10,
                           gpsProj4="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                           saveRGL=FALSE,
                           mapoutputdir=NA,
                           outputName="most recent",
                           silent=FALSE,noisy=FALSE) {
  
  if (is.null(featureLevels)) 
    featureLevels <- list("spTown"=1,
                          "spRoads"=1,
                          "spWaterA"=1,
                          "spWaterL"=1)

  if (!is.null(trackdf)) {
    trackLines <- trackpts_to_lines(lon=trackdf$lon,lat=trackdf$lat,
                                    id_field=trackdf$segment) 
    trackPoints <- trackpts_to_spPointDF(trackdf=trackdf,
                                         gpsProj4=gpsProj4,
                                         workProj4=raster::crs(mapRaster))
    if (trackCurveElevFromRaster)
      #  get altitude
      trackPoints@data[,"altitude.m"] <- 
               raster::extract(mapRaster[["elevations"]],sp::coordinates(trackPoints),
                               method="simple")
  }
  
  if (!is.null(drawProj4)) { 
    if (drawProj4=="UTM") {
      drawProj4 <- UTMProj4(lon=(extent(mapRaster)[1]+extent(mapRaster)[2])/2,
                            lat=(extent(mapRaster)[3]+extent(mapRaster)[4])/2)
    } else if (drawProj4=="Lambert") {
      drawProj4 <- CRS_LambertAzimuthalEqualArea(lon=(extent(mapRaster)[1]+extent(mapRaster)[2])/2,
                                                 lat=(extent(mapRaster)[3]+extent(mapRaster)[4])/2)
    } else if (drawProj4=="Albers") {
      drawProj4 <- CRS_AlbersEqualArea(lon=(extent(mapRaster)[1]+extent(mapRaster)[2])/2,
                                       lat1=extent(mapRaster)[3],
                                       lat2=extent(mapRaster)[4] )
    } 
    if (!silent) print(paste0("projecting raster to ",drawProj4))
    if (is.null(spList) | (class(mapRaster)=="RasterLayer")) {
      mapRaster <- raster::projectRaster(mapRaster,crs=drawProj4,method="ngb")
    } else {
      # if spList non-NULL and rasterStack, project only elevation layer
      mapRaster <- 
        raster::projectRaster(mapRaster[["elevations"]],crs=drawProj4)
    }
    if (!is.null(spList)) {
      if (!silent) print(paste0("projecting feature shapes to ",drawProj4))
      for (x in names(spList)) {
        spList[[x]] <-  spXformNullOK(spList[[x]],CRS(drawProj4))
      }
    }
    if (!is.null(imageRaster)) {
      if (!silent) print(paste0("projecting map image to ",drawProj4))
      imageRaster <- raster::projectRaster(imageRaster,crs=drawProj4,method="ngb")
    }
    if (!is.null(trackdf)) {
      if (!silent) print(paste0("projecting tracks to ",drawProj4))
      trackLines <-  spXformNullOK(trackLines,CRS(drawProj4))
      trackPoints <-  spXformNullOK(trackPoints,CRS(drawProj4))
    }
  }
  if (class(mapRaster)=="RasterLayer") {
    elevations <- mapRaster
  } else {
    elevations <- mapRaster[["elevations"]]
  }
  if (!is.null(spList)) {
    mapRaster <- buildFeatureStack(baseLayer=elevations,
                                   mapshape=NULL,
                                   spList=spList,
                                   filterList=featureLevels,
                                   maxRasterize=50000,
                                   polySimplify=0,polyMethod="vis",
                                   polyWeighting=0.85,polySnapInt=0.0001) 
  }  

  mmmelev <- raster::as.matrix(elevations)
  x <- seq(1,length.out=nrow(mmmelev))
  y <- seq(1,length.out=ncol(mmmelev))
  yscale <- yRatio(elevations)
  
  if (!is.null(imageRaster)) {
    mapImage <- raster::crop(imageRaster,elevations)
    col <- t(matrix(
      mapply(rgb2hex,as.vector(mapImage[[1]]),
             as.vector(mapImage[[2]]),as.vector(mapImage[[3]]),
             colordepth=mapColorDepth,
             SIMPLIFY=TRUE),
      ncol=nrow(mmmelev),nrow=ncol(mmmelev)))
  } else if (rglColorScheme %in% c("bing","apple-iphoto","stamen-terrain")) { 
    #  appear dead  "nps","maptoolkit-topo"
    mapImage <- getMapImageRaster(elevations,
                                  mapImageType=rglColorScheme,
                                  silent=silent) 
    col <- t(matrix(
             mapply(rgb2hex,as.vector(mapImage[[1]]),
              as.vector(mapImage[[2]]),as.vector(mapImage[[3]]),
              colordepth=mapColorDepth,
              SIMPLIFY=TRUE),
             ncol=nrow(mmmelev),nrow=ncol(mmmelev)))
  } else {                           
    #  assign elevation-based colors 
    tcolors <- terrainColors(rglColorScheme,206)
    tmpelev <- mmmelev/maxElev    #  rescale in terms of maximum
    tmpelev[is.na(tmpelev)] <- 0
    #tmpelev <- sign(tmpelev)*sqrt(abs(tmpelev)) # f(0)=0, f(1)=1, f'(x>0) decreasing, reasonable for x<0 
    tmpelev[mmmelev <= 0] <- 0  #  go off original 
    colidx <- floor(200*tmpelev) + 1
    colidx[colidx>201] <- 201   #  cap at maxElev
    colidx[colidx<1] <- 1       #  and at 0
    colidx <- colidx + 5
    colidx[mmmelev == 0]  <- 1
    col <- tcolors[colidx]
    if (!is.na(rglNegcolor)) col[mmmelev < -10] <- gplots::col2hex(rglNegcolor)
  }
  if (!is.na(rglNAcolor)) {
    col[is.na(mmmelev)] <- gplots::col2hex(rglNAcolor)
  } else {
    col[is.na(mmmelev)] <- NA
  }
  mmmelev[is.na(mmmelev)] <- -10    #  have missing elevations slightly below zero
  #  draw cities, water and roads in that order
  if ("town" %in% names(mapRaster)) {
    town <- as.matrix(mapRaster[["town"]])
    town[ is.na(town) ] <- 0
    col[ town >= featureLevels[["spTown"]] ] <- gplots::col2hex(citycolor)
    town <- NULL
  }
  if ("waterA" %in% names(mapRaster)) {
    waterA <- as.matrix(mapRaster[["waterA"]])
    waterA[ is.na(waterA) ] <- 0
    col[ waterA >= featureLevels[["spWaterA"]] ] <- gplots::col2hex(watercolor)
  }
  if ("waterL" %in% names(mapRaster)) {
    waterL <- as.matrix(mapRaster[["waterL"]])
    waterL[ is.na(waterL) ] <- 0
    col[ waterL >= featureLevels[["spWaterL"]] ] <- gplots::col2hex(watercolor)
    waterL <- NULL    
  }
  if ("waterA" %in% names(mapRaster)) {
      col[ waterA == 8 ] <- gplots::col2hex(glaciercolor) # Glaciers overwrite other water
      waterA <- NULL
  }
  if ("roads" %in% names(mapRaster)) {
    road <- as.matrix(mapRaster[["roads"]])
    road[ is.na(road) ] <- 0
    col[ road >= featureLevels[["spRoads"]] ] <- gplots::col2hex(roadcolor)
    road <- NULL
  }
  if (!is.null(trackdf)) {
    if (!trackCurve) {
      trackRaster <- shapeToRasterLayer(sxdf=trackLines,
                                        templateRaster=elevations,
                                        maxRasterize=5000,
                                        keepTouch=TRUE,
                                        silent=silent,noisy=noisy)
      temp <- as.matrix(trackRaster)
      temp[ is.na(temp)] <- 0
      col[temp >= 1] <- gplots::col2hex(trackColor)
      trackRaster <- NULL
    } else {
      xmin <- raster::extent(mapRaster)[1]
      xmax <- raster::extent(mapRaster)[2]
      ymin <- raster::extent(mapRaster)[3]
      ymax <- raster::extent(mapRaster)[4]
      xlen <- ncol(mapRaster)
      ylen <- nrow(mapRaster)
      xpath <- ylen*(1 - (sp::coordinates(trackPoints)[,2]-ymin)/(ymax-ymin)) -
        origin(mapRaster)[2]
      ypath <- xlen*(sp::coordinates(trackPoints)[,1]-xmin)/(xmax-xmin) -
        origin(mapRaster)[1]
      zpath <- trackdf$altitude.m  +  trackCurveHeight
    }
  }
  
  #  and output the graph using rgl
  rgl::par3d("windowRect"= c(100,100,1200,1000))
  userMatrix <- matrix(c(-0.02,-0.80,0.632,0,1,0,0.04,0,
                         -0.03,0.60,0.80,0,0,0,0,1),ncol=4,nrow=4)
  rgl::rgl.clear()
  rgl::surface3d(x,y,mmmelev,color=col)
  rgl::material3d(alpha=rglAlpha,
                  point_antialias=rglAntiAlias,
                  line_antialias=rglAntiAlias,
                  smooth=rglSmooth,
                  shininess=rglShininess,
                  ambient=rglAmbient,emission=rglEmission,
                  specular=rglSpecular)
  rgl::aspect3d(x=1,y=1/yscale,z=0.035*vScale)
  
  rgl::rgl.clear("lights")
  rgl::light3d(theta=rglTheta, phi=rglPhi, viewpoint.rel=TRUE,
                 specular=rglSpecular, diffuse=rglDiffuse,
                 ambient=rglAmbient)
  
  rgl::rgl.viewpoint(userMatrix=userMatrix,type="modelviewpoint")
  pan3d(2)  # right button for panning, doesn't play well with zoom)
  if (!is.null(trackdf) & trackCurve) {
    rgl::lines3d(xpath,ypath,zpath,
                 lwd=2,col = trackColor, alpha=1.0)
    
  }
  if (saveRGL) 
    rgl::writeWebGL(dir=paste0(mapoutputdir), 
                    filename=paste0(mapoutputdir,"/",outputName," rgl map.html"))
  return(NULL)
}
terrainColors <- function(palettename="default",numshades=206) {
  # print(paste0("drawing map using ",palettename," for color"))
  if (palettename == "beach") {
    terrcolors <- 
      colorRampPalette(c("blue","bisque1","bisque2","bisque3",
                         "palegreen","greenyellow","lawngreen",
                         "chartreuse","green","springgreen",
                         "limegreen","forestgreen","darkgreen",
                         "olivedrab","darkkhaki","darkgoldenrod",
                         "sienna","brown","saddlebrown","rosybrown",
                         "gray35","gray45","gray55",
                         "gray65","gray70","gray75","gray85"))(numshades)
  } else if (palettename == "viridis") {
    terrcolors <- 
      viridis::viridis_pal(begin=0.2,end=0.9,direction=1,option="C")(numshades)
  } else if (palettename == "plasma") {
    terrcolors <- 
      viridis::viridis_pal(begin=0.0,end=1.0,direction=-1,option="D")(numshades)
  } else if (palettename %in% c("terrain","oleron")) {
    terrcolors <- 
      scico::scico(numshades,begin=0.52,end=1.0,direction=1,palette="oleron")
  } else if (palettename %in% c("snow","oslo")) {
    terrcolors <- 
      scico::scico(numshades,begin=0.52,end=1.0,direction=1,palette="oslo")
  } else if (palettename %in% c("desert","lajolla")) {
    terrcolors <- 
      scico::scico(numshades,begin=0.0,end=0.8,direction=-1,palette="lajolla")
  } else if (palettename %in% c("niccoli")) {
    terrcolors <- 
      pals::linearl(2*numshades)[(numshades+1):(2*numshades)]
  } else if (palettename %in% c("bright")) {
    terrcolors <- 
      pals::gnuplot(2*numshades)[floor(3*numshades/5):(floor(3*numshades/5)+numshades)]
  } else {  #"default"
    terrcolors <-
      colorRampPalette(c("blue","darkturquoise","turquoise","aquamarine",
                         "palegreen","greenyellow","lawngreen",
                         "chartreuse","green","springgreen",
                         "limegreen","forestgreen","darkgreen",
                         "olivedrab","darkkhaki","darkgoldenrod",
                         "sienna","brown","saddlebrown","rosybrown",
                         "gray35","gray45","gray55",
                         "gray65","gray70","gray75","gray85"))(numshades)
  }
  return(terrcolors)
}
yRatio <- function(rrr) {
  xmin <- rrr@extent@xmin
  xmax <- rrr@extent@xmax
  ymin <- rrr@extent@ymin
  ymax <- rrr@extent@ymax
  lonlat <- grepl("+proj=longlat",crs(rrr,asText=TRUE))
  return(yRatioPts(xmin,xmax,ymin,ymax,lonlat))
}
yRatioPts <- function(xmin,xmax,ymin,ymax,lonlat) {
  width <- rasterWidth(xmin,xmax,ymin,ymax,lonlat)
  height <- rasterHeight(xmin,xmax,ymin,ymax,lonlat)
  return(height/width)
}
rasterWidth <- function(xmin,xmax,ymin,ymax,lonlat) {
  (raster::pointDistance(cbind(xmin,ymin),cbind(xmax,ymin),lonlat=lonlat) +
   raster::pointDistance(cbind(xmin,ymax),cbind(xmax,ymax),lonlat=lonlat)) / 2
}
rasterHeight <- function(xmin,xmax,ymin,ymax,lonlat) {
  (raster::pointDistance(cbind(xmin,ymin),cbind(xmin,ymax),lonlat=lonlat) +
   raster::pointDistance(cbind(xmax,ymin),cbind(xmax,ymax),lonlat=lonlat)) / 2
}  
pan3d <- function(button) {
  start <- list()
  begin <- function(x, y) {
    start$userMatrix <<- rgl::par3d("userMatrix")
    start$viewport <<- rgl::par3d("viewport")
    start$scale <<- rgl::par3d("scale")
    start$projection <<- rgl::rgl.projection()
    start$pos <<- rgl::rgl.window2user( x/start$viewport[3], 
                                        1 - y/start$viewport[4], 
                                        0.5,
                                        projection = start$projection)
  }
  update <- function(x, y) {
    xlat <- (rgl::rgl.window2user( x/start$viewport[3], 
                                   1 - y/start$viewport[4], 
                                   0.5,
                                   projection = start$projection) - start$pos)*start$scale
    mouseMatrix <- rgl::translationMatrix(xlat[1], xlat[2], xlat[3])
    rgl::par3d(userMatrix = start$userMatrix %*% t(mouseMatrix) )
  }
  rgl::rgl.setMouseCallbacks(button, begin, update)
  cat("Callbacks set on button", button, "of rgl device", rgl.cur(), "\n")
}
getMapImageRaster <- function(mapRaster,mapImageType="bing",
                              silent=FALSE) {
  if (grepl("+proj=longlat",crs(mapRaster,asText=TRUE))) {
    llextent <- raster::extent(mapRaster) 
  } else {
    llextent <- raster::extent(raster::projectExtent(mapRaster,
        crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 +no_defs")) 
  }
  upperLeft <-c(llextent[4],llextent[1])
  lowerRight <-c(llextent[3],llextent[2])
  # calculate zoom based on width/pixel
  metersPerPixel <- rasterHeight(raster::extent(mapRaster)[1],
                                 raster::extent(mapRaster)[2],
                                 raster::extent(mapRaster)[3],
                                 raster::extent(mapRaster)[4],
                                 lonlat=grepl("+proj=longlat",
                                              crs(mapRaster,asText=TRUE)) 
                                  ) / ncol(mapRaster)
  zoomcalc <- 13 - floor(max(log2(metersPerPixel/20),0))                   
  if (!silent) print(paste0("downloading ",mapImageType," map tiles, zoom = ",zoomcalc))
  mapImage <- OpenStreetMap::openmap(upperLeft,lowerRight,
                                     zoom=zoomcalc,type=mapImageType) 
  gc()
  if (!silent) print(paste0("projecting ",mapImageType," map tiles"))
  mapImage <- OpenStreetMap::openproj(mapImage,
                                      projection=raster::crs(mapRaster)) 
  mapImage <- raster::raster(mapImage)
  if (!silent) print(paste0("resampling ",mapImageType," map tiles")) 
  mapImage <- raster::resample(mapImage,mapRaster) 
  return(mapImage)
} 
rgb2hex <- function(r,g,b,colordepth=16) {
  topcolor <- colordepth-1
  rgb(red  = round(r*topcolor/255), 
      green= round(g*topcolor/255), 
      blue = round(b*topcolor/255), 
      maxColorValue=topcolor) 
}
spXformNullOK <- function(sp,crs) {
  if (is.null(sp)) {
    return(NULL)
  } else {
    if (nrow(sp) > 0) {
      return(spTransform(sp,crs))
    } else {
      return(NULL)
    }
  }
}
trackpts_to_spPointDF <- function(trackdf,
  gpsProj4="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
  workProj4="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") {
  
  if ("position_lon.dd" %in% names(trackdf)) {
    lon <- trackdf$position_lon.dd
    lat <- trackdf$position_lat.dd
  } else if ("lon" %in% names(trackdf)) {
    lon <- trackdf$lon
    lat <- trackdf$lat
  }
  datadf <- data.frame(lon=lon,lat=lat)
  if ("altitude.m" %in% names(trackdf)) {
    datadf$altitude.m <- trackdf$altitude.m
  } else {
    datadf$altitude.m <- NA
  }
  if ("segment" %in% names(trackdf)) {
    datadf$segment <- trackdf$segment
  } else {
    datadf$segment <- 1
  }
  retdf <-
    SpatialPointsDataFrame(coords=datadf[,c("lon","lat")], 
                           data=datadf,
                           proj4string=CRS(gpsProj4))
  if (as.character(gpsProj4) != as.character(workProj4)) {
    retdf <- spXformNullOK(retdf,CRS(workProj4))
  }   
  return(retdf)
}
trackpts_to_lines <- function(lon, lat, id_field = NULL) {
  # adapted from rpubs.com code by Kyle Walker
  #  greatly simplified 
  if (is.null(id_field)) id_field <- rep(1,length(lon))
  
  xy <- data.frame(lon=lon,lat=lat)
  # enhance this to allow gps other than wgs84
  trackProj4 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  datapts <- sp::SpatialPointsDataFrame(coords = xy, data = data.frame(id=id_field),
                      proj4string = CRS(trackProj4))
  #Split into a list by ID field
  paths <- split(datapts, datapts[["id"]])
  sp_lines <- sp::SpatialLines(list(sp::Lines(list(sp::Line(paths[[1]])), "track1")),
                               proj4string = CRS(trackProj4))
  idvec <- 1
  if (length(paths)>1) {
    for (p in 2:length(paths)) {
      id <- paste0("track",p)
      idvec <- c(idvec,p)
      l <- sp::SpatialLines(list(sp::Lines(list(sp::Line(paths[[p]])), id)),
                            proj4string = CRS(trackProj4))
      sp_lines <- rbind(sp_lines, l)
    }
  }
  temp <- data.frame("value"=idvec,
                     row.names=paste0("track",idvec))
  sp_lines <- sp::SpatialLinesDataFrame(sp_lines,
                                        data=temp)
  return(sp_lines)
}
