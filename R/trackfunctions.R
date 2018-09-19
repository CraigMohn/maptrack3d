trackNameFix <- function(trackdf,
                         gpsProj4="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                         gapTooLong=100,noisy=FALSE) {
  if ("position_lon.dd" %in% names(trackdf)) {
    lon <- trackdf$position_lon.dd
    lat <- trackdf$position_lat.dd
  } else if ("lon" %in% names(trackdf)) {
    lon <- trackdf$lon
    lat <- trackdf$lat
  }
  if ("altitude.m" %in% names(trackdf)) {
    altitude.m <- trackdf$altitude.m
  } else {
    altitude.m <- NA
  }
  if ("segment" %in% names(trackdf)) {
    segment <- trackdf$segment
  } else {
    segment <- rep(1,nrow(trackdf))
  }
  if ("color"  %in% names(trackdf)) {
    color <- trackdf$color
  } else {
    color <- NA
  }
  distToPrev <- 
            c(0,raster::pointDistance(cbind(lon[-1],lat[-1]),
                                      cbind(lon[-length(lon)],lat[-length(lat)]),
                                      lonlat=grepl("+proj=longlat",gpsProj4)))
  newSegment <- (distToPrev >= gapTooLong) & 
                c(TRUE,segment[-1]==segment[-length(segment)])
  if(noisy) print(paste0(sum(newSegment)," new segments"))
  return(data.frame(lon=lon,lat=lat,altitude.m=altitude.m,
                    segment=segment+cumsum(newSegment),
                    color=color,subseg=1,
                    stringsAsFactors=FALSE))
}
trackFill <- function(trackdf,maxdist=20) {
  npts <- nrow(trackdf)
  dists <- raster::pointDistance(as.matrix(trackdf[-1,c("lon","lat")]),
                                 as.matrix(trackdf[1:(npts-1),c("lon","lat")]),
                                 lonlat=TRUE)
  outpts <- c(ceiling(dists/maxdist),1)
  outpts[outpts==0] <- 1  # don't toss duplicates
  outpts[trackdf$segment != c(trackdf$segment[-1],1)] <- 1 # last-in-seg stays
  origptstart <- (c(0,cumsum(outpts)[-npts])+1) # offset of original pt into output df
  newptseq <- sequence(outpts)
  totoutpts <- sum(outpts)
  
  origlon <- trackdf$lon
  origlat <- trackdf$lat
  nextlon <- c(trackdf$lon[-1],trackdf$lon[npts])
  nextlat <- c(trackdf$lat[-1],trackdf$lat[npts])
  origlon <- rep(origlon,outpts)                          
  origlat <- rep(origlat,outpts)                          
  nextlon <- rep(nextlon,outpts)                          
  nextlat <- rep(nextlat,outpts)
  origalt <- trackdf$altitude.m
  nextalt <- c(trackdf$alt[-1],trackdf$alt[npts])
  origalt <- rep(origalt,outpts)                          
  nextalt <- rep(nextalt,outpts)
  nnewpts <- rep(outpts,outpts)
  
  wtleft <- (nnewpts-newptseq+1)/nnewpts
  lon <- origlon*wtleft + nextlon*(1-wtleft)
  lat <- origlat*wtleft + nextlat*(1-wtleft)
  altitude.m <- origalt*wtleft + nextalt*(1-wtleft)
  segment <- rep(trackdf$segment,outpts)
  subseg <- rep(trackdf$subseg,outpts)
  color <- rep(trackdf$color,outpts)
  
  return(data.frame(lon=lon,lat=lat,altitude.m=altitude.m,
                    segment=segment,color=color,subseg=subseg,
                    stringsAsFactors=FALSE))
}
trackpts_to_spPointDF <- function(trackdf,
  gpsProj4="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
  workProj4="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") {
  
  retdf <-
    SpatialPointsDataFrame(coords=trackdf[,c("lon","lat")], 
                           data=trackdf,
                           proj4string=CRS(gpsProj4))
  if (as.character(gpsProj4) != as.character(workProj4)) {
    retdf <- spXformNullOK(retdf,CRS(workProj4))
  }   
  return(retdf)
}
trackpts_to_spLineDF <- function(trackdf,
  gpsProj4="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
  workProj4="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") {
  # greatly simplified from rpubs.com code by Kyle Walker

  xy <- data.frame(lon=trackdf$lon,lat=trackdf$lat)
  datapts <- sp::SpatialPointsDataFrame(coords = xy, data = trackdf,
                      proj4string = CRS(gpsProj4))
  #Split into a list by segment field
  paths <- split(datapts, datapts[["segment"]])
  sp_lines <- sp::SpatialLines(list(sp::Lines(list(sp::Line(paths[[1]])), "track1")),
                               proj4string = CRS(gpsProj4))
  idvec <- 1
  colorvec <- paths[[1]]$color[1]
  if (length(paths)>1) {
    for (p in 2:length(paths)) {
      id <- paste0("track",p)
      idvec <- c(idvec,p)
      colorvec <- c(colorvec,paths[[p]]$color[1])
      l <- sp::SpatialLines(list(sp::Lines(list(sp::Line(paths[[p]])), id)),
                            proj4string = CRS(gpsProj4))
      sp_lines <- rbind(sp_lines, l)
    }
  }
  temp <- data.frame("segment"=idvec,"color"=colorvec,
                     row.names=paste0("track",idvec),stringsAsFactors=FALSE)
  sp_lines <- sp::SpatialLinesDataFrame(sp_lines,
                                        data=temp)
  if (as.character(gpsProj4) != as.character(workProj4))
    sp_lines <- spTransform(sp_lines,crs(workProj4))
  return(sp_lines)
}
cropPointsDF <- function(spPoints,rasterExt) {
  # remove the points of lines in spPoints which are outside extent
  #  update subseg field to reflect breaks from section deletions

  spPoly <- as(rasterExt,"SpatialPolygons")
  sp::proj4string(spPoly) <- sp::proj4string(spPoints)
  pointsInside <- sp::over(spPoints,spPoly)
  pointsInside[is.na(pointsInside)] <- 0
  lagInside <- c(0,pointsInside[-length(pointsInside)])
  leadInside <- c(pointsInside[-1],0)
  begsubseg <-  (pointsInside > 0) & (lagInside == 0)
  endsubseg <- (pointsInside > 0) & (leadInside == 0)
  begoutseg <- c(FALSE,endsubseg[-1])
  endoutseg <- c(begsubseg[-1],FALSE)
  pointsToUse <- pointsInside | begoutseg 
  outPoints <- spPoints[as.logical(pointsToUse),]
  begsubseg <- begsubseg[as.logical(pointsToUse)]
  endoutseg <- endoutseg[as.logical(pointsToUse)]
  outPoints$subseg <- cumsum(begsubseg) + endoutseg
  return(outPoints)
}
