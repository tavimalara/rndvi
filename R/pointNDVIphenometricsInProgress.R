#' Calculate annual phenological metrics of NDVI series for point coordinates
#' 
#' The NDVI data from AVHRR is gathered from the FTP server 
#' and NDVI values are extracted for (a list of) point 
#' coordinates. 
#' @param yrs 
#' vector c() or sequence seq() of years do be extracted
#' @param lns 
#' vector of longitudes of point locations
#' @param lts 
#' vector of latutudes of point locations
#' @param nms 
#' vector of names of point locations
#' @return A object of the class SpatialPointsDataFrame will be 
#' created. The extracted values are stored in the slot data and 
#' the column.
#' @export
pointNDVIphenometrics <- function(yrs,lns,lts,nms) {
  dir.create(paste(tempdir(),"hdfTEMP//",sep="//"))
  hdfFolder=paste(tempdir(),"hdfTEMP//",sep="//")
  dir.create(paste(tempdir(),"tiffTEMP//",sep="//"))
  tiffFolder=paste(tempdir(),"tiffTEMP//",sep="//")
  FTPurl <- "ftp://ftp.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/VHP_16km/VH/"
  filenames1 <- getURI(FTPurl)
  filenames2 <- substr(filenames1,11,(nchar(filenames1))-2)
  filevector1 <- strsplit(filenames2,"\r\n-rw-r--r--")[[1]]
  filevector2 <- substr(filevector1,46,75)
  filevector3 <- filevector2[grep("*.SM.hdf",filevector2)]
  filevector5 <- unique(grep(paste(paste("P",yrs,sep=""),collapse="|"),filevector3, value=TRUE))
  satTest<-data.frame(sat=substr(filevector5,13,14), 
                      year=substr(filevector5,17,20), 
                      week=substr(filevector5,22,23),
                      filter=rep(NA,times=length(filevector5)),stringsAsFactors = FALSE)
  for (i in 1:length(filevector5)) {
    vect <- ((as.numeric(paste0(satData$year2,satData$week2))-as.numeric(paste0(satTest$year,satTest$week)[i]))+(as.numeric(paste0(satData$year1,satData$week1))-as.numeric(paste0(satTest$year,satTest$week)[i])))
    nr<-which(abs(vect-0)==min(abs(vect-0)))
    if (as.numeric(paste0(satData$year2,satData$week2))[nr] < as.numeric(paste0(satTest$year,satTest$week)[i])) {
      nr = nr+1
    }
    satTest$filter[i] <- as.character(satTest$sat[i]) == satData$satID[nr]
  }
  filevector <- filevector5[satTest$filter]
  tempout <- SpatialPointsDataFrame(coords=cbind(lon=lns,lat=lts),
                                data=data.frame(names=nms),
                                proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  tempout@data$NDVI <- matrix(NA,ncol=length(filevector),nrow=length(tempout),
                          dimnames=list(tempout@data$names,substr(filevector,17,23)))
  for (i in 1:length(filevector)) {
    download.file(url=paste(FTPurl,filevector[i],sep=""),
                  destfile=paste(hdfFolder,substr(filevector[i],13,nchar(filevector[i])),sep=""), 
                  mode='wb',method='auto',quiet=T,cacheOK=FALSE)
    hdf <- paste(hdfFolder,substr(filevector[i],13,nchar(filevector[i])),sep="")
    tiff <- paste(tiffFolder,paste(substr(filevector[i],13,nchar(filevector[i])-4),"tif",sep="."),sep="")
    try(gdal_translate(hdf,tiff,sds=F,verbose=TRUE,of = "GTiff",sd_index=2),silent=T)
    r <- raster(tiff, values=TRUE)
    mat <- as.matrix(r)
    mat[mat == min(mat)] <- NA
    r <- raster(mat)
    if (sum(grepl("START_LONGITUDE_RANGE=",gdalinfo(hdf))) == 0){
      extent(r) <- extent(as.numeric(strsplit(gdalinfo(hdf)[grep("geospatial_lon_min=",gdalinfo(hdf))],
                                              "geospatial_lon_min=")[[1]][2]),
                          as.numeric(strsplit(gdalinfo(hdf)[grep("geospatial_lon_max=",gdalinfo(hdf))],
                                              "geospatial_lon_max=")[[1]][2]),
                          as.numeric(strsplit(gdalinfo(hdf)[grep("geospatial_lat_min=",gdalinfo(hdf))],
                                              "geospatial_lat_min=")[[1]][2]),
                          as.numeric(strsplit(gdalinfo(hdf)[grep("geospatial_lat_max=",gdalinfo(hdf))],
                                              "geospatial_lat_max=")[[1]][2]))
    } else {
      extent(r) <- extent(as.numeric(strsplit(gdalinfo(hdf)[grep("START_LONGITUDE_RANGE=",gdalinfo(hdf))],
                                              "START_LONGITUDE_RANGE=")[[1]][2]),
                          as.numeric(strsplit(gdalinfo(hdf)[grep("END_LONGITUDE_RANGE=",gdalinfo(hdf))],
                                              "END_LONGITUDE_RANGE=")[[1]][2]),
                          as.numeric(strsplit(gdalinfo(hdf)[grep("END_LATITUDE_RANGE=",gdalinfo(hdf))],
                                              "END_LATITUDE_RANGE=")[[1]][2]),
                          as.numeric(strsplit(gdalinfo(hdf)[grep("START_LATITUDE_RANGE=",gdalinfo(hdf))],
                                              "START_LATITUDE_RANGE=")[[1]][2]))
    }
    projection(r) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    tempout@data$NDVI[,i] <- raster::extract(r,tempout@coords)
    lst <- c("Eureka!","Neat-o!","Huzzah!","Yaus!")
    print(paste0("The week ", substr(filevector[i],21,23)," of the year ",substr(filevector[i],17,20)," is done!", " ",lst[sample(1:4, 1)]))
  }
  
  
  #! TODO
  ndvi.series <- tempout@data$NDVI[4,]
  xz <- as.zoo(ndvi.series)
  mins <- which(rollapply(xz, 20, function(x) which.min(x)==2)==T)
  maxs <- which(rollapply(xz, 20, function(x) which.max(x)==2)==T)
  for (n in 1:length(mins)){
    temp.data<-data.frame(weeks=c((as.numeric(substr(names(ndvi.series[mins[n]]),5,7))-4):(as.numeric(substr(names(ndvi.series[mins[n]]),5,7))),
                                  as.numeric(substr(names(ndvi.series[mins[n]:maxs[n]]),5,7)),
                                  (as.numeric(substr(names(ndvi.series[maxs[n]]),5,7))):(as.numeric(substr(names(ndvi.series[maxs[n]]),5,7))+4)),
                          ndvi=c(rep(ndvi.series[mins[n]],5),ndvi.series[mins[n]:maxs[n]],rep(ndvi.series[maxs[n]],5))-ndvi.series[mins[n]]+0.000000000001)
    output<-getInitial(ndvi~SSlogis(weeks,ASym,xmin,scal),data=temp.data)
    slope <- as.numeric(output[3])
    asym <- as.numeric(output[1])+ndvi.series[mins[n]]
    predicts<-(output[1]/(1+exp((output[2]-temp.data$weeks)/output[3])))+ndvi.series[mins[n]]
    result<-list(sgu,slope,asym,predicts)
    names(result)<-list("greenup","slope","asym","fitval")
    plot(ndvi.series[temp.data$weeks])
    lines(output$fitval)
    abline(h=output$asym,lty=2)
    abline(v=output$greenup,lty=3)
  }


  
  
  
  
  return(out)
  unlink(tempdir(),recursive=F,force=T)
}
