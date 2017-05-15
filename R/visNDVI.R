#' Visulize point coordinates with one NDVI map
#' 
#' Point coordinates are visualized with a weekly NDVI 
#' map of choice.
#' @param yrs 
#' vector c() or sequence seq() of years do be visualized
#' @param wks 
#' vector c() or sequence seq() of weeks do be visualized
#' @param lonlim 
#' longitude range c(min,max) for the plot
#' @param latlim
#' latitude range c(min,max) for the plot
#' @return A multi-panel graph of the NDVI rasters is
#' plotted.
#' @export
visNDVI <- function(yrs,wks,lonlim,latlim,lns=T) {
  temp <- tempfile()
  download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip",temp)
  unzip(zipfile = temp, exdir = tempdir())
  map <- readOGR(dsn=tempdir(),layer="TM_WORLD_BORDERS_SIMPL-0.3")
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
  filevector4 <- unique(grep(paste(paste("P",yrs,sep=""),collapse="|"),filevector3, value=TRUE))
  filevector5 <-unique(grep(paste(paste(intWeeks[wks],".SM",sep=""),collapse="|"),filevector4, value=TRUE))
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
  par(mfrow=c(length(yrs),length(wks)),
      mar=c(2,2,2,1), oma=c(1,1,1,2))
  leg <- c(rep(F,length(filevector)-1),T)
  lonlab <- ### add here ###
  latlab <- ### add here ###
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
    plot(r,xlim=lonlim,ylim=latlim,col=rev(terrain.colors(255)),legend=leg[i],xaxt="n",yaxt="n",
         main=paste0("w:",substr(filevector[i],21,23)," y:",substr(filevector[i],17,20)))
    if(lns){
      plot(map,col=NA,border="dimgrey",fill=F,add=T)
    }
    axis(1,labels=F,tick=T)
    axis(2,labels=F,tick=T)
    lst <- c("Eureka!","Neat-o!","Huzzah!","Yaus!")
    print(paste0("The week ", substr(filevector[i],21,23)," of the year ",substr(filevector[i],17,20)," is plotted!", " ",lst[sample(1:4, 1)]))
  }
  unlink(tempdir(),recursive=F,force=T)
}