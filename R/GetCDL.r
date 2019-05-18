#' @importFrom magrittr %>%
NULL


#' Get CDL raster
#'
#' Get CDL raster file by spatial extent in WGS84.
#'
#' @param year one of the available CDL years
#' @param b a spatial object in the projection of WGS84
#' @return A CDL raster within the given extent
#' @examples
#' ## not run
#' # GetCDLFile(2017, poly)
#' @export
#' @seealso \code{\link{GetCDLValue}}
GetCDLFile <- function(year, b){

  bb.poly <- methods::as(raster::extent(b), "SpatialPolygons")
  sp::proj4string(bb.poly) <- sp::CRS("+init=epsg:4326")
  baseurl <- "https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?"
  crs.cropscape <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  bb.poly.proj <- sp::spTransform(bb.poly, crs.cropscape)
  b2 <- as.vector(sp::bbox(bb.poly.proj))
  url <- paste0(baseurl, 'year=', year, '&bbox=', glue::glue_collapse(b2, sep = ","))
  html <- RCurl::getURL(url, .opts = list(ssl.verifypeer = FALSE), crlf = TRUE)
  doc <- XML::xmlTreeParse(html)
  top <- XML::xmlRoot(doc)
  tifurl <- XML::xmlValue(top[[1]][["text"]])
  # cdl_raster <- raster(tifurl) ## not working on windows
  dir.create("temp", showWarnings = FALSE)
  destfile <- paste("temp", "tmp.tif", sep = "/")
  if(Sys.info()["sysname"] == "Windows"){
    downloader::download(tifurl, destfile, mode = "wb", quiet = TRUE)
  }
  if(Sys.info()["sysname"] != "Windows"){
    utils::download.file(tifurl, destfile = destfile, mode = "wb", method = "curl", extra = "-k")
  }
  cdl_raster <- raster::raster(destfile)
  return(cdl_raster)
}


#' Get CDL point
#'
#' Get CDL point information by longitude and latitude in WGS84.
#'
#' @param year one of the available CDL years
#' @param lon longitude in WGS84
#' @param lat latitude in WGS84
#' @return A list with the CDL value, category and color
#' @examples
#' ## not run
#' # GetCDLValue(2017, -93.65, 42.03)
#' @export
#' @seealso \code{\link{GetCDLFile}} \code{\link{GetSDLValue}}
 GetCDLValue <- function(year, lon, lat){
  if(length(year)!=1|length(lon)!=1|length(lat)!=1){
    stop("One year and one location at a time!")
  }
  pt <- cbind(lon = lon, lat = lat) %>% sp::SpatialPoints(proj4string = sp::CRS("+init=epsg:4326"))
  baseurl <- "https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLValue?"
  crs.cropscape <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  pt.proj <- sp::spTransform(pt, crs.cropscape)
  pt2 <- sp::coordinates(pt.proj)
  url <- paste0(baseurl, 'year=', year, '&x=', pt2[[1]], "&y=", pt2[[2]])
  # Fixing "Peer certificate cannot be authenticated"
  html <- RCurl::getURL(url, .opts = list(ssl.verifypeer = FALSE), crlf = TRUE)
  # html <- readLines(url, warn = FALSE)
  doc <- XML::xmlTreeParse(html)
  top <- XML::xmlRoot(doc)
  text <- top[[1]][["text"]] %>% XML::xmlValue()
  text.split <- strsplit(text, ": |, ") %>% unlist
  value <- text.split[6]
  category <- gsub('\"', "", text.split[8])
  color <- gsub('\"|}', "", text.split[10])

  return(list(value = value, category = category, color = color))
}

#' Get SDL point
#'
#' Get soil point information by longitude and latitude in WGS84.
#'
#' @param lon longitude in WGS84
#' @param lat latitude in WGS84
#' @return a dataframe with soil areasymbol, musym, mukey, muname, muacres
#' @examples
#' ## not run
#' # GetSDLValue(-93.65, 42.03)
#' @export
#' @seealso \code{\link{GetCDLValue}}
GetSDLValue <- function(lon, lat){
  if(length(lon)!=1|length(lat)!=1){
    stop("One location at a time!")
  }
  ## lon, lat: longitude and latitude in WGS84
  pt <- matrix(c(lon, lat), nc = 2, byrow = T)
  circ <- dismo::circles(pt, d = .1, lonlat = TRUE)
  p <- rgeos::writeWKT(circ@polygons)
  q <- paste0(
          "SELECT areasymbol, musym, mukey, muname, muacres
          FROM mapunit mu
          INNER JOIN legend on legend.lkey = mu.lkey    
          WHERE mukey IN (
          SELECT * from SDA_Get_Mukey_from_intersection_with_WktWgs84('", p, "')
          )")
  qres <- soilDB::SDA_query(q)
  if(nrow(qres)>1) {
    warning(sprintf("%.0f soil mapunits are found, choose the first one", nrow(qres)))
    qres <- qres[1,]
  }
  qres
}
