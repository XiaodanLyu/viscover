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
#' @seealso \code\link{GetCDLValue}
GetCDLFile <- function(year, b){

  bb.poly <- methods::as(raster::extent(b), "SpatialPolygons")
  sp::proj4string(bb.poly) <- sp::CRS("+init=epsg:4326")
  baseurl <- "https://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLFile?"
  crs.cropscape <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  bb.poly.proj <- sp::spTransform(bb.poly, crs.cropscape)
  b2 <- as.vector(sp::bbox(bb.poly.proj))
  url <- paste0(baseurl, 'year=', year, '&bbox=', glue::collapse(b2, sep = ","))
  html <- readLines(url, warn = FALSE)
  doc <- XML::xmlTreeParse(html)
  top <- XML::xmlRoot(doc)
  tifurl <- XML::xmlValue(top[[1]][["text"]])
  # cdl_raster <- raster(tifurl) ## only works on Mac OS
  dir.create("temp", showWarnings = FALSE)
  destfile <- paste("temp", "tmp.tif", sep = "/")
  utils::download.file(tifurl, destfile = destfile, mode = "wb")
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
#' @seealso \code\link{GetCDLFile}
GetCDLValue <- function(year, lon, lat){

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
#' @return A list with the soil musym, mukey, muname, muacres
#' @examples
#' ## not run
#' # GetSDLValue(-93.65, 42.03)
#' @export
#' @seealso \code\link{GetCDLValue}
GetSDLValue <- function(lon, lat){
  ## lon, lat: longitude and latitude in WGS84
  pt <- matrix(c(lon, lat), nc = 2, byrow = T)
  circ <- dismo::circles(pt, d = .1, lonlat = TRUE)
  p <- rgeos::writeWKT(circ@polygons)
  q <- paste0("SELECT musym, mukey, muname, muacres
              FROM mapunit
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
