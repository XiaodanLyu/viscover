myMarkerIcon <- function(type){
  leaflet::makeAwesomeIcon(
    text = ifelse(type == 1, "C", "S"),
    markerColor = ifelse(type == 1, "orange", "blue"))
}


#' CDL color palette
#'
#' Obtain the color code for given CDL value.
#'
#' @param x a integer value or vector of CDL values
#' @return color code used in CDL for given CDL value(s)
#' @examples
#' cdlpal(0:10)
#' @export
cdlpal <- function(x){
  notNA <- !is.na(cdl.dbf$OPACITY)
  pal <- leaflet::colorFactor(
    palette = with(cdl.dbf[notNA,], grDevices::rgb(RED, GREEN, BLUE, OPACITY)),
    domain = c(0:254)[notNA],
    na.color = "transparent",
    alpha = TRUE)
  pal(x)
}
