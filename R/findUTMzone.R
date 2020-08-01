#' @title Identify the Universal Transverse Mercator coordinate system zone
#'
#' @description Identifies the UTM zone number for the specified data and
#' returns a UTM code for WGS84 (i.e. 32612, 32614). Any of the three
#' arguments can be used to identify the UTM zone. When a shapefile is
#' passed in with the 'FILE' argument, the spatial data is used to determine
#' the UTM zone mathematically, same if the 'bounds' argument is used. If only
#' the 'FARMERNAME' is passed in, the UTM zone returned will be for Montana,
#' unless the farmer's name is "loewen", in which case the UTM zone is for Manitoba.
#'
#' NEEDS UPDATING. This function needs to be updated to use the
#' mathematical approach from the site below.
#'
#' @param FILE Spatial shapefile for determining UTM zone.
#' @param FARMERNAME Name of the OFPE farmer, for hardcoded cases (i.e. Loewen).
#' @return WGS84 UTM code (i.e. 32612, 32614).
#' @source https://apollomapping.com/blog/gtm-finding-a-utm-zone-number-easily
#' @export
findUTMzone <- function(FILE = NULL, FARMERNAME = NULL) {
  # quick and dirty - not sustainable
  if (!is.null(FARMERNAME)) {
    utmEpsg <- ifelse(FARMERNAME == "loewen",
                      32614,
                      32612)
  }

  # legit way
  if (!is.null(FILE)) {
    sf::st_crs(FILE) <- 4326
    FILE <- sf::st_transform(FILE, 4326)
    bounds <- sf::st_bbox(FILE) # get boundary of file
    # calculate zone
    utmZone <- ceiling((bounds["xmin"] + 180) / 6) %>% as.numeric()
    # check hemisphere
    if (bounds["ymin"] > 0 | bounds["ymax"] > 0) {
      utmEpsg <- paste0(326, utmZone)
    } else {
      utmEpsg <- paste0(327, utmZone)
    }
  }
  return(utmEpsg)
}



