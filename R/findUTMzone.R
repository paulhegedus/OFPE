#' @title Identify the Universal Transverse Mercator coordinate system zone
#'
#' @description Function for querying the database to identify the UTM zone
#' from the corresponding farm for the field the user is working with. This
#' function is used many times and requires a database connection and either
#' a farm name or farm id, or a field name. The returned value
#' is the UTM EPSG code for the WGS84 datum.
#'
#' @param db Connection to an OFPE formatted database.
#' @param farm Either a farm id (integer), or a farm name.
#' @param fieldname Name of the field for aggregation.
#' @return WGS84 UTM code (i.e. 32612, 32614).
#' @export
findUTMzone <- function(db,
                        farm = NULL,
                        fieldname = NULL) {
  if (!is.null(fieldname)) {
    stopifnot(is.character(fieldname))
    farm <- DBI::dbGetQuery(
      db,
      paste0("SELECT DISTINCT farmidx
             FROM all_farms.fields
             WHERE fieldname = '", fieldname, "'")
    )
    farm <- as.numeric(farm)
  }
  stopifnot(!is.null(farm),
            is.character(farm) | is.numeric(farm))
  if (is.character(farm)) {
    utm_epsg <- DBI::dbGetQuery(
      db,
      paste0("SELECT utm_epsg
                FROM all_farms.farms
                WHERE farm = '", farm, "'")
    )
  } else {
    utm_epsg <- DBI::dbGetQuery(
      db,
      paste0("SELECT utm_epsg
                FROM all_farms.farms
                WHERE farmidx = '", farm, "'")
    )
  }
  return(as.integer(utm_epsg))
}
#' @title Calculate the Universal Transverse Mercator coordinate system zone
#'
#' @description Calculates the UTM zone for the specified data and
#' returns a UTM code for WGS84 (i.e. 32612, 32614). This function is
#' performed in the ManageParms class to store the UTM zone for the farm.
#' A shapefile is passed in with the 'FILE' argument, the spatial data is
#' used to determine the UTM zone mathematically, same if the 'bounds'
#' argument is used. The returned value is the UTM EPSG code for the WGS84 datum.
#'
#' @param FILE Spatial shapefile for determining UTM zone.
#' @return WGS84 UTM code (i.e. 32612, 32614).
#' @source https://apollomapping.com/blog/gtm-finding-a-utm-zone-number-easily
#' @export
calcUTMzone = function(FILE) {
  if (!is.null(FILE)) {
    crs_raster <- suppressWarnings(raster::crs(FILE))
    if (is.na(crs_raster)) {
      sf::st_crs(FILE) <- 4326
    }
    FILE <- sf::st_transform(FILE, 4326)
    # get boundary of file
    bounds <- sf::st_bbox(FILE)
    # calculate zone
    utm_zone <- ceiling((bounds["xmin"] + 180) / 6) %>% as.numeric()
    # check hemisphere
    if (bounds["ymin"] > 0 | bounds["ymax"] > 0) {
      utm_epsg <- paste0(326, utm_zone)
    } else {
      utm_epsg <- paste0(327, utm_zone)
    }
  }
  return(as.integer(utm_epsg))
}
