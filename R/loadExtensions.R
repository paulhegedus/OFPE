#' @title Load extensions for a new OFPE database
#'
#' @description Loads PostGIS tools and a function for generating a net across
#' an area of interest. From PostGIS website, see source.
#'
#' @param db Connection to a database.
#' @param postgis_version character PostGIS version installed.
#'
#' @return None.
#' @source \url{https://trac.osgeo.org/postgis/wiki/UsersWikiCreateFishnet}
#' @export
loadExtensions <- function(db, postgis_version) {
  extensions <- DBI::dbGetQuery(db, "SELECT * FROM pg_extension")

  if (as.numeric(postgis_version) >= 3) {
    if (!any(grepl("postgis", extensions$extname))) {
      DBI::dbSendQuery(db, paste0("CREATE EXTENSION postgis;
                               CREATE EXTENSION postgis_raster;"))
    } else {
      if (!any(grepl("postgis_raster", extensions$extname))) {
        DBI::dbSendQuery(db, paste0("CREATE EXTENSION postgis_raster;"))
      }
    }
  } else {
    if (!any(grepl("postgis", extensions$extname))) {
      DBI::dbSendQuery(db, paste0("CREATE EXTENSION postgis;"))
    }
  }
  DBI::dbSendQuery(
    db,
    paste0("
      CREATE OR REPLACE FUNCTION ST_CreateFishnet(
        nrow integer, ncol integer,
        xsize float8, ysize float8,
        x0 float8 DEFAULT 0, y0 float8 DEFAULT 0,
        OUT \"row\" integer, OUT col integer,
           OUT geom geometry)
          RETURNS SETOF record AS
        $$
        SELECT i + 1 AS row, j + 1 AS col, ST_Translate(cell, j * $3 + $5, i * $4 + $6) AS geom
        FROM generate_series(0, $1 - 1) AS i,
             generate_series(0, $2 - 1) AS j,
        (
        SELECT ('POLYGON((0 0, 0 '||$4||', '||$3||' '||$4||', '||$3||' 0,0 0))')::geometry AS cell
        ) AS foo;
        $$ LANGUAGE sql IMMUTABLE STRICT;
    ")
  )
}
