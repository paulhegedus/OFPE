#' @title R6 Class for building OFPE database
#'
#' @description R6 class for for loading required extensions and
#' building the the OFPE database schemas and initial tables.
BuildDB <- R6::R6Class(
  "BuildDB",
  public = list(
    #' @field db Database connection, such as PostgreSQLConnection.
    db = NULL,
    #' @field postgis_version Version of PostGIS installed (i.e. 2.5, 3.0).
    postgis_version = NULL,
    #' @field farmers Vector of farmer names to use for building initial schemas.
    farmers = NULL,

    #' @description
    #' Create a database builder object.
    #' @param db Database connection.
    #' @param postgis_version Version of PostGIS.
    #' @param farmers Vector of farmer names.
    #' @return A new `BuildDB` object.
    initialize = function(db = NA, postgis_version = NA, farmers = NA) {
      stopifnot(
        class(farmers) == "character",
      )
      self$db <- db
      self$postgis_version <- postgis_version
      self$farmers <- farmers
    },

    #' @description
    #' Execute database builder functions. Runs, loadExtension(), buildSchemas(),
    #' and buildTables().
    #' @param None No arguments needed because they are provided during class
    #' initialization.
    buildDatabase = function() {
      loadExtensions()
      buildSchemas()
      buildTables()
    },

    #' @description
    #' Loads extensions needed for OFPE database functions such
    #' as PostGIS tools and a function for generating a net across an area of
    #' interest (see source). No arguments needed if provided on class
    #' initialization.
    #' @param db Connection to a database.
    #' @param postgis_version character PostGIS version installed.
    #' @source \url{https://trac.osgeo.org/postgis/wiki/UsersWikiCreateFishnet}
    loadExtensions = function(db = NULL, postgis_version = NULL) {
      if (is.null(db)) {
        db <- self$db
      }
      if (is.null(postgis_version)) {
        postgis_version <- self$postgis_version
      }
      extensions <- DBI::dbGetQuery(db, "SELECT * FROM pg_extension")

      if (as.numeric(postgis_version) >= 3) {
        if (!any(grepl("postgis", extensions$extname))) {
          DBI::dbSendQuery(db, paste0("CREATE EXTENSION postgis;
                               CREATE EXTENSION postgis_raster;"))
        } else {
          if (!any(grepl("postgis_raster", extensions$extname))) {
            DBI::dbSendQuery(db,
                             paste0("CREATE EXTENSION postgis_raster;"))
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
  )
)
#' Loads PostGIS tools and
#' a function for generating a net across an area of interest. From PostGIS website, see source.
