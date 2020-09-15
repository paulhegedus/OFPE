#' @title R6 Class for building OFPE database
#'
#' @description R6 class for for loading required extensions and
#' building the the OFPE database schemas and initial tables.
#' @export
BuildDB <- R6::R6Class(
  "BuildDB",
  public = list(
    #' @field dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    dbCon = NULL,
    #' @field postgis_version Version of PostGIS installed (i.e. 2.5, 3.0).
    postgis_version = NULL,
    #' @field farmers Vector of farmer names to use for building initial schemas.
    farmers = NULL,

    #' @description
    #' Create a database builder object.
    #' @param dbCon dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    #' @param postgis_version Version of PostGIS installed (i.e. 2.5, 3.0).
    #' @param farmers Vector of farmer names to use for building initial schemas.
    #' @return A new 'BuildDB' object.
    initialize = function(dbCon, postgis_version, farmers) {
      stopifnot(
        class(farmers) == "character"
      )
      self$dbCon <- dbCon
      self$postgis_version <- postgis_version
      self$farmers <- farmers
    },
    #' @description
    #' Execute database builder functions. Runs, .loadExtensions(), .buildSchemas(),
    #' and .buildTables().
    #' @param None No arguments needed because they are provided during class
    #' initialization.
    buildDatabase = function() {
      self$.loadExtensions()
      self$.buildSchemas()
      self$.buildTables()
      self$.buildSpatialIndexes()
    },
    #' @description
    #' Loads extensions needed for OFPE database functions such
    #' as PostGIS tools and a function for generating a net across an area of
    #' interest (see source). No arguments needed if provided on class
    #' initialization, otherwise arguments provided take precedence.
    #' ST_CreateFishnet was written by the PostGIS team and built in this
    #' method, see source for reference and credit. The dot indicates that this
    #' function would be private if not for documentations sake.
    #' @param db Connection to a database.
    #' @param postgis_version PostGIS version installed.
    #' @return Enabled database extensions.
    #' @source \url{https://trac.osgeo.org/postgis/wiki/UsersWikiCreateFishnet}
    .loadExtensions = function(db = NULL, postgis_version = NULL) {
      if (is.null(db)) {
        db <- self$dbCon$db
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
    },
    #' @description
    #' Builds the skeleton of the database. Two schemas for each farmer supplied
    #' is built; one for holding raw data collected on-farms from equipment, and
    #' another for storing aggregated data files. A general schema call 'all_farms'
    #' is also created to hold data that is not farmer specific or farm-wide data.
    #' No arguments needed if provided on class initialization, otherwise arguments
    #' provided take precedence. The dot indicates that this function would be
    #' private if not for documentations sake. Note that farmer names are converted
    #' to lowercase upon addition to the database if not already.
    #' @param db Connection to a database.
    #' @param farmers Vector of farmer names.
    #' @return Built database schemas.
    .buildSchemas = function(db = NULL, farmers = NULL) {
      if (is.null(db)) {
        db <- self$dbCon$db
      }
      if (is.null(farmers)) {
        farmers <- self$farmers
      }
      farmers <- tolower(farmers)
      farmSchemas <- rep(farmers, each = 2) %>%
        paste0(c("_r", "_a"))
      schemas <- c("all_farms", farmSchemas)

      for (i in 1:length(schemas)) {
        DBI::dbSendQuery(db, paste0("CREATE SCHEMA ", schemas[i]))
      }
    },
    #' @description
    #' Builds initial tables in the 'all_farms' schema. None of these tables
    #' are filled with information. These tables will be filled with methods
    #' in the UpdateDB class. The tables created are 'farmers' with an integer
    #' column for a farmer ID and column for the farmer name. The 'farms' table
    #' contains an ID column, and columns for the name of the farm and the ID of
    #' the farmer who owns it. The 'fields' table contains an ID column for
    #' sections of the field, a field ID, the ID of the farm the field falls
    #' within, the farmer's ID, and the name of the field. Geometry columns are
    #' also added to the 'farms' and 'fields' tables. No arguments needed if
    #' provided on class initialization, otherwise arguments provided take
    #' precedence. The dot indicates that this function would be private if not
    #' for documentations sake.
    #' @param db Connection to a database.
    #' @return Built 'all_farms' tables
    .buildTables = function(db = NULL) {
      if (is.null(db)) {
        db <- self$dbCon$db
      }
      DBI::dbSendQuery(
        db,
        "CREATE TABLE all_farms.farmers (
           farmeridx SERIAL PRIMARY KEY,
           farmer VARCHAR(100) NOT NULL,
           CONSTRAINT norepfarmers UNIQUE (farmer));
        CREATE TABLE all_farms.farms (
           farmidx SERIAL PRIMARY KEY,
           farm VARCHAR(100) NOT NULL,
           farmeridx INTEGER REFERENCES all_farms.farmers(farmeridx),
           area DOUBLE PRECISION,
           utm_epsg INTEGER,
           CONSTRAINT norepfarms UNIQUE (farm, farmeridx));
        CREATE TABLE all_farms.field_ids (
           fieldidx SERIAL PRIMARY KEY,
           fieldname VARCHAR(100) NOT NULL,
           CONSTRAINT norepfieldids UNIQUE (fieldname));
        CREATE TABLE all_farms.fields (
          fieldidx INTEGER REFERENCES all_farms.field_ids(fieldidx),
          wfid INTEGER NOT NULL,
          farmidx INTEGER REFERENCES all_farms.farms(farmidx),
          farmeridx INTEGER REFERENCES all_farms.farmers(farmeridx),
          fieldname VARCHAR(100) NOT NULL,
          area DOUBLE PRECISION,
          CONSTRAINT norepfields UNIQUE (wfid, fieldname),
          PRIMARY KEY (wfid, fieldname));"
      )
      DBI::dbSendQuery(db, "ALTER TABLE all_farms.farms ADD COLUMN geom geometry")
      DBI::dbSendQuery(db, "ALTER TABLE all_farms.fields ADD COLUMN geom geometry")
    },
    #' @description
    #' Builds spatial indexes on the tabels created in the 'all_farms' schema.
    #' These indexes are used to speed up spatial queries and are a PostGIS tool.
    #' This means that this function must be run after loading the extensions and
    #' creating the tables. No arguments needed if provided on class initialization,
    #' otherwise arguments provided take precedence. The dot indicates that this
    #' function would be private if not for documentations sake.
    #' @param db Connection to a database.
    #' @return Built spatial indexes on the 'farms' and 'fields' tables.
    .buildSpatialIndexes = function(db = NULL) {
      if (is.null(db)) {
        db <- self$dbCon$db
      }
      DBI::dbSendQuery(
        db,
        "CREATE INDEX farms_geom_idx
          ON all_farms.farms
          USING GIST (geom);
        CREATE INDEX fields_geom_idx
          ON all_farms.fields
          USING GIST (geom);"
      )
    }
  )
)
