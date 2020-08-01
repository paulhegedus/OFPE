#' @title R6 Class for executing the OFPE data aggregation process
#'
#' @description R6 Class for executing the OFPE data aggregation process that
#' consolidates data from various sources into one dataset for analysis and
#' simulation.
#'
#' This class takes the inputs selected by a user with the 'AggInputs' class
#' to direct the data to gather and aggregate together in the database. Most
#' methods are executed in the database.
#'
#' There are not outputs in this class, as data is worked with and stored
#' within the OFPE database. See the 'AggInputs' class for descriptions of the
#' options for aggregating data. The general workflow consists of extracting the
#' response variable data from the year of interest, then adding previous year
#' response variable data based on nearest neighbor or an average, depending on
#' the selected location of aggregated data ('Grid' vs. 'Observed'). Experimental
#' data is gathered from the year of interest and the year in which a crop was
#' previously grown. These data are all cleaned in the same process. Across the
#' whole field, data are removed when exceeding 4SD from the mean, and within
#' each 10m grid cell, data are removed when exceeding 1SD from the cell mean.
#' This occurs regardless of whether 'Grid' or 'Observed' is selected. Then,
#' data from remotely sensed sources are gathered. The data selected is dependent
#' on the 'DAT_USED' variable, which indicates whether to use data from Jan 1 to
#' Mar 30 for the year of interest or to use data from Jan 1 to Dec 31. This is
#' discussed more in the 'AggInputs' class documentation. Finally, after all on-farm
#' and remotely sensed data has been combined into one dataset, this is saved to the
#' database in the farmer's aggregated ('_a') schema.
#' @export
AggDat <- R6::R6Class(
  "AggDat",
  public = list(
    #' @field aggInputs An object of the 'AggInputs' class containaing the options
    #' for aggregating data. This includes the database connection
    aggInputs = NULL,
    #' @field FARMIDX The ID of the farm in which the field falls within.
    FARMIDX = NULL,
    #' @field FARMERIDX The ID of the farmer that owns the farm that the field
    #' falls within.
    FARMERIDX = NULL,

    #' @description
    #' Initialize an object for executing the process of aggregating OFPE
    #' data. Clears out temporary tables from the database and creates a
    #' temporary table for the field boundary of the field selected.
    #' @param aggInputs An 'AggInputs' R6 class with the user's aggregation
    #' options.
    #' @return An initialized 'AggDat' object.
    initialize = function(aggInputs) {
      self$aggInputs <- aggInputs
      OFPE::removeTempTables(self$aggInputs$dbCon$db)

      if (self$aggInputs$bboxImport == "No") {
        invisible(
          DBI::dbSendQuery(
            self$aggInputs$dbCon$db,
            paste0("CREATE TABLE all_farms.temp AS
             SELECT * FROM all_farms.fields fields
             WHERE fields.fieldname = '", self$aggInputs$FIELDNAME, "';
             ALTER TABLE all_farms.temp
             RENAME COLUMN geom TO geometry;")
          )
        )
      } else {
        stopifnot(
          !is.null(self$aggInputs$bboxLocation)
        )
        ## import bounding box
        BBOX <- sf::read_sf(self$aggInputs$bboxLocation) %>%
          sf::st_zm() %>%
          sf::`st_crs<-`(4326) %>%
          sf::st_transform("epsg:4326")
        invisible(
          suppressMessages(
            rpostgis::pgInsert(self$aggInputs$dbCon$db,
                               c("all_farms", "temp"),
                               as(BBOX, "Spatial"),
                               geom = "geometry")
          )
        )
      }
      invisible(
        DBI::dbSendQuery(
          self$aggInputs$dbCon$db,
          paste0("UPDATE all_farms.temp SET
                 geometry = ST_Multi(geometry);
                 CREATE INDEX temp_geom_idx
                 ON all_farms.temp
                 USING GIST (geometry);")
        )
      )
      invisible(
        DBI::dbSendQuery(
          self$aggInputs$dbCon$db,
          "VACUUM ANALYZE all_farms.temp"
        )
      )
      OFPE::removeTempFarmerTables(self$aggInputs$dbCon$db, self$aggInputs$FARMERNAME)
    },
    #' @description
    #' Execution of the aggregation method for OFPE data. Occurs mainly
    #' within the database using PostGIS tools. Creates a 10m grid, a
    #' temporary aggregation table, extracts and consolidates on-farm data,
    #' aggregates remotely sensed data, and saves to the 'aggregated' schema
    #' for the specified farmer.
    #' @param None No arguments needed. All required arguments in 'AggInputs'
    #' class.
    #' @return Aggregated data in the 'farmername_a' schema
    aggregateData = function() {
      # make 10m grid
      self$.make10mGrid(
        self$aggInputs$dbCon$db,
        self$aggInputs$bboxImport,
        self$aggInputs$FIELDNAME,
        10, # size is 10m, but could be changed
        self$aggInputs$FARMERNAME
      )

      # create agg table
      self$.createAggTable(
        self$aggInputs$dbCon$db,
        self$aggInputs$FARMERNAME,
        self$aggInputs$FIELDNAME,
        self$aggInputs$CY_RESP,
        self$aggInputs$PY_RESP,
        self$aggInputs$GRID,
        self$aggInputs$DAT_USED
      )

      # if not sat data
      if (self$aggInputs$RESPVAR != "sat") {

        # clean resp data (CY) & aggregate resp data (CY)
        if (!any(self$aggInputs$CY_RESP_FILES == "None")) {
          self$.cleanRespData(
            self$aggInputs$RESPVAR,
            self$aggInputs$dbCon$db,
            self$aggInputs$CY_RESP_FILES,
            self$aggInputs$bboxImport,
            self$aggInputs$CY_RESP_COL,
            self$aggInputs$CY_RESP,
            self$aggInputs$FIELDNAME,
            self$aggInputs$FARMERNAME
          )
          self$.aggRespData(
            self$aggInputs$dbCon$db,
            self$aggInputs$FARMERNAME,
            self$aggInputs$RESPVAR,
            self$aggInputs$FIELDNAME,
            self$aggInputs$GRID,
            self$aggInputs$CY_RESP,
            self$aggInputs$PY_RESP,
            self$aggInputs$DAT_USED
          )
        } else { # end if file != none
          invisible(
            DBI::dbSendQuery(
              self$aggInputs$dbCon$db,
              paste0("ALTER TABLE ",
                     self$aggInputs$FARMERNAME, "_a.temp
                     ADD COLUMN ", self$aggInputs$RESPVAR, " REAL;")
            )
          )
        }

        # clean resp data (PY) & aggregate resp data (PY)
        if (!any(self$aggInputs$PY_RESP_FILES == "None")) {
          self$.cleanRespData(
            self$aggInputs$RESPVAR,
            self$aggInputs$dbCon$db,
            self$aggInputs$PY_RESP_FILES,
            self$aggInputs$bboxImport,
            self$aggInputs$PY_RESP_COL,
            self$aggInputs$PY_RESP,
            self$aggInputs$FIELDNAME,
            self$aggInputs$FARMERNAME
          )
          self$.aggRespData(
            self$aggInputs$dbCon$db,
            self$aggInputs$FARMERNAME,
            self$aggInputs$RESPVAR,
            self$aggInputs$FIELDNAME,
            self$aggInputs$GRID
          )
        } else { # end if file != none
          invisible(
            DBI::dbSendQuery(
              self$aggInputs$dbCon$db,
              paste0("ALTER TABLE ",
                     self$aggInputs$FARMERNAME, "_a.temp
                     ADD COLUMN prev_", self$aggInputs$RESPVAR, " REAL;")
            )
          )
        }
        # clean exp data (CY) & aggregate exp data (CY)
        if (!any(self$aggInputs$CY_EXP_FILES == "None")) {
          ## if file exist
          self$.cleanExpData(
            self$aggInputs$CY_EXP_FILES,
            self$aggInputs$dbCon$db,
            self$aggInputs$CY_EXP_COL,
            self$aggInputs$FARMERNAME,
            self$aggInputs$CY_EXP
          )
          self$.aggExpData(
            self$aggInputs$CY_EXP_FILES,
            self$aggInputs$dbCon$db,
            self$aggInputs$CY_EXP_COL,
            self$aggInputs$CY_EXP_CONV,
            self$aggInputs$FARMERNAME,
            self$aggInputs$EXPVAR,
            self$aggInputs$GRID,
            CY=TRUE
          )
        } else { #
          ## if no exp data files
          invisible(
            DBI::dbSendQuery(
              self$aggInputs$dbCon$db,
              paste0("ALTER TABLE ", self$aggInputs$FARMERNAME, "_a.temp
                     ADD COLUMN ", self$aggInputs$EXPVAR, " REAL;")
            )
          )
          if (self$aggInputs$FARMERNAME == "merja" &
              self$aggInputs$CY_EXP == "2019") {
            ## chuck only applied 15lbs N/ac in 2019
            invisible(
              DBI::dbSendQuery(
                self$aggInputs$dbCon$db,
                paste0("UPDATE ", self$aggInputs$FARMERNAME, "_a.temp
                       SET ", self$aggInputs$EXPVAR, " = 15;")
              )
            )
          }
        }
        # clean exp data (PY) & aggregate exp data (PY)
        if (!any(self$aggInputs$PY_EXP_FILES == "None")) {
          ## if files exist
          self$.cleanExpData(
            self$aggInputs$PY_EXP_FILES,
            self$aggInputs$dbCon$db,
            self$aggInputs$PY_EXP_COL,
            self$aggInputs$FARMERNAME,
            self$aggInputs$PY_EXP
          )
          self$.aggExpData(
            self$aggInputs$PY_EXP_FILES,
            self$aggInputs$dbCon$db,
            self$aggInputs$PY_EXP_COL,
            self$aggInputs$PY_EXP_CONV,
            self$aggInputs$FARMERNAME,
            self$aggInputs$EXPVAR,
            self$aggInputs$GRID,
            CY=FALSE
          )
        } else { #
          ## if no exp data files
          invisible(
            DBI::dbSendQuery(
              self$aggInputs$dbCon$db,
              paste0("ALTER TABLE ", self$aggInputs$FARMERNAME, "_a.temp
                       ADD COLUMN prev_", self$aggInputs$EXPVAR, " REAL;")
            )
          )
          if (self$aggInputs$FARMERNAME == "merja" &
              self$aggInputs$PY_EXP == "2019") {
            ## chuck only applied 15lbs N/ac in 2019
            invisible(
              DBI::dbSendQuery(
                self$aggInputs$dbCon$db,
                paste0("UPDATE ", self$aggInputs$FARMERNAME, "_a.temp
                         SET prev_", self$aggInputs$EXPVAR, " = 15;")
              )
            )
          }
        }
      } # end if not sat dat
      # clip to field boundary
      self$.clipAggDat()
      private$.idFarm()
      # remote sensing data
      aggGEE <- OFPE::AggGEE$new(self$aggInputs,
                                 self$FARMIDX,
                                 self$FARMERIDX)
      aggGEE$aggregateGEE()
      # ssurgo data
      self$.aggSSURGO()
      # remove 0 rates from seed data
      if (self$aggInputs$RESPVAR != "sat") {
        if (self$aggInputs$EXPVAR == "aa_sr") {
          invisible(
            DBI::dbSendQuery(
              self$aggInputs$dbCon$db,
              paste0("DELETE FROM ",
                      self$aggInputs$FARMERNAME, "_a.temp temp
                      WHERE temp.aa_sr = 0
                      OR temp.prev_aa_sr = 0;")
            )
          )
        }
      }
      # vacuum analyze often
      invisible(
        DBI::dbSendQuery(
          self$aggInputs$dbCon$db,
          paste0("VACUUM ANALYZE ",
                 self$aggInputs$FARMERNAME, "_a.temp;")
        )
      )
      # export data
      if (self$aggInputs$saveInDB == "Yes") {
        self$.saveAggDat
      }
      if (self$aggInputs$export == "Yes") {
        self$.exportAggDat
      }
      # clean up
      OFPE::removeTempTables(self$aggInputs$dbCon$db)
      OFPE::removeTempFarmerTables(self$aggInputs$dbCon$db,
                                   self$aggInputs$FARMERNAME)
    },
    #' @description
    #' Makes a grid across a field of a specified size. The dot indicates that
    #' this function would be private
    #' if not for documentations sake.
    #' @param DB Connection to an OFPE formatted database.
    #' @param BBOXIMPORT Whether the user uploaded their own field boundary
    #' or are using a boundary stored in the database.
    #' @param FIELDNAME Name of the field for aggregation.
    #' @param SIZE Size of grid to make (meters).
    #' @param FARMERNAME Name of farmer that owns the field.
    #' @return None.
    .make10mGrid = function(DB, BBOXIMPORT, FIELDNAME, SIZE, FARMERNAME) {
      utmEpsg <- OFPE::findUTMzone(FARMERNAME = FARMERNAME)
      gridsExist <- FALSE
      fieldExist <- FALSE
      # check if grids exist
      gridsExist <- as.logical(
        DBI::dbGetQuery(
          DB,
          paste0("SELECT EXISTS (
               SELECT 1
               FROM information_schema.tables
               WHERE  table_schema = 'all_farms'
               AND table_name = 'grids')")
        )
      )
      # if grids exists check if field exists
      if (gridsExist) {
        # check if field has a grid
        fieldExist <- as.logical(
          DBI::dbGetQuery(
            DB,
            paste0("SELECT EXISTS (
                 SELECT 1
                 FROM all_farms.grids grids
                 WHERE grids.field = '", FIELDNAME, "')")
          )
        )
        # if it does copy to gridtemp
        if (fieldExist) {
          invisible(
            DBI::dbSendQuery(
              DB,
              paste0("CREATE TABLE all_farms.gridtemp AS
                SELECT *
                FROM all_farms.grids
                WHERE field = '", FIELDNAME, "'")
            )
          )
        }
      }
      # if field does not exists
      if (!fieldExist) {
        BBOX <- sf::st_read(
          DB,
          query = paste0("SELECT * FROM all_farms.temp"),
          geometry_column="geometry") %>%
          sf::st_transform(paste0("epsg:", utmEpsg)) %>%
          as("Spatial") %>%
          sp::bbox()
        NCOL <- ceiling((BBOX["x", "max"] - BBOX["x", "min"]) / SIZE)
        NROW <- ceiling((BBOX["y", "max"] - BBOX["y", "min"]) / SIZE)

        invisible(
          DBI::dbSendQuery(
            DB,
            paste0("CREATE TABLE all_farms.gridtemp AS
               SELECT *
               FROM ST_CreateFishnet (", NROW, ", ", NCOL, ", ", SIZE, ", ", SIZE, ", ", BBOX["x", "min"], ", ", BBOX["y", "min"], ") AS cells;
               ALTER TABLE all_farms.gridtemp
               ADD COLUMN cell_id VARCHAR,
               ADD COLUMN field VARCHAR;
               UPDATE all_farms.gridtemp SET
               cell_id = row::text ||'_'|| col::text,
               field = '", FIELDNAME, "';
               UPDATE all_farms.gridtemp SET geom = ST_SetSRID (geom, ", utmEpsg, ");
               ALTER TABLE all_farms.gridtemp
               ADD COLUMN x double precision,
               ADD COLUMN y double precision;
               UPDATE all_farms.gridtemp SET
               x = ST_X(ST_Centroid(geom)),
               y = ST_Y(ST_Centroid(geom));
               ALTER TABLE all_farms.gridtemp
               ADD PRIMARY KEY (cell_id, field);
               CREATE INDEX gridtemp_geom_idx
               ON all_farms.gridtemp
               USING gist (geom);")
          )
        )
      }
      # if bboximport = no, field does not exist in grids
      if (BBOXIMPORT == "No" & !fieldExist) {
        if (gridsExist) { # if gridsExists append
          invisible(
            DBI::dbSendQuery(
              DB,
              paste0("INSERT INTO all_farms.grids
                 SELECT * FROM all_farms.gridtemp;")
            )
          )
        }else{ # if not create grids
          invisible(
            DBI::dbSendQuery(
              DB,
              paste0("CREATE TABLE all_farms.grids AS
                 SELECT * FROM all_farms.gridtemp;")
            )
          )
        }
        invisible(
          DBI::dbSendQuery(
            DB,
            paste0("VACUUM ANALYZE all_farms.grids")
          )
        )
      }
      invisible(
        DBI::dbSendQuery(
          DB,
          paste0("VACUUM ANALYZE all_farms.gridtemp")
        )
      )
    },
    #' @description
    #' Creates a temporary aggregated table in the database for processing.
    #' The dot indicates that this function would be private if not for
    #' documentations sake.
    #' @param DB Connection to an OFPE formatted database.
    #' @param FARMERNAME Name of farmer that owns the field.
    #' @param FIELDNAME Name of the field for aggregation.
    #' @param CY The year of interest for aggregation (aka 'current year').
    #' @param PY The 'previous year' in which the crop was harvested in the field.
    #' @param GRID Whether to aggregate data to the centroids of a grid or use
    #' the raw observed data locations.
    #' @param DAT_USED The length of year to gather data over for the CY.
    #' @return Temporary table in the database.
    .createAggTable = function(DB,
                               FARMERNAME,
                               FIELDNAME,
                               CY,
                               PY,
                               GRID,
                               DAT_USED) {
      utmEpsg <- OFPE::findUTMzone(FARMERNAME = FARMERNAME)

      if (GRID == "grid") {
        invisible(
          DBI::dbSendQuery(
            DB,
            paste0("CREATE TABLE ", FARMERNAME, "_a.temp AS
                    SELECT *
                    FROM all_farms.gridtemp gridtemp
                    WHERE gridtemp.field = '", FIELDNAME, "';
                    ALTER TABLE ", FARMERNAME, "_a.temp
                    DROP COLUMN row,
                    DROP COLUMN col,
                    ADD COLUMN grid VARCHAR,
                    ADD COLUMN datused VARCHAR,
                    ADD COLUMN farmer VARCHAR,
                    ADD COLUMN year VARCHAR,
                    ADD COLUMN prev_year VARCHAR;
                    UPDATE ", FARMERNAME, "_a.temp SET
                    grid = '", GRID, "',
                    datused = '", DAT_USED, "',
                    farmer = '", FARMERNAME, "',
                    year = '", CY, "',
                    prev_year = '", PY, "';
                    ALTER TABLE ", FARMERNAME, "_a.temp
                    ALTER COLUMN geom TYPE geometry(Point, ", utmEpsg, ")
                    USING ST_Centroid(geom);
                    ALTER TABLE ", FARMERNAME, "_a.temp
                    RENAME COLUMN geom TO geometry;")
          )
        )
        invisible(
          DBI::dbSendQuery(
            DB,
            paste0("CREATE INDEX aggtemp_geom_idx ON ",
                   FARMERNAME,
                   "_a.temp USING gist (geometry)")
          )
        )
        invisible(
          DBI::dbSendQuery(
            DB,
            paste0("VACUUM ANALYZE ",
                   FARMERNAME,
                   "_a.temp")
          )
        )
      }else{
        invisible(
          DBI::dbSendQuery(
            DB,
            paste0("CREATE TABLE ", FARMERNAME, "_a.temp (
            x double precision,
            y double precision,
            cell_id VARCHAR,
            field VARCHAR,
            grid VARCHAR,
            datused VARCHAR,
            farmer  VARCHAR,
            year  VARCHAR,
            prev_year  VARCHAR);
            ALTER TABLE ", FARMERNAME, "_a.temp
            ADD COLUMN geometry geometry;")
          )
        )
      }
    },
    #' @description
    #' Go through the cleaning process of cleaning the response variable data.
    #' Removes data that are outside of four standard deviations from the mean
    #' of the resposne variable and the distance, if supplied. Removes points
    #' within 30m of the field boundary. This table is stored in the farmer's
    #' raw database schema before it is processed and added to the temporary
    #' table in the farmer's aggregated schema. The dot indicates that this
    #' function would be private if not for documentations sake.
    #' @param RESPVAR The response variable to aggregate.
    #' @param DB Connection to an OFPE database.
    #' @param RESP_FILES Files used for aggregation.
    #' @param BBOXIMPORT Whether the user imported thier own field boundary or
    #' using a predefined experimental field in the database.
    #' @param RESP_COL The column in the response variable data that corresponds
    #' to the measured response.
    #' @param YEAR The year of the data being aggregated.
    #' @param FIELDNAME Name of the field for aggregation.
    #' @param FARMERNAME Name of the farmer that owns the field for aggregation.
    #' @return Temporary table in farmer's 'raw' schema.
    .cleanRespData = function(RESPVAR,
                              DB,
                              RESP_FILES,
                              BBOXIMPORT,
                              RESP_COL,
                              YEAR,
                              FIELDNAME,
                              FARMERNAME) {
      ## create temp file to clean
      for (i in 1:length(RESP_FILES)) {
        if (i == 1) {
          if (is.na(RESP_COL[i, "DIST"])) {
            invisible(
              DBI::dbSendQuery(
                DB,
                paste0("CREATE TABLE ", FARMERNAME, "_r.temp AS
                        SELECT ", RESP_COL[i, "RESP"], " resp, orig_file, ", RESPVAR, ".geometry
                        FROM ", FARMERNAME, "_r.", RESPVAR, " ", RESPVAR, "
                        JOIN all_farms.temp temp
                        ON ST_Within(", RESPVAR, ".geometry, temp.geometry)
                        WHERE ", RESPVAR, ".year = '", YEAR, "'
                        AND ", RESPVAR, ".orig_file = '", RESP_FILES[i], "';
                        ALTER TABLE ", FARMERNAME, "_r.temp
                        ADD COLUMN dist VARCHAR;")
              )
            )
          }else{
            invisible(
              DBI::dbSendQuery(
                DB,
                paste0("CREATE TABLE ", FARMERNAME, "_r.temp AS
                        SELECT ", RESP_COL[i, "RESP"], " resp, orig_file, ", RESPVAR, ".geometry, ", RESP_COL[i, "DIST"], " dist
                        FROM ", FARMERNAME, "_r.", RESPVAR, " ", RESPVAR, "
                        JOIN all_farms.temp temp
                        ON ST_Within(", RESPVAR, ".geometry, temp.geometry)
                        WHERE ", RESPVAR, ".year = '", YEAR, "'
                        AND ", RESPVAR, ".orig_file = '", RESP_FILES[i], "';")
              )
            )
          }
        }else{
          if (is.na(RESP_COL[i, "DIST"])) {
            invisible(
              DBI::dbSendQuery(
                DB,
                paste0("INSERT INTO ", FARMERNAME, "_r.temp
                        SELECT ", RESP_COL[i, "RESP"], " resp, orig_file, ", RESPVAR, ".geometry
                        FROM ", FARMERNAME, "_r.", RESPVAR, " ", RESPVAR, "
                        JOIN all_farms.temp temp
                        ON ST_Within(", RESPVAR, ".geometry, temp.geometry)
                        WHERE ", RESPVAR, ".year = '", YEAR, "'
                        AND ", RESPVAR, ".orig_file = '", RESP_FILES[i], "'")
              )
            )
          }else{
            invisible(
              DBI::dbSendQuery(
                DB,
                paste0("INSERT INTO ", FARMERNAME, "_r.temp
                        SELECT ", RESP_COL[i, "RESP"], " resp, orig_file, ", RESPVAR, ".geometry, ", RESP_COL[i, "DIST"], " dist
                        FROM ", FARMERNAME, "_r.", RESPVAR, " ", RESPVAR, "
                        JOIN all_farms.temp temp
                        ON ST_Within(", RESPVAR, ".geometry, temp.geometry)
                        WHERE ", RESPVAR, ".year = '", YEAR, "'
                        AND ", RESPVAR, ".orig_file = '", RESP_FILES[i], "';")
              )
            )
          }
        }
      }
      invisible(
        DBI::dbSendQuery(
          DB,
          paste0("ALTER TABLE ", FARMERNAME, "_r.temp
                  ALTER COLUMN resp TYPE real USING resp::real,
                  ALTER COLUMN dist TYPE real USING dist::real,
                  ADD COLUMN max_resp REAL,
                  ADD COLUMN min_resp REAL,
                  ADD COLUMN max_dist REAL,
                  ADD COLUMN min_dist REAL;
                  DELETE FROM ", FARMERNAME, "_r.temp temp
                  WHERE temp.resp = 'NaN';
                  CREATE INDEX temp_geom_idx ON ", FARMERNAME, "_r.temp USING gist (geometry);")
        )
      )
      invisible(
        DBI::dbSendQuery(
          DB,
          paste0("VACUUM ANALYZE ",
                 FARMERNAME,
                 "_r.temp")
        )
      )

      ## Calculate the mean and 4SD for the response and distance variable if present by the orig_file.
      meansNsd <- DBI::dbGetQuery(
        DB,
        paste0("SELECT AVG(resp) mean_resp, STDDEV(resp) sd_resp, AVG(dist) mean_dist, STDDEV(dist) sd_dist, orig_file
                FROM ", FARMERNAME, "_r.temp
                GROUP BY temp.orig_file;")
      )
      meansNsd$max_resp <- meansNsd$mean_resp + (4 * meansNsd$sd_resp)
      meansNsd$max_dist <- meansNsd$mean_dist + (4 * meansNsd$sd_dist)
      meansNsd$min_resp <- ifelse((meansNsd$mean_resp - (4 * meansNsd$sd_resp)) < 0,
                                  0,
                                  (meansNsd$mean_resp - (4 * meansNsd$sd_resp)))
      meansNsd$min_dist <- ifelse((meansNsd$mean_dist - (4 * meansNsd$sd_dist)) < 0,
                                  0,
                                  (meansNsd$mean_dist - (4 * meansNsd$sd_dist)))
      for (i in 1:nrow(meansNsd)) {
        if (is.na(meansNsd[i, "mean_dist"]) | is.na(meansNsd[i, "sd_dist"])) {
          meansNsd[i, "max_dist"] <- 10000
          meansNsd[i, "min_dist"] <- 0
        }
        invisible(
          DBI::dbSendQuery(
            DB,
            paste0("UPDATE ", FARMERNAME, "_r.temp temp
                    SET max_resp = ", meansNsd[i, "max_resp"], "
                    WHERE temp.orig_file = '", meansNsd[i, "orig_file"], "';
                    UPDATE ", FARMERNAME, "_r.temp temp
                    SET min_resp = ", meansNsd[i, "min_resp"], "
                    WHERE temp.orig_file = '", meansNsd[i, "orig_file"], "';
                    UPDATE ", FARMERNAME, "_r.temp temp
                    SET max_dist = ", meansNsd[i, "max_dist"], "
                    WHERE temp.orig_file = '", meansNsd[i, "orig_file"], "';
                    UPDATE ", FARMERNAME, "_r.temp temp
                    SET min_dist = ", meansNsd[i, "min_dist"], "
                    WHERE temp.orig_file = '", meansNsd[i, "orig_file"], "';")
          )
        )
      }
      ## Remove observations by orig_file above or below 4SD of the mean response or distance
      invisible(
        DBI::dbSendQuery(
          DB,
          paste0("DELETE FROM ", FARMERNAME, "_r.temp
                  WHERE resp >= max_resp
                  OR resp <= min_resp
                  OR dist >= max_dist
                  OR dist <= min_dist;")
        )
      )
      ## Remove observations by orig_file that are within 30m of the field boundary
      invisible(
        DBI::dbSendQuery(
          DB,
          paste0( "WITH buff AS(
                  SELECT ST_Buffer(geometry, -30) geometry
                  FROM all_farms.temp temp)

                  DELETE FROM ", FARMERNAME, "_r.temp AS temp
                  USING buff
                  WHERE ST_Within(temp.geometry, buff.geometry);")
        )
      )
      invisible(
        DBI::dbSendQuery(
          DB,
          paste0("ALTER TABLE ", FARMERNAME, "_r.temp
                  DROP COLUMN dist,
                  DROP COLUMN max_resp,
                  DROP COLUMN min_resp,
                  DROP COLUMN max_dist,
                  DROP COLUMN min_dist;")
        )
      )
      invisible(
        DBI::dbSendQuery(
          DB,
          paste0("VACUUM ANALYZE ",
                 FARMERNAME,
                 "_r.temp")
        )
      )
    },
    #' @description
    #' Method for aggregating the response variable data in the database. Uses
    #' the output of the .cleanRespData method to add the response variable data
    #' to the aggregated table in the database. if you want to upload PY data
    #' leave CY and PY NULL, otherwise provide at least CY for 'grid' and CY & PY
    #' for 'obs' grid options. Implements a second cleaning process based on the
    #' grid cells where observations outside of one standard deviation of the mean
    #' in each grid cell are omitted. The dot indicates that this function would be
    #' private if not for documentations sake.
    #' @param DB Connection to an OFPE database.
    #' @param FARMERNAME Name of the farmer that owns the field for aggregation.
    #' @param RESPVAR The response variable to aggregate.
    #' @param FIELDNAME Name of the field for aggregation.
    #' @param GRID Whether to aggregate data to the centroids of a grid or use
    #' the raw observed data locations.
    #' @param CY The year of interest for aggregation (aka the 'current year').
    #' @param PY The year prior to the year of interest that a crop was harvested
    #' in the field.
    #' @param DAT_USED The length of year to gather data over for the CY.
    #' @return Data in temporary aggregated table.
    .aggRespData = function(DB,
                            FARMERNAME,
                            RESPVAR,
                            FIELDNAME,
                            GRID,
                            CY = NULL,
                            PY = NULL,
                            DAT_USED) {
      utmEpsg <- OFPE::findUTMzone(FARMERNAME = FARMERNAME)
      # Get the cell_id for each point in the temporary table
      invisible(
        DBI::dbSendQuery(
          DB,
          paste0("ALTER TABLE ", FARMERNAME, "_r.temp
                  ADD COLUMN cell_id VARCHAR;
                  ALTER TABLE ", FARMERNAME, "_r.temp
                  ALTER COLUMN geometry TYPE geometry(POINT, ", utmEpsg, ")
                  USING ST_Transform(geometry, ", utmEpsg, ");
                  UPDATE ", FARMERNAME, "_r.temp temp
                  SET cell_id = gridtemp.cell_id
                  FROM all_farms.gridtemp
                  WHERE gridtemp.field = '", FIELDNAME, "'
                  AND ST_Within(temp.geometry, gridtemp.geom);")
        )
      )
      # Get the mean and 1SD of the response for all cell_ids and remove rows with outliers
      meansNsd <- DBI::dbGetQuery(
        DB,
        paste0("SELECT AVG(resp) mean_resp, STDDEV(resp) sd_resp, cell_id
                FROM ", FARMERNAME, "_r.temp
                GROUP BY temp.cell_id;")
      )
      meansNsd$sd_resp[is.na(meansNsd$sd_resp)|
                         meansNsd$sd_resp == 0] <- 10
      meansNsd$max_resp <- meansNsd$mean_resp + meansNsd$sd_resp
      meansNsd$min_resp <- ifelse((meansNsd$mean_resp - meansNsd$sd_resp) < 0,
                                  0,
                                  (meansNsd$mean_resp - meansNsd$sd_resp))
      invisible(
        DBI::dbWriteTable(
          DB,
          c(paste0(FARMERNAME, "_r"), "means"),
          meansNsd,
          row.names=FALSE)
      )
      invisible(
        DBI::dbSendQuery(
          DB,
          paste0("ALTER TABLE ", FARMERNAME, "_r.temp
                  ADD COLUMN max_resp REAL,
                  ADD COLUMN min_resp REAL;")
        )
      )
      invisible(
        DBI::dbSendQuery(
          DB,
          paste0("UPDATE ", FARMERNAME, "_r.temp temp
                  SET max_resp = means.max_resp
                  FROM ", FARMERNAME, "_r.means means
                  WHERE temp.cell_id = means.cell_id;
                  UPDATE ", FARMERNAME, "_r.temp temp
                  SET min_resp = means.min_resp
                  FROM ", FARMERNAME, "_r.means means
                  WHERE temp.cell_id = means.cell_id;")
        )
      )
      invisible(
        DBI::dbSendQuery(
          DB,
          paste0("DELETE FROM ", FARMERNAME, "_r.temp
                  WHERE resp >= max_resp
                  OR resp <= min_resp;")
        )
      )
      invisible(
        DBI::dbSendQuery( DB, paste0("DROP TABLE ", FARMERNAME, "_r.means"))
      )

      ## add to agg table
      if (GRID == "grid") {
        newcol <- ifelse(is.null(CY), paste0("prev_", RESPVAR), RESPVAR)
        # if gridded option & current year
        # Add the mean response to the aggregated table by cell_id
        invisible(
          DBI::dbSendQuery(
            DB,
            paste0("ALTER TABLE ", FARMERNAME, "_a.temp
                    ADD COLUMN ", newcol, " REAL;

                    WITH vtemp AS (
                    SELECT
                      b.cell_id,
                      to_char(
                        AVG (b.resp),
                        '99999999999999999D99'
                      ) AS resp
                    FROM ", FARMERNAME, "_r.temp b
                    INNER JOIN ", FARMERNAME, "_a.temp a ON a.cell_id = b.cell_id
                    GROUP BY  b.cell_id
                    )

                    UPDATE ", FARMERNAME, "_a.temp aggtemp
                    SET ", newcol, " = CAST ( vtemp.resp AS REAL )
                    FROM vtemp
                    WHERE aggtemp.cell_id = vtemp.cell_id;")
          )
        )
      }else{
        if (GRID == "obs" & !is.null(CY)) {
          # if aggregating to obs points and cy; add x, y, grid, etc. from farmername_r.temp
          invisible(
            DBI::dbSendQuery(
              DB,
              paste0("ALTER TABLE ", FARMERNAME, "_r.temp
                      ADD COLUMN x double precision,
                      ADD COLUMN y double precision;
                      UPDATE ", FARMERNAME, "_r.temp SET
                      x = ST_X(geometry),
                      y = ST_Y(geometry);

                      INSERT INTO ", FARMERNAME, "_a.temp
                      SELECT x, y, cell_id
                      FROM ", FARMERNAME, "_r.temp;

                      UPDATE ", FARMERNAME, "_a.temp SET
                      geometry = ST_MakePoint(x, y, ", utmEpsg, ");

                      UPDATE ", FARMERNAME, "_a.temp SET
                      geometry = ST_SetSRID (geometry, ", utmEpsg, "),
                      field = '", FIELDNAME, "',
                      grid = '", GRID, "',
                      datused = '", DAT_USED, "',
                      farmer = '", FARMERNAME, "',
                      year = '", CY, "',
                      prev_year = '", PY, "';

                      ALTER TABLE ", FARMERNAME, "_a.temp
                      ADD COLUMN ", RESPVAR, " REAL;
                      UPDATE ", FARMERNAME, "_a.temp aggtemp
                      SET ", RESPVAR, " = temp.resp
                      FROM ", FARMERNAME, "_r.temp temp
                      WHERE aggtemp.x = temp.x
                      AND aggtemp.y = temp.y;")
            )
          )
          invisible(
            DBI::dbSendQuery(
              DB,
              paste0("CREATE INDEX aggtemp_geom_idx ON ", FARMERNAME,
                     "_a.temp USING gist (geometry)")
            )
          )
        }else{
          # if grid = obs but not current year, insert into
          # otherwise append to current agg table with coordinates (gridded or PY obs)
          invisible(
            DBI::dbSendQuery(
              DB,
              paste0("ALTER TABLE ", FARMERNAME, "_a.temp
                      ADD COLUMN prev_", RESPVAR, " REAL;

                      WITH vtemp AS (
                        SELECT a.cell_id,
                        b.resp,
                        a.x,
                        a.y,
                        a.geometry
                        FROM ", FARMERNAME, "_a.temp a
                        JOIN LATERAL (
                          SELECT resp
                          FROM ", FARMERNAME, "_r.temp temp
                          ORDER BY a.geometry <-> temp.geometry
                          LIMIT 1
                        ) AS b
                        ON true
                      )

                      UPDATE ", FARMERNAME, "_a.temp a SET
                      prev_", RESPVAR, " = vtemp.resp
                      FROM vtemp
                      WHERE vtemp.cell_id = a.cell_id
                      AND vtemp.x = a.x
                      AND vtemp.y = a.y;")
            )
          )
        }
      }
      invisible(
        DBI::dbSendQuery(
          DB,
          paste0("DROP TABLE ",
                 FARMERNAME,
                 "_r.temp")
        )
      )
      invisible(
        DBI::dbSendQuery(DB, paste0("VACUUM ANALYZE ", FARMERNAME, "_a.temp"))
      )
    },
    #' @description
    #' Go through the cleaning process of cleaning the experimental variable data.
    #' Removes data that are outside of four standard deviations from the mean
    #' of the resposne variable and the distance, if supplied. Removes points
    #' within 30m of the field boundary. This table is stored in the farmer's
    #' raw database schema before it is processed and added to the temporary
    #' table in the farmer's aggregated schema. Differs from the cleaning of
    #' response variable data because of the different structure of inputs
    #' required for the experimental data and because experimental data may
    #' include polygon data. The dot indicates that this function would be
    #' private if not for documentations sake.
    #' @param EXP_FILES The experimental variable files for aggregating.
    #' @param DB Connection to an OFPE database.
    #' @param EXP_COL The column in the experimental variable data that corresponds
    #' to the measured experimental variable.
    #' @param FARMERNAME Name of the farmer that owns the field for aggregation.
    #' @param YEAR The year of the data being aggregated.
    #' @return Temporary table in the farmer's 'raw' schema.
    .cleanExpData = function(EXP_FILES, DB, EXP_COL, FARMERNAME, YEAR) {
      ## create temp file to clean, but check if polygon
      isPoly <- ifelse(any(grepl("poly", EXP_FILES$table)),
                       "MULTIPOLYGON",
                       "POINT")
      if (grepl("POLY", isPoly) ) {
        ## if polygon data
        EXP_FILES <- EXP_FILES[grep("poly", EXP_FILES$table), ]
        for (i in 1:nrow(EXP_FILES)) {
          if (i == 1) {
            if (is.na(EXP_COL[i, "DIST"])) {
              invisible(
                DBI::dbSendQuery(
                  DB,
                  paste0("CREATE TABLE ", FARMERNAME, "_r.temp AS
                          SELECT ", EXP_COL[i, "EXP"], " exp, orig_file, geometry
                          FROM ", FARMERNAME, "_r.", EXP_FILES$table[i], " ", EXP_FILES$table[i], "
                          WHERE ", EXP_FILES$table[i], ".orig_file = '",
                                     as.character(
                                       EXP_COL[i, "orig_file"]
                                     ), "';
                          ALTER TABLE ", FARMERNAME, "_r.temp
                          ADD COLUMN dist VARCHAR;")
                )
              )
            }else{
              invisible(
                DBI::dbSendQuery(
                  DB,
                  paste0("CREATE TABLE ", FARMERNAME, "_r.temp AS
                          SELECT ", EXP_COL[i, "EXP"], " exp, orig_file, geometry, ", EXP_COL[i, "DIST"], " dist
                          FROM ", FARMERNAME, "_r.", EXP_FILES$table[i], " ", EXP_FILES$table[i], "
                          WHERE ", EXP_FILES$table[i], ".orig_file = '",
                                     as.character(
                                       EXP_COL[i, "orig_file"]
                                     ), "';")
                            )
              )
            }
          }else{
            if (is.na(EXP_COL[i, "DIST"])) {
              invisible(
                DBI::dbSendQuery(
                  DB,
                  paste0("INSERT INTO ", FARMERNAME, "_r.temp
                          SELECT ", EXP_COL[i, "EXP"], " exp, orig_file, geometry
                          FROM ", FARMERNAME, "_r.", EXP_FILES$table[i], " ", EXP_FILES$table[i], "
                          WHERE ", EXP_FILES$table[i], ".orig_file = '",
                                     as.character(
                                       EXP_COL[i, "orig_file"]
                                     ), "';")
                )
              )
            }else{
              invisible(
                DBI::dbSendQuery(
                  DB,
                  paste0("INSERT INTO ", FARMERNAME, "_r.temp
                          SELECT ", EXP_COL[i, "EXP"], " exp, orig_file, geometry, ", EXP_COL[i, "DIST"], " dist
                          FROM ", FARMERNAME, "_r.", EXP_FILES$table[i], " ", EXP_FILES$table[i], "
                          WHERE ", EXP_FILES$table[i], ".orig_file = '",
                                     as.character(
                                       EXP_COL[i, "orig_file"]
                                     ), "';")
                )
              )
            }
          }
        }
      }else{
        ## if point data
        for (i in 1:nrow(EXP_FILES)) {
          if (i == 1) {
            if (is.na(EXP_COL[i, "DIST"])) {
              invisible(
                DBI::dbSendQuery(
                  DB,
                  paste0("CREATE TABLE ", FARMERNAME, "_r.temp AS
                          SELECT ", EXP_COL[i, "EXP"], " exp, orig_file, ", EXP_FILES$table[i], ".geometry
                          FROM ", FARMERNAME, "_r.", EXP_FILES$table[i], " ", EXP_FILES$table[i], "
                          JOIN all_farms.temp temp
                          ON ST_Within(", EXP_FILES$table[i], ".geometry, temp.geometry)
                          WHERE ", EXP_FILES$table[i], ".year = '", YEAR, "'
                          AND ", EXP_FILES$table[i], ".orig_file = '", as.character(EXP_COL[i, "orig_file"]), "';
                          ALTER TABLE ", FARMERNAME, "_r.temp
                          ADD COLUMN dist VARCHAR;")
                )
              )
            }else{
              invisible(
                DBI::dbSendQuery(
                  DB,
                  paste0("CREATE TABLE ", FARMERNAME, "_r.temp  AS
                          SELECT ", EXP_COL[i, "EXP"], " exp, orig_file, ", EXP_FILES$table[i], ".geometry, ", EXP_COL[i, "DIST"], " dist
                          FROM ", FARMERNAME, "_r.", EXP_FILES$table[i], " ", EXP_FILES$table[i], "
                          JOIN all_farms.temp temp
                          ON ST_Within(", EXP_FILES$table[i], ".geometry, temp.geometry)
                          WHERE ", EXP_FILES$table[i], ".year = '", YEAR, "'
                          AND ", EXP_FILES$table[i], ".orig_file = '", as.character(EXP_COL[i, "orig_file"]), "';")
                )
              )
            }
          }else{
            if (is.na(EXP_COL[i, "DIST"])) {
              invisible(
                DBI::dbSendQuery(
                  DB,
                  paste0("INSERT INTO ", FARMERNAME, "_r.temp
                          SELECT ", EXP_COL[i, "EXP"], " exp, orig_file, ", EXP_FILES$table[i], ".geometry
                          FROM ", FARMERNAME, "_r.", EXP_FILES$table[i], " ", EXP_FILES$table[i], "
                          JOIN all_farms.temp temp
                          ON ST_Within(", EXP_FILES$table[i], ".geometry, temp.geometry)
                          WHERE ", EXP_FILES$table[i], ".year = '", YEAR, "'
                          AND ", EXP_FILES$table[i], ".orig_file = '", as.character(EXP_COL[i, "orig_file"]), "'")
                )
              )
            }else{
              invisible(
                DBI::dbSendQuery(
                  DB,
                  paste0("INSERT INTO ", FARMERNAME, "_r.temp
                          SELECT ", EXP_COL[i, "EXP"], " exp, orig_file, ", EXP_FILES$table[i], ".geometry, ", EXP_COL[i, "DIST"], " dist
                          FROM ", FARMERNAME, "_r.", EXP_FILES$table[i], " ", EXP_FILES$table[i], "
                          JOIN all_farms.temp temp
                          ON ST_Within(", EXP_FILES$table[i], ".geometry, temp.geometry)
                          WHERE ", EXP_FILES$table[i], ".year = '", YEAR, "'
                          AND ", EXP_FILES$table[i], ".orig_file = '", as.character(EXP_COL[i, "orig_file"]), "';")
                )
              )
            }
          }
        }
      }
      invisible(
        DBI::dbSendQuery(
          DB,
          paste0("ALTER TABLE ", FARMERNAME, "_r.temp
                  ALTER COLUMN exp TYPE real USING exp::real,
                  ALTER COLUMN dist TYPE real USING dist::real,
                  ADD COLUMN max_exp REAL,
                  ADD COLUMN min_exp REAL,
                  ADD COLUMN max_dist REAL,
                  ADD COLUMN min_dist REAL;
                  DELETE FROM ", FARMERNAME, "_r.temp temp
                  WHERE temp.exp = 'NaN';
                  CREATE INDEX temp_geom_idx ON ", FARMERNAME, "_r.temp
                  USING gist (geometry);")
        )
      )
      invisible(
        DBI::dbSendQuery(DB, paste0("VACUUM ANALYZE ", FARMERNAME, "_r.temp"))
      )

      ## Calculate the mean and 4SD for the experimental variable and distance variable if present by the orig_file.
      meansNsd <- DBI::dbGetQuery(
        DB,
        paste0("SELECT AVG(exp) mean_exp, STDDEV(exp) sd_exp, AVG(dist) mean_dist, STDDEV(dist) sd_dist, orig_file
                FROM ", FARMERNAME, "_r.temp GROUP BY temp.orig_file;")
      )
      meansNsd$max_exp <- meansNsd$mean_exp + (4 * meansNsd$sd_exp)
      meansNsd$max_dist <- meansNsd$mean_dist + (4 * meansNsd$sd_dist)
      meansNsd$min_exp <- ifelse((meansNsd$mean_exp - (4 * meansNsd$sd_exp)) < 0,
                                 0,
                                 (meansNsd$mean_exp - (4 * meansNsd$sd_exp)))
      meansNsd$min_dist <- ifelse((meansNsd$mean_dist - (4 * meansNsd$sd_dist)) < 0,
                                  0,
                                  (meansNsd$mean_dist - (4 * meansNsd$sd_dist)))

      for (i in 1:nrow(meansNsd)) {
        if (is.na(meansNsd[i, "mean_dist"]) | is.na(meansNsd[i, "sd_dist"])) {
          meansNsd[i, "max_dist"] <- 10000
          meansNsd[i, "min_dist"] <- 0
        }
        invisible(
          DBI::dbSendQuery(
            DB,
            paste0("UPDATE ", FARMERNAME, "_r.temp temp
                    SET max_exp = ", meansNsd[i, "max_exp"], "
                    WHERE temp.orig_file = '", meansNsd[i, "orig_file"], "';
                    UPDATE ", FARMERNAME, "_r.temp temp
                    SET min_exp = ", meansNsd[i, "min_exp"], "
                    WHERE temp.orig_file = '", meansNsd[i, "orig_file"], "';
                    UPDATE ", FARMERNAME, "_r.temp temp
                    SET max_dist = ", meansNsd[i, "max_dist"], "
                    WHERE temp.orig_file = '", meansNsd[i, "orig_file"], "';
                    UPDATE ", FARMERNAME, "_r.temp temp
                    SET min_dist = ", meansNsd[i, "min_dist"], "
                    WHERE temp.orig_file = '", meansNsd[i, "orig_file"], "';")
          )
        )
      }
      ## Remove observations by orig_file above or below 4SD of the mean experimental variable or distance
      invisible(
        DBI::dbSendQuery(
          DB,
          paste0("DELETE FROM ", FARMERNAME, "_r.temp
                  WHERE exp >= max_exp
                  OR exp <= min_exp
                  OR dist >= max_dist
                  OR dist <= min_dist;")
        )
      )

      ## if any null values replace with 0 for as-applied data
      invisible(
        DBI::dbSendQuery(
          DB,
          paste0("UPDATE ", FARMERNAME, "_r.temp
               SET exp = 0
               WHERE exp IS NULL;")
        )
      )

      # 30m buffe
      if (!grepl("POLY", isPoly)) {
        invisible(
          DBI::dbSendQuery(
            DB,
            paste0("WITH buff AS(
                    SELECT ST_Buffer(geometry, -30) geometry
                    FROM all_farms.temp temp)

                    DELETE FROM ", FARMERNAME, "_r.temp AS temp
                    USING buff
                    WHERE ST_Within(temp.geometry, buff.geometry);")
          )
        )
      }
      invisible(
        DBI::dbSendQuery(
          DB,
          paste0("ALTER TABLE ", FARMERNAME, "_r.temp
                  DROP COLUMN dist,
                  DROP COLUMN max_exp,
                  DROP COLUMN min_exp,
                  DROP COLUMN max_dist,
                  DROP COLUMN min_dist;")
        )
      )
      invisible(
        DBI::dbSendQuery(DB, paste0("VACUUM ANALYZE ", FARMERNAME, "_r.temp"))
      )
    },
    #' @description
    #' Method for aggregating the response variable data in the database. Uses
    #' the output of the .cleanExpData method to add the experimental variable data
    #' to the aggregated table in the database. Assumes current year is true, set
    #' to false to create "prev_EXPVAR" col. Implements a second cleaning process
    #' based on the grid cells where observations outside of one standard deviation
    #' of the mean in each grid cell are omitted. The dot indicates that this function
    #' would be private if not for documentations sake.
    #' @param EXP_FILES The experimental variable files for use in aggregating.
    #' @param DB Connection to an OFPE database.
    #' @param EXP_COL The data.frame containaing the information on the columns
    #' corresponding to the experimental variable.
    #' @param EXP_CONV The data.frame containaing the information for the product
    #' formula and any conversion to lbs per acre.
    #' @param FARMERNAME Name of the farmer that owns the field for aggregation.
    #' @param EXPVAR The experimental variable to aggregate.
    #' @param GRID Whether to aggregate data to the centroids of a grid or use
    #' the raw observed data locations.
    #' @param CY Logical, whether the data for aggregation is from the year of
    #' interest (aka the 'current year').
    #' @return Data in temporary aggregated table.
    .aggExpData = function(EXP_FILES,
                           DB,
                           EXP_COL,
                           EXP_CONV,
                           FARMERNAME,
                           EXPVAR,
                           GRID,
                           CY = TRUE) {
      utmEpsg <- OFPE::findUTMzone(FARMERNAME = FARMERNAME)
      isPoly <- ifelse(any(grepl("poly", EXP_FILES$table)),
                       "MULTIPOLYGON",
                       "POINT")
      for (i in 1:nrow(EXP_FILES)) {
        if (!any(is.na(EXP_COL[i, "PRODUCT"]) |
                is.na(EXP_CONV[i, "CONVERSION"]))) {
          invisible(
            DBI::dbSendQuery(
              DB,
              paste0("UPDATE ", FARMERNAME, "_r.temp temp
                      SET exp = exp * ", EXP_CONV[i, "CONVERSION"], "
                      WHERE orig_file = '", EXP_CONV[i, "orig_file"], "'")
            )
          )
        }
      }
      newcol <- ifelse(CY, EXPVAR, paste0("prev_", EXPVAR))
      if (grepl("POLY", isPoly)) {
        ## if polygon
        invisible(
          DBI::dbSendQuery(
            DB,
            paste0("ALTER TABLE ", FARMERNAME, "_a.temp
                    ADD COLUMN ", newcol, " REAL;
                    ALTER TABLE ", FARMERNAME, "_r.temp
                    ALTER COLUMN geometry TYPE geometry(",
                               as.character(isPoly),
                               ", ", utmEpsg, ")
                    USING ST_Transform(geometry, ", utmEpsg, ");
                    UPDATE ", FARMERNAME, "_a.temp aggtemp
                    SET ", newcol, " = temp.exp
                    FROM ", FARMERNAME, "_r.temp temp
                    WHERE ST_Within(aggtemp.geometry, temp.geometry);")
          )
        )
      }else{
        ## if not polygon
        # Get the cell_id for each point in the temporary table
        invisible(
          DBI::dbSendQuery(
            DB,
            paste0("ALTER TABLE ", FARMERNAME, "_r.temp
                    ADD COLUMN cell_id VARCHAR;
                    ALTER TABLE ", FARMERNAME, "_r.temp
                    ALTER COLUMN geometry TYPE geometry(POINT, ", utmEpsg, ")
                    USING ST_Transform(geometry, ", utmEpsg, ");
                    UPDATE ", FARMERNAME, "_r.temp temp
                    SET cell_id = gridtemp.cell_id
                    FROM all_farms.gridtemp
                    WHERE ST_Within(temp.geometry, gridtemp.geom);")
          )
        )

        # Get the mean and 1SD of the experimental variable for all cell_ids and remove rows with outliers
        meansNsd <- DBI::dbGetQuery(
          DB,
          paste0("SELECT AVG(exp) mean_exp, STDDEV(exp) sd_exp, cell_id
                  FROM ", FARMERNAME, "_r.temp
                  GROUP BY temp.cell_id;")
        )
        meansNsd$sd_exp[is.na(meansNsd$sd_exp)|
                          meansNsd$sd_exp == 0] <- 10
        meansNsd$max_exp <- meansNsd$mean_exp + meansNsd$sd_exp
        meansNsd$min_exp <- ifelse((meansNsd$mean_exp - meansNsd$sd_exp) < 0,
                                   0,
                                   (meansNsd$mean_exp - meansNsd$sd_exp))
        invisible(
          DBI::dbWriteTable(
            DB,
            c(paste0(FARMERNAME, "_r"), "means"),
            meansNsd,
            row.names = FALSE)
        )
        invisible(
          DBI::dbSendQuery(
            DB,
            paste0("ALTER TABLE ", FARMERNAME, "_r.temp
                    ADD COLUMN max_exp REAL,
                    ADD COLUMN min_exp REAL;")
          )
        )
        invisible(
          DBI::dbSendQuery(
            DB,
            paste0("UPDATE ", FARMERNAME, "_r.temp temp
                    SET max_exp = means.max_exp
                    FROM ", FARMERNAME, "_r.means means
                    WHERE temp.cell_id = means.cell_id;
                    UPDATE ", FARMERNAME, "_r.temp temp
                    SET min_exp = means.min_exp
                    FROM ", FARMERNAME, "_r.means means
                    WHERE temp.cell_id = means.cell_id;")
          )
        )
        invisible(
          DBI::dbSendQuery(
            DB,
            paste0("DELETE FROM ", FARMERNAME, "_r.temp
                    WHERE exp >= max_exp
                    OR exp <= min_exp;")
          )
        )
        invisible(
          DBI::dbSendQuery(DB, paste0("DROP TABLE ", FARMERNAME, "_r.means"))
        )
        if (GRID == "grid") {
          # if gridded option get average from cell id
          # Add the mean experimental var to the aggregated table by cell_id
          invisible(
            DBI::dbSendQuery(
              DB,
              paste0("ALTER TABLE ", FARMERNAME, "_a.temp
                      ADD COLUMN ", newcol, " REAL;

                      WITH vtemp AS (
                      SELECT
                        b.cell_id,
                        to_char(
                          AVG (b.exp),
                          '99999999999999999D99'
                        ) AS exp
                      FROM ", FARMERNAME, "_r.temp b
                      INNER JOIN ", FARMERNAME, "_a.temp a ON a.cell_id = b.cell_id
                      GROUP BY  b.cell_id
                      )

                      UPDATE ", FARMERNAME, "_a.temp aggtemp
                      SET ", newcol, " = CAST ( vtemp.exp AS REAL )
                      FROM vtemp
                      WHERE aggtemp.cell_id = vtemp.cell_id;")
            )
          )
        }else{
          # if obs get nearest
          invisible(
            DBI::dbSendQuery(
              DB,
              paste0("ALTER TABLE ", FARMERNAME, "_a.temp
                      ADD COLUMN ", newcol, " REAL;

                      WITH vtemp AS (
                        SELECT a.cell_id,
                        b.exp,
                        a.x,
                        a.y,
                        a.geometry
                        FROM ", FARMERNAME, "_a.temp a
                        JOIN LATERAL (
                          SELECT exp
                          FROM ", FARMERNAME, "_r.temp temp
                          ORDER BY a.geometry <-> temp.geometry
                          LIMIT 1
                        ) AS b
                        ON true
                      )

                      UPDATE ", FARMERNAME, "_a.temp a
                      SET ", newcol, " = vtemp.exp
                      FROM vtemp
                      WHERE vtemp.cell_id = a.cell_id
                      AND vtemp.x = a.x
                      AND vtemp.y = a.y;")
            )
          )
        }
      }
      invisible(
        DBI::dbSendQuery(DB, paste0("DROP TABLE ", FARMERNAME, "_r.temp"))
      )
      invisible(
        DBI::dbSendQuery(DB, paste0("VACUUM ANALYZE ", FARMERNAME, "_a.temp"))
      )
    },
    #' @description
    #' Clip aggregated data to the field boundary. Put the aggregated data
    #' into long lat for raster extraction. Clip the aggregated dataset to
    #' the field boundary for faster processing. If experimental rates are
    #' null convert to zero because the sprayer does not record rather than
    #' recording zeroes. The dot indicates that this function would be private
    #' if not for documentations sake.
    #' @param None No arguments needed because of class instantiation.
    #' @return None.
    .clipAggDat = function() {
      epsg <- sf::st_read(self$aggInputs$dbCon$db,
                          query = paste0(
                            "SELECT *
                            FROM ", self$aggInputs$FARMERNAME, "_a.temp"
                          ))  %>%
        sf::st_crs()
      utmEpsg <- stringr::str_sub(epsg$input, 6, nchar(epsg$input))

      ## clip the aggregated data to the field boundary
      invisible(
        DBI::dbSendQuery(
          self$aggInputs$dbCon$db,
          paste0("ALTER TABLE ", self$aggInputs$FARMERNAME, "_a.temp
                 ALTER COLUMN geometry
                 TYPE geometry(POINT, ", utmEpsg, ")
                 USING ST_Force2D(geometry);
                 ALTER TABLE ", self$aggInputs$FARMERNAME, "_a.temp
                 ALTER COLUMN geometry TYPE geometry(POINT, 4326)
                 USING ST_Transform(geometry, 4326);")
        )
      )
      invisible(
        DBI::dbSendQuery(
          self$aggInputs$dbCon$db,
          paste0("ALTER TABLE ", self$aggInputs$FARMERNAME, "_a.temp
                 ADD COLUMN id SERIAL;
                 DELETE FROM ", self$aggInputs$FARMERNAME, "_a.temp AS temp
                 WHERE temp.id IN (
                 SELECT a.id
                 FROM ", self$aggInputs$FARMERNAME, "_a.temp a, (
                 SELECT ST_Union(geometry) As geometry
                 FROM all_farms.temp
                 ) b
                 WHERE NOT ST_Within(a.geometry, b.geometry)
                 );
                 ALTER TABLE ", self$aggInputs$FARMERNAME, "_a.temp
                 DROP COLUMN id;")
          )
        )
    },
    #' @description
    #' Gather SSURGO soils data if present in the database. Extract the
    #' MUSYM attribute. The dot indicates that this function would be private
    #' if not for documentations sake.
    #' @param None No arguments needed because of class instantiation.
    #' @return None.
    .aggSSURGO = function() {
      if (!length(self$FARMIDX) == 0) {
        ssurgoExist <- as.logical(
          DBI::dbGetQuery(
            self$aggInputs$dbCon$db,
            paste0("SELECT EXISTS (
             SELECT 1
             FROM   information_schema.tables
             WHERE  table_schema = 'all_farms'
             AND table_name = 'ssurgo')")
          )
        )
        if (ssurgoExist) {
          fileExist <- as.character(
            DBI::dbGetQuery(
              self$aggInputs$dbCon$db,
              paste0("SELECT DISTINCT orig_file
                    FROM all_farms.ssurgo
                    WHERE farmidx = '", self$FARMIDX, "'")
            )
          )
          if (length(fileExist) != 0) { # if a file exists
            invisible(
              DBI::dbSendQuery(
                self$aggInputs$dbCon$db,
                paste0("CREATE TABLE all_farms.ssurgo_temp AS
                      SELECT *
                      FROM all_farms.ssurgo
                      WHERE farmidx = '", self$FARMIDX, "'")
              )
            )
            invisible(
              DBI::dbSendQuery(
                self$aggInputs$dbCon$db,
                paste0("UPDATE ", self$aggInputs$FARMERNAME, "_a.temp aggtemp
                      SET musym = ssurgo_temp.musym
                      FROM all_farms.ssurgo_temp ssurgo_temp
                      WHERE ST_Within(aggtemp.geometry, ssurgo_temp.geometry);")
              )
            )
            invisible(
              DBI::dbSendQuery(
                self$aggInputs$dbCon$db,
                paste0("DROP TABLE all_farms.ssurgo_temp;")
              )
            )
          } # end if fileExist
        }
      } # end if farmidx
    },
    #' @description
    #' Save the temporary aggregated table to the appropriate table in
    #' the OFPE database. Puts the data in the correct farmer's aggregated
    #' schema in the table corresponding to the response variable (RESPVAR).
    #' The dot indicates that this function would be private if not for
    #' documentations sake.
    #' @param None No arguments needed because of class instantiation.
    #' @return Aggregated data in the OFPE database.
    .saveAggDat = function() {
      aggExist <- as.logical(
        DBI::dbGetQuery(
          self$aggInputs$dbCon$db,
          paste0("SELECT EXISTS (
                  SELECT 1
                  FROM   information_schema.tables
                  WHERE  table_schema = '", self$aggInputs$FARMERNAME, "_a'
                  AND    table_name = '", self$aggInputs$RESPVAR, "')")
        )
      )
      if (!aggExist) {
        ## if aggregated table doesn't exist make it
        invisible(
          DBI::dbSendQuery(
            self$aggInputs$dbCon$db,
            paste0("CREATE TABLE ", self$aggInputs$FARMERNAME, "_a.", self$aggInputs$RESPVAR, " AS
                    SELECT * FROM ", self$aggInputs$FARMERNAME, "_a.temp;")
          )
        )
      } else {
        ## get column names of agg table
        dbCols <- sf::st_read(
          self$aggInputs$dbCon$db,
          query = paste0("SELECT *
                          FROM ", self$aggInputs$FARMERNAME, "_a.", self$aggInputs$RESPVAR, "
                          LIMIT 1"),
          geometry_column="geometry") %>%
          as.data.frame()

        ## else append to it
        invisible(
          DBI::dbSendQuery(
            self$aggInputs$dbCon$db,
            paste0("DELETE FROM ", self$aggInputs$FARMERNAME, "_a.", self$aggInputs$RESPVAR, "
                    WHERE field = '", self$aggInputs$FIELDNAME, "'
                    AND grid = '", self$aggInputs$GRID, "'
                    AND year = '", self$aggInputs$CY_RESP, "'
                    AND datused = '", self$aggInputs$DAT_USED, "';

                    INSERT INTO ", self$aggInputs$FARMERNAME, "_a.", self$aggInputs$RESPVAR, "
                    SELECT ", paste(names(dbCols), collapse=", "), "
                    FROM ", self$aggInputs$FARMERNAME, "_a.temp;")
          )
        )
      }
      invisible(
        DBI::dbSendQuery(
          self$aggInputs$dbCon$db,
          paste0("VACUUM ANALYZE ",
                 self$aggInputs$FARMERNAME, "_a.", self$aggInputs$RESPVAR, ";")
        )
      )
    },
    #' @description
    #' Export the aggregated data to the local file system. Uses the user
    #' inputs for a path to the data. The dot indicates that this function
    #' would be private if not for documentations sake.
    #' @param None No arguments needed because of class instantiation.
    #' @return Exported data from the OFPE database.
    .exportAggDat = function() {
      stopifnot(
        !is.null(self$aggInputs$exportName)
      )
      ## drop column geometry from aggregated table
      invisible(
        DBI::dbSendQuery(
          self$aggInputs$dbCon$db,
          paste0("ALTER TABLE ",
                 self$aggInputs$FARMERNAME, "_a.temp
                 DROP COLUMN geometry;")
        )
      )
      dat <- sf::st_read(
        self$aggInputs$dbCon$db,
        query = paste0("SELECT * FROM ", self$aggInputs$FARMERNAME, "_a.temp")) %>%
        as.data.frame() %<%
        sf::st_write(paste0(self$inputs$exportName))
    }
  ),

  private = list(
    .idFarm = function() {
      self$FARMIDX <- as.character(
        DBI::dbGetQuery(
          self$aggInputs$dbCon$db,
          paste0("SELECT DISTINCT farmidx
                 FROM  all_farms.farms farms
                 JOIN ", self$aggInputs$FARMERNAME, "_a.temp temp
                 ON ST_Within(temp.geometry, farms.geom);")
        )
      )
      self$FARMERIDX <- as.character(
        DBI::dbGetQuery(
          self$aggInputs$dbCon$db,
          paste0("SELECT DISTINCT farmeridx
                 FROM  all_farms.farms farms
                 WHERE farms.farmidx = '", self$FARMIDX, "';")
        )
      )
    }
  )
)









