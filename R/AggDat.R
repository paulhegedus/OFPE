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
#' whole field, data are removed when exceeding 5SD from the mean, and within
#' each 10m grid cell, data are removed when exceeding 1SD from the cell mean.
#' This occurs regardless of whether 'Grid' or 'Observed' is selected. Then,
#' data from remotely sensed sources are gathered. The data selected is dependent
#' on the 'dat_used' variable, which indicates whether to use data from Jan 1 to
#' Mar 30 for the year of interest or to use data from Jan 1 to Dec 31. This is
#' discussed more in the 'AggInputs' class documentation. Finally, after all on-farm
#' and remotely sensed data has been combined into one dataset, this is saved to the
#' database in the farmer's aggregated ('_a') schema.
#' @seealso \code{\link{DBCon}} for the database connection class,
#' \code{\link{AggGEE}} for the class responsible for aggregating Google Earth
#' Engine data, \code{\link{AggInputs}} for the inputs required for the aggregation
#' process.
#' @export
AggDat <- R6::R6Class(
  "AggDat",
  public = list(
    #' @field aggInputs An object of the 'AggInputs' class containaing the options
    #' for aggregating data. This includes the database connection
    aggInputs = NULL,
    #' @field farmidx The ID of the farm in which the field falls within.
    farmidx = NULL,
    #' @field farmeridx The ID of the farmer that owns the farm that the field
    #' falls within.
    farmeridx = NULL,

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
      if (self$aggInputs$boundary_import == "No") {
        invisible(DBI::dbSendQuery(
            self$aggInputs$dbCon$db,
            paste0("CREATE TABLE all_farms.temp AS
             SELECT * FROM all_farms.fields fields
             WHERE fields.fieldname = '", self$aggInputs$fieldname, "';
             ALTER TABLE all_farms.temp
             RENAME COLUMN geom TO geometry;")
        ))
      } else {
        stopifnot(!is.null(self$aggInputs$boundary_location))
        ## import bounding box
        BBOX <- sf::read_sf(self$aggInputs$boundary_location) %>%
          sf::st_zm() %>%
          sf::`st_crs<-`(4326) %>%
          sf::st_transform("epsg:4326")
        invisible(suppressMessages(rpostgis::pgInsert(
          self$aggInputs$dbCon$db,
          c("all_farms", "temp"),
          as(BBOX, "Spatial"),
          geom = "geometry"
        )))
      }
      invisible(DBI::dbSendQuery(
          self$aggInputs$dbCon$db,
          paste0("UPDATE all_farms.temp SET
                 geometry = ST_Multi(geometry);
                 CREATE INDEX temp_geom_idx
                 ON all_farms.temp
                 USING GIST (geometry);")
      ))
      invisible(DBI::dbSendQuery(
          self$aggInputs$dbCon$db,
          "VACUUM ANALYZE all_farms.temp"
      ))
      OFPE::removeTempFarmerTables(self$aggInputs$dbCon$db, self$aggInputs$farmername)
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
      tryCatch({
          # make 10m grid
          self$.makeXmGrid(
            self$aggInputs$dbCon$db,
            self$aggInputs$boundary_import,
            self$aggInputs$fieldname,
            ifelse(is.null(self$aggInputs$size), 10, self$aggInputs$size), # size is 10m, but could be changed
            self$aggInputs$farmername
          )
          # create agg table
          self$.createAggTable(
            self$aggInputs$dbCon$db,
            self$aggInputs$farmername,
            self$aggInputs$fieldname,
            self$aggInputs$cy_resp,
            self$aggInputs$py_resp,
            self$aggInputs$GRID,
            self$aggInputs$dat_used,
            ifelse(is.null(self$aggInputs$size), 10, self$aggInputs$size)
          )
          # if not sat data
          if (self$aggInputs$respvar != "sat") {
            # clean resp data (CY) & aggregate resp data (CY)
            if (!any(self$aggInputs$cy_resp_files == "None")) {
              self$.cleanRespData(
                self$aggInputs$respvar,
                self$aggInputs$dbCon$db,
                self$aggInputs$cy_resp_files,
                self$aggInputs$boundary_import,
                self$aggInputs$cy_resp_col,
                self$aggInputs$cy_resp,
                self$aggInputs$fieldname,
                self$aggInputs$farmername
              )
              self$.aggRespData(
                self$aggInputs$dbCon$db,
                self$aggInputs$farmername,
                self$aggInputs$respvar,
                self$aggInputs$fieldname,
                self$aggInputs$GRID,
                self$aggInputs$cy_resp,
                self$aggInputs$py_resp,
                self$aggInputs$dat_used,
                ifelse(is.null(self$aggInputs$size), 10, self$aggInputs$size)
              )
            } else { # end if file != none
              invisible(
                DBI::dbSendQuery(
                  self$aggInputs$dbCon$db,
                  paste0("ALTER TABLE ",
                         self$aggInputs$farmername, "_a.temp
                     ADD COLUMN ", self$aggInputs$respvar, " REAL;")
                )
              )
            }
            # clean resp data (PY) & aggregate resp data (PY)
            if (!any(self$aggInputs$py_resp_files == "None")) {
              self$.cleanRespData(
                self$aggInputs$respvar,
                self$aggInputs$dbCon$db,
                self$aggInputs$py_resp_files,
                self$aggInputs$boundary_import,
                self$aggInputs$py_resp_col,
                self$aggInputs$py_resp,
                self$aggInputs$fieldname,
                self$aggInputs$farmername
              )
              self$.aggRespData(
                db = self$aggInputs$dbCon$db,
                farmername = self$aggInputs$farmername,
                respvar = self$aggInputs$respvar,
                fieldname = self$aggInputs$fieldname,
                GRID = self$aggInputs$GRID,
                CY = NULL,
                PY = NULL,
                dat_used = self$aggInputs$dat_used,
                size = ifelse(is.null(self$aggInputs$size), 10, self$aggInputs$size)
              )
            } else { # end if file != none
              invisible(DBI::dbSendQuery(
                self$aggInputs$dbCon$db,
                paste0("ALTER TABLE ",
                       self$aggInputs$farmername, "_a.temp
                     ADD COLUMN prev_", self$aggInputs$respvar, " REAL;")
              ))
            }
            # clean exp data (CY) & aggregate exp data (CY)
            if (!any(self$aggInputs$cy_exp_files == "None")) {
              ## if file exist
              self$.cleanExpData(
                self$aggInputs$cy_exp_files,
                self$aggInputs$dbCon$db,
                self$aggInputs$cy_exp_col,
                self$aggInputs$farmername,
                self$aggInputs$cy_exp,
                self$aggInputs$cy_exp_conv,
                self$aggInputs$expvar,
                CY = TRUE,
                self$aggInputs$fieldname
              )
              self$.aggExpData(
                self$aggInputs$dbCon$db,
                self$aggInputs$farmername,
                self$aggInputs$expvar,
                self$aggInputs$GRID,
                CY = TRUE,
                self$aggInputs$fieldname,
                size = ifelse(is.null(self$aggInputs$size), 10, self$aggInputs$size),
                self$aggInputs$cy_exp_files
              )
            } else { #
              ## if no exp data files
              invisible(DBI::dbSendQuery(
                self$aggInputs$dbCon$db,
                paste0("ALTER TABLE ", self$aggInputs$farmername, "_a.temp
                     ADD COLUMN ", self$aggInputs$expvar, " REAL;")
              ))
              if (self$aggInputs$farmername == "merja" &
                  self$aggInputs$cy_exp == "2019") {
                ## chuck only applied 15lbs N/ac in 2019
                invisible(DBI::dbSendQuery(
                  self$aggInputs$dbCon$db,
                  paste0("UPDATE ", self$aggInputs$farmername, "_a.temp
                       SET ", self$aggInputs$expvar, " = 15;")
                ))
              }
            }
            # clean exp data (PY) & aggregate exp data (PY)
            if (!any(self$aggInputs$py_exp_files == "None")) {
              ## if files exist
              self$.cleanExpData(
                self$aggInputs$py_exp_files,
                self$aggInputs$dbCon$db,
                self$aggInputs$py_exp_col,
                self$aggInputs$farmername,
                self$aggInputs$py_exp,
                self$aggInputs$py_exp_conv,
                self$aggInputs$expvar,
                CY = FALSE,
                self$aggInputs$fieldname
              )
              self$.aggExpData(
                self$aggInputs$dbCon$db,
                self$aggInputs$farmername,
                self$aggInputs$expvar,
                self$aggInputs$GRID,
                CY = FALSE,
                self$aggInputs$fieldname,
                size = ifelse(is.null(self$aggInputs$size), 10, self$aggInputs$size),
                self$aggInputs$py_exp_files
              )
            } else { #
              ## if no exp data files
              invisible(DBI::dbSendQuery(
                self$aggInputs$dbCon$db,
                paste0("ALTER TABLE ", self$aggInputs$farmername, "_a.temp
                       ADD COLUMN prev_", self$aggInputs$expvar, " REAL;")
              ))
              if (self$aggInputs$farmername == "merja" &
                  self$aggInputs$py_exp == "2019") {
                ## chuck only applied 15lbs N/ac in 2019
                invisible(DBI::dbSendQuery(
                  self$aggInputs$dbCon$db,
                  paste0("UPDATE ", self$aggInputs$farmername, "_a.temp
                         SET prev_", self$aggInputs$expvar, " = 15;")
                ))
              }
            }
          } # end if not sat dat
          # clip to field boundary
          self$.clipAggDat()
          private$.idFarm()
          # remote sensing data
          aggGEE <- OFPE::AggGEE$new(self$aggInputs,
                                     self$farmidx,
                                     self$farmeridx)
          aggGEE$aggregateGEE()
          # ssurgo data
          self$.aggSSURGO()
          # vacuum analyze often
          invisible(DBI::dbSendQuery(
            self$aggInputs$dbCon$db,
            paste0("VACUUM ANALYZE ",
                   self$aggInputs$farmername, "_a.temp;")
          ))
          # export data
          if (self$aggInputs$save_in_db == "Yes") {
            self$.saveAggDat()
          }
          if (self$aggInputs$export == "Yes") {
            self$.exportAggDat()
          }
          # clean up
          OFPE::removeTempTables(self$aggInputs$dbCon$db)
          OFPE::removeTempFarmerTables(self$aggInputs$dbCon$db,
                                       self$aggInputs$farmername)
          print(paste0("AGGREGATION COMPLETE: ",
                       self$aggInputs$farmername,
                       " ", self$aggInputs$fieldname,
                       " ", self$aggInputs$cy_resp,
                       " ", self$aggInputs$respvar,
                       "."))},
        warning = function(w) {print()},
        error = function(e) {
          print(paste0("!!! ERROR AGGREGATING ",
                       self$aggInputs$farmername,
                       " ", self$aggInputs$fieldname,
                       " ", self$aggInputs$cy_resp,
                       " ", self$aggInputs$respvar,
                       " DATA !!!"))
        })
    },
    #' @description
    #' Makes a grid across a field of a specified size. If the grid
    #' option was selected observations will be aggregated onto
    #' the centroids of 10m grid cells generated from the bounding box of
    #' the field boundary. If the field boundary is from the database, the
    #' centroid locations will be consistent for aggregted data across years.
    #' If the user imported a field boundary, these points may not be consistent
    #' across years unless the user uses the same field boundary each year.
    #' Again, it is strongly encouraged that the user updates the database with
    #' the field boundary they have imported, to assure the centroid location
    #' remains constant year to year. To create the aggregation table, a 10m grid
    #' is applied to the field using the bounding box of the field boundary. The
    #' locations of the center of the grid cells are extracted, as well as the
    #' grid ID. These create the basis of the aggregation table. If the observed
    #' option was selected, a grid is still made to identify the grid cells,
    #' however data is not aggregated to the grid cell centroid locations. The
    #' dot indicates that this function would be private if not for
    #' documentations sake.
    #' @param db Connection to an OFPE formatted database.
    #' @param boundary_import Whether the user uploaded their own field boundary
    #' or are using a boundary stored in the database.
    #' @param fieldname Name of the field for aggregation.
    #' @param size Size of grid to make (meters), default = 10.
    #' @param farmername Name of farmer that owns the field.
    #' @return None.
    .makeXmGrid = function(db, boundary_import, fieldname, size = 10, farmername) {
      utm_epsg <- OFPE::findUTMzone(db, fieldname = fieldname[1])
      grids_exist <- FALSE
      field_exist <- FALSE
      # check if grids exist
      grids_exist <- as.logical(DBI::dbGetQuery(
          db,
          paste0("SELECT EXISTS (
               SELECT 1
               FROM information_schema.tables
               WHERE  table_schema = 'all_farms'
               AND table_name = 'grids')")
      ))
      # if grids exists check if field exists
      if (grids_exist) {
        # check if field has a grid
        field_exist <- as.logical(DBI::dbGetQuery(
            db,
            paste0("SELECT EXISTS (
                 SELECT 1
                 FROM all_farms.grids grids
                 WHERE grids.field = '", fieldname, "'
                 AND grids.size = ", size,")")
        ))
        # if it does copy to gridtemp
        if (field_exist) {
          invisible(DBI::dbSendQuery(
              db,
              paste0("CREATE TABLE all_farms.gridtemp AS
                SELECT *
                FROM all_farms.grids
                WHERE field = '", fieldname, "'
                AND size = ", size)
          ))
        }
      }
      # if field does not exists
      if (!field_exist) {
        BBOX <- sf::st_read(
          db,
          query = paste0("SELECT * FROM all_farms.temp"),
          geometry_column = "geometry") %>%
          sf::st_transform(paste0("epsg:", utm_epsg)) %>%
          as("Spatial") %>%
          sp::bbox()
        NCOL <- ceiling((BBOX["x", "max"] - BBOX["x", "min"]) / size)
        NROW <- ceiling((BBOX["y", "max"] - BBOX["y", "min"]) / size)

        invisible(DBI::dbSendQuery(
            db,
            paste0("CREATE TABLE all_farms.gridtemp AS
               SELECT *
               FROM ST_CreateFishnet (", NROW, ", ", NCOL, ", ", size, ", ", size, ", ", BBOX["x", "min"], ", ", BBOX["y", "min"], ") AS cells;
               ALTER TABLE all_farms.gridtemp
               ADD COLUMN cell_id VARCHAR,
               ADD COLUMN field VARCHAR,
               ADD COLUMN size double precision;
               UPDATE all_farms.gridtemp SET
               cell_id = row::text ||'_'|| col::text,
               field = '", fieldname, "',
               size = ", size, ";
               UPDATE all_farms.gridtemp SET geom = ST_SetSRID (geom, ", utm_epsg, ");
               ALTER TABLE all_farms.gridtemp
               ADD COLUMN x double precision,
               ADD COLUMN y double precision;
               UPDATE all_farms.gridtemp SET
               x = ST_X(ST_Centroid(geom)),
               y = ST_Y(ST_Centroid(geom));
               ALTER TABLE all_farms.gridtemp
               ADD PRIMARY KEY (cell_id, field, size);
               CREATE INDEX gridtemp_geom_idx
               ON all_farms.gridtemp
               USING gist (geom);")
        ))
      }
      # if boundary_import = no, field does not exist in grids
      if (boundary_import == "No" & !field_exist) {
        if (grids_exist) { # if grids_exists append
          invisible(DBI::dbSendQuery(
              db,
              paste0("INSERT INTO all_farms.grids
                 SELECT * FROM all_farms.gridtemp;")
          ))
        } else { # if not create grids
          invisible(DBI::dbSendQuery(
              db,
              paste0("CREATE TABLE all_farms.grids AS
                 SELECT * FROM all_farms.gridtemp;")
          ))
        }
        invisible(DBI::dbSendQuery(
            db,
            paste0("VACUUM ANALYZE all_farms.grids")
        ))
      }
      invisible(DBI::dbSendQuery(
          db,
          paste0("VACUUM ANALYZE all_farms.gridtemp")
      ))
    },
    #' @description
    #' Creates a temporary aggregated table in the database for processing.
    #' The dot indicates that this function would be private if not for
    #' documentations sake. Create the initial aggregated table based on the
    #' option, either grid or observed locations. If the grid option is
    #' selected, create the initial aggregated table with the centroid
    #' locations and grid cell identifier. Fill in informational variables
    #' for the farmer and field name, the year data is aggregated for,
    #' and the previous harvest year, as well as whether it is a grid or
    #' observed values (grid vs obs). Finally, upload to database for
    #' aggregation.
    #' @param db Connection to an OFPE formatted database.
    #' @param farmername Name of farmer that owns the field.
    #' @param fieldname Name of the field for aggregation.
    #' @param CY The year of interest for aggregation (aka 'current year').
    #' @param PY The 'previous year' in which the crop was harvested in the field.
    #' @param GRID Whether to aggregate data to the centroids of a grid or use
    #' the raw observed data locations.
    #' @param dat_used The length of year to gather data over for the CY.
    #' @param size Size of grid to make (meters), default = 10.
    #' @return Temporary table in the database.
    .createAggTable = function(db,
                               farmername,
                               fieldname,
                               CY,
                               PY,
                               GRID,
                               dat_used,
                               size) {
      utm_epsg <- OFPE::findUTMzone(db, fieldname = fieldname[1])
      if (GRID == "grid") {
        invisible(DBI::dbSendQuery(
            db,
            paste0("CREATE TABLE ", farmername, "_a.temp AS
                    SELECT *
                    FROM all_farms.gridtemp gridtemp
                    WHERE gridtemp.field = '", fieldname, "'
                    AND gridtemp.size = ", size, ";
                    ALTER TABLE ", farmername, "_a.temp
                    DROP COLUMN row,
                    DROP COLUMN col,
                    ADD COLUMN grid VARCHAR,
                    ADD COLUMN datused VARCHAR,
                    ADD COLUMN farmer VARCHAR,
                    ADD COLUMN year VARCHAR,
                    ADD COLUMN prev_year VARCHAR;
                    UPDATE ", farmername, "_a.temp SET
                    grid = '", GRID, "',
                    datused = '", dat_used, "',
                    farmer = '", farmername, "',
                    year = '", CY, "',
                    prev_year = '", PY, "';
                    ALTER TABLE ", farmername, "_a.temp
                    ALTER COLUMN geom TYPE geometry(Point, ", utm_epsg, ")
                    USING ST_Centroid(geom);
                    ALTER TABLE ", farmername, "_a.temp
                    RENAME COLUMN geom TO geometry;")
        ))
        invisible(DBI::dbSendQuery(
            db,
            paste0("CREATE INDEX aggtemp_geom_idx ON ",
                   farmername,
                   "_a.temp USING gist (geometry)")
        ))
        invisible(
          DBI::dbSendQuery(db, paste0("VACUUM ANALYZE ", farmername, "_a.temp"))
        )
      } else {
        invisible(DBI::dbSendQuery(
            db,
            paste0("CREATE TABLE ", farmername, "_a.temp (
            x double precision,
            y double precision,
            cell_id VARCHAR,
            field VARCHAR,
            size double precision,
            grid VARCHAR,
            datused VARCHAR,
            farmer  VARCHAR,
            year  VARCHAR,
            prev_year  VARCHAR);
            ALTER TABLE ", farmername, "_a.temp
            ADD COLUMN geometry geometry;")
        ))
      }
    },
    #' @description
    #' Go through the cleaning process of cleaning the response variable data.
    #' Removes data that are outside of four standard deviations from the mean
    #' of the response variable and the distance, if supplied. Removes points
    #' within 30m of the field boundary. This table is stored in the farmer's
    #' raw database schema before it is processed and added to the temporary
    #' table in the farmer's aggregated schema. The dot indicates that this
    #' function would be private if not for documentations sake.
    #' @param respvar The response variable to aggregate.
    #' @param db Connection to an OFPE database.
    #' @param resp_files Files used for aggregation.
    #' @param boundary_import Whether the user imported their own field boundary or
    #' using a predefined experimental field in the database.
    #' @param resp_col The column in the response variable data that corresponds
    #' to the measured response.
    #' @param year The year of the data being aggregated.
    #' @param fieldname Name of the field for aggregation.
    #' @param farmername Name of the farmer that owns the field for aggregation.
    #' @return Temporary table in farmer's 'raw' schema.
    .cleanRespData = function(respvar,
                              db,
                              resp_files,
                              boundary_import,
                              resp_col,
                              year,
                              fieldname,
                              farmername) {
      ## create temp file to clean
      for (i in 1:length(resp_files)) {
        if (i == 1) {
          if (is.na(resp_col[i, "dist"])) {
            invisible(DBI::dbSendQuery(
                db,
                paste0("CREATE TABLE ", farmername, "_r.temp AS
                        SELECT ", resp_col[i, "resp"], " resp, orig_file, ", respvar, ".geometry
                        FROM ", farmername, "_r.", respvar, " ", respvar, "
                        JOIN all_farms.temp temp
                        ON ST_Within(", respvar, ".geometry, temp.geometry)
                        WHERE ", respvar, ".year = '", year, "'
                        AND ", respvar, ".orig_file = '", resp_files[i], "';
                        ALTER TABLE ", farmername, "_r.temp
                        ADD COLUMN dist VARCHAR;")
            ))
          } else {
            invisible(DBI::dbSendQuery(
                db,
                paste0("CREATE TABLE ", farmername, "_r.temp AS
                        SELECT ", resp_col[i, "resp"], " resp, orig_file, ", respvar, ".geometry, ", resp_col[i, "dist"], " dist
                        FROM ", farmername, "_r.", respvar, " ", respvar, "
                        JOIN all_farms.temp temp
                        ON ST_Within(", respvar, ".geometry, temp.geometry)
                        WHERE ", respvar, ".year = '", year, "'
                        AND ", respvar, ".orig_file = '", resp_files[i], "';")
            ))
          }
        } else {
          if (is.na(resp_col[i, "dist"])) {
            invisible(DBI::dbSendQuery(
                db,
                paste0("INSERT INTO ", farmername, "_r.temp
                        SELECT ", resp_col[i, "resp"], " resp, orig_file, ", respvar, ".geometry
                        FROM ", farmername, "_r.", respvar, " ", respvar, "
                        JOIN all_farms.temp temp
                        ON ST_Within(", respvar, ".geometry, temp.geometry)
                        WHERE ", respvar, ".year = '", year, "'
                        AND ", respvar, ".orig_file = '", resp_files[i], "'")
            ))
          } else {
            invisible(DBI::dbSendQuery(
                db,
                paste0("INSERT INTO ", farmername, "_r.temp
                        SELECT ", resp_col[i, "resp"], " resp, orig_file, ", respvar, ".geometry, ", resp_col[i, "dist"], " dist
                        FROM ", farmername, "_r.", respvar, " ", respvar, "
                        JOIN all_farms.temp temp
                        ON ST_Within(", respvar, ".geometry, temp.geometry)
                        WHERE ", respvar, ".year = '", year, "'
                        AND ", respvar, ".orig_file = '", resp_files[i], "';")
            ))
          }
        }
      }
      invisible(DBI::dbSendQuery(
          db,
          paste0("ALTER TABLE ", farmername, "_r.temp
                  ALTER COLUMN resp TYPE real USING resp::real,
                  ALTER COLUMN dist TYPE real USING dist::real,
                  ADD COLUMN max_resp REAL,
                  ADD COLUMN min_resp REAL,
                  ADD COLUMN max_dist REAL,
                  ADD COLUMN min_dist REAL;
                  DELETE FROM ", farmername, "_r.temp temp
                  WHERE temp.resp = 'NaN';
                  CREATE INDEX temp_geom_idx ON ", farmername, "_r.temp USING gist (geometry);")
      ))
      invisible(DBI::dbSendQuery(
          db,
          paste0("VACUUM ANALYZE ",
                 farmername,
                 "_r.temp")
      ))
      ## Calculate the mean and 5SD for the response and distance variable if present by the orig_file.
      means_sd <- DBI::dbGetQuery(
        db,
        paste0("SELECT AVG(resp) mean_resp, STDDEV(resp) sd_resp, AVG(dist) mean_dist, STDDEV(dist) sd_dist, orig_file
                FROM ", farmername, "_r.temp
                GROUP BY temp.orig_file;")
      )
      means_sd$max_resp <- means_sd$mean_resp + (5 * means_sd$sd_resp)
      means_sd$max_dist <- means_sd$mean_dist + (5 * means_sd$sd_dist)
      means_sd$min_resp <- 0
      means_sd$min_dist <- ifelse((means_sd$mean_dist - (5 * means_sd$sd_dist)) < 0,
                                  0,
                                  (means_sd$mean_dist - (5 * means_sd$sd_dist)))
      for (i in 1:nrow(means_sd)) {
        if (is.na(means_sd[i, "mean_dist"]) | is.na(means_sd[i, "sd_dist"])) {
          means_sd[i, "max_dist"] <- 10000
          means_sd[i, "min_dist"] <- 0
        }
        invisible(DBI::dbSendQuery(
            db,
            paste0("UPDATE ", farmername, "_r.temp temp
                    SET max_resp = ", means_sd[i, "max_resp"], "
                    WHERE temp.orig_file = '", means_sd[i, "orig_file"], "';
                    UPDATE ", farmername, "_r.temp temp
                    SET min_resp = ", means_sd[i, "min_resp"], "
                    WHERE temp.orig_file = '", means_sd[i, "orig_file"], "';
                    UPDATE ", farmername, "_r.temp temp
                    SET max_dist = ", means_sd[i, "max_dist"], "
                    WHERE temp.orig_file = '", means_sd[i, "orig_file"], "';
                    UPDATE ", farmername, "_r.temp temp
                    SET min_dist = ", means_sd[i, "min_dist"], "
                    WHERE temp.orig_file = '", means_sd[i, "orig_file"], "';")
        ))
      }
      ## Remove observations by orig_file above or below 5SD of the mean response or distance
      invisible(DBI::dbSendQuery(
          db,
          paste0("DELETE FROM ", farmername, "_r.temp
                  WHERE resp >= max_resp
                  OR resp <= min_resp
                  OR dist >= max_dist
                  OR dist <= min_dist;")
      ))
      invisible(DBI::dbSendQuery(
          db,
          paste0("ALTER TABLE ", farmername, "_r.temp
                  DROP COLUMN dist,
                  DROP COLUMN max_resp,
                  DROP COLUMN min_resp,
                  DROP COLUMN max_dist,
                  DROP COLUMN min_dist;")
      ))
      invisible(DBI::dbSendQuery(
          db,
          paste0("VACUUM ANALYZE ",
                 farmername,
                 "_r.temp")
      ))
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
    #' @param db Connection to an OFPE database.
    #' @param farmername Name of the farmer that owns the field for aggregation.
    #' @param respvar The response variable to aggregate.
    #' @param fieldname Name of the field for aggregation.
    #' @param GRID Whether to aggregate data to the centroids of a grid or use
    #' the raw observed data locations.
    #' @param CY The year of interest for aggregation (aka the 'current year').
    #' NOTE: that when aggregating "PY" data, leave NULL.
    #' @param PY The year prior to the year of interest that a crop was harvested
    #' in the field. NOTE: that when aggregating "PY" data, leave NULL
    #' (Counterintuitive... I know).
    #' @param dat_used The length of year to gather data over for the CY.
    #' @param size Size of grid to make (meters), default = 10.
    #' @return Data in temporary aggregated table.
    .aggRespData = function(db,
                            farmername,
                            respvar,
                            fieldname,
                            GRID,
                            CY = NULL,
                            PY = NULL,
                            dat_used,
                            size) {
      utm_epsg <- OFPE::findUTMzone(db, fieldname = fieldname[1])
      # Get the cell_id for each point in the temporary table
      invisible(DBI::dbSendQuery(
          db,
          paste0("ALTER TABLE ", farmername, "_r.temp
                  ADD COLUMN cell_id VARCHAR;
                  ALTER TABLE ", farmername, "_r.temp
                  ALTER COLUMN geometry TYPE geometry(POINT, ", utm_epsg, ")
                  USING ST_Transform(geometry, ", utm_epsg, ");
                  UPDATE ", farmername, "_r.temp temp
                  SET cell_id = gridtemp.cell_id
                  FROM all_farms.gridtemp
                  WHERE gridtemp.field = '", fieldname, "'
                  AND gridtemp.size = ", size, "
                  AND ST_Within(temp.geometry, gridtemp.geom);")
      ))
      # Get the mean and 1SD of the response for all cell_ids and remove rows with outliers
      means_sd <- DBI::dbGetQuery(
        db,
        paste0("SELECT AVG(resp) mean_resp, STDDEV(resp) sd_resp, cell_id
                FROM ", farmername, "_r.temp
                GROUP BY temp.cell_id;")
      )
      means_sd$sd_resp[is.na(means_sd$sd_resp)|
                         means_sd$sd_resp == 0] <- 10
      means_sd$max_resp <- means_sd$mean_resp + means_sd$sd_resp
      means_sd$min_resp <- 0
      invisible(
        DBI::dbWriteTable(
          db,
          c(paste0(farmername, "_r"), "means"),
          means_sd,
          row.names=FALSE)
      )
      invisible(DBI::dbSendQuery(
          db,
          paste0("ALTER TABLE ", farmername, "_r.temp
                  ADD COLUMN max_resp REAL,
                  ADD COLUMN min_resp REAL;")
      ))
      invisible(DBI::dbSendQuery(
          db,
          paste0("UPDATE ", farmername, "_r.temp temp
                  SET max_resp = means.max_resp
                  FROM ", farmername, "_r.means means
                  WHERE temp.cell_id = means.cell_id;
                  UPDATE ", farmername, "_r.temp temp
                  SET min_resp = means.min_resp
                  FROM ", farmername, "_r.means means
                  WHERE temp.cell_id = means.cell_id;")
      ))
      invisible(DBI::dbSendQuery(
          db,
          paste0("DELETE FROM ", farmername, "_r.temp
                  WHERE resp >= max_resp
                  OR resp <= min_resp;")
      ))
      invisible(
        DBI::dbSendQuery( db, paste0("DROP TABLE ", farmername, "_r.means"))
      )
      ## add to agg table
      if (GRID == "grid") {
        newcol <- ifelse(is.null(CY), paste0("prev_", respvar), respvar)
        # if gridded option & current year
        # Add the mean response to the aggregated table by cell_id
        invisible(DBI::dbSendQuery(
            db,
            paste0("ALTER TABLE ", farmername, "_a.temp
                    ADD COLUMN ", newcol, " REAL;

                    WITH vtemp AS (
                    SELECT
                      b.cell_id,
                      to_char(
                        AVG (b.resp),
                        '9999999999999999999'
                      ) AS resp
                    FROM ", farmername, "_r.temp b
                    INNER JOIN ", farmername, "_a.temp a ON a.cell_id = b.cell_id
                    GROUP BY  b.cell_id
                    )

                    UPDATE ", farmername, "_a.temp aggtemp
                    SET ", newcol, " = CAST ( vtemp.resp AS REAL )
                    FROM vtemp
                    WHERE aggtemp.cell_id = vtemp.cell_id;")
        ))
      } else {
        if (GRID == "obs" & !is.null(CY)) {
          # if aggregating to obs points and cy; add x, y, grid, etc. from farmername_r.temp
          invisible(DBI::dbSendQuery(
              db,
              paste0("ALTER TABLE ", farmername, "_r.temp
                      ADD COLUMN x double precision,
                      ADD COLUMN y double precision;
                      UPDATE ", farmername, "_r.temp SET
                      x = ST_X(geometry),
                      y = ST_Y(geometry);

                      INSERT INTO ", farmername, "_a.temp
                      SELECT x, y, cell_id
                      FROM ", farmername, "_r.temp;

                      UPDATE ", farmername, "_a.temp SET
                      geometry = ST_MakePoint(x, y, ", utm_epsg, ");

                      UPDATE ", farmername, "_a.temp SET
                      geometry = ST_SetSRID (geometry, ", utm_epsg, "),
                      field = '", fieldname, "',
                      grid = '", GRID, "',
                      datused = '", dat_used, "',
                      size = ", size,",
                      farmer = '", farmername, "',
                      year = '", CY, "',
                      prev_year = '", PY, "';

                      ALTER TABLE ", farmername, "_a.temp
                      ADD COLUMN ", respvar, " REAL;
                      UPDATE ", farmername, "_a.temp aggtemp
                      SET ", respvar, " = temp.resp
                      FROM ", farmername, "_r.temp temp
                      WHERE aggtemp.x = temp.x
                      AND aggtemp.y = temp.y;")
          ))
          invisible(DBI::dbSendQuery(
              db,
              paste0("CREATE INDEX aggtemp_geom_idx ON ", farmername,
                     "_a.temp USING gist (geometry)")
          ))
        } else {
          # if grid = obs but not current year, insert into
          # otherwise append to current agg table with coordinates (gridded or PY obs)
          invisible(DBI::dbSendQuery(
              db,
              paste0("ALTER TABLE ", farmername, "_a.temp
                      ADD COLUMN prev_", respvar, " REAL;

                      WITH vtemp AS (
                        SELECT a.cell_id,
                        b.resp,
                        a.x,
                        a.y,
                        a.geometry
                        FROM ", farmername, "_a.temp a
                        JOIN LATERAL (
                          SELECT resp
                          FROM ", farmername, "_r.temp temp
                          ORDER BY a.geometry <-> temp.geometry
                          LIMIT 1
                        ) AS b
                        ON true
                      )

                      UPDATE ", farmername, "_a.temp a SET
                      prev_", respvar, " = vtemp.resp
                      FROM vtemp
                      WHERE vtemp.cell_id = a.cell_id
                      AND vtemp.x = a.x
                      AND vtemp.y = a.y;")
          ))
        }
      }
      invisible(
        DBI::dbSendQuery(db, paste0("DROP TABLE ", farmername, "_r.temp"))
      )
      invisible(
        DBI::dbSendQuery(db, paste0("VACUUM ANALYZE ", farmername, "_a.temp"))
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
    #' include polygon data. Implements a second cleaning process
    #' based on the grid cells where observations outside of one standard deviation
    #' of the mean in each grid cell are omitted. The dot indicates that this function
    #' would be private if not for documentations sake.
    #' @param exp_files The experimental variable files for aggregating.
    #' @param db Connection to an OFPE database.
    #' @param exp_col The column in the experimental variable data that corresponds
    #' to the measured experimental variable.
    #' @param farmername Name of the farmer that owns the field for aggregation.
    #' @param year The year of the data being aggregated.
    #' @param exp_conv The data.frame containaing the information for the product
    #' formula and any conversion to lbs per acre.
    #' @param expvar The experimental variable to aggregate.
    #' @param CY Logical, whether the data for aggregation is from the year of
    #' interest (aka the 'current year').
    #' @param fieldname Name of the field for aggregation.
    #' @return Temporary table in the farmer's 'raw' schema.
    .cleanExpData = function(exp_files,
                             db,
                             exp_col,
                             farmername,
                             year,
                             exp_conv,
                             expvar,
                             CY = TRUE,
                             fieldname) {
      ## create temp file to clean, but check if polygon
      is_poly <- ifelse(any(grepl("poly", exp_files$table)),
                       "MULTIPOLYGON",
                       "POINT")
      if (grepl("MULTIPOLYGON", is_poly) ) {
        ## if polygon data
        exp_files <- exp_files[grep("poly", exp_files$table), ]
        for (i in 1:nrow(exp_files)) {
          if (i == 1) {
            if (is.na(exp_col[i, "dist"])) {
              invisible(DBI::dbSendQuery(
                  db,
                  paste0(
                    "CREATE TABLE ", farmername, "_r.temp AS
                    SELECT ", exp_col[i, "EXP"], " exp, orig_file, geometry
                    FROM ", farmername, "_r.", exp_files$table[i], " ", exp_files$table[i], "
                    WHERE ", exp_files$table[i], ".orig_file = '",
                    as.character(exp_col[i, "orig_file"]), "';
                    ALTER TABLE ", farmername, "_r.temp
                    ADD COLUMN dist VARCHAR;"
                  )
              ))
            } else {
              invisible(DBI::dbSendQuery(
                  db,
                  paste0(
                    "CREATE TABLE ", farmername, "_r.temp AS
                    SELECT ", exp_col[i, "EXP"], " exp, orig_file, geometry, ",
                    exp_col[i, "dist"], " dist
                    FROM ", farmername, "_r.", exp_files$table[i], " ",
                    exp_files$table[i], "
                    WHERE ", exp_files$table[i], ".orig_file = '",
                    as.character(exp_col[i, "orig_file"]), "';")
              ))
            }
          } else {
            if (is.na(exp_col[i, "dist"])) {
              invisible(DBI::dbSendQuery(
                  db,
                  paste0(
                    "INSERT INTO ", farmername, "_r.temp
                    SELECT ", exp_col[i, "EXP"], " exp, orig_file, geometry
                    FROM ", farmername, "_r.", exp_files$table[i], " ",
                    exp_files$table[i], "
                    WHERE ", exp_files$table[i], ".orig_file = '",
                    as.character(exp_col[i, "orig_file"]), "';"
                  )
              ))
            } else {
              invisible(DBI::dbSendQuery(
                  db,
                  paste0(
                    "INSERT INTO ", farmername, "_r.temp
                    SELECT ", exp_col[i, "EXP"], " exp, orig_file, geometry, ",
                    exp_col[i, "dist"], " dist
                    FROM ", farmername, "_r.", exp_files$table[i], " ",
                    exp_files$table[i], "
                    WHERE ", exp_files$table[i], ".orig_file = '",
                    as.character(exp_col[i, "orig_file"]), "';"
                  )
              ))
            }
          }
        }
      } else {
        ## if point data
        for (i in 1:nrow(exp_files)) {
          if (i == 1) {
            if (is.na(exp_col[i, "dist"])) {
              invisible(DBI::dbSendQuery(
                  db,
                  paste0(
                    "CREATE TABLE ", farmername, "_r.temp AS
                    SELECT ", exp_col[i, "EXP"], " exp, orig_file, ",
                    exp_files$table[i], ".geometry
                    FROM ", farmername, "_r.", exp_files$table[i], " ",
                    exp_files$table[i], "
                    JOIN all_farms.temp temp
                    ON ST_Within(", exp_files$table[i], ".geometry, temp.geometry)
                    WHERE ", exp_files$table[i], ".year = '", year, "'
                    AND ", exp_files$table[i], ".orig_file = '",
                    as.character(exp_col[i, "orig_file"]), "';
                    ALTER TABLE ", farmername, "_r.temp
                    ADD COLUMN dist VARCHAR;"
                  )
              ))
            } else {
              invisible(DBI::dbSendQuery(
                  db,
                  paste0(
                    "CREATE TABLE ", farmername, "_r.temp  AS
                    SELECT ", exp_col[i, "EXP"], " exp, orig_file, ",
                    exp_files$table[i], ".geometry, ", exp_col[i, "dist"], " dist
                    FROM ", farmername, "_r.", exp_files$table[i], " ",
                    exp_files$table[i], "
                    JOIN all_farms.temp temp
                    ON ST_Within(", exp_files$table[i], ".geometry, temp.geometry)
                    WHERE ", exp_files$table[i], ".year = '", year, "'
                    AND ", exp_files$table[i], ".orig_file = '",
                    as.character(exp_col[i, "orig_file"]), "';"
                  )
              ))
            }
          } else {
            if (is.na(exp_col[i, "dist"])) {
              invisible(DBI::dbSendQuery(
                  db,
                  paste0(
                    "INSERT INTO ", farmername, "_r.temp
                    SELECT ", exp_col[i, "EXP"], " exp, orig_file, ",
                    exp_files$table[i], ".geometry
                    FROM ", farmername, "_r.", exp_files$table[i], " ",
                    exp_files$table[i], "
                    JOIN all_farms.temp temp
                    ON ST_Within(", exp_files$table[i], ".geometry, temp.geometry)
                    WHERE ", exp_files$table[i], ".year = '", year, "'
                    AND ", exp_files$table[i], ".orig_file = '",
                    as.character(exp_col[i, "orig_file"]), "'"
                  )
              ))
            } else {
              invisible(DBI::dbSendQuery(
                  db,
                  paste0(
                    "INSERT INTO ", farmername, "_r.temp
                    SELECT ", exp_col[i, "EXP"], " exp, orig_file, ",
                    exp_files$table[i], ".geometry, ", exp_col[i, "dist"], " dist
                    FROM ", farmername, "_r.", exp_files$table[i], " ",
                    exp_files$table[i], "
                    JOIN all_farms.temp temp
                    ON ST_Within(", exp_files$table[i], ".geometry, temp.geometry)
                    WHERE ", exp_files$table[i], ".year = '", year, "'
                    AND ", exp_files$table[i], ".orig_file = '",
                    as.character(exp_col[i, "orig_file"]), "';"
                  )
              ))
            }
          }
        }
      }
      invisible(DBI::dbSendQuery(
          db,
          paste0("ALTER TABLE ", farmername, "_r.temp
                  ALTER COLUMN exp TYPE real USING exp::real,
                  ALTER COLUMN dist TYPE real USING dist::real,
                  ADD COLUMN max_exp REAL,
                  ADD COLUMN min_exp REAL,
                  ADD COLUMN max_dist REAL,
                  ADD COLUMN min_dist REAL;
                  DELETE FROM ", farmername, "_r.temp temp
                  WHERE temp.exp = 'NaN';
                  CREATE INDEX temp_geom_idx ON ", farmername, "_r.temp
                  USING gist (geometry);")
      ))
      invisible(
        DBI::dbSendQuery(db, paste0("VACUUM ANALYZE ", farmername, "_r.temp"))
      )
      ## Calculate the mean and 5SD for the experimental variable and distance variable if present by the orig_file.
      means_sd <- DBI::dbGetQuery(
        db,
        paste0("SELECT AVG(exp) mean_exp, STDDEV(exp) sd_exp, AVG(dist) mean_dist, STDDEV(dist) sd_dist, orig_file
                FROM ", farmername, "_r.temp GROUP BY temp.orig_file;")
      )
      means_sd$max_exp <- means_sd$mean_exp + (5 * means_sd$sd_exp)
      means_sd$max_dist <- means_sd$mean_dist + (5 * means_sd$sd_dist)
      means_sd$min_exp <- ifelse((means_sd$mean_exp - (5 * means_sd$sd_exp)) < 0,
                                 0, (means_sd$mean_exp - (5 * means_sd$sd_exp)))
      means_sd$min_dist <- ifelse((means_sd$mean_dist - (5 * means_sd$sd_dist)) < 0,
                                  0, (means_sd$mean_dist - (5 * means_sd$sd_dist)))
      for (i in 1:nrow(means_sd)) {
        if (is.na(means_sd[i, "mean_dist"]) | is.na(means_sd[i, "sd_dist"])) {
          means_sd[i, "max_dist"] <- 10000
          means_sd[i, "min_dist"] <- 0
        }
        invisible(DBI::dbSendQuery(
            db,
            paste0("UPDATE ", farmername, "_r.temp temp
                    SET max_exp = ", means_sd[i, "max_exp"], "
                    WHERE temp.orig_file = '", means_sd[i, "orig_file"], "';
                    UPDATE ", farmername, "_r.temp temp
                    SET min_exp = ", means_sd[i, "min_exp"], "
                    WHERE temp.orig_file = '", means_sd[i, "orig_file"], "';
                    UPDATE ", farmername, "_r.temp temp
                    SET max_dist = ", means_sd[i, "max_dist"], "
                    WHERE temp.orig_file = '", means_sd[i, "orig_file"], "';
                    UPDATE ", farmername, "_r.temp temp
                    SET min_dist = ", means_sd[i, "min_dist"], "
                    WHERE temp.orig_file = '", means_sd[i, "orig_file"], "';")
        ))
      }
      ## Remove observations by orig_file above or below 5SD of the mean experimental variable or distance
      invisible(DBI::dbSendQuery(
          db,
          paste0("DELETE FROM ", farmername, "_r.temp
                  WHERE exp >= max_exp
                  OR exp <= min_exp
                  OR dist >= max_dist
                  OR dist <= min_dist;")
      ))
      invisible(DBI::dbSendQuery(
          db,
          paste0("ALTER TABLE ", farmername, "_r.temp
                  DROP COLUMN dist,
                  DROP COLUMN max_exp,
                  DROP COLUMN min_exp,
                  DROP COLUMN max_dist,
                  DROP COLUMN min_dist;")
      ))
      invisible(
        DBI::dbSendQuery(db, paste0("VACUUM ANALYZE ", farmername, "_r.temp"))
      )
      utm_epsg <- OFPE::findUTMzone(db, fieldname = fieldname[1])
      for (i in 1:nrow(exp_files)) {
        if (!is.na(exp_conv[i, "conversion"])) {
          invisible(DBI::dbSendQuery(
              db,
              paste0("UPDATE ", farmername, "_r.temp temp
                      SET exp = exp * ", exp_conv[i, "conversion"], "
                      WHERE orig_file = '", exp_conv[i, "orig_file"], "'")
          ))
        }
      }
      newcol <- ifelse(CY, expvar, paste0("prev_", expvar))
      if (!grepl("MULTIPOLYGON", is_poly)) {
        ## if not polygon
        # Get the cell_id for each point in the temporary table
        invisible(DBI::dbSendQuery(
            db,
            paste0("ALTER TABLE ", farmername, "_r.temp
                    ADD COLUMN cell_id VARCHAR;
                    ALTER TABLE ", farmername, "_r.temp
                    ALTER COLUMN geometry TYPE geometry(POINT, ", utm_epsg, ")
                    USING ST_Transform(geometry, ", utm_epsg, ");
                    UPDATE ", farmername, "_r.temp temp
                    SET cell_id = gridtemp.cell_id
                    FROM all_farms.gridtemp
                    WHERE ST_Within(temp.geometry, gridtemp.geom);")
        ))
        # Get the mean and 1SD of the experimental variable for all cell_ids and remove rows with outliers
        means_sd <- DBI::dbGetQuery(
          db,
          paste0("SELECT AVG(exp) mean_exp, STDDEV(exp) sd_exp, cell_id
                  FROM ", farmername, "_r.temp
                  GROUP BY temp.cell_id;")
        )
        means_sd$sd_exp[is.na(means_sd$sd_exp)|
                          means_sd$sd_exp == 0] <- 10
        means_sd$max_exp <- means_sd$mean_exp + means_sd$sd_exp
        means_sd$min_exp <- ifelse((means_sd$mean_exp - means_sd$sd_exp) < 0,
                                   0, (means_sd$mean_exp - means_sd$sd_exp))
        invisible(DBI::dbWriteTable(
            db,
            c(paste0(farmername, "_r"), "means"),
            means_sd,
            row.names = FALSE
        ))
        invisible(DBI::dbSendQuery(
            db,
            paste0("ALTER TABLE ", farmername, "_r.temp
                    ADD COLUMN max_exp REAL,
                    ADD COLUMN min_exp REAL;")
        ))
        invisible(DBI::dbSendQuery(
            db,
            paste0("UPDATE ", farmername, "_r.temp temp
                    SET max_exp = means.max_exp
                    FROM ", farmername, "_r.means means
                    WHERE temp.cell_id = means.cell_id;
                    UPDATE ", farmername, "_r.temp temp
                    SET min_exp = means.min_exp
                    FROM ", farmername, "_r.means means
                    WHERE temp.cell_id = means.cell_id;")
        ))
        invisible(DBI::dbSendQuery(
            db,
            paste0("DELETE FROM ", farmername, "_r.temp
                    WHERE exp >= max_exp
                    OR exp <= min_exp;")
        ))
        invisible(
          DBI::dbSendQuery(db, paste0("DROP TABLE ", farmername, "_r.means"))
        )
      } else {
        invisible(DBI::dbSendQuery(
            db,
            paste0("ALTER TABLE ", farmername, "_r.temp
                    ALTER COLUMN geometry TYPE geometry(",
                    as.character(is_poly),
                    ", ", utm_epsg, ")
                    USING ST_Transform(geometry, ", utm_epsg, ");")
        ))
      }

      invisible(
        DBI::dbSendQuery(db, paste0("VACUUM ANALYZE ", farmername, "_r.temp"))
      )
    },
    #' @description
    #' Method for aggregating the response variable data in the database. Uses
    #' the output of the .cleanExpData method to add the experimental variable data
    #' to the aggregated table in the database. Assumes current year is true, set
    #' to false to create "prev_EXPVAR" col. A set of points from the grid created
    #' across the fields that the aggregation data is based on is recreated
    #' and clipped to the bounding box of the experimental raw data. The raw experimental
    #' data is then extracted to the grid centroids where missing values are filled with
    #' NULL. This is a difference compared to the previous approach that did this after
    #' aggregation, where some missing values should be missing (field edges, partial files,
    #' etc.) and fills in the gaps within observations (real zeroes where the machine
    #' stopped applying the inptu) are filled with zeroes more reliably. This data is then
    #' used when aggregating experimental data. The The dot indicates that this function
    #' would be private if not for documentations sake.
    #' @param db Connection to an OFPE database.
    #' @param farmername Name of the farmer that owns the field for aggregation.
    #' @param expvar The experimental variable to aggregate.
    #' @param GRID Whether to aggregate data to the centroids of a grid or use
    #' the raw observed data locations.
    #' @param CY Logical, whether the data for aggregation is from the year of
    #' interest (aka the 'current year').
    #' @param fieldname Name of the field for aggregation.
    #' @param size Size of grid to make (meters), default = 10.
    #' @param exp_files The experimental variable files for aggregating.
    #' @return Data in temporary aggregated table.
    .aggExpData = function(db,
                           farmername,
                           expvar,
                           GRID,
                           CY = TRUE,
                           fieldname,
                           size,
                           exp_files) {
      newcol <- ifelse(CY, expvar, paste0("prev_", expvar))
      utm_epsg <- OFPE::findUTMzone(db, fieldname = fieldname[1])
      is_poly <- ifelse(any(grepl("poly", exp_files$table)),
                        "MULTIPOLYGON",
                        "POINT")
      invisible(DBI::dbSendQuery(
          db,
          paste0("CREATE TABLE ", farmername, "_a.exp_grid AS
                    SELECT *
                    FROM all_farms.gridtemp gridtemp
                    WHERE gridtemp.field = '", fieldname, "'
                    AND gridtemp.size = ", size, ";
                    ALTER TABLE ", farmername, "_a.exp_grid
                    DROP COLUMN row,
                    DROP COLUMN col,
                    ADD COLUMN grid VARCHAR,
                    ADD COLUMN farmer VARCHAR;
                    UPDATE ", farmername, "_a.exp_grid SET
                    grid = '", GRID, "',
                    farmer = '", farmername, "';
                    ALTER TABLE ", farmername, "_a.exp_grid
                    ALTER COLUMN geom TYPE geometry(Point, ", utm_epsg, ")
                    USING ST_Centroid(geom);
                    ALTER TABLE ", farmername, "_a.exp_grid
                    RENAME COLUMN geom TO geometry;")
      ))
      invisible(DBI::dbSendQuery(
          db,
          paste0("CREATE INDEX exp_grid_geom_idx ON ",
                 farmername,
                 "_a.exp_grid USING gist (geometry)")
      ))
      invisible(
        DBI::dbSendQuery(db, paste0("VACUUM ANALYZE ", farmername, "_a.exp_grid"))
      )
      invisible(DBI::dbSendQuery(
          db,
          paste0("CREATE TABLE ", farmername,"_a.exp_box AS
          (SELECT ST_SetSRID(ST_Extent(temp.geometry), ", utm_epsg,") AS
          geometry FROM ", farmername,"_r.temp temp);")
      ))
      invisible(DBI::dbSendQuery(
          db,
          paste0("ALTER TABLE ", farmername, "_a.exp_grid
                 ADD COLUMN id SERIAL;
                 DELETE FROM ", farmername, "_a.exp_grid AS exp_grid
                 WHERE exp_grid.id IN (
                 SELECT a.id
                 FROM ", farmername, "_a.exp_grid a, (
                 SELECT ST_Union(geometry) As geometry
                 FROM ", farmername, "_a.exp_box
                 ) b
                 WHERE NOT ST_Within(a.geometry, b.geometry)
                 );
                 ALTER TABLE ", farmername, "_a.exp_grid
                 DROP COLUMN id;")
      ))
      # buff exp_grid within 5m of boundary?

      if (grepl("MULTIPOLYGON", is_poly)) {
        ## if polygon
        invisible(DBI::dbSendQuery(
            db,
            paste0("ALTER TABLE ", farmername, "_a.exp_grid
                    ADD COLUMN exp REAL;
                    ALTER TABLE ", farmername, "_r.temp
                    ALTER COLUMN geometry TYPE geometry(",
                    as.character(is_poly), ", ", utm_epsg, ")
                    USING ST_Transform(geometry, ", utm_epsg, ");
                    UPDATE ", farmername, "_a.exp_grid aggexp_grid
                    SET exp = temp.exp
                    FROM ", farmername, "_r.temp temp
                    WHERE ST_Within(aggexp_grid.geometry, temp.geometry);")
        ))
      } else {
        if (GRID == "grid") {
          # if gridded option get average from cell id
          # Add the mean experimental var to the aggregated table by cell_id
          invisible(DBI::dbSendQuery(
              db,
              paste0("ALTER TABLE ", farmername, "_a.exp_grid
                      ADD COLUMN exp REAL;

                      WITH vtemp AS (
                      SELECT
                        b.cell_id,
                        to_char(
                          AVG (b.exp),
                          '9999999999999999999'
                        ) AS exp
                      FROM ", farmername, "_r.temp b
                      INNER JOIN ", farmername, "_a.exp_grid a ON a.cell_id = b.cell_id
                      GROUP BY  b.cell_id
                      )

                      UPDATE ", farmername, "_a.exp_grid aggexp_grid
                      SET exp = CAST ( vtemp.exp AS REAL )
                      FROM vtemp
                      WHERE aggexp_grid.cell_id = vtemp.cell_id;")
          ))
        } else {
          # if obs get nearest
          invisible(DBI::dbSendQuery(
              db,
              paste0("ALTER TABLE ", farmername, "_a.exp_grid
                      ADD COLUMN exp REAL;

                      WITH vtemp AS (
                        SELECT a.cell_id,
                        b.exp,
                        b.dist,
                        a.x,
                        a.y,
                        a.geometry
                        FROM ", farmername, "_a.exp_grid a
                        JOIN LATERAL (
                          SELECT exp,
                          ST_Distance(temp.geometry, a.geometry) as dist
                          FROM ", farmername, "_r.temp temp
                          ORDER BY a.geometry <-> temp.geometry
                          LIMIT 1
                        ) AS b
                        ON true
                      )

                      UPDATE ", farmername, "_a.exp_grid a
                      SET exp = vtemp.exp
                      FROM vtemp
                      WHERE vtemp.cell_id = a.cell_id
                      AND vtemp.x = a.x
                      AND vtemp.y = a.y
                      AND vtemp.dist < 3.25;")
          ))
        }
      }
      invisible(DBI::dbSendQuery(
          db,
          paste0("UPDATE ", farmername, "_a.exp_grid
               SET exp = 0
               WHERE exp IS NULL;")
      ))
      if (GRID == "grid") {
        # if gridded option get average from cell id
        # Add the mean experimental var to the aggregated table by cell_id
        invisible(DBI::dbSendQuery(
            db,
            paste0("ALTER TABLE ", farmername, "_a.temp
                      ADD COLUMN ", newcol, " REAL;

                      WITH vtemp AS (
                      SELECT
                        b.cell_id,
                        to_char(
                          AVG (b.exp),
                          '9999999999999999999'
                        ) AS exp
                      FROM ", farmername, "_a.exp_grid b
                      INNER JOIN ", farmername, "_a.temp a ON a.cell_id = b.cell_id
                      GROUP BY  b.cell_id
                      )

                      UPDATE ", farmername, "_a.temp aggtemp
                      SET ", newcol, " = CAST ( vtemp.exp AS REAL )
                      FROM vtemp
                      WHERE aggtemp.cell_id = vtemp.cell_id;")
        ))
      } else {
        # if obs get nearest
        invisible(DBI::dbSendQuery(
            db,
            paste0("ALTER TABLE ", farmername, "_a.temp
                      ADD COLUMN ", newcol, " REAL;

                      WITH vtemp AS (
                        SELECT a.cell_id,
                        b.exp,
                        b.dist,
                        a.x,
                        a.y,
                        a.geometry
                        FROM ", farmername, "_a.temp a
                        JOIN LATERAL (
                          SELECT exp,
                          ST_Distance(temp.geometry, a.geometry) as dist
                          FROM ", farmername, "_a.exp_grid temp
                          ORDER BY a.geometry <-> temp.geometry
                          LIMIT 1
                        ) AS b
                        ON true
                      )

                      UPDATE ", farmername, "_a.temp a
                      SET ", newcol, " = vtemp.exp
                      FROM vtemp
                      WHERE vtemp.cell_id = a.cell_id
                      AND vtemp.x = a.x
                      AND vtemp.y = a.y
                      AND vtemp.dist < 10;")
        ))
      }
      invisible(
        DBI::dbSendQuery(db, paste0("DROP TABLE ", farmername, "_a.exp_box"))
      )
      invisible(
        DBI::dbSendQuery(db, paste0("DROP TABLE ", farmername, "_a.exp_grid"))
      )
      invisible(
        DBI::dbSendQuery(db, paste0("DROP TABLE ", farmername, "_r.temp"))
      )
      invisible(
        DBI::dbSendQuery(db, paste0("VACUUM ANALYZE ", farmername, "_a.temp"))
      )
    },
    #' @description
    #' Clip aggregated data to the field boundary. Put the aggregated data
    #' into long lat for raster extraction. Clip the aggregated dataset to
    #' the field boundary for faster processing. If experimental rates are
    #' null convert to zero because the sprayer does not record rather than
    #' recording zeroes. The dot indicates that this function would be private
    #' if not for documentations sake. Put the aggregated data into long lat
    #' for raster extraction. Clip the aggregated dataset to the field boundary
    #' for faster processing. If experimental rates are null convert to zero
    #' because the sprayer does not record rather than recording zeroes.
    #' Also, this function
    #' @param None No arguments needed because of class instantiation.
    #' @return None.
    .clipAggDat = function() {
      utm_epsg <- OFPE::findUTMzone(self$aggInputs$dbCon$db,
                                    fieldname = self$aggInputs$fieldname[1])
      ## clip the aggregated data to the field boundary
      invisible(DBI::dbSendQuery(
          self$aggInputs$dbCon$db,
          paste0("ALTER TABLE ", self$aggInputs$farmername, "_a.temp
                 ALTER COLUMN geometry
                 TYPE geometry(POINT, ", utm_epsg, ")
                 USING ST_Force2D(geometry);
                 ALTER TABLE ", self$aggInputs$farmername, "_a.temp
                 ALTER COLUMN geometry TYPE geometry(POINT, 4326)
                 USING ST_Transform(geometry, 4326);")
      ))
      invisible(DBI::dbSendQuery(
          self$aggInputs$dbCon$db,
          paste0("ALTER TABLE ", self$aggInputs$farmername, "_a.temp
                 ADD COLUMN id SERIAL;
                 DELETE FROM ", self$aggInputs$farmername, "_a.temp AS temp
                 WHERE temp.id IN (
                 SELECT a.id
                 FROM ", self$aggInputs$farmername, "_a.temp a, (
                 SELECT ST_Union(geometry) As geometry
                 FROM all_farms.temp
                 ) b
                 WHERE NOT ST_Within(a.geometry, b.geometry)
                 );
                 ALTER TABLE ", self$aggInputs$farmername, "_a.temp
                 DROP COLUMN id;")
      ))
      # 30m buffer
      invisible(DBI::dbSendQuery(
          self$aggInputs$dbCon$db,
          paste0("ALTER TABLE ", self$aggInputs$farmername, "_a.temp
                 ADD COLUMN id SERIAL;

                 WITH buff AS(
                 SELECT ST_Buffer(temp.geometry, -0.0002727273) AS geometry
                 FROM all_farms.temp temp)
                 DELETE FROM ", self$aggInputs$farmername, "_a.temp AS temp
                 WHERE temp.id IN (
                 SELECT a.id
                 FROM ", self$aggInputs$farmername, "_a.temp a, (
                 SELECT ST_Union(geometry) As geometry
                 FROM buff
                 ) b
                 WHERE NOT ST_Within(a.geometry, b.geometry)
                 );
                 ALTER TABLE ", self$aggInputs$farmername, "_a.temp
                 DROP COLUMN id;")
      ))
      # set NULL values to 0
      # remove 0 rates from seed data
      if (self$aggInputs$respvar != "sat") {
        if (self$aggInputs$expvar == "aa_sr") {
          invisible(DBI::dbSendQuery(
              self$aggInputs$dbCon$db,
              paste0("DELETE FROM ",
                     self$aggInputs$farmername, "_a.temp temp
                      WHERE temp.aa_sr = 0;
                      OR temp.prev_aa_sr = 0;")
          ))
        }
      }
    },
    #' @description
    #' Gather SSURGO soils data if present in the database. Extract the
    #' MUSYM attribute. The dot indicates that this function would be private
    #' if not for documentations sake.
    #' @param None No arguments needed because of class instantiation.
    #' @return None.
    .aggSSURGO = function() {
      if (!length(self$farmidx) == 0) {
        ssurgoExist <- as.logical(DBI::dbGetQuery(
            self$aggInputs$dbCon$db,
            paste0("SELECT EXISTS (
             SELECT 1
             FROM   information_schema.tables
             WHERE  table_schema = 'all_farms'
             AND table_name = 'ssurgo')")
        ))
        if (ssurgoExist) {
          fileExist <- as.character(DBI::dbGetQuery(
              self$aggInputs$dbCon$db,
              paste0("SELECT DISTINCT orig_file
                    FROM all_farms.ssurgo
                    WHERE farmidx = '", self$farmidx, "'")
          ))
          if (length(fileExist) != 0) { # if a file exists
            invisible(DBI::dbSendQuery(
                self$aggInputs$dbCon$db,
                paste0("CREATE TABLE all_farms.ssurgo_temp AS
                      SELECT *
                      FROM all_farms.ssurgo
                      WHERE farmidx = '", self$farmidx, "'")
            ))
            invisible(DBI::dbSendQuery(
                self$aggInputs$dbCon$db,
                paste0("UPDATE ", self$aggInputs$farmername, "_a.temp aggtemp
                      SET musym = ssurgo_temp.musym
                      FROM all_farms.ssurgo_temp ssurgo_temp
                      WHERE ST_Within(aggtemp.geometry, ssurgo_temp.geometry);")
            ))
            invisible(DBI::dbSendQuery(
                self$aggInputs$dbCon$db,
                paste0("DROP TABLE all_farms.ssurgo_temp;")
            ))
          } # end if fileExist
        }
      } # end if farmidx
    },
    #' @description
    #' Save the temporary aggregated table to the appropriate table in
    #' the OFPE database. Puts the data in the correct farmer's aggregated
    #' schema in the table corresponding to the response variable (respvar).
    #' If using a field boundary from the database and the user selected to
    #' save the data to the database, the aggregated file is added to the
    #' aggregated schema for the variable type (yld, pro, sat). Sat = data
    #' aggregated without response or explanatory variables. This includes
    #' only data collected from remote sensing sources or SSURGO, and are
    #' aggregated to the center points of the gridcells. The dot indicates
    #' that this function would be private if not for documentations sake.
    #' @param None No arguments needed because of class instantiation.
    #' @return Aggregated data in the OFPE database.
    .saveAggDat = function() {
      aggExist <- as.logical(DBI::dbGetQuery(
          self$aggInputs$dbCon$db,
          paste0("SELECT EXISTS (
                  SELECT 1
                  FROM   information_schema.tables
                  WHERE  table_schema = '", self$aggInputs$farmername, "_a'
                  AND    table_name = '", self$aggInputs$respvar, "')")
      ))
      if (!aggExist) {
        ## if aggregated table doesn't exist make it
        invisible(DBI::dbSendQuery(
            self$aggInputs$dbCon$db,
            paste0("CREATE TABLE ", self$aggInputs$farmername, "_a.", self$aggInputs$respvar, " AS
                    SELECT * FROM ", self$aggInputs$farmername, "_a.temp;")
        ))
      } else {
        ## get column names of agg table
        # db_cols <- sf::st_read(
        #   self$aggInputs$dbCon$db,
        #   query = paste0("SELECT *
        #                   FROM ", self$aggInputs$farmername, "_a.", self$aggInputs$respvar, "
        #                   LIMIT 1"),
        #   geometry_column = "geometry") %>%
        #   as.data.frame()
        # db_cols$geometry <- NULL
        # db_cols <- paste(c("geometry" ,names(db_cols)), collapse=", ")
        ## else append to it

        invisible(DBI::dbSendQuery(
            self$aggInputs$dbCon$db,
            paste0("DELETE FROM ", self$aggInputs$farmername, "_a.", self$aggInputs$respvar, "
                    WHERE field = '", self$aggInputs$fieldname, "'
                    AND size = ", ifelse(is.null(self$aggInputs$size), 10, self$aggInputs$size), "
                    AND grid = '", self$aggInputs$GRID, "'
                    AND year = '", self$aggInputs$cy_resp, "'
                    AND datused = '", self$aggInputs$dat_used, "';")
        ))
        temp_dat <- sf::st_read(
          self$aggInputs$dbCon$db,
          query = paste0("SELECT * FROM ",
                         self$aggInputs$farmername, "_a.temp;")) %>%
          sf::`st_crs<-`(4326) %>%
          sf::st_transform("epsg:4326")
        sf::st_write(temp_dat,
                     self$aggInputs$dbCon$db,
                     c(paste0(self$aggInputs$farmername, "_a"),
                       self$aggInputs$respvar),
                     layer_options = "OVERWRITE=false",
                     append = TRUE)

        # DBI::dbSendQuery(
        #   self$aggInputs$dbCon$db,
        #   paste0("INSERT INTO ", self$aggInputs$farmername, "_a.", self$aggInputs$respvar, "
        #             SELECT ", db_cols, "
        #             FROM ", self$aggInputs$farmername, "_a.temp;"))
      }
      invisible(DBI::dbSendQuery(
          self$aggInputs$dbCon$db,
          paste0("VACUUM ANALYZE ",
                 self$aggInputs$farmername, "_a.", self$aggInputs$respvar, ";")
      ))
    },
    #' @description
    #' Export the aggregated data to the local file system. Uses the user
    #' inputs for a path to the data. The dot indicates that this function
    #' would be private if not for documentations sake.
    #' @param None No arguments needed because of class instantiation.
    #' @return Exported data from the OFPE database.
    .exportAggDat = function() {
      stopifnot(!is.null(self$aggInputs$export_name))
      ## drop column geometry from aggregated table
      invisible(DBI::dbSendQuery(
          self$aggInputs$dbCon$db,
          paste0("ALTER TABLE ",
                 self$aggInputs$farmername, "_a.temp
                 DROP COLUMN geometry;")
      ))
      dat <- sf::st_read(
        self$aggInputs$dbCon$db,
        query = paste0("SELECT * FROM ", self$aggInputs$farmername, "_a.temp")) %>%
        as.data.frame() %<%
        sf::st_write(paste0(self$inputs$export_name))
    }
  ),
  private = list(
    .idFarm = function() {
      if (self$aggInputs$respvar != "sat") {
        self$farmidx <- as.integer(DBI::dbGetQuery(
          self$aggInputs$dbCon$db,
          paste0("SELECT DISTINCT farmidx
                 FROM  all_farms.farms farms
                 JOIN ", self$aggInputs$farmername, "_a.temp temp
                 ON ST_Within(temp.geometry, farms.geom);")
        ))
      } else {
        self$farmidx <- as.integer(DBI::dbGetQuery(
          self$aggInputs$dbCon$db,
          paste0("SELECT DISTINCT farmidx
                 FROM  all_farms.fields
                 WHERE fieldname = '", self$aggInputs$fieldname, "';")
        ))
      }
      self$farmeridx <- as.integer(DBI::dbGetQuery(
        self$aggInputs$dbCon$db,
        paste0("SELECT DISTINCT farmeridx
                 FROM  all_farms.farms farms
                 WHERE farms.farmidx = '", self$farmidx, "';")
      ))
    }
  )
)









