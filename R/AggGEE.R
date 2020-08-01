#' @title R6 Class for aggregating GEE data
#'
#' @description R6 Class for executing the OFPE data aggregation process that
#' consolidates data from Google Earth Engine to on-farm data. This is used
#' within the 'AggDat' R6 class.
#' @export
AggGEE <- R6::R6Class(
  "AggGEE",
  public = list(
    #' @field aggInputs An object of the 'AggInputs' class containaing the options
    #' for aggregating data. This includes the database connection.
    aggInputs = NULL,
    #' @field FARMIDX The ID of the farm in which the field falls within.
    FARMIDX = NULL,
    #' @field FARMERIDX The ID of the farmer that owns the farm that the field
    #' falls within.
    FARMERIDX = NULL,
    #' @field PY The year prior to the selected year of interest (CY_RESP).
    #' This differs from the previous crop year used prior.
    PY = NULL,
    #' @field PY2 The year two years prior to the selected year of interest
    #' (CY_RESP).
    PY2 = NULL,
    #' @field labels The labels of the GEE covariates that are collected and
    #' aggregated.
    labels = NULL,
    #' @field type The type of data corresponding to each respective label.
    type = NULL,
    #' @field source The source of data corresponding to each respective label.
    source = NULL,
    #' @field year The year of data to gather,  corresponding to each respective
    #' label.
    year = NULL,
    #' @field loy The length of year for which to gather data for each respective
    #' label.
    loy = NULL,

    #' @description
    #' Initialize an object for executing the process of aggregating
    #' Google Earth Engine data to OFPE data. Adds necessary columns to
    #' the temporary aggregated table,  sets up the necessary options for
    #' gathering and extracting GEE data.
    #' @param aggInputs An 'AggInputs' R6 class with the user's aggregation
    #' options.
    #' @param FARMIDX The ID of the farm in which the field falls within.
    #' @param FARMERIDX The ID of the farmer that owns the farm that the field
    #' falls within.
    #' @return An initialized 'AggGEE' object.
    initialize = function(aggInputs, FARMIDX, FARMERIDX) {
      self$aggInputs <- aggInputs
      self$FARMIDX <- FARMIDX
      self$FARMERIDX <- FARMERIDX


      invisible(
        DBI::dbSendQuery(
          self$aggInputs$dbCon$db,
          paste0("ALTER TABLE ", self$aggInputs$FARMERNAME, "_a.temp
                  ADD COLUMN aspect_rad REAL,
                  ADD COLUMN slope REAL,
                  ADD COLUMN elev REAL,
                  ADD COLUMN tpi REAL,
                  ADD COLUMN prec_cy_d REAL,
                  ADD COLUMN prec_py_d REAL,
                  ADD COLUMN gdd_cy_d REAL,
                  ADD COLUMN gdd_py_d REAL,
                  ADD COLUMN prec_cy_g REAL,
                  ADD COLUMN prec_py_g REAL,
                  ADD COLUMN gdd_cy_g REAL,
                  ADD COLUMN gdd_py_g REAL,
                  ADD COLUMN ndvi_cy_s REAL,
                  ADD COLUMN ndvi_py_s REAL,
                  ADD COLUMN ndvi_2py_s REAL,
                  ADD COLUMN ndvi_cy_l REAL,
                  ADD COLUMN ndvi_py_l REAL,
                  ADD COLUMN ndvi_2py_l REAL,
                  ADD COLUMN ndre_cy REAL,
                  ADD COLUMN ndre_py REAL,
                  ADD COLUMN ndre_2py REAL,
                  ADD COLUMN cire_cy REAL,
                  ADD COLUMN cire_py REAL,
                  ADD COLUMN cire_2py REAL,
                  ADD COLUMN ssm_cy REAL,
                  ADD COLUMN ssm_py REAL,
                  ADD COLUMN susm_cy REAL,
                  ADD COLUMN susm_py REAL,
                  ADD COLUMN musym VARCHAR;")
        )
      )
      invisible(
        DBI::dbSendQuery(
          self$aggInputs$dbCon$db,
          paste0("VACUUM ANALYZE ",
                 self$aggInputs$FARMERNAME, "_a.temp")
        )
      )

      self$PY <-  as.character(as.numeric(self$aggInputs$CY_RESP) - 1 )
      self$PY2 <-  as.character(as.numeric(self$aggInputs$CY_RESP) - 2 )
      self$labels <- c("aspect_rad", "slope", "elev", "tpi",
                  "prec_cy_d", "prec_py_d", "gdd_cy_d", "gdd_py_d",
                  "prec_cy_g", "prec_py_g", "gdd_cy_g", "gdd_py_g",
                  "ndvi_cy_s", "ndvi_py_s", "ndvi_2py_s",
                  "ndvi_cy_l", "ndvi_py_l", "ndvi_2py_l",
                  "ndre_cy", "ndre_py", "ndre_2py",
                  "cire_cy", "cire_py", "cire_2py",
                  "ssm_cy", "ssm_py", "susm_cy", "susm_py")
      self$type <- c("aspect_rad", "slope", "elev", "tpi",
                rep(("prec"), 2), rep("gdd", 2), rep("prec", 2),
                rep("gdd", 2), rep("ndvi", 6), rep("ndre", 3),
                rep("cire", 3), rep("ssm", 2), rep("susm", 2))
      self$source <- c(ifelse(self$aggInputs$FARMERNAME == "loewen", "srtm", "ned"),
                  ifelse(self$aggInputs$FARMERNAME == "loewen", "srtm", "ned"),
                  ifelse(self$aggInputs$FARMERNAME == "loewen", "cdem", "ned"),
                  ifelse(self$aggInputs$FARMERNAME == "loewen", "srtm", "ned"),
                  rep("daymet", 4), rep("gridmet", 4), rep("S2", 3),
                  ifelse(self$aggInputs$CY_RESP >= 2014, "L8",
                         ifelse(self$aggInputs$CY_RESP == 2013, "L7", "L5")),
                  rep(ifelse(self$PY >= 2013, "L8",
                           ifelse(self$PY == 2012, "L7", "L5")), 2),
                  rep("S2", 6), rep("smap", 4))
      self$year <- c(rep("2015", 4), rep(c(self$aggInputs$CY_RESP, self$PY), 4),
                rep(c(self$aggInputs$CY_RESP, self$PY, self$PY2), 4),
                rep(c(self$aggInputs$CY_RESP, self$PY), 2))
      if (self$aggInputs$DAT_USED == "decision_point") {
        self$loy <- c(rep("full", 4),
                 rep(c("mar", "full"), 4),
                 rep(c("mar", "full", "full"), 4),
                 rep(c("mar", "full"), 2))
      } else {
        self$loy <- rep("full", length(self$labels))
      }
    },
    #' @description
    #' Method for executing the aggregation of GEE data to the on-farm data
    #' within the OFPE database.
    #' @param None No argumetns necessary because identified in class
    #' instantiation.
    #' @return GEE data aggregated to the temporary aggregated table
    #' in the database.
    aggregateGEE = function() {
      if (!length(self$FARMIDX) == 0) {
        invisible(
          mapply(
            self$.getGEEdata,
            self$labels,
            self$type,
            self$source,
            self$year,
            self$loy,
            MoreArgs = list(DB=self$aggInputs$dbCon$db,
                            FARMIDX=self$FARMIDX,
                            FARMERIDX=self$FARMERIDX,
                            FARMERNAME=self$aggInputs$FARMERNAME)
          )
        )
      }
    },
    #' @description
    #' Method for aggregating the Google Earth Engine data to the on-farm
    #' aggregated data. Identifies the appropirate data from the 'all_farms.
    #' gee' schema and extracts the values of each raster to each point in
    #' the field. All labels, type, source etc. are identified in the GEE
    #' data original filenames exported from GEE. The dot indicates that
    #' this function would be private if not for documentations sake.
    #' @param LABEL The labels for the GEE data to aggregate.
    #' @param TYPE The type of data to aggregate.
    #' @param SOURCE The source of the data. (i.e. Landsat vs. Sentinel etc.).
    #' @param YEAR The year of the data to gather.
    #' @param LOY The length of year to get data from (i.e. 'mar' vs. 'full').
    #' @param DB Connection to an OFPE database.
    #' @param FARMIDX ID of the farm that the field falls within.
    #' @param FARMERIDX ID of the farmer that owns the farm that the field
    #' falls within.
    #' @param FARMERNAME Name of the farmer that owns the field for aggregation.
    #' @return Data in temporary aggregated table.
    .getGEEdata = function(LABEL,
                           TYPE,
                           SOURCE,
                           YEAR,
                           LOY,
                           DB,
                           FARMIDX,
                           FARMERIDX,
                           FARMERNAME) {
      ## check if file exists in db
      fileExist <- as.character(
        DBI::dbGetQuery(
          DB,
          paste0("SELECT DISTINCT orig_file
                  FROM all_farms.gee
                  WHERE farmidx = '", FARMIDX, "'
                  AND year = '", YEAR, "'
                  AND type = '", TYPE, "'
                  AND source = '", SOURCE, "'
                  AND loy = '", LOY, "'
                  AND farmeridx = '", FARMERIDX, "';")
        )
      )
      if (length(fileExist) != 0) { # if a file exists
        # make temporary table
        invisible(
          DBI::dbSendQuery(
            DB,
            paste0("CREATE TABLE all_farms.geetemp AS
                    SELECT *
                    FROM all_farms.gee
                    WHERE orig_file = '", fileExist, "'")
          )
        )
        # clip raster
        invisible(
          DBI::dbSendQuery(
            DB,
            paste0("ALTER TABLE all_farms.geetemp
                    ADD COLUMN id SERIAL;

                    DELETE FROM all_farms.geetemp AS geetemp
                    WHERE geetemp.id IN (
                      SELECT a.id
                      FROM all_farms.geetemp a, (
                        SELECT ST_Union(geometry) As geometry FROM all_farms.temp
                      ) b
                    WHERE NOT ST_Intersects(a.rast, b.geometry)
                    );

                    ALTER TABLE all_farms.geetemp
                    DROP COLUMN id;")
          )
        )
        ## below only works if not multipolygon
        #invisible(
        #  dbSendQuery(
        #    DB,
        #    paste0( "WITH temp AS(
        #    SELECT *
        #    FROM all_farms.temp temp)

        #    DELETE FROM all_farms.geetemp AS geetemp
        #    USING temp
        #    WHERE NOT ST_Intersects(temp.geometry, geetemp.rast);")
        #  )
        #)
        # extract values
        invisible(
          DBI::dbSendQuery(
            DB,
            paste0("WITH temp AS (SELECT * FROM ", FARMERNAME, "_a.temp)
                    UPDATE ", FARMERNAME, "_a.temp
                    SET ", LABEL, " = ST_Value(geetemp.rast, temp.geometry)
                    FROM all_farms.geetemp
                    WHERE ST_Intersects(geetemp.rast, temp.geometry);")
          )
        )
        invisible(
          DBI::dbSendQuery(DB, paste0("DROP TABLE all_farms.geetemp;"))
        )
      }
    }
  )
)
