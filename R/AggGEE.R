#' @title R6 Class for aggregating GEE data
#'
#' @description R6 Class for executing the OFPE data aggregation process that
#' consolidates data from Google Earth Engine to on-farm data. This is used
#' within the 'AggDat' R6 class.
#' @seealso \code{\link{DBCon}} for the database connection class,
#' \code{\link{AggDat}} for the class responsible for aggregating on-farm data,
#' \code{\link{AggInputs}} for the inputs required for the aggregation
#' process.
#' @export
AggGEE <- R6::R6Class(
  "AggGEE",
  public = list(
    #' @field aggInputs An object of the 'AggInputs' class containaing the options
    #' for aggregating data. This includes the database connection.
    aggInputs = NULL,
    #' @field farmidx The ID of the farm in which the field falls within.
    farmidx = NULL,
    #' @field farmeridx The ID of the farmer that owns the farm that the field
    #' falls within.
    farmeridx = NULL,
    #' @field PY The year prior to the selected year of interest (cy_resp).
    #' This differs from the previous crop year used prior.
    PY = NULL,
    #' @field PY2 The year two years prior to the selected year of interest
    #' (cy_resp).
    PY2 = NULL,
    #' @field labels The labels of the GEE covariates that are collected and
    #' aggregated.
    labels = NULL,
    #' @field type The type of data corresponding to each respective label.
    type = NULL,
    #' @field SOURCE The SOURCE of data corresponding to each respective label.
    SOURCE = NULL,
    #' @field year The year of data to gather, corresponding to each respective
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
    #' @param farmidx The ID of the farm in which the field falls within.
    #' @param farmeridx The ID of the farmer that owns the farm that the field
    #' falls within.
    #' @return An initialized 'AggGEE' object.
    initialize = function(aggInputs, farmidx, farmeridx) {
      self$aggInputs <- aggInputs
      self$farmidx <- farmidx
      self$farmeridx <- farmeridx
      invisible(
        DBI::dbSendQuery(
          self$aggInputs$dbCon$db,
          paste0("ALTER TABLE ", self$aggInputs$farmername, "_a.temp
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
                  ADD COLUMN grtgroup VARCHAR,
                  ADD COLUMN texture0cm VARCHAR,
                  ADD COLUMN texture10cm VARCHAR,
                  ADD COLUMN texture30cm VARCHAR,
                  ADD COLUMN texture60cm VARCHAR,
                  ADD COLUMN texture100cm VARCHAR,
                  ADD COLUMN texture200cm VARCHAR,
                  ADD COLUMN bulkdensity0cm REAL,
                  ADD COLUMN bulkdensity10cm REAL,
                  ADD COLUMN bulkdensity30cm REAL,
                  ADD COLUMN bulkdensity60cm REAL,
                  ADD COLUMN bulkdensity100cm REAL,
                  ADD COLUMN bulkdensity200cm REAL,
                  ADD COLUMN claycontent0cm REAL,
                  ADD COLUMN claycontent10cm REAL,
                  ADD COLUMN claycontent30cm REAL,
                  ADD COLUMN claycontent60cm REAL,
                  ADD COLUMN claycontent100cm REAL,
                  ADD COLUMN claycontent200cm REAL,
                  ADD COLUMN sandcontent0cm REAL,
                  ADD COLUMN sandcontent10cm REAL,
                  ADD COLUMN sandcontent30cm REAL,
                  ADD COLUMN sandcontent60cm REAL,
                  ADD COLUMN sandcontent100cm REAL,
                  ADD COLUMN sandcontent200cm REAL,
                  ADD COLUMN phw0cm REAL,
                  ADD COLUMN phw10cm REAL,
                  ADD COLUMN phw30cm REAL,
                  ADD COLUMN phw60cm REAL,
                  ADD COLUMN phw100cm REAL,
                  ADD COLUMN phw200cm REAL,
                  ADD COLUMN watercontent0cm REAL,
                  ADD COLUMN watercontent10cm REAL,
                  ADD COLUMN watercontent30cm REAL,
                  ADD COLUMN watercontent60cm REAL,
                  ADD COLUMN watercontent100cm REAL,
                  ADD COLUMN watercontent200cm REAL,
                  ADD COLUMN carboncontent0cm REAL,
                  ADD COLUMN carboncontent10cm REAL,
                  ADD COLUMN carboncontent30cm REAL,
                  ADD COLUMN carboncontent60cm REAL,
                  ADD COLUMN carboncontent100cm REAL,
                  ADD COLUMN carboncontent200cm REAL;")
        )
      )
      invisible(
        DBI::dbSendQuery(
          self$aggInputs$dbCon$db,
          paste0("VACUUM ANALYZE ",
                 self$aggInputs$farmername, "_a.temp")
        )
      )
      self$PY <-  as.character(as.numeric(self$aggInputs$cy_resp) - 1 )
      self$PY2 <-  as.character(as.numeric(self$aggInputs$cy_resp) - 2 )
      self$labels <- c("aspect_rad", "slope", "elev", "tpi",
                  "prec_cy_d", "prec_py_d", "gdd_cy_d", "gdd_py_d",
                  "prec_cy_g", "prec_py_g", "gdd_cy_g", "gdd_py_g",
                  "ndvi_cy_s", "ndvi_py_s", "ndvi_2py_s",
                  "ndvi_cy_l", "ndvi_py_l", "ndvi_2py_l",
                  "ndre_cy", "ndre_py", "ndre_2py",
                  "cire_cy", "cire_py", "cire_2py",
                  "ssm_cy", "ssm_py", "susm_cy", "susm_py",
                  "grtgroup",
                  "texture0cm", "texture10cm", "texture30cm", "texture60cm", "texture100cm", "texture200cm",
                  "bulkdensity0cm", "bulkdensity10cm", "bulkdensity30cm", "bulkdensity60cm", "bulkdensity100cm", "bulkdensity200cm",
                  "claycontent0cm", "claycontent10cm", "claycontent30cm", "claycontent60cm", "claycontent100cm", "claycontent200cm",
                  "sandcontent0cm", "sandcontent10cm", "sandcontent30cm", "sandcontent60cm", "sandcontent100cm", "sandcontent200cm",
                  "phw0cm", "phw10cm", "phw30cm", "phw60cm", "phw100cm", "phw200cm",
                  "watercontent0cm", "watercontent10cm", "watercontent30cm", "watercontent60cm", "watercontent100cm", "watercontent200cm",
                  "carboncontent0cm", "carboncontent10cm", "carboncontent30cm", "carboncontent60cm", "carboncontent100cm", "carboncontent200cm"
                  )
      self$type <- c("aspect_rad", "slope", "elev", "tpi",
                rep(("prec"), 2), rep("gdd", 2), rep("prec", 2),
                rep("gdd", 2), rep("ndvi", 6), rep("ndre", 3),
                rep("clre", 3), rep("ssm", 2), rep("susm", 2),
                "grtgroup",
                "texture0cm", "texture10cm", "texture30cm", "texture60cm", "texture100cm", "texture200cm",
                "bulkdensity0cm", "bulkdensity10cm", "bulkdensity30cm", "bulkdensity60cm", "bulkdensity100cm", "bulkdensity200cm",
                "claycontent0cm", "claycontent10cm", "claycontent30cm", "claycontent60cm", "claycontent100cm", "claycontent200cm",
                "sandcontent0cm", "sandcontent10cm", "sandcontent30cm", "sandcontent60cm", "sandcontent100cm", "sandcontent200cm",
                "phw0cm", "phw10cm", "phw30cm", "phw60cm", "phw100cm", "phw200cm",
                "watercontent0cm", "watercontent10cm", "watercontent30cm", "watercontent60cm", "watercontent100cm", "watercontent200cm",
                "carboncontent0cm", "carboncontent10cm", "carboncontent30cm", "carboncontent60cm", "carboncontent100cm", "carboncontent200cm")
      self$SOURCE <- c(ifelse(self$aggInputs$farmername == "loewen", "srtm", "ned"),
                  ifelse(self$aggInputs$farmername == "loewen", "srtm", "ned"),
                  ifelse(self$aggInputs$farmername == "loewen", "cdem", "ned"),
                  ifelse(self$aggInputs$farmername == "loewen", "srtm", "ned"),
                  rep("daymet", 4), rep("gridmet", 4), rep("S2", 3),
                  ifelse(self$aggInputs$cy_resp >= 2014, "L8",
                         ifelse(self$aggInputs$cy_resp == 2013, "L7", "L5")),
                  rep(ifelse(self$PY >= 2013, "L8",
                           ifelse(self$PY == 2012, "L7", "L5")), 2),
                  rep("S2", 6), rep("smap", 4), rep("olm", 43))
      self$year <- c(rep("2015", 4), rep(c(self$aggInputs$cy_resp, self$PY), 4),
                rep(c(self$aggInputs$cy_resp, self$PY, self$PY2), 4),
                rep(c(self$aggInputs$cy_resp, self$PY), 2), rep("2015", 43))
      if (self$aggInputs$dat_used == "decision_point") {
        self$loy <- c(rep("full", 4),
                 rep(c("mar", "full"), 4),
                 rep(c("mar", "full", "full"), 4),
                 rep(c("mar", "full"), 2), rep("full", 43))
      } else {
        self$loy <- rep("full", length(self$labels))
      }
    },
    #' @description
    #' Method for executing the aggregation of GEE data to the on-farm data
    #' within the OFPE database. First, gather the environmental variables
    #' that do not change year to year. Gather from 2015 to make sure they
    #' are there. Gather Daymet V3 and GRIDMET data, when possible, for
    #' both the current and previous year. Get current and previous year
    #' precipitation and growing degree day data. Gather vegetation index
    #' data from the current, previous, and two years prior. Do this for
    #' NDVI, NDRE, and CIRE indices. Additionally, when applicable, gather
    #' Landsat 8 and Sentinel 2 data. Because Landsat does not have the
    #' band frequencies to calculate NDRE and CIRE these are only available
    #' when Sentinel 2 data is available. If available, gather SMAP data
    #' from the current and previous year. Get the surface soil moisture
    #' and subsurface soil moisture from SMAP.
    #' @param None No arguments necessary because identified in class
    #' instantiation.
    #' @return GEE data aggregated to the temporary aggregated table
    #' in the database.
    aggregateGEE = function() {
      if (!length(self$farmidx) == 0) {
        invisible(
          mapply(
            self$.getGEEdata,
            self$labels,
            self$type,
            self$SOURCE,
            self$year,
            self$loy,
            MoreArgs = list(db=self$aggInputs$dbCon$db,
                            farmidx=self$farmidx,
                            farmeridx=self$farmeridx,
                            farmername=self$aggInputs$farmername)
          )
        )
      }
    },
    #' @description
    #' Method for aggregating the Google Earth Engine data to the on-farm
    #' aggregated data. Identifies the appropriate data from the 'all_farms.
    #' gee' schema and extracts the values of each raster to each point in
    #' the field. All labels, type, SOURCE etc. are identified in the GEE
    #' data original filenames exported from GEE. The dot indicates that
    #' this function would be private if not for documentations sake.
    #' @param label The labels for the GEE data to aggregate.
    #' @param type The type of data to aggregate.
    #' @param SOURCE The SOURCE of the data. (i.e. Landsat vs. Sentinel etc.).
    #' @param year The year of the data to gather.
    #' @param loy The length of year to get data from (i.e. 'mar' vs. 'full').
    #' @param db Connection to an OFPE database.
    #' @param farmidx ID of the farm that the field falls within.
    #' @param farmeridx ID of the farmer that owns the farm that the field
    #' falls within.
    #' @param farmername Name of the farmer that owns the field for aggregation.
    #' @return Data in temporary aggregated table.
    .getGEEdata = function(label,
                           type,
                           SOURCE,
                           year,
                           loy,
                           db,
                           farmidx,
                           farmeridx,
                           farmername) {
      ## check if file exists in db
      file_exist <- as.character(
        DBI::dbGetQuery(
          db,
          paste0("SELECT DISTINCT orig_file
                  FROM all_farms.gee
                  WHERE farmidx = '", farmidx, "'
                  AND year = '", year, "'
                  AND type = '", type, "'
                  AND SOURCE = '", SOURCE, "'
                  AND loy = '", loy, "'
                  AND farmeridx = '", farmeridx, "';")
        )
      )
      if (length(file_exist) != 0) { # if a file exists
        # make temporary table
        invisible(
          DBI::dbSendQuery(
            db,
            paste0("CREATE TABLE all_farms.geetemp AS
                    SELECT *
                    FROM all_farms.gee
                    WHERE orig_file = '", file_exist, "'")
          )
        )
        # clip raster
        invisible(
          DBI::dbSendQuery(
            db,
            paste0("ALTER TABLE all_farms.geetemp
                    ADD COLUMN id SERIAL;")
          )
        )
        invisible(
          DBI::dbSendQuery(
            db,
            paste0("DELETE FROM all_farms.geetemp AS geetemp
                    WHERE geetemp.id IN (
                      SELECT a.id
                      FROM all_farms.geetemp a, (
                        SELECT ST_Union(geometry) As geometry FROM all_farms.temp
                      ) b
                    WHERE NOT ST_Intersects(a.rast, b.geometry)
                    );")
          )
        )
        invisible(
          DBI::dbSendQuery(
            db,
            paste0("ALTER TABLE all_farms.geetemp
                    DROP COLUMN id;")
          )
        )
        # extract values
        invisible(
          DBI::dbSendQuery(
            db,
            paste0("WITH temp AS (SELECT * FROM ", farmername, "_a.temp)
                    UPDATE ", farmername, "_a.temp
                    SET ", label, " = ST_Value(geetemp.rast, temp.geometry)
                    FROM all_farms.geetemp
                    WHERE ST_Intersects(geetemp.rast, temp.geometry);")
          )
        )
        if (grepl("phw", type)) {
          invisible(
            DBI::dbSendQuery(
              db,
              paste0("UPDATE ", farmername, "_a.temp
                    SET ", label, " = ", label, " / 10;")
            )
          )
        }
        if (type == "aspect_rad") {
          invisible(DBI::dbSendQuery(
            db,
            paste0("ALTER TABLE ", farmername, "_a.temp
               ADD COLUMN aspect_cos REAL,
               ADD COLUMN aspect_sin REAL;")
          ))
          aspect_vec <- DBI::dbGetQuery(
            db,
            paste0("SELECT aspect_rad
                FROM ", farmername, "_a.temp;")
          )
          aspect <- data.frame(aspect_rad = aspect_vec)
          aspect$aspect_cos <- cos(aspect$aspect_rad)
          aspect$aspect_sin <- sin(aspect$aspect_rad)
          invisible(DBI::dbSendQuery(
            db,
            paste0("UPDATE ", farmername, "_a.temp temp
                  SET aspect_cos = cos(temp.aspect_rad);")
          ))
          invisible(DBI::dbSendQuery(
            db,
            paste0("UPDATE ", farmername, "_a.temp temp
                  SET aspect_sin = sin(temp.aspect_rad);")
          ))
        }
        invisible(
          DBI::dbSendQuery(db, paste0("DROP TABLE all_farms.geetemp;"))
        )
      }
    }
  )
)
