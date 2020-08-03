#' @title R6 Class for importing Google Earth Engine data to an OFPE database
#'
#' @description R6 class for for importing data collected from Google Earth
#' Engine into an OFPE formatted database.
#' @export
ImportGEE <- R6::R6Class(
  "ImportGEE",
  public = list(
    #' @field dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    dbCon = NULL,
    #' @field dat_path Path to the Google Drive folder that contains the data
    #' for upload into the OFPE database.
    dat_path = NULL,
    #' @field file_names Names of the files in the Google Drive folder.
    file_names = NULL,
    #' @field overwrite Logical, whether to overwrite the GEE data in the
    #' database.
    overwrite = NULL,
    #' @field farmers The 'farmers' table from the 'all_farms' schema of an
    #' OFPE formatted database.
    farmers = NULL,
    #' @field farms The 'farms' table from the 'all_farms' schema of an
    #' OFPE formatted database.
    farms = NULL,

    #' @description
    #' Create a database importer object.
    #' @param dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    #' @param dat_path Path to the Google Drive folder that contains the data
    #' for upload into the OFPE database.
    #' @param overwrite Logical, whether to overwrite the GEE data in the
    #' database.
    #' @return A new 'ImportGEE' object.
    initialize = function(dbCon, dat_path, overwrite) {
      stopifnot(
        is.character(dat_path),
        is.logical(overwrite)
      )
      self$dbCon <- dbCon
      self$dat_path <- dat_path
      self$overwrite <- overwrite
      self$file_names <- invisible(googledrive::drive_ls(self$dat_path))

      self$farmers <- DBI::dbGetQuery(self$dbCon$db,
                                      "SELECT * FROM all_farms.farmers")
      self$farms <- sf::st_read(self$dbCon$db,
                                query = "SELECT * FROM all_farms.farms") %>%
        sf::`st_crs<-`(4326) %>%
        sf::st_transform("epsg:4326")
    },
    #' @description
    #' Google Earth Engine data import execution.
    #' @param None No arguments needed because of class instantiation.
    #' @return See 'GEE' table in database.
    executeUpload = function() {
      # prevents annoying and useless notices from postgresql
      invisible(DBI::dbGetQuery(dbCon$db, "SET client_min_messages TO WARNING"))
      invisible(
        apply(self$file_names,
              1,
              self$.uploadGEE,
              OVERWRITE = self$overwrite,
              self$dbCon$db)
      )
    },
    #' @description
    #' Uploads data to an OFPE formatted database from a Google Drive folder.
    #' The dot indicates that this function would be private if not for
    #' documentations sake.
    #' @param FILE Name of the data file to upload to the database.
    #' @param OVERWRITE Logical, whether to overwrite when data already present.
    #' @param db Connection to an OFPE formatted database.
    #' @return See 'GEE' table in database.
    .uploadGEE = function(FILE, OVERWRITE, db) {
      # check if table exists yet (after first upload it should)
      gee <- as.logical(
        DBI::dbGetQuery(db,
                        "SELECT EXISTS (SELECT 1
                         FROM information_schema.tables
                         WHERE table_schema = 'all_farms'
                         AND table_name = 'gee')")
      )
      dbCheck <- FALSE # assumes file not in db
      if (gee) { # if GEE file exists & user selects to overwrite
        #check for if the FILE exists in db by checking 'orig_file' col
        dbCheck <- as.logical(
          DBI::dbGetQuery(db,
                          paste0("SELECT EXISTS (SELECT 1
                                  FROM all_farms.gee
                                 WHERE orig_file = '", FILE$name,"')"))
        )
        if (dbCheck & OVERWRITE) {
          DBI::dbSendQuery(
            db,
            paste0("DELETE FROM all_farms.gee
                    WHERE orig_file = '", FILE$name,"'")
          )
          dbCheck <- FALSE
        }
      }
      if (!dbCheck) { # if file exists in db
        ## download file to working directory
        suppressMessages(
          googledrive::drive_download(googledrive::as_id(FILE$id),
                                      overwrite=TRUE)
        )
        ## get field and farmer and year etc. info
        info <- self$.getInfo(FILE$name)
        ## bring in data file and convert from utm to wgs84
        tif <- raster::raster(FILE$name) %>%
          raster:: projectRaster(crs= sp::CRS("+proj=longlat +datum=WGS84"))
        ## upload to temp database folder
        invisible(
          suppressMessages(
            rpostgis::pgWriteRast(db, c("all_farms", "temp"), tif)
          )
        )
        ## add and fill info columns in temp table
        invisible(mapply(self$.addColsToDB,
                         info,
                         names(info),
                         MoreArgs = list(db = db)))
        ## put all data in the 'gee' table of the 'all_farms' schema in db
        if (!gee) { # if table does not exist create it
          invisible(
            DBI::dbGetQuery(db,
                            "CREATE TABLE all_farms.gee AS
                             SELECT * FROM all_farms.temp")
          )
          invisible(
            DBI::dbGetQuery(
              db,
              paste0("ALTER TABLE all_farms.gee
                     ADD PRIMARY KEY (rid,orig_file,year,loy,type,scale,source,farmidx,farmeridx)")
            )
          )
          # create spatial index
          tryCatch({
            invisible(
              DBI::dbSendQuery(db,
                               paste0("CREATE INDEX gee_rast_idx",
                                      " ON all_farms.gee",
                                      " USING GIST (ST_ConvexHull(rast))"))
            )},
            error=function(e) {
              print(paste0("error creating spatial index for all_farms.gee"))
            }
          )
        } else { # otherwise append to it
          tryCatch({
            DBI::dbSendQuery(db,
                             paste0("INSERT INTO all_farms.gee
                                    SELECT * FROM all_farms.temp"))
          },
          error=function(e) {
            print(paste0(info$orig_file, " already exists in database"))
          },
          warning=function(w) {print()})
        }
        ## remove temp database table and remove file from working directory
        invisible(
          DBI::dbGetQuery(db, paste0("DROP TABLE all_farms.temp"))
        )
        file.remove(FILE$name)
        message <- paste0(FILE$name, " import complete")
      } else {
        message <- paste0(FILE$name, " already exists in database")
      }
      return(print(message))
    },
    #' @description
    #' Function for identifying information from the filenames exported from
    #' Google Drive
    #' @param NAME Name of the data file to upload to the database.
    #' @return Raster information.
    .getInfo = function(NAME) {
      ## identify queriable info
      INFO <- rep(as.list(NA), 8) # 8 slots for orig_file, year, loy, type, scale, source, farmidx, and farmeridx
      names(INFO) <- c("orig_file",
                       "year",
                       "loy",
                       "type",
                       "scale",
                       "source",
                       "farmidx",
                       "farmeridx")
      # get orig_file name
      INFO$orig_file <- NAME
      # identify year
      if (grepl("2020", INFO$orig_file)) {
        INFO$year <- '2020'
      } else {
        # only looks for data more recent than 1999 NEEDS 2020 UPDATING
        strLocs <- as.data.frame(stringr::str_locate_all(INFO$orig_file,
                                                         "200|201|1999"))
        if (!anyNA(strLocs)) {
          if (strLocs[2] - strLocs[1] == 3) { # if the year is 1999
            INFO$year <- '1999'
          } else { # else if year is 20something
            INFO$year <- suppressWarnings(
              stringr::str_sub(INFO$orig_file,
                               strLocs[1, "end"] + 1,
                               strLocs[1, "end"] + 2) %>%
                as.numeric()
            )
            if (!is.na(INFO$year)) {
              INFO$year <- stringr::str_sub(INFO$orig_file,
                                            strLocs[1, "start"],
                                            strLocs[1, "start"] + 3)
            } else {
              INFO$year <- suppressWarnings(
                stringr::str_sub(INFO$orig_file,
                                 strLocs[2, "end"] + 1,
                                 strLocs[2, "end"] + 2) %>%
                  as.numeric()
              )
              if (!is.na(INFO$year)) {
                INFO$year <- stringr::str_sub(INFO$orig_file,
                                              strLocs[2, "start"],
                                              strLocs[2, "start"] + 3)
              } else {
                INFO$year <- "see_README" # if the year is not in the second slot then users naming convention wrong
              }
            }
          }
        } else {
          INFO$year <- "see_README" # go to the README file and look for GEE file naming conventions
        }
      }

      # identify length of year (loy) of measurement (either thru mar 30 or full year) specified by _mar in filename
      if (grepl("sep", INFO$orig_file)) {
        INFO$loy <- "sep"
      } else {
        # id whether the file corresponds to a full year or partial to mar 30
        INFO$loy <- ifelse(grepl("mar|currYr",
                                 INFO$orig_file),
                           "mar",
                           "full")
      }
      # identify the data type
      if (any(grepl("ndvi|ndre|clre|aspect_rad|slope|elev|tpi|prec|gdd|ssm|susm",
                   INFO$orig_file))) {
        #******************************************************************
        # < IF OTHER DATATYPES NEED DEALING WITH, INSERT IN IF STATEMENT ^ >
        #******************************************************************
        strLocs <- stringr::str_locate(INFO$orig_file,
                                       "ndvi|ndre|clre|aspect_rad|slope|elev|tpi|prec|gdd|ssm|susm") # CHANGED dem to elev
        INFO$type <- stringr::str_sub(INFO$orig_file,
                                      strLocs[1],
                                      strLocs[2])
      } else {
        INFO$type <- "see_README" # if the data type is not specified see naming convention and document what data this is.
      }
      # identify scale (look for 4km or 10m or 20m or 30m or 20km or 1km )
      if (any(grepl("4km|10m|20m|30m|20km|1km", INFO$orig_file))) {
        #******************************************************************
        # < IF OTHER SCALES NEED DEALING WITH, INSERT IN IF STATEMENT ^ >
        #******************************************************************
        strLocs <- stringr::str_locate(INFO$orig_file,
                                       "4km|10m|20m|30m|20km|1km")
        INFO$scale <- stringr::str_sub(INFO$orig_file,
                                       strLocs[1],
                                       strLocs[2])
        INFO$scale <- ifelse(INFO$scale == "20km", "9km", INFO$scale) # mistakenly labeled ssm/susm as 20km when it is 9km
      } else {
        INFO$scale <- "see_README" # scale not identified
      }
      # identify source (look for gridmet, daymet, ned, cdem, srtm, smap, S2, L5, L7, L8)
      if (any(grepl("gridmet|daymet|ned|cdem|srtm|smap|S2|L5|L7|L8",
                   INFO$orig_file))) {
        #******************************************************************
        # < IF OTHER SOURCES NEED DEALING WITH, INSERT IN IF STATEMENT ^ >
        #******************************************************************
        strLocs <- stringr::str_locate(INFO$orig_file,
                                       "gridmet|daymet|ned|cdem|srtm|smap|S2|L5|L7|L8")
        INFO$source <- stringr::str_sub(INFO$orig_file,
                                        strLocs[1],
                                        strLocs[2])
      } else {
        INFO$source <- "see_README" # scale not identified
      }
      # identify farmidx and farmeridx
      if (any(grepl(paste(as.data.frame(self$farms[, 2])$farm,
                         collapse = '|'),
                   tolower(INFO$orig_file)))) {
        strLocs <- stringr::str_locate(tolower(INFO$orig_file),
                                       paste(as.data.frame(self$farms[, 2])$farm,
                                             collapse = '|'))
        farm <- stringr::str_sub(tolower(INFO$orig_file),
                                 strLocs[1],
                                 strLocs[2])
        INFO$farmidx <- as.data.frame(self$farms)[which(as.data.frame(self$farms)$farm == farm), "farmidx"]
        INFO$farmeridx <- as.data.frame(self$farms)[which(as.data.frame(self$farms)$farm == farm), "farmeridx"]
      } else {
        INFO$farmidx <- "see_README" # the farm name does not match a farm name in database
        INFO$farmeridx <- "see_README"
      }
      return(INFO)
    },
    #' @description
    #' Adds columns to the data file importing to the database.
    #' @param INFO Object holding all necessary information about the data.
    #' @param name The name of the column to add.
    #' @param db The conncetion to the OFPE database.
    #' @return Columns added to database table.
    .addColsToDB = function(INFO, name, db) {
     DBI::dbGetQuery(db,
                     paste0("ALTER TABLE all_farms.temp
                            ADD COLUMN ", name, " TEXT
                            DEFAULT '", INFO, "'"))
    }
  )
)



