#' @title R6 Class for importing Google Earth Engine data to an OFPE database
#'
#' @description R6 class for for importing data collected from Google Earth
#' Engine into an OFPE formatted database.
#' @seealso \code{\link{DBCon}} for the database connection class, and
#' \code{\link{ImportOF}} for the associated on-farm data import
#' class in the data import step.
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
      OFPE::removeTempTables(dbCon$db) # removes temporary tables
      stopifnot(
        is.character(dat_path),
        is.logical(overwrite)
      )
      self$dbCon <- dbCon
      self$dat_path <- dat_path
      self$overwrite <- overwrite
      self$file_names <- invisible(googledrive::drive_ls(self$dat_path))
      self$file_names$id <- as.character(self$file_names$id)

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
      invisible(DBI::dbSendQuery(dbCon$db, "SET client_min_messages TO WARNING"))
      invisible(
        apply(self$file_names,
              1,
              self$.uploadGEE,
              overwrite = self$overwrite,
              self$dbCon$db)
      )
    },
    #' @description
    #' Uploads data to an OFPE formatted database from a Google Drive folder.
    #' The dot indicates that this function would be private if not for
    #' documentations sake.
    #' @param FILE Name of the data file to upload to the database.
    #' @param overwrite Logical, whether to overwrite when data already present.
    #' @param db Connection to an OFPE formatted database.
    #' @return See 'GEE' table in database.
    .uploadGEE = function(FILE, overwrite, db) {
      # check if table exists yet (after first upload it should)
      gee <- as.logical(as.numeric(
        DBI::dbGetQuery(db,
                        "SELECT EXISTS (SELECT 1
                         FROM information_schema.tables
                         WHERE table_schema = 'all_farms'
                         AND table_name = 'gee')")
      ))
      db_check <- FALSE # assumes file not in db
      if (gee) { # if GEE file exists & user selects to overwrite
        #check for if the FILE exists in db by checking 'orig_file' col
        db_check <- as.logical(as.numeric(
          DBI::dbGetQuery(db,
                          paste0("SELECT EXISTS (SELECT 1
                                  FROM all_farms.gee
                                 WHERE orig_file = '", FILE$name,"')"))
        ))
        if (db_check & overwrite) {
          DBI::dbSendQuery(
            db,
            paste0("DELETE FROM all_farms.gee
                    WHERE orig_file = '", FILE$name,"'")
          )
          db_check <- FALSE
        }
      }
      if (!db_check) { # if file exists in db
        ## download file to working directory
        suppressMessages(
          googledrive::drive_download(googledrive::as_id(FILE$id),
                                      overwrite=TRUE)
        )
        ## get field and farmer and year etc. info
        info <- self$.getInfo(FILE$name)
        ## bring in data file and convert from utm to wgs84
        tif <- raster::raster(FILE$name)
        if (!grepl("longlat|WGS84", tif@crs)) {
          tif <- raster:: projectRaster(tif, crs= sp::CRS("+proj=longlat +datum=WGS84"))
        }
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
            DBI::dbSendQuery(db,
                            "CREATE TABLE all_farms.gee AS
                             SELECT * FROM all_farms.temp")
          )
          invisible(
            DBI::dbSendQuery(
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
          warning=function(w) {})
        }
        ## remove temp database table and remove file from working directory
        invisible(
          DBI::dbSendQuery(db, paste0("DROP TABLE all_farms.temp"))
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
    #' @param name Name of the data file to upload to the database.
    #' @return Raster information.
    .getInfo = function(name) {
      ## identify queriable info
      info <- rep(as.list(NA), 8) # 8 slots for orig_file, year, loy, type, scale, source, farmidx, and farmeridx
      names(info) <- c("orig_file",
                       "year",
                       "loy",
                       "type",
                       "scale",
                       "source",
                       "farmidx",
                       "farmeridx")
      # get orig_file name
      info$orig_file <- name
      # identify year
      if (grepl("2020", info$orig_file)) {
        info$year <- '2020'
      } else {
        str_locs <- as.data.frame(
          stringr::str_locate_all(info$orig_file, "200|201|1999|202")
        )
        if (grepl("200cm", name)) {
          str_locs <- str_locs[2, ]
        }
        if (!anyNA(str_locs)) {
          if (str_locs[2] - str_locs[1] == 3) { # if the year is 1999
            info$year <- '1999'
          } else { # else if year is 20something
            info$year <- suppressWarnings(
              stringr::str_sub(info$orig_file,
                               str_locs[1, "start"],
                               str_locs[1, "end"] + 1) %>%
                as.numeric()
            )
            if (!is.na(info$year)) {
              info$year <- stringr::str_sub(info$orig_file,
                                            str_locs[1, "start"],
                                            str_locs[1, "start"] + 3)
            } else {
              info$year <- suppressWarnings(
                stringr::str_sub(info$orig_file,
                                 str_locs[2, "end"] + 1,
                                 str_locs[2, "end"] + 2) %>%
                  as.numeric()
              )
              if (!is.na(info$year)) {
                info$year <- stringr::str_sub(info$orig_file,
                                              str_locs[2, "start"],
                                              str_locs[2, "start"] + 3)
              } else {
                info$year <- "see_README" # if the year is not in the second slot then users naming convention wrong
              }
            }
          }
        } else {
          info$year <- "see_README" # go to the README file and look for GEE file naming conventions
        }
      }

      # identify length of year (loy) of measurement (either thru mar 30 or full year) specified by _mar in filename
      if (grepl("sep", info$orig_file)) {
        info$loy <- "sep"
      } else {
        # id whether the file corresponds to a full year or partial to mar 30
        info$loy <- ifelse(grepl("mar|currYr",
                                 info$orig_file),
                           "mar",
                           "full")
      }
      # identify the data type
      if (any(grepl(paste(c("ndvi", "ndre", "clre", "aspect_rad", "slope", 
                            "elev", "tpi", "prec", "gdd", "ssm", "susm", 
                            "grtgroup", "texture0cm", "texture10cm", 
                            "texture30cm", "texture60cm", "texture100cm", 
                            "texture200cm", "bulkdensity0cm", "bulkdensity10cm",
                            "bulkdensity30cm", "bulkdensity60cm", "bulkdensity100cm", 
                            "bulkdensity200cm", "claycontent0cm", "claycontent10cm", 
                            "claycontent30cm", "claycontent60cm", "claycontent100cm",
                            "claycontent200cm", "sandcontent0cm", "sandcontent10cm", 
                            "sandcontent30cm", "sandcontent60cm", "sandcontent100cm", 
                            "sandcontent200cm", "phw0cm", "phw10cm", "phw30cm",
                            "phw60cm", "phw100cm", "phw200cm", "watercontent0cm",
                            "watercontent10cm", "watercontent30cm", "watercontent60cm", 
                            "watercontent100cm", "watercontent200cm", "carboncontent0cm", 
                            "carboncontent10cm", "carboncontent30cm", "carboncontent60cm",
                            "carboncontent100cm", "carboncontent200cm", "ndwi"), 
                     collapse = "|"),
                   info$orig_file))) {
        #******************************************************************
        # < IF OTHER DATATYPES NEED DEALING WITH, INSERT IN IF STATEMENT ^ >
        #******************************************************************
        str_locs <- stringr::str_locate(info$orig_file,
                                        paste(c("ndvi", "ndre", "clre", "aspect_rad", "slope", 
                                                "elev", "tpi", "prec", "gdd", "ssm", "susm", 
                                                "grtgroup", "texture0cm", "texture10cm", 
                                                "texture30cm", "texture60cm", "texture100cm", 
                                                "texture200cm", "bulkdensity0cm", "bulkdensity10cm",
                                                "bulkdensity30cm", "bulkdensity60cm", "bulkdensity100cm", 
                                                "bulkdensity200cm", "claycontent0cm", "claycontent10cm", 
                                                "claycontent30cm", "claycontent60cm", "claycontent100cm",
                                                "claycontent200cm", "sandcontent0cm", "sandcontent10cm", 
                                                "sandcontent30cm", "sandcontent60cm", "sandcontent100cm", 
                                                "sandcontent200cm", "phw0cm", "phw10cm", "phw30cm",
                                                "phw60cm", "phw100cm", "phw200cm", "watercontent0cm",
                                                "watercontent10cm", "watercontent30cm", "watercontent60cm", 
                                                "watercontent100cm", "watercontent200cm", "carboncontent0cm", 
                                                "carboncontent10cm", "carboncontent30cm", "carboncontent60cm",
                                                "carboncontent100cm", "carboncontent200cm", "ndwi"), 
                                              collapse = "|")) # CHANGED dem to elev
        info$type <- stringr::str_sub(info$orig_file,
                                      str_locs[1],
                                      str_locs[2])
      } else {
        info$type <- "see_README" # if the data type is not specified see naming convention and document what data this is.
      }
      # identify scale (look for 4km or 10m or 20m or 30m or 20km or 1km or 250m )
      if (any(grepl("4km|10m|20m|30m|20km|1km|250m", info$orig_file))) {
        #******************************************************************
        # < IF OTHER SCALES NEED DEALING WITH, INSERT IN IF STATEMENT ^ >
        #******************************************************************
        str_locs <- stringr::str_locate(info$orig_file,
                                       "4km|10m|20m|30m|20km|1km|250m")
        info$scale <- stringr::str_sub(info$orig_file,
                                       str_locs[1],
                                       str_locs[2])
        info$scale <- ifelse(info$scale == "20km", "9km", info$scale) # mistakenly labeled ssm/susm as 20km when it is 9km
      } else {
        info$scale <- "see_README" # scale not identified
      }
      # identify source (look for gridmet, daymet, ned, cdem, srtm, smap, S2, L5, L7, L8, olm)
      if (any(grepl("gridmet|daymet|ned|cdem|srtm|smap|S2|L5|L7|L8|olm",
                   info$orig_file))) {
        #******************************************************************
        # < IF OTHER SOURCES NEED DEALING WITH, INSERT IN IF STATEMENT ^ >
        #******************************************************************
        str_locs <- stringr::str_locate(info$orig_file,
                                       "gridmet|daymet|ned|cdem|srtm|smap|S2|L5|L7|L8|olm")
        info$source <- stringr::str_sub(info$orig_file,
                                        str_locs[1],
                                        str_locs[2])
      } else {
        info$source <- "see_README" # scale not identified
      }
      # identify farmidx and farmeridx
      if (any(grepl(paste(as.data.frame(self$farms[, 2])$farm,
                         collapse = '|'),
                   tolower(info$orig_file)))) {
        str_locs <- stringr::str_locate(tolower(info$orig_file),
                                       paste(as.data.frame(self$farms[, 2])$farm,
                                             collapse = '|'))
        farm <- stringr::str_sub(tolower(info$orig_file),
                                 str_locs[1],
                                 str_locs[2])
        info$farmidx <- as.data.frame(self$farms)[which(as.data.frame(self$farms)$farm == farm), "farmidx"]
        info$farmeridx <- as.data.frame(self$farms)[which(as.data.frame(self$farms)$farm == farm), "farmeridx"]
      } else {
        info$farmidx <- "see_README" # the farm name does not match a farm name in database
        info$farmeridx <- "see_README"
      }
      return(info)
    },
    #' @description
    #' Adds columns to the data file importing to the database.
    #' @param info Object holding all necessary information about the data.
    #' @param name The name of the column to add.
    #' @param db The connection to the OFPE database.
    #' @return Columns added to database table.
    .addColsToDB = function(info, name, db) {
     DBI::dbSendQuery(db,
                     paste0("ALTER TABLE all_farms.temp
                            ADD COLUMN ", name, " TEXT
                            DEFAULT '", info, "'"))
    }
  )
)



