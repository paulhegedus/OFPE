#' @title R6 Class for importing on-farm data to an OFPE database
#'
#' @description R6 class for for importing data collected on-farms data into an
#' OFPE formatted database.
#' @seealso \code{\link{DBCon}} for the database connection class, and
#' \code{\link{ImportGEE}} for the associated Google Earth Engine data import
#' class in the data import step.
#' @export
ImportOF <- R6::R6Class(
  "ImportOF",
  public = list(
    #' @field dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    dbCon = NULL,
    #' @field dat_path Path to the folder that contains the data for upload into
    #' the OFPE database.
    dat_path = NULL,
    #' @field file_names Names of the files in the data folder.
    file_names = NULL,
    #' @field status Data frame with a status update on the completion
    #' of each file's data import.
    status = NULL,
    #' @field farmers The 'farmers' table from the 'all_farms' schema of an
    #' OFPE formatted database.
    farmers = NULL,
    #' @field farms The 'farms' table from the 'all_farms' schema of an
    #' OFPE formatted database.
    farms = NULL,
    #' @field fields The 'fields' table from the 'all_farms' schema of an
    #' OFPE formatted database.
    fields = NULL,

    #' @description
    #' Create a database importer object.
    #' @param dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    #' @param dat_path Path to the folder that contains the data for upload into
    #' the OFPE database.
    #' @return A new 'ImportOF' object.
    initialize = function(dbCon, dat_path) {
      OFPE::removeTempTables(dbCon$db) # removes temporary tables
      stopifnot(
        is.character(dat_path)
      )
      self$dbCon <- dbCon
      self$dat_path <- dat_path
      self$file_names <- c(list.files(self$dat_path, "shp$"),
                           list.files(self$dat_path, "csv$"))
      self$status <- rep(list(0), length(self$file_names)) %>%
        `names<-`(self$file_names)

      self$farmers <- DBI::dbGetQuery(self$dbCon$db,
                                      "SELECT * FROM all_farms.farmers")
      self$farms <- sf::st_read(self$dbCon$db,
                                query = "SELECT * FROM all_farms.farms") %>%
        sf::`st_crs<-`(4326) %>%
        sf::st_transform("epsg:4326")

      self$fields <- sf::st_read(self$dbCon$db,
                                 query = "SELECT * FROM all_farms.fields") %>%
        sf::`st_crs<-`(4326) %>%
        sf::st_transform("epsg:4326") %>%
        sf::st_cast("MULTIPOLYGON")
    },
    #' @description
    #' On-farm data import execution.
    #' @param None No arguments needed because of class instantiation.
    #' @return See 'status' object.
    executeUpload = function() {
      lapply(self$file_names, self$.uploadData) %>%
        invisible()
      self$status <- do.call(rbind.data.frame, self$status)
      names(self$status) <- "status"
      self$status$fileName <- names(self$status)
    },
    #' @description
    #' Uploads data to an OFPE formatted database following a chain of methods
    #' and returning the status of the upload. The dot indicates that this
    #' function would be private if not for documentations sake.
    #' @param name Name of the data file to upload to the database.
    #' @return See 'status' object.
    .uploadData = function(name) { # MASTER FUNCTION
      tryCatch({FILE <- self$.impDat(name)},
               warning = function(w) {print()},
               error = function(e) {
                 self$status[[which(names(self$status) == name)]] <-
                   paste0("!!! ERROR IN impDat() FOR ", name, " !!!")
                 print(paste0("!!! ERROR IN impDat() FOR " ,name, " !!!"))
               })
      if (self$status[[which(names(self$status) == name)]] == 0) {
        tryCatch({FILE <- self$.makeSptl(FILE, name)},
                 warning = function(w) {print()},
                 error = function(e) {
                   self$status[[which(names(self$status) == name)]] <-
                     paste0("!!! ERROR IN makeSptl() FOR ", name, " !!!")
                   print(paste0("!!! ERROR IN makeSptl() FOR ", name," !!!"))
                 })
      }
      if (self$status[[which(names(self$status) == name)]] == 0) {
        tryCatch({FILE <- self$.oldDatClean(FILE)},
                 warning = function(w) {print()},
                 error = function(e) {
                   self$status[[which(names(self$status) == name)]] <-
                     paste0("!!! ERROR IN oldDatClean() FOR ", name, " !!!")
                   print(paste0("!!! ERROR IN oldDatClean() FOR ", name, " !!!"))
                 })
      }
      if (self$status[[which(names(self$status) == name)]] == 0) {
        tryCatch({FILE <- self$.findInfo(FILE, name)},
                 warning = function(w) {print()},
                 error = function(e) {
                   self$status[[which(names(self$status) == name)]] <-
                     paste0("!!! ERROR IN findInfo() FOR ", name, " !!!")
                   print(paste0("!!! ERROR IN findInfo() FOR ", name, " !!!"))
                 }
        )
      }
      if (self$status[[which(names(self$status) == name)]] == 0) {
        tryCatch({FILE <- self$.fixCols(FILE)},
                 warning = function(w) {print()},
                 error = function(e) {
                   self$status[[which(names(self$status) == name)]] <-
                     paste0("!!! ERROR IN fixCols() FOR ", name, " !!!")
                   print(paste0("!!! ERROR IN fixCols() FOR ", name, " !!!"))
                 })
      }
      if (self$status[[which(names(self$status) == name)]] == 0) {
        tryCatch({FILE <- FILE %>%
          sf::st_transform("epsg:4326")},
          warning = function(w) {print()},
          error = function(e) {
            self$status[[which(names(self$status) == name)]] <-
              paste0("!!! ERROR IN st_transform(", name,
                     ") TO LONGLAT WGS84 !!!")
            print(paste0("!!! ERROR IN st_transform(", name,
                         ") TO LONGLAT WGS84 !!!"))
          })
      }
      if (self$status[[which(names(self$status) == name)]] == 0) {
        tryCatch({FILE <- as(FILE, "Spatial")},
                 warning = function(w) {print()},
                 error = function(e) {
                   self$status[[which(names(self$status) == name)]] <-
                     paste0("!!! ERROR CONVERTING ", name, " TO CLASS 'sp' !!!")
                   print(paste0("!!! ERROR IN CONVERTING ", name, " TO CLASS 'sp' !!!"))
                 })
      }
      if (self$status[[which(names(self$status) == name)]] == 0) {
        self$.uploadFun(FILE, name, dbCon$db)
        self$status[[which(names(self$status) == name)]] <-
          paste0("IMPORT COMPLETE: ", name)
      }
    },
    #' @description
    #' Imports data for uploading to an OFPE database. If data is a shapefile
    #' imported directly using 'sf' package, otherwise brings in data as .csv.
    #' The dot indicates that this function would be private if not for
    #' documentations sake.
    #' @param name Name of the data file to upload to the database.
    #' @return Imported data.
    .impDat = function(name) {
      if (grepl("shp$", name)) {
        FILE <- sf::read_sf(self$dat_path,
                            stringr::str_sub(name, 1, nchar(name) - 4)) %>%
          sf::st_zm()
      }
      if (grepl("csv$",name)) {
        tryCatch({
          FILE <- read.csv(paste0(self$dat_path, "/", name),
                           skip = 2, header = FALSE)
          header <- read.csv(paste0(self$dat_path, "/", name),
                             header = TRUE, nrows = 1)
          if (length(names(header)) < length(names(FILE))) {
            names(FILE) <- c(names(header), "utc_time", "y", "n", "x", "w")
          } else {
            if (any(grepl("X", names(header)))) {
              real_names <- names(header)[1:(ncol(header) - 5)]
              names(FILE) <- c(real_names, "utc_time", "y", "n", "x", "w")
            } else {
              names(FILE) <- names(header)
            }
          }
          FILE$x <- ifelse(FILE$x > 0, FILE$x * -1, FILE$x)
        },
        warning = function(w) {print()},
        error = function(e) {
          tryCatch({
            FILE <- data.table::fread(paste0(self$dat_path, "/", name)) %>%
              as.data.frame()
          },
          warning = function(w) {
            print()},
          error = function(e) {
            print(paste0("Error: loading ", name, " !!!"))})
        })
        if (any(grepl("Longitude|Latitude", names(FILE)))) {
          names(FILE)[which(names(FILE) == "Longitude")] <- "x"
          names(FILE)[which(names(FILE) == "Latitude")] <- "y"
        }
      }
      if (length(FILE) == 1) {
        FILE <- read.csv(paste0(self$dat_path, "/", name), skip = 2, header = FALSE)
        header <- read.csv(paste0(self$dat_path, "/", name), header = TRUE, nrows = 1)
        names(FILE) <- c(names(header), "utc_time", "y", "n", "x", "w")
        FILE$x <- ifelse(FILE$x > 0, FILE$x * -1, FILE$x)
      }
      if (any(names(FILE) == "orig_file")) {
        names(FILE)[which(names(FILE) == "orig_file")] <- "orig_filePREV"
      }
      FILE$orig_file <- paste(name)
      if (grepl("GMC", name, ignore.case = TRUE)) {
        FILE$farmer <- "broyles"
      }
      return(FILE)
    },
    #' @description
    #' Makes data that was imported for uploading into spatial data. This only
    #' applies to non-shapefiles, as they are already spatial. This is for the
    #' .csv protein or SSURGO data that is imported. The dot indicates that this
    #' function would be private if not for documentations sake.
    #' @param FILE The imported data from the .impDat method.
    #' @param name The filename for the data imported from the .impDat method.
    #' @return Data in spatial format.
    .makeSptl = function(FILE, name) {
      tryCatch({
          if (grepl("csv$",name)) {
            FILE$X <- FILE$x
            FILE$Y <- FILE$y
            FILE <- FILE[!is.na(FILE$X) & !is.na(FILE$Y), ]
            if (ncol(FILE) > 12) {
              ## put correct sign on lat long
              x_dir <- grep("^x$", names(FILE)) + 1
              x_dir <- unique(na.omit(FILE[, x_dir]))
              x_dir <- x_dir[grep("W|E", x_dir)]
              y_dir <- grep("^y$", names(FILE)) + 1
              y_dir <- unique(na.omit(FILE[, y_dir]))
              y_dir <- y_dir[grep("N|S", y_dir)]
              if (x_dir == "W") {
                FILE$X <- ifelse(FILE$X < 0,
                                 FILE$X,
                                 FILE$X * -1)
              }
              if (y_dir == "S") {
                FILE$X <- ifelse(FILE$Y < 0,
                                 FILE$Y,
                                 FILE$Y * -1)
              }
            }

            sp::coordinates(FILE) <- c("X", "Y") # makes spatial points df
            sp::proj4string(FILE) <- sp::CRS("+proj=longlat +datum=WGS84")
            FILE <- sf::st_as_sf(FILE)
          }
        },
        warning = function(w) {print()},
        error = function(e) {
          print(paste0("Error: making ", name, " spatial."))
        }
      )
      tryCatch({
        crs_raster <- suppressWarnings(raster::crs(FILE))
        if (is.na(crs_raster)) {
          sf::st_crs(FILE) <- 4326
        }
        if (!grepl("+datum=WGS84 +ellps=WGS84 +init=epsg:4326",
                   crs_raster)) {
          FILE <- sf::st_transform(FILE, "epsg:4326")
        }
        utm_epsg <- OFPE::calcUTMzone(FILE)
        tempCRS <- paste0("epsg:", utm_epsg)
        FILE <- sf::st_transform(FILE, tempCRS)
        return(FILE)
      },
      warning = function(w) {print()},
      error = function(e) {print(paste0("Error: making ", name, " UTM."))}
      )
    },
    #' @description
    #' Cleans up any column names that might mess with new ones that are
    #' created in the data workflow. This is mainly to prevent issues when using
    #' data from Phil's database. The dot indicates that this would be private if
    #' not for documentations sake.
    #' @param FILE The imported data for upload.
    #' @return Data without old columns.
    .oldDatClean = function(FILE) {
      if (any(names(FILE) == "gid")) {
        FILE[, which(names(FILE) == "gid")] <- NULL
      }
      if (any(names(FILE) == "geom")) {
        FILE[, which(names(FILE) == "geom")] <- NULL
      }
      if (any(names(FILE) == "rm_code")) {
        FILE[, which(names(FILE) == "rm_code")] <- NULL
      }
      # if (any(names(FILE) == "desc")) {
      #   FILE[, which(names(FILE) == "desc")] <- NULL
      # }
      return(FILE)
    },
    #' @description
    #' Identifies distinguishing features of the data, such as a field name if
    #' present, the corresponding farmer, and the year the data is collected.
    #' The dot indicates that this would be private if not for documentations
    #' sake.
    #' @param FILE The imported data for upload.
    #' @param name The name of the file for upload.
    #' @return Data without old columns.
    .findInfo = function(FILE, name) {
      anyFarmer <- 0 # index arg
      ## look for bad polygons
      if (any(grepl("geometry", names(FILE)))) {
        if (any(grepl("polygon", class(FILE$geometry), ignore.case=TRUE))) {
          FILE <- sf::st_buffer(FILE, dist = 0)
        }
      } else {
        if (any(grepl("polygon", class(FILE$geom), ignore.case=TRUE))) {
          FILE <- sf::st_buffer(FILE, dist = 0)
        }
      }
      utm_epsg <- OFPE::calcUTMzone(FILE)
      self$fields <-
        sf::st_transform(self$fields,
                         paste0("epsg:", utm_epsg))
      self$farms <-
        sf::st_transform(self$farms,
                         paste0("epsg:", utm_epsg))
      ## get info
      if (!grepl("ssurgo", name)) {
        ## 1) update "fieldname" and "fieldidx" and "farmeridx" if possible in FILE
        # see if the file intersects any of the exp fields in db
        if (any(unlist(lapply(sf::st_intersects(self$fields, FILE), length)) > 0)) {
          intscts <- lapply(sf::st_intersects(self$fields, FILE), length)
          instscts <- intscts[which(intscts > 0)]
          if (length(intscts) > 1) {
            intscts <- sum(unlist(intscts))
          } else {
            intscts <- as.numeric(intscts)
          }
          # if more than 80% of the points are within the field boundary
          if ((intscts / nrow(FILE)) > 0.60) {
            FILE <- suppressWarnings(sf::st_intersection(self$fields, FILE))
            anyFarmer <- 1
          } else {
            FILE$fieldname <- private$.findField(FILE)
            FILE$wfid <- 0
            FILE$fieldidx <- 0
            if (FILE$fieldname[1] == "unknown") {
              print(paste0("no fieldname in ", name, " identificed."))
            }
          }
        } else { # if not, look in column names for a like "field" to extract name for fieldname, fieldidx = 0 (non exp field)
          FILE$fieldname <- private$.findField(FILE)
          FILE$wfid <- 0
          FILE$fieldidx <- 0
          if (FILE$fieldname[1] == "unknown") {
            print(paste0("no fieldname in ", name, " identified."))
          }
        }
      }
      ## 2) update "farmeridx" column in FILE if not done so already
      if (anyFarmer == 0) { # if no farmer was identified with the field then farmeridx = 0, if not 0 farmer found
        # see if the file intersects any of the farms in db
        if (any(unlist(lapply(sf::st_intersects(self$farms, FILE), length)) > 0)) {
          FILE <- suppressWarnings(sf::st_intersection(self$farms, FILE))
          if (any(names(FILE) == "farm")) {
            FILE$farm <- NULL
          }
        } else {  # if not, look in file for match to a farmer name in db
          FILE$farmeridx <- private$.findFarmer(FILE, self$farmers)
          FILE$farmidx <- 0
          ## NOTE: output will either be a farmeridx or 0 (= no farmer identified)
          ## NOTE: when querying from db, if farmeridx != 0 get farmer name
          if (FILE$farmeridx[1] == 0) {
            print(paste0("no farmer in ", name, " identified."))
          }
        }
      }
      ## 3) update "year" column in FILE
      ## identify and extract the year from columns in table & update 'year' col in FILE
      FILE$year <- private$.findYear(FILE)
      if (FILE$year[1] == "unknown") {
        if (!grepl("ssurgo", name)) {
          print(paste0("no year in ", name, " identified."))
        }
      }
      return(FILE)
    },
    #' @description
    #' Goes through each file and replaces any column names with more palatable
    #' formats such as lower case and without any special characters. The dot
    #' indicates that this would be private if not for documentations sake.
    #' @param FILE The imported data for upload.
    #' @return Data with cleaned up column names.
    .fixCols = function(FILE) {
      colNames <- names(FILE) %>%
        lapply(tolower) %>%
        unlist() %>%
        lapply(OFPE::noSpecialChar, FALSE) %>%
        unlist()
      names(FILE) <- colNames
      return(FILE)
    },
    #' @description
    #' Function for uploading each file to the appropriate section of the
    #' database. This function identifies the data type and uses the information
    #' identified in the data to organize and upload the data. The dot
    #' indicates that this would be private if not for documentations sake.
    #' @param FILE The imported data for upload.
    #' @param name The name of the file for upload.
    #' @param db Connection to an OFPE database.
    #' @return Data uploaded to the database
    .uploadFun = function(FILE, name, db) {
      if (grepl("ssurgo", name)) {
        FILE@data$year <- NULL
        NAMES <- names(FILE)
      } else {
        NAMES <- names(FILE)
      }
      farmer <- as.character(
        DBI::dbGetQuery(db,
                        paste0("SELECT farmer
                               FROM all_farms.farmers
                               WHERE farmeridx = ",
                               FILE$farmeridx[1]))
      )
      dtype <- private$.findDtype(FILE, name, NAMES)
      #******************************************************************
      #******************************************************************
      # check if table exists for data type in farmer's schema
      if (dtype == "rx") {
        schema <- paste0(farmer, "_a")
        query <- paste0("ALTER TABLE ", schema,".", dtype,
                        " ADD PRIMARY KEY (gid, fieldname, year, orig_file)")
        upserts <- c("gid", "fieldname", "year", "orig_file")
      } else {
        if (dtype == "ssurgo") {
          schema <- "all_farms"
          query <- paste0("ALTER TABLE ", schema,".", dtype,
                          " ADD PRIMARY KEY (gid, farmidx, orig_file)")
          upserts <- c("gid", "farmidx", "orig_file")
        } else {
          schema <- paste0(farmer, "_r")
          query <- paste0("ALTER TABLE ", schema,".", dtype,
                          " ADD PRIMARY KEY (gid, fieldname, year, orig_file)")
          upserts <- c("gid", "fieldname", "year", "orig_file")
        }
      }
      tab_exist <- OFPE::tabExist(db, schema, dtype)
      # if not exists, create table called dtype and upload the data
      if (!tab_exist) { # if exists = false this returns true
        FILE <- OFPE::setNAtoNaN(FILE)
        OFPE::importNewDat(db, FILE, schema, dtype)
        invisible(DBI::dbGetQuery(db, query))
        geom_idx <- paste0(dtype, "_geom_idx")
        OFPE::makeDBcolsText(db, NAMES, schema, dtype)
        OFPE::convPolyToMulti(db, FILE, schema, dtype)
        OFPE::makeSpatialIndex(db, geom_idx, schema, dtype)
      } else { # if table does exist. if exists = true than if () returns false
        OFPE::removeTempFarmerTables(db, farmer)
        FILE <- OFPE::standardizeColNames(db, FILE, schema, dtype)
        FILE <- OFPE::setNAtoNaN(FILE)
        if (any(grepl("poly",
                      class(sf::st_geometry(sf::st_as_sf(FILE))),
                      ignore.case = TRUE))) {
          if (!any(grepl("multi",
                         class(sf::st_geometry(sf::st_as_sf(FILE))),
                         ignore.case = TRUE))) {
            OFPE::importMulti(db, FILE, schema, dtype)
          } else {
            OFPE::importDat(db, FILE, schema, dtype, upserts)
            OFPE::makeDBcolsText(db, NAMES, schema, dtype)
          }
        } else {
          OFPE::importDat(db, FILE, schema, dtype, upserts)
          OFPE::makeDBcolsText(db, NAMES, schema, dtype)
        }
      }
      return(print(paste0("IMPORT COMPLETE: ", FILE@data$orig_file[1])))
    }
  ),
  private = list(
    .findField = function(FILE) {
      fieldname <- "unknown"
      if (any(grepl("field|sample", names(FILE), ignore.case = TRUE))) {
        cols <- grep("field|sample", names(FILE), ignore.case = TRUE)
        fieldNames <- lapply(cols, private$.extractField, FILE)
        fieldNames <- fieldNames[!unlist(lapply(fieldNames, is.null))]
        # take first element that isn't null, otherwise takes from 'field' column first, if that isn't there then it takes from what should be 'sample_id' (protein data)
        fieldname <- fieldNames[[1]]
      }
      return(fieldname)
    },
    .extractField = function(COL, FILE) {
      if (is.na(as.character(FILE[1, COL])[1])) {
        fieldname <- "unknown"
      } else {
        # [1] is b.c geometry included
        fieldname <- as.character(FILE[1, COL])[1] %>%
          OFPE::noSpecialChar(FALSE) %>%
          tolower()
      }
      return(fieldname)
    },
    .findFarmer = function(FILE, farmers) {
      farmeridx <- 0
      if (any(grepl(paste(farmers$farmer,collapse="|"),
                    FILE[1,],
                    ignore.case = TRUE))) {
        cols <- grep(paste(farmers$farmer,collapse="|"),
                     FILE[1,],
                     ignore.case = TRUE)
        anyFarmers <- apply(farmers,1,
                            private$.extractFarmer,
                            cols,
                            FILE) # finds which farmer name was found
        farmeridx <- anyFarmers[!is.na(anyFarmers)]
      }
      return(farmeridx)
    },
    .extractFarmer = function(farmer, COL, FILE) {
      # second element of farmer is farmer name
      if (any(grepl(farmer[2],FILE[1,COL],ignore.case = TRUE))) {
        farmeridx <- as.numeric(
          DBI::dbGetQuery(db,
                          paste0("SELECT farmers.farmeridx
                                 FROM all_farms.farmers
                                 WHERE farmers.farmer = '",farmer[2],"'"))
        )
      } else {
        farmeridx <- NA
      }
    },
    .findYear = function(FILE) {
      ## look for a year column
      if (any(grepl("^year$", names(FILE), ignore.case = TRUE))) {
        year <- as.numeric(as.character(FILE$year[1]))
        # to be more sure that it is actually a date and not a measure
        if (year > 2000 & year < 2030) {
          return(year)
        }
      }
      if (any(grepl("^rxyear$", names(FILE), ignore.case = TRUE))) {
        year <- as.numeric(as.character(FILE$rxyear[1]))
        # to be more sure that it is actually a date and not a measure
        if (year > 2000 & year < 2030) {
          return(year)
        }
      }
      ## look in the orig_file name for year
      OGfile <- as.character(
          as.data.frame(
            FILE[1, which(grepl("orig_file", names(FILE), ignore.case = TRUE))]
          )
        )[1]
      str_locs <- stringr::str_locate(OGfile, "20")
      if (!anyNA(str_locs)) {
        yr <- suppressWarnings(
          stringr::str_sub(OGfile, str_locs[2] + 1, str_locs[2] + 2) %>%
            as.numeric()
          )
        if (!is.na(yr)) {
          year <- stringr::str_sub(OGfile, str_locs[1], str_locs[1] + 3)
          year <- as.numeric((as.character(year)))
          # to be more sure that it is actually a date and not a measure
          if (year > 2000 & year < 2030) {
            return(year)
          }
        }
      }
      ## look in columns named time or date for a year
      if (any(grepl("time", names(FILE), ignore.case = TRUE)|
              grepl("date", names(FILE), ignore.case = TRUE))) {
        # look for 20*
        # get cols that have time or date in them
        cols <- which(grepl("time",names(FILE), ignore.case = TRUE)|
                        grepl("date",names(FILE), ignore.case = TRUE))
        # get columns with date or time
        df <- data.frame(FILE[, cols])[, 1:length(cols)] %>%
          # convert from factor to character
          lapply(as.character) %>%
          lapply(private$.anyYear) # look for any year in these columns
        # if all elements = NULL this is skipped, if not that means year found
        if (any(!as.logical(lapply(df, is.null)))) {
          df <- df[!unlist(lapply(df, is.null))]
          # take the first year found (either time or date), must be a year
          # otherwise anyYear() returned NULL
          year <- df[[1]]
          return(year)
        }
        # if no 20* in time or date col convert to dates? and look for 20*
        # get the 2 chars after and convert to NA... if NA don't use...
        # otherwise use to fill in year
        df <- data.frame(FILE[, cols])[1:length(cols)] %>%
          lapply(as.Date, tryFormats=c("%y/%m/%d",
                                       "%d/%m/%y",
                                       "%d-%m-%y",
                                       "%y-%m-%d"), optional=TRUE) %>%
          lapply(as.character) %>%
          lapply(private$.anyYear)
        # if all elements = NULL this is skipped, if not that means year found
        if (any(!as.logical(lapply(df, is.null)))) {
          df <- df[!unlist(lapply(df, is.null))]
          # take the first year found (either time or date), must be a year
          # otherwise anyYear() returned NULL
          year <- df[[1]]
          return(year)
        }
      }
      ## look in column called dataset for year (pertains to Wood)
      if (any(grepl("dataset", names(FILE), ignore.case = TRUE))) {
        # look for 20*
        # get cols that have dataset in them
        cols <- which(grepl("dataset", names(FILE), ignore.case = TRUE))
        df <- data.frame(FILE[, cols])[, 1:length(cols)] %>%
          as.Date(tryFormats = c("%y/%m/%d",
                                 "%d/%m/%y",
                                 "%d-%m-%y",
                                 "%y-%m-%d"),
                  optional = TRUE,
                  # shouldn't change any dates that are already in %Y-%m-%d
                  origin = "1970-01-01") %>%
          private$.anyYear()
        # if df = NULL this is skipped, if not that means year found
        if (!is.null(df)) {
          # if year is found df is the year because you only looked for
          # one column ("dataset")
          return(df)
        }
      }
      ## last attempt ... look for 20* in any columns
      # warning suppressed is for column not a atomic vector
      # (I think it's geometry)
      df <- suppressWarnings(lapply(FILE, private$.anyYear))
      # if all elements = NULL this is skipped, if not that means year found
      if (any(!as.logical(lapply(df, is.null)))) {
        df <- df[!unlist(lapply(df, is.null))] # get elements that aren't null
        # take the first year found, must be a year otherwise anyYear() returned NULL
        year <- df[[1]]
        return(year)
      }
      # if there is no year info found return NA
      year <- "unknown"
      return(year)
    },
    .anyYear = function(VEC) {
      str_locs <- stringr::str_locate(VEC[1], "20")
      if (!anyNA(str_locs)) {
        # warnings are if it is NA, which is okay (see next lines logic)
        yr <- suppressWarnings(
          stringr::str_sub(VEC[1], str_locs[2] + 1, str_locs[2] + 2) %>%
            as.numeric()
        )
        if (!is.na(yr)) {
          year <- stringr::str_sub(VEC[1], str_locs[1], str_locs[1] + 3)
          # to be more sure that it is actually a date and not a measure
          if (year > 2000 & year < 2030) {
            return(year)
          } else {
            year <- NULL
            return(year)
          }
        }
      }
    },
    .findDtype = function(FILE, name, NAMES) {
      dtype <- NA
      #******************************************************************
      ## < NOTE: UPDATE grepl() STATEMENTS BELOW IF NEW KEYWORDS >
      #******************************************************************
      if (is.na(dtype) & grepl("ssurgo", name)) {
        dtype <- "ssurgo"
      }
      # RX or ssopt data
      if (is.na(dtype) &
          any(grepl("RX|ssopt", name, ignore.case = TRUE)|
              any(grepl("RX|ssopt", NAMES, ignore.case = TRUE)))) {
        if (any(grepl("poly",
                      class(sf::st_geometry(sf::st_as_sf(FILE))),
                      ignore.case = TRUE))) {
          dtype <- "rx_poly"
        } else {
          dtype <- "rx"
        }
      }
      # Yield data - looks for "yield", otherwise looks for "yld" but not rate (N)
      if (is.na(dtype) &
          any(any(grepl("yield", NAMES, ignore.case = TRUE))|
              any(grepl("yld", NAMES, ignore.case = TRUE))|
              any(grepl("yld", name, ignore.case = TRUE)))) {
        if (!any(grepl("rate|bulk_rt|blk_rt_", NAMES, ignore.case = TRUE))) {
          dtype <- "yld"
        } else {
          dtype <- NA
        }
      }
      # Protein data - looks for the string protein and sample_id (cropscan formate)
      if (is.na(dtype) &
          any(grepl("protein", NAMES, ignore.case = TRUE))) {
        dtype <- "pro"
      }
      # N data - looks for colnames containing "rate" or "AA" (as-applied) and doesn't include "seed"
      if (is.na(dtype) &
          any(grepl("rate|AA", NAMES, ignore.case = TRUE))|
          any(grepl("rate|aaN|AA|_aa|Rate|RATE|AA_N|_N_",
                    name, ignore.case = FALSE))) {
        if (!any(grepl("seed", NAMES, ignore.case = TRUE))) {
          # check in data file for "seed"
          anySeed <- private$.findSeed(FILE)
          # if not seed data it is N data
          if (anySeed=="no") {
            if (any(grepl("poly",
                          class(sf::st_geometry(sf::st_as_sf(FILE))),
                          ignore.case = TRUE))) {
              dtype <- "aa_n_poly"
            } else {
              dtype <- "aa_n_pnts"
            }
          } else {
            dtype <- "aa_sr"
          }
        } else {
          dtype <- "aa_sr"
        }
      }
      # looks for seed or rate b/c N should have been identified earlier
      if (is.na(dtype) & any(grepl("seed|_SR_", name, ignore.case = TRUE))) {
        dtype <- "aa_sr"
      }
      # looks for seed or rate b/c N should have been identified earlier
      if (is.na(dtype) & any(grepl("seed|rate",NAMES,ignore.case = TRUE))) {
        dtype <- "aa_sr"
      }
      # Protein data - again... now looks for "pro"
      if (is.na(dtype) &
          any(grepl("pro", NAMES, ignore.case = TRUE))) {
        dtype <- "pro"
      }
      return(dtype)
    },
    .findSeed = function(FILE) {
      anySeed <- "no"
      if (any(grepl("seed", FILE@data[1,], ignore.case = TRUE))) {
        anySeed <- "yes"
        return(anySeed)
      }
      return(anySeed)
    },
    .noMatch = function(source, target) {
      inSource <- rep(NA, length(source))
      for (j in 1:length(source)) {
        if (!any(grepl(paste0("^", source[j], "$"), target))) {
          inSource[j] <- source[j]
        }
      }
      inSource <- inSource[!is.na(inSource)]
      return(inSource)
    }
  )
)



