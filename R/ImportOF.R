#' @title R6 Class for importing on-farm data to an OFPE database
#'
#' @description R6 class for for importing data collected on-farms data into an
#' OFPE formatted database.
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
    #' Uploads data to an OFPE formatted database following a chain of mehtods
    #' and returning the status of the upload. The dot indicates that this
    #' function would be private if not for documentations sake.
    #' @param NAME Name of the data file to upload to the database.
    #' @return See 'status' object.
    .uploadData = function(NAME) { # MASTER FUNCTION
      tryCatch(
        {
          FILE <- self$.impDat(NAME)
        }, warning = function(w) {print()},
        error = function(e) {
          self$status[[which(names(self$status) == NAME)]] <-
            paste0("!!! ERROR IN impDat() FOR ", NAME, " !!!")
          print(paste0("!!! ERROR IN impDat() FOR " ,NAME, " !!!"))
        }
      )
      if (self$status[[which(names(self$status) == NAME)]] == 0) {
        tryCatch(
          {
            FILE <- self$.makeSptl(FILE, NAME)
          }, warning = function(w) {print()},
          error = function(e) {
            self$status[[which(names(self$status) == NAME)]] <-
              paste0("!!! ERROR IN makeSptl() FOR ", NAME, " !!!")
            print(paste0("!!! ERROR IN makeSptl() FOR ", NAME," !!!"))
          }
        )
      }
      if (self$status[[which(names(self$status) == NAME)]] == 0) {
        tryCatch(
          {
            FILE <- self$.oldDatClean(FILE)
          }, warning = function(w) {print()},
          error = function(e) {
            self$status[[which(names(self$status) == NAME)]] <-
              paste0("!!! ERROR IN oldDatClean() FOR ", NAME, " !!!")
            print(paste0("!!! ERROR IN oldDatClean() FOR ", NAME, " !!!"))
          }
        )
      }
      if (self$status[[which(names(self$status) == NAME)]] == 0) {
        tryCatch(
          {
            FILE <- self$.findInfo(FILE, NAME)
          },warning = function(w) {print()},
          error = function(e) {
            self$status[[which(names(self$status) == NAME)]] <-
              paste0("!!! ERROR IN findInfo() FOR ", NAME, " !!!")
            print(paste0("!!! ERROR IN findInfo() FOR ", NAME, " !!!"))
          }
        )
      }
      if (self$status[[which(names(self$status) == NAME)]] == 0) {
        tryCatch(
          {
            FILE <- self$.fixCols(FILE)
          }, warning = function(w) {print()},
          error = function(e) {
            self$status[[which(names(self$status) == NAME)]] <-
              paste0("!!! ERROR IN fixCols() FOR ", NAME, " !!!")
            print(paste0("!!! ERROR IN fixCols() FOR ", NAME, " !!!"))
          }
        )
      }
      if (self$status[[which(names(self$status) == NAME)]] == 0) {
        tryCatch(
          {
            FILE <- FILE %>%
              sf::st_transform("epsg:4326")
          }, warning = function(w) {print()},
          error = function(e) {
            self$status[[which(names(self$status) == NAME)]] <-
              paste0("!!! ERROR IN st_transform(", NAME,
                     ") TO LONGLAT WGS84 !!!")
            print(paste0("!!! ERROR IN st_transform(", NAME,
                         ") TO LONGLAT WGS84 !!!"))
          }
        )
      }
      if (self$status[[which(names(self$status) == NAME)]] == 0) {
        tryCatch(
          {
            FILE <- as(FILE, "Spatial")
          }, warning = function(w) {print()},
          error = function(e) {
            self$status[[which(names(self$status) == NAME)]] <-
              paste0("!!! ERROR CONVERTING ", NAME, " TO CLASS 'sp' !!!")
            print(paste0("!!! ERROR IN CONVERTING ", NAME, " TO CLASS 'sp' !!!"))
          }
        )
      }
      if (self$status[[which(names(self$status) == NAME)]] == 0) {
        self$.uploadFun(FILE, NAME, dbCon$db)
        self$status[[which(names(self$status) == NAME)]] <-
          paste0("IMPORT COMPLETE: ", NAME)
        # tryCatch(
        #   {
        #
        #   }, warning = function(w) {print()},
        #   error = function(e) {
        #     self$status[[which(names(self$status) == NAME)]] <-
        #       paste0("!!! ERROR UPLOADING ", NAME, " TO DB !!!")
        #     print(paste0("!!! ERROR UPLOADING ", NAME, " TO DB !!!"))
        #   }
        # )
      }
    },
    #' @description
    #' Imports data for uploading to an OFPE database. If data is a shapefile
    #' imported directly using 'sf' package, otherwise brings in data as .csv.
    #' The dot indicates that this function would be private if not for
    #' documentations sake.
    #' @param NAME Name of the data file to upload to the database.
    #' @return Imported data.
    .impDat = function(NAME) {
      if (grepl("shp$", NAME)) {
        FILE <- sf::read_sf(self$dat_path,
                            stringr::str_sub(NAME, 1, nchar(NAME) - 4)) %>%
          sf::st_zm()
      }
      if (grepl("csv$",NAME)) {
        tryCatch(
          {
            FILE <- data.table::fread(paste0(self$dat_path, NAME)) %>%
              as.data.frame()
            if (any(grepl("Longitude|Latitude", names(FILE)))) {
              names(FILE)[which(names(FILE) == "Longitude")] <- "x"
              names(FILE)[which(names(FILE) == "Latitude")] <- "y"
            }
          },
          warning = function(w) {
            print()},
          error = function(e) {
            tryCatch(
              {
                FILE <- read.csv(paste0(self$dat_path, NAME),
                                 skip=2, header=FALSE)
                header <- read.csv(paste0(self$dat_path, NAME),
                                   header = TRUE, nrows = 1)
                names(FILE) <- c(names(header), "utc_time", "y", "n", "x", "w")
                FILE$x <- ifelse(FILE$x > 0, FILE$x * -1, FILE$x)
              },
              warning = function(w) {
                print()},
              error = function(e) {
                print(paste0("Error: loading ", NAME, " !!!"))
              }
            )
          }
        )
      }
      if (length(FILE) == 1) {
        FILE <- read.csv(paste0(self$dat_path, NAME), skip=2, header=FALSE)
        header <- read.csv(paste0(self$dat_path, NAME), header = TRUE, nrows=1)
        names(FILE) <- c(names(header), "utc_time", "y", "n", "x", "w")
        FILE$x <- ifelse(FILE$x > 0, FILE$x * -1, FILE$x)
      }
      if (any(names(FILE) == "orig_file")) {
        names(FILE)[which(names(FILE) == "orig_file")] <- "orig_filePREV"
      }
      FILE$orig_file <- paste(NAME)
      if (grepl("GMC", NAME, ignore.case = TRUE)) {
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
    #' @param NAME The filename for the data imported from the .impDat method.
    #' @return Data in spatial format.
    .makeSptl = function(FILE, NAME) {
      tryCatch({
        if (grepl("csv$",NAME)) {
          FILE$X <- FILE$x
          FILE$Y <- FILE$y
          sp::coordinates(FILE) <- c("X", "Y") # makes spatial points df
          sp::proj4string(FILE) <- sp::CRS("+proj=longlat +datum=WGS84")
          FILE <- sf::st_as_sf(FILE)
        }
        if (is.na(raster::crs(FILE))) {
          sf::st_crs(FILE) <- 4326
        }
        if (!grepl("+datum=WGS84 +ellps=WGS84 +init=epsg:4326",
                   raster::crs(FILE))) {
          FILE <- sf::st_transform(FILE, "epsg:4326")
        }
        #******************************************************************
        bounds <- sf::st_bbox(FILE) # get boundary of file
        utmZone <- ceiling((bounds["xmin"] + 180) / 6) %>% as.numeric()
        utmEpsg <- ifelse(utmZone == 14,
                          32614, # use right epsg code for utm zone 14
                          32612) # use right epsg code for utm zone 12
        tempCRS <- paste0("epsg:", utmEpsg)
        FILE <- sf::st_transform(FILE, tempCRS)

        #******************************************************************
        return(FILE)
      },
      warning = function(w) {print()},
      error = function(e) {print(paste0("Error: making ", NAME, " spatial."))}
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
      return(FILE)
    },
    #' @description
    #' Identifies distinguishing features of the data, such as a field name if
    #' present, the corresponding farmer, and the year the data is collected.
    #' The dot indicates that this would be private if not for documentations
    #' sake.
    #' @param FILE The imported data for upload.
    #' @param NAME The name of the file for upload.
    #' @return Data without old columns.
    .findInfo = function(FILE, NAME) {
      anyFarmer <- 0 # index arg
      ## look for bad polygons
      if (any(grepl("geometry", names(FILE)))) {
        if (any(grepl("polygon", class(FILE$geometry), ignore.case=TRUE))) {
          FILE <- sf::st_buffer(FILE, dist=0)
        }
      } else {
        if (any(grepl("polygon", class(FILE$geom), ignore.case=TRUE))) {
          FILE <- sf::st_buffer(FILE, dist=0)
        }
      }
      utmEpsg <- ifelse(grepl("zone=12", raster::crs(FILE), ignore.case = TRUE),
                        32612,
                        32614)
      self$fields <-
        sf::st_transform(self$fields,
                         paste0("epsg:", utmEpsg))
      self$farms <-
        sf::st_transform(self$farms,
                         paste0("epsg:", utmEpsg))
      ## get info
      if (!grepl("ssurgo", NAME)) {
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
          if ((intscts / nrow(FILE)) > 0.80) {
            FILE <- suppressWarnings(sf::st_intersection(self$fields, FILE))
            anyFarmer <- 1
          } else {
            FILE$fieldname <- private$.findField(FILE)
            FILE$wfid <- 0
            FILE$fieldidx <- 0
            if (FILE$fieldname[1] == "unknown") {
              print(paste0("no fieldname in ", NAME, " identificed."))
            }
          }
        } else { # if not, look in column names for a like "field" to extract name for fieldname, fieldidx = 0 (non exp field)
          FILE$fieldname <- private$.findField(FILE)
          FILE$wfid <- 0
          FILE$fieldidx <- 0
          if (FILE$fieldname[1] == "unknown") {
            print(paste0("no fieldname in ", NAME, " identified."))
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
            print(paste0("no farmer in ", NAME, " identified."))
          }
        }
      }
      ## 3) update "year" column in FILE
      ## identify and extract the year from columns in table & update 'year' col in FILE
      FILE$year <- private$.findYear(FILE)
      if (FILE$year[1] == "unknown") {
        if (!grepl("ssurgo", NAME)) {
          print(paste0("no year in ", NAME, " identified."))
        }
      }
      return(FILE)
    },
    #' @description
    #' Goes through each file and replaces any column names with more palatable
    #' formats such as lower case and without any specialc characters. The dot
    #' indicates that this would be private if not for documentations sake.
    #' @param FILE The imported data for upload.
    #' @return Data with cleaned up column names.
    .fixCols = function(FILE) {
      colNames <- names(FILE) %>%
        lapply(tolower) %>%
        unlist() %>%
        lapply(noSpecialChar, FALSE) %>%
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
    #' @param NAME The name of the file for upload.
    #' @param db Connection to an OFPE database.
    #' @return Data uploaded to the database
    .uploadFun = function(FILE, NAME, db) {

      if (grepl("ssurgo", NAME)) {
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
      dtype <- private$.findDtype(FILE, NAME, NAMES)
      #******************************************************************
      #******************************************************************
      # check if table exists for data type in farmer's schema
      if (dtype == "rx") {
        tabExist <- as.logical(
          DBI::dbGetQuery(db,
                          paste0("SELECT EXISTS (
                                  SELECT 1
                                  FROM information_schema.tables
                                  WHERE table_schema = '",farmer,"_a'
                                  AND table_name = '",dtype,"')")
                          )
        )
      } else {
        if (dtype == "ssurgo") {
          tabExist <- as.logical(
            DBI::dbGetQuery(db,
                            paste0("SELECT EXISTS (
                                    SELECT 1
                                    FROM information_schema.tables
                                    WHERE table_schema = 'all_farms'
                                    AND table_name = '",dtype,"')")
                            )
          )
        } else {
          tabExist <- as.logical(
            DBI::dbGetQuery(db,
                            paste0("SELECT EXISTS (
                                    SELECT 1
                                    FROM information_schema.tables
                                    WHERE table_schema = '",farmer,"_r'
                                    AND table_name = '",dtype,"')")
                            )
          )
        }
      }
      # if not exists, create table called dtype and upload the data
      if (!tabExist) { # if exists = false this returns true
        if (dtype == "rx") {
          for (j in 1:ncol(FILE)) { # convert NA to NaN for db import
            if (anyNA(as.data.frame(FILE[, j])[1])) {
              naIndex <- which(is.na(as.data.frame(FILE[, j])[1]))
              FILE[naIndex[1]:naIndex[length(naIndex)], j] <- NaN
            }
          }
          suppressMessages(
            rpostgis::pgInsert(db,
                               c(paste0(farmer, "_a"),
                                 paste0(dtype)),
                               FILE,
                               geom = "geometry",
                               new.id = "gid")
          )
          # make field and year and orig_file primary keys to prevent overwriting (updates instead)
          invisible(
            DBI::dbGetQuery(
              db,
              paste0("ALTER TABLE ", farmer,"_a.", dtype,
                     " ADD PRIMARY KEY (gid, fieldname, year, orig_file)")
            )
          )
          for (j in 1:length(NAMES)) {
            tryCatch({
              DBI::dbSendQuery(db,
                               paste0("ALTER TABLE ", farmer,"_a.", dtype,
                                      " ALTER COLUMN ", NAMES[j], " TYPE TEXT"))
            },
            error = function(e) {
              print(paste0("warning in column ", NAMES[j],
                           " from ",FILE@data$orig_file[1]))
            })
          }
          # convert polygon table to multiploygon geometry
          if (any(grepl("poly",
                        class(sf::st_geometry(sf::st_as_sf(FILE))),
                        ignore.case = TRUE))) {
            # if it is NOT a multipolygon df
            if (!any(grepl("multi",
                           class(sf::st_geometry(sf::st_as_sf(FILE))),
                           ignore.case = TRUE))) {
              invisible(
                DBI::dbGetQuery(
                  db,
                  paste0("ALTER TABLE ", farmer, "_a.", dtype,
                         " ALTER COLUMN geometry
                         TYPE geometry(MultiPolygon,4326)
                         USING ST_Multi(geometry)")
                )
              )
            }
          }
          # create spatial index
          tryCatch({
            invisible(
              DBI::dbSendQuery(db,
                               paste0("CREATE INDEX ", dtype, "_geom_idx",
                                      " ON ", farmer, "_a.", dtype,
                                      " USING GIST (geometry)"))
            )},
            error=function(e) {
              print(paste0("error creating spatial index for ",
                           farmer, "_a.", dtype))
            }
          )
        } else {
          if (dtype=="ssurgo") {
            for (j in 1:ncol(FILE)) { # convert NA to NaN for db import
              if (anyNA(as.data.frame(FILE[, j])[1])) {
                naIndex <- which(is.na(as.data.frame(FILE[, j])[1]))
                FILE[naIndex[1]:naIndex[length(naIndex)], j] <- NaN
              }
            }
            suppressMessages(
              rpostgis::pgInsert(db,
                                 c(paste0("all_farms"),
                                   paste0(dtype)),
                                 FILE,
                                 geom = "geometry",
                                 new.id = "gid")
            ) #,return.pgi=TRUE
            # make field and year and orig_file primary keys to prevent overwriting (updates instead)
            invisible(
              DBI::dbGetQuery(
                db,
                paste0("ALTER TABLE all_farms.", dtype,
                       " ADD PRIMARY KEY (gid, farmidx, orig_file)")
              )
            )
            for (j in 1:length(NAMES)) {
              tryCatch({
                DBI::dbSendQuery(db,
                                 paste0("ALTER TABLE all_farms.", dtype,
                                        " ALTER COLUMN ", NAMES[j], " TYPE TEXT"))
              },
              error=function(e) {
                print(paste0("warning in column ",
                             NAMES[j],
                             " from ",
                             FILE@data$orig_file[1]))
              })
            }
            # convert polygon table to multiploygon geometry
            if (any(grepl("poly",
                          class(sf::st_geometry(sf::st_as_sf(FILE))),
                          ignore.case = TRUE))) { # if it is a polygon df
              if (!any(grepl("multi",
                             class(sf::st_geometry(sf::st_as_sf(FILE))),
                             ignore.case = TRUE))) { # if it is NOT a multipolygon df
                invisible(
                  DBI::dbGetQuery(
                    db,
                    paste0("ALTER TABLE all_farms.", dtype,
                           " ALTER COLUMN geometry
                           TYPE geometry(MultiPolygon,4326)
                           USING ST_Multi(geometry)")
                  )
                )
              }
            }
            # create spatial index
            tryCatch({
              invisible(
                DBI::dbSendQuery(db,
                                 paste0("CREATE INDEX ", dtype, "_geom_idx",
                                        " ON all_farms.", dtype,
                                        " USING GIST (geometry)"))
              )},
              error=function(e) {
                print(paste0("error creating spatial index for all_farms.",
                             dtype))
              }
            )
          } else { ## if not ssurgo or rx
            for (j in 1:ncol(FILE)) { # convert NA to NaN for db import
              if (anyNA(as.data.frame(FILE[, j])[1])) {
                naIndex <- which(is.na(as.data.frame(FILE[, j])[1]))
                FILE[naIndex[1]:naIndex[length(naIndex)], j] <- NaN
              }
            }
            suppressMessages(
              rpostgis::pgInsert(db,
                                 c(paste0(farmer, "_r"),
                                   paste0(dtype)),
                                 FILE,
                                 geom = "geometry",
                                 new.id = "gid")
            ) #,return.pgi=TRUE
            # make field and year and orig_file primary keys to prevent overwriting (updates instead)
            invisible(
              DBI::dbGetQuery(
                db,
                paste0("ALTER TABLE ", farmer, "_r.", dtype,
                       " ADD PRIMARY KEY (gid, fieldname, year, orig_file)")
              )
            )
            for (j in 1:length(NAMES)) {
              tryCatch({
                DBI::dbSendQuery(db,
                                 paste0("ALTER TABLE ", farmer, "_r.", dtype,
                                        " ALTER COLUMN ", NAMES[j], " TYPE TEXT"))
              },
              error=function(e) {
                print(paste0("warning in column ", NAMES[j],
                             " from ", FILE@data$orig_file[1]))
              })
            }
            # convert polygon table to multiploygon geometry
            if (any(grepl("poly",
                          class(sf::st_geometry(sf::st_as_sf(FILE))),
                          ignore.case = TRUE))) { # if it is a polygon df
              if (!any(grepl("multi",
                             class(sf::st_geometry(sf::st_as_sf(FILE))),
                             ignore.case = TRUE))) { # if it is NOT a multipolygon df
                invisible(
                  DBI::dbGetQuery(
                    db,
                    paste0("ALTER TABLE ", farmer,"_r.", dtype,
                           " ALTER COLUMN geometry
                           TYPE geometry(MultiPolygon,4326)
                           USING ST_Multi(geometry)")
                  )
                )
              }
            }
            # create spatial index
            tryCatch({
              invisible(
                DBI::dbSendQuery(
                  db,
                  paste0("CREATE INDEX ", dtype, "_geom_idx",
                         " ON ", farmer, "_r.", dtype,
                         " USING GIST (geometry)")
                )
              )},
              error=function(e) {
                print(paste0("error creating spatial index for ",
                             farmer, "_r.", dtype))
              }
            )
          }
        }
      } else { # if table does exist. if exists = true than if () returns false
        if (dtype=="rx") {
          # get column names from db
          dbCols <- DBI::dbGetQuery(
            db,
            paste0("SELECT column_name
                    FROM information_schema.columns
                    WHERE table_schema = '", farmer, "_a'
                   AND table_name = '", dtype,"'")
          )[,1]
          dbCols <- dbCols[c(-which(dbCols == "gid"),
                             -which(dbCols == "geometry"))] # take out "gid" & "geometry" column, will be added later
          # get columns from db that are not in file & columns from file that are not in db
          inDB <- private$.noMatch(dbCols, NAMES)
          inDF <- private$.noMatch(NAMES, dbCols)
          # add cols from db to file and cols from file to db
          if (length(inDB) > 0) {
            row.names(FILE) <- as.character(seq(1, nrow(FILE), 1))
            FILE <- maptools::spCbind(
              FILE,
              as.data.frame(matrix(NA, nrow(FILE), length(inDB)))
            )
            names(FILE) <- c(NAMES, inDB)
          }
          if (length(inDF) > 0) {
            for (j in 1:length(inDF)) {
              DBI::dbGetQuery(db,
                              paste0("ALTER TABLE ", farmer, "_a.", dtype,
                                     " ADD COLUMN ", inDF[j], " TEXT"))
            }
          }
          # convert NA to NaN for db import
          for (j in 1:ncol(FILE)) {
            if (anyNA(as.data.frame(FILE[, j])[1])) {
              naIndex <- which(is.na(as.data.frame(FILE[, j])[1]))
              FILE[naIndex[1]:naIndex[length(naIndex)], j] <- NaN
            }
          }
          # if geometry is polygon change to multipolygon
          if (any(grepl("poly",
                        class(sf::st_geometry(sf::st_as_sf(FILE))),
                        ignore.case = TRUE))) { # if it is a polygon df
            if (!any(grepl("multi",
                           class(sf::st_geometry(sf::st_as_sf(FILE))),
                           ignore.case = TRUE))) { # if it is NOT a multipolygon df
              FILE <- sf::st_cast(sf::st_as_sf(FILE), "MULTIPOLYGON")
              FILE$gid <- 1:nrow(FILE)
              tryCatch(sf::st_write(FILE,
                                    db,
                                    c(paste0(farmer,"_a"), dtype),
                                    append = TRUE),
                       error=function(e) {
                         print(paste0(FILE$orig_file[1],
                                      " already exists in database"))
                       })
              FILE <- as(FILE, "Spatial")
            } else {
              suppressMessages(
                rpostgis::pgInsert(db,
                                   c(paste0(farmer,"_a"),
                                     paste0(dtype)),
                                   FILE,
                                   geom = "geometry",
                                   new.id = "gid",
                                   upsert.using = c("gid",
                                                    "fieldname",
                                                    "year",
                                                    "orig_file"))
              )
              for (j in 1:length(NAMES)) {
                tryCatch({
                  DBI::dbSendQuery(
                    db,
                    paste0("ALTER TABLE ", farmer, "_a.", dtype,
                           " ALTER COLUMN ", NAMES[j], " TYPE TEXT")
                  )
                },
                error=function(e) {
                  print(paste0("warning in column ", NAMES[j],
                               " from ", FILE@data$orig_file[1]))
                })
              }
            }
          } else {
            # add file to database (upsert using field, year, orig_file) - gid added here
            suppressMessages(
              rpostgis::pgInsert(db,
                                 c(paste0(farmer,"_a"), paste0(dtype)),
                                 FILE,
                                 geom = "geometry",
                                 new.id = "gid",
                                 upsert.using = c("gid",
                                                "fieldname",
                                                "year",
                                                "orig_file"))
            )
            for (j in 1:length(NAMES)) {
              tryCatch({
                DBI::dbSendQuery(
                  db,
                  paste0("ALTER TABLE ", farmer, "_a.", dtype,
                         " ALTER COLUMN ", NAMES[j], " TYPE TEXT")
                )
              },
              error=function(e) {
                print(paste0("warning in column ", NAMES[j], " from ",
                             FILE@data$orig_file[1]))
              })
            }
          }
        } else {
          if (dtype=="ssurgo") {
            # get column names from db
            dbCols <- DBI::dbGetQuery(
              db,
              paste0("SELECT column_name
                     FROM information_schema.columns
                     WHERE table_schema = 'all_farms'
                     AND table_name = '",dtype,"'")
            )[,1]
            dbCols <- dbCols[c(-which(dbCols=="gid"),
                               -which(dbCols=="geometry"))] # take out "gid" & "geometry" column, will be added later
            # get columns from db that are not in file & columns from file that are not in db
            inDB <- private$.noMatch(dbCols, NAMES)
            inDF <- private$.noMatch(NAMES, dbCols)
            # add cols from db to file and cols from file to db
            if (length(inDB) > 0) {
              row.names(FILE) <- as.character(seq(1, nrow(FILE), 1))
              FILE <- maptools::spCbind(
                FILE,
                as.data.frame(matrix(NA, nrow(FILE), length(inDB)))
              )
              names(FILE) <- c(NAMES, inDB)
            }
            if (length(inDF) > 0) {
              for (j in 1:length(inDF)) {
                DBI::dbGetQuery(
                  db,
                  paste0("ALTER TABLE all_farms.", dtype,
                         " ADD COLUMN ", inDF[j], " TEXT")
                )
              }
            }
            # convert NA to NaN for db import
            for (j in 1:ncol(FILE)) {
              if (anyNA(as.data.frame(FILE[, j])[1])) {
                naIndex <- which(is.na(as.data.frame(FILE[, j])[1]))
                FILE[naIndex[1]:naIndex[length(naIndex)], j] <- NaN
              }
            }
            # if geometry is polygon change to multipolygon
            if (any(grepl("poly",
                          class(sf::st_geometry(sf::st_as_sf(FILE))),
                          ignore.case = TRUE))) { # if it is a polygon df
              if (!any(grepl("multi",
                             class(sf::st_geometry(sf::st_as_sf(FILE))),
                             ignore.case = TRUE))) { # if it is NOT a multipolygon df
                FILE <- sf::st_cast(sf::st_as_sf(FILE), "MULTIPOLYGON")
                FILE$gid <- 1:nrow(FILE)
                tryCatch(sf::st_write(FILE,
                                      db,
                                      c(paste0("all_farms"), dtype),
                                      append=TRUE),
                         error=function(e) {
                           print(paste0(FILE$orig_file[1],
                                        " already exists in database"))
                         })
                FILE <- as(FILE, "Spatial")
              } else {
                suppressMessages(
                  rpostgis::pgInsert(db,
                                     c(paste0("all_farms"),paste0(dtype)),
                                     FILE,
                                     geom = "geometry",
                                     new.id = "gid",
                                     upsert.using = c("gid",
                                                      "farmidx",
                                                      "orig_file"))
                )
                for (j in 1:length(NAMES)) {
                  tryCatch({
                    DBI::dbSendQuery(
                      db,
                      paste0("ALTER TABLE all_farms.", dtype,
                             " ALTER COLUMN ", NAMES[j], " TYPE TEXT")
                    )
                  },
                  error=function(e) {
                    print(paste0("warning in column ", NAMES[j],
                                 " from ", FILE@data$orig_file[1]))
                  })
                }
              }
            } else {
              # add file to database (upsert using farm, orig_file) - gid added here
              suppressMessages(
                rpostgis::pgInsert(db,
                                   c(paste0("all_farms"), paste0(dtype)),
                                   FILE,
                                   geom = "geometry",
                                   new.id = "gid",
                                   upsert.using = c("gid",
                                                    "farmidx","
                                                    orig_file"))
              )
              for (j in 1:length(NAMES)) {
                tryCatch({
                  DBI::dbSendQuery(
                    db,
                    paste0("ALTER TABLE all_farms.", dtype,
                           " ALTER COLUMN ", NAMES[j], " TYPE TEXT")
                  )
                },
                error=function(e) {
                  print(paste0("warning in column ", NAMES[j],
                               " from ", FILE@data$orig_file[1]))
                })
              }
            }
          } else { ## if not rx or ssurgo data
            # get column names from db
            dbCols <- DBI::dbGetQuery(
              db,
              paste0("SELECT column_name
                     FROM information_schema.columns
                     WHERE table_schema = '", farmer, "_r'
                     AND table_name = '", dtype, "'")
            )[,1]
            dbCols <- dbCols[c(-which(dbCols=="gid"),
                               -which(dbCols=="geometry"))] # take out "gid" & "geometry" column, will be added later
            # get columns from db that are not in file & columns from file that are not in db
            inDB <- private$.noMatch(dbCols, NAMES)
            inDF <- private$.noMatch(NAMES, dbCols)
            # add cols from db to file and cols from file to db
            if (length(inDB) > 0) {
              row.names(FILE) <- as.character(seq(1, nrow(FILE), 1))
              FILE <- maptools::spCbind(
                FILE,
                as.data.frame(matrix(NA, nrow(FILE), length(inDB)))
              )
              names(FILE) <- c(NAMES, inDB)
            }
            if (length(inDF) > 0) {
              for (j in 1:length(inDF)) {
                DBI::dbGetQuery(
                  db,
                  paste0("ALTER TABLE ", farmer, "_r.", dtype,
                         " ADD COLUMN ", inDF[j], " TEXT")
                )
              }
            }
            # convert NA to NaN for db import
            for (j in 1:ncol(FILE)) {
              if (anyNA(as.data.frame(FILE[,j])[1])) {
                naIndex <- which(is.na(as.data.frame(FILE[,j])[1]))
                FILE[naIndex[1]:naIndex[length(naIndex)],j] <- NaN
              }
            }
            # if geometry is polygon change to multipolygon
            if (any(grepl("poly",
                          class(sf::st_geometry(sf::st_as_sf(FILE))),
                          ignore.case = TRUE))) { # if it is a polygon df
              if (!any(grepl("multi",
                             class(sf::st_geometry(sf::st_as_sf(FILE))),
                             ignore.case = TRUE))) { # if it is NOT a multipolygon df
                FILE <- sf::st_cast(sf::st_as_sf(FILE), "MULTIPOLYGON")
                FILE$gid <- 1:nrow(FILE)
                tryCatch(sf::st_write(FILE,
                                      db,
                                      c(paste0(farmer,"_r"), dtype),
                                      append=TRUE),
                         error=function(e) {
                           print(paste0(FILE$orig_file[1],
                                        " already exists in database"))
                         })
                FILE <- as(FILE, "Spatial")
              } else {
                suppressMessages(
                  rpostgis::pgInsert(db,
                                     c(paste0(farmer,"_r"), paste0(dtype)),
                                     FILE,
                                     geom = "geometry",
                                     new.id = "gid",
                                     upsert.using = c("gid",
                                                      "fieldname",
                                                      "year",
                                                      "orig_file"))
                )
                for (j in 1:length(NAMES)) {
                  tryCatch({
                    DBI::dbSendQuery(
                      db,
                      paste0("ALTER TABLE ", farmer, "_r.", dtype,
                             " ALTER COLUMN ", NAMES[j], " TYPE TEXT")
                    )
                  },
                  error=function(e) {
                    print(paste0("warning in column ", NAMES[j],
                                 " from ", FILE@data$orig_file[1]))
                  })
                }
              }
            } else {
              # add file to database (upsert using field, year, orig_file) - gid added here
              suppressMessages(
                rpostgis::pgInsert(db,
                                   c(paste0(farmer,"_r"), paste0(dtype)),
                                   FILE,
                                   geom = "geometry",
                                   new.id = "gid",
                                   upsert.using = c("gid",
                                                    "fieldname",
                                                    "year",
                                                    "orig_file"))
              ) # ,"geometry"
              for (j in 1:length(NAMES)) {
                tryCatch({
                  DBI::dbSendQuery(
                    db,
                    paste0("ALTER TABLE ", farmer,"_r.", dtype,
                           " ALTER COLUMN ", NAMES[j], " TYPE TEXT")
                  )
                },
                error=function(e) {
                  print(paste0("warning in column ", NAMES[j],
                               " from ", FILE@data$orig_file[1]))
                })
              }
            }
          }
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
          noSpecialChar() %>%
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
    .extractFarmer = function(FARMER, COL, FILE) {
      # second element of FARMER is farmer name
      if (any(grepl(FARMER[2],FILE[1,COL],ignore.case = TRUE))) {
        farmeridx <- as.numeric(
          DBI::dbGetQuery(db,
                          paste0("SELECT farmers.farmeridx
                                 FROM all_farms.farmers
                                 WHERE farmers.farmer = '",FARMER[2],"'"))
        )
      } else {
        farmeridx <- NA
      }
    },
    .findYear = function(FILE) {
      ## look for a year column
      if (any(grepl("year", names(FILE), ignore.case = TRUE))) {
        year <- FILE$year[1]
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
      strLocs <- stringr::str_locate(OGfile, "20")
      if (!anyNA(strLocs)) {
        yr <- suppressWarnings(
          stringr::str_sub(OGfile, strLocs[2] + 1, strLocs[2] + 2) %>%
            as.numeric()
          )
        if (!is.na(yr)) {
          year <- stringr::str_sub(OGfile, strLocs[1], strLocs[1] + 3)
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
      strLocs <- stringr::str_locate(VEC[1], "20")
      if (!anyNA(strLocs)) {
        # warnings are if it is NA, which is okay (see next lines logic)
        yr <- suppressWarnings(
          stringr::str_sub(VEC[1], strLocs[2] + 1, strLocs[2] + 2) %>%
            as.numeric()
        )
        if (!is.na(yr)) {
          year <- stringr::str_sub(VEC[1], strLocs[1], strLocs[1] + 3)
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
    .findDtype = function(FILE, NAME, NAMES) {
      dtype <- NA
      #******************************************************************
      ## < NOTE: UPDATE grepl() STATEMENTS BELOW IF NEW KEYWORDS >
      #******************************************************************
      if (is.na(dtype) & grepl("ssurgo", NAME)) {
        dtype <- "ssurgo"
      }
      # RX or ssopt data
      if (is.na(dtype) &
          any(grepl("RX|ssopt", NAME, ignore.case = TRUE)|
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
              any(grepl("yld", NAME, ignore.case = TRUE)))) {
        if (!any(grepl("rate|bulk_rt|blk_rt_", NAMES, ignore.case = TRUE))) {
          dtype <- "yld"
        } else {
          dtype <- NA
        }
      }
      # Protein data - looks for the string protein and sample_id (cropscan formate)
      if (is.na(dtype) &
          any(grepl("protein", NAMES, ignore.case = TRUE)) |
          any(grepl("pro", NAMES, ignore.case = TRUE))) {
        dtype <- "pro"
      }
      # N data - looks for colnames containing "rate" or "AA" (as-applied) and doesn't include "seed"
      if (is.na(dtype) &
          any(grepl("rate|AA", NAMES, ignore.case = TRUE))|
          any(grepl("rate|aaN|AA|_aa|Rate|RATE|AA_N|_N_",
                    NAME, ignore.case = FALSE))) {
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
      if (is.na(dtype) & any(grepl("seed|_SR_", NAME, ignore.case = TRUE))) {
        dtype <- "aa_sr"
      }
      # looks for seed or rate b/c N should have been identified earlier
      if (is.na(dtype) & any(grepl("seed|rate",NAMES,ignore.case = TRUE))) {
        dtype <- "aa_sr"
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



