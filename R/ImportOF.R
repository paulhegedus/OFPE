#' @title R6 Class for importing OFPE database
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
      self$status <- as.list(self$file_names) %>%
        `names<-`(self$file_names)
    },
    #' @description
    #' On-farm data import execution.
    #' @param None No arguments needed because of class instantiation.
    #' @return See 'status' object.
    executeUpload = function() {
      lapply(self$file_names, self$.uploadData) %>%
        invisible()
    },
    #' @description
    #' Uploads data to an OFPE formatted database
    #' @param NAME Name of the data file to upload to the database.
    #' @return See 'status' object.
    .uploadData = function(NAME) { # MASTER FUNCTION
      # tryCatch(
      #   {
      #     FILE <- self$.impDat(NAME)
      #   }, warning = function(w) {print()},
      #   error = function(e) {
      #     self$status[[which(names(self$status) == NAME)]] <-
      #       paste0("!!! ERROR IN impDat() FOR ", NAME, " !!!")
      #     print(paste0("!!! ERROR IN impDat() FOR " ,NAME, " !!!"))
      #   }
      # )
      # if (self$status[[which(names(self$status) == NAME)]] == 0) {
      #   tryCatch(
      #     {
      #       FILE <- self$.makeSptl(FILE, NAME)
      #     }, warning = function(w) {print()},
      #     error = function(e) {
      #       self$status[[which(names(self$status) == NAME)]] <-
      #         paste0("!!! ERROR IN makeSptl() FOR ", NAME, " !!!")
      #       print(paste0("!!! ERROR IN makeSptl() FOR ", NAME," !!!"))
      #     }
      #   )
      # }
      # if (self$status[[which(names(self$status) == NAME)]] == 0) {
      #   tryCatch(
      #     {
      #       FILE <- self$.oldDatClean(FILE)
      #     }, warning = function(w) {print()},
      #     error = function(e) {
      #       self$status[[which(names(self$status) == NAME)]] <-
      #         paste0("!!! ERROR IN oldDatClean() FOR ", NAME, " !!!")
      #       print(paste0("!!! ERROR IN oldDatClean() FOR ", NAME, " !!!"))
      #     }
      #   )
      # }
      # if (self$status[[which(names(self$status) == NAME)]] == 0) {
      #   tryCatch(
      #     {
      #       FILE <- self$.findInfo(FILE, NAME, farmers, fields)
      #     },warning = function(w) {print()},
      #     error = function(e) {
      #       self$status[[which(names(self$status) == NAME)]] <-
      #         paste0("!!! ERROR IN findInfo() FOR ", NAME, " !!!")
      #       print(paste0("!!! ERROR IN findInfo() FOR ", NAME, " !!!"))
      #     }
      #   )
      # }
      # if (self$status[[which(names(self$status) == NAME)]] == 0) {
      #   tryCatch(
      #     {
      #       FILE <- self$.fixCols(FILE)
      #     }, warning = function(w) {print()},
      #     error = function(e) {
      #       self$status[[which(names(self$status) == NAME)]] <-
      #         paste0("!!! ERROR IN fixCols() FOR ", NAME, " !!!")
      #       print(paste0("!!! ERROR IN fixCols() FOR ", NAME, " !!!"))
      #     }
      #   )
      # }
      # if (self$status[[which(names(self$status) == NAME)]] == 0) {
      #   tryCatch(
      #     {
      #       FILE <- sf::st_transform(FILE,
      #                                raster::crs("+proj=longlat +datum=WGS84"))
      #     }, warning = function(w) {print()},
      #     error = function(e) {
      #       self$status[[which(names(self$status) == NAME)]] <-
      #         paste0("!!! ERROR IN st_transform(", NAME,
      #                ") TO LONGLAT WGS84 !!!")
      #       print(paste0("!!! ERROR IN st_transform(", NAME,
      #                    ") TO LONGLAT WGS84 !!!"))
      #     }
      #   )
      # }
      # if(self$status[[which(names(self$status) == NAME)]] == 0) {
      #   tryCatch(
      #     {
      #       FILE <- as(FILE, "Spatial")
      #     }, warning = function(w) {print()},
      #     error = function(e) {
      #       self$status[[which(names(self$status) == NAME)]] <-
      #         paste0("!!! ERROR CONVERTING ", NAME, " TO CLASS 'sp' !!!")
      #       print(paste0("!!! ERROR IN CONVERTING ", NAME, " TO CLASS 'sp' !!!"))
      #     }
      #   )
      # }
      # if(self$status[[which(names(self$status) == NAME)]] == 0) {
      #   if (grepl("ssurgo", NAME)) {
      #     FILE@data$year <- NULL
      #     NAMES <- names(FILE)
      #   }else{
      #     NAMES <- names(FILE)
      #   }
      #   tryCatch(
      #     {
      #       self$.uploadFun(FILE, NAMES, NAME)
      #       self$status[[which(names(self$status) == NAME)]] <-
      #         paste0("IMPORT COMPLETE: ", NAME)
      #     }, warning = function(w) {print()},
      #     error = function(e) {
      #       self$status[[which(names(self$status) == NAME)]] <-
      #         paste0("!!! ERROR UPLOADING ", NAME, " TO DB !!!")
      #       print(paste0("!!! ERROR UPLOADING ", NAME, " TO DB !!!"))
      #     }
      #   )
      # }
    }

  )
)



