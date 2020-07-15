#' @title R6 Class for adding farmer information in OFPE database.
#'
#' @description R6 class that takes a vector of farmer names to add to the
#' database. Adds a unique ID field for each farmer that is used for
#' internal relationships. If a farmer is already present in the database,
#' it will be skipped. This follows the ManageDB interface and includes
#' a setup and execute method that is called from the ManageDB class.
#' @export
AddFarmers <- R6::R6Class(
  "AddFarmers",
  public = list(
    #' @field dbCon Database connection object, see DBCon class.
    dbCon = NULL,
    #' @field farmers Vector of farmer names to add to the database.
    farmers = NULL,

    #' @description
    #' Create an AddFarmers object. The database connection and a vector of
    #' farmer names are used to upload farmer information to the database.
    #' Instantiates the class for running the setup and execute methods.
    #' @param dbCon Database connection object, see DBCon class.
    #' @param farmers Vector of farmer names to add to the database.
    #' @return A new initialized 'AddFarmers' object.
    initialize = function(dbCon, farmers) {
      stopifnot(
        !is.null(farmers),
        !is.character(farmers)
      )
      self$dbCon <- dbCon
      self$farmers <- farmers
    },
    #' @description
    #' Prepares the farmer data prior to uploading to the database.
    #' Queries the database for existing farmer information, and if found
    #' matches existing farmer ID's to farmer names where needed, else
    #' provides a new farmer ID.
    #' @param None No arguments needed because they are provided during class
    #' initialization.
    #' @return Prepared R6 class for uploading farmer information.
    setup = function() {


    },
    #' @description
    #' Executes the upload of prepared farmer information to the database in the
    #' 'all_farms' schema and the 'farmers' table. If a farmer is already
    #' present with the same name and ID, it will be skipped.
    #' @param None No arguments needed because they are provided during class
    #' initialization.
    #' @return Farmer data uploaded into 'all_farms.farmers' table in the database.
    execute = function() {


    }
  )
)

