#' @title R6 Class for adding farm information in OFPE database.
#'
#' @description R6 class that takes a vector of farm names and a path
#' to the location of farm boundary shapefiles. Uploads boundaries to the
#' database. Adds a unique ID field for each farm that is used for
#' internal relationships and identifies the associated farmer from the
#' names of the farms. If a farm is already present in the database,
#' it will be replaced with the same farm ID. This follows the ManageDB
#' interface and includes a setup and execute method that is called from
#' the ManageDB class.
#' @export
AddFarms <- R6::R6Class(
  "AddFarms",
  public = list(
    #' @field dbCon Database connection object, see DBCon class.
    dbCon = NULL,
    #' @field farm_names Vector of farm names to add to the database.
    farm_names = NULL,
    #' @field farm_farmer_names Vector of farmer names reflecting the
    #' owner of the farm names supplied.
    farm_farmer_names = NULL,
    #' @field farm_path File path to the location of farm boundary shapefiles.
    farm_path = NULL,

    #' @description
    #' Create an AddFarms object. The database connection and a vector of
    #' farm names and farmer names are used to upload farm information
    #' to the database with the appropriate owner information. The farm_path
    #' argument indicates the file path in which to follow for the farm boundary
    #' shapefiles that are to be uploaded to the database. Instantiates the class
    #' for running the setup and execute methods.
    #' @param dbCon Database connection object, see DBCon class.
    #' @param farm_names Vector of farm names to add to the database.
    #' @param farm_farmer_names Vector of farmer names reflecting the
    #' owner of the farm names supplied.
    #' @param farm_path File path to the location of farm boundary shapefiles.
    #' @return A new initialized 'AddFarms' object.
    initialize = function(dbCon, farm_names, farm_farmer_names, farm_path) {
      stopifnot(
        !is.null(farm_names),
        !is.character(farm_names),
        !is.null(farm_farmer_names),
        !is.character(farm_farmer_names),
        !is.null(farm_path),
        !is.character(farm_path)
      )
      self$dbCon <- dbCon
      self$farm_names <- farm_names
      self$farm_farmer_names <- farm_farmer_names
      self$farm_path <- farm_path
    },
    #' @description
    #' Prepares the farm data prior to uploading to the database. Imports
    #' shapefiles from the farm path and puts them into a list with the
    #' corresponding farmer to be uploaded to the database.
    #' @param None No arguments needed because they are provided during class
    #' initialization.
    #' @return Prepared R6 class for uploading farmer information.
    setup = function() {


    },
    #' @description
    #' Executes the upload of prepared farm information to the database in the
    #' 'all_farms' schema and the 'farms' table. If a farm boundary is already
    #' present with the same farm name and farmer that owns it, the entry in the
    #' database is replaced.
    #' @param None No arguments needed because they are provided during class
    #' initialization.
    #' @return Farm data uploaded into the 'all_farms.farms' table in the database.
    execute = function() {


    }
  )
)

