#' @title R6 Class for adding or updating field information in OFPE database.
#'
#' @description R6 class that takes a vector of field names and a path
#' to the location of field boundary shapefiles. Uploads boundaries to the
#' database. Adds a unique ID field for each field that is used for
#' internal relationships and identifies the associated farmer with the fields.
#' If a field is already present in the database,
#' it will be replaced with the same field ID. This follows the ManageDB
#' interface and includes a setup and execute method that is called from
#' the ManageDB class.
#' @export
ManageFields <- R6::R6Class(
  "ManageFields",
  public = list(
    #' @field dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    dbCon = NULL,
    #' @field field_names Vector of field names to add or update in the database.
    field_names = NULL,
    #' @field field_farmer_names Vector of farmer names reflecting the
    #' owner of the field names supplied.
    field_farmer_names = NULL,
    #' @field field_path File path to the location of field boundary shapefiles.
    field_path = NULL,

    #' @description
    #' Create an ManageFields object. The database connection and a vector of
    #' field names and farmer names are used to upload field information
    #' to the database with the appropriate owner information. The field_path
    #' argument indicates the file path in which to follow for the field boundary
    #' shapefiles that are to be uploaded to the database. Instantiates the class
    #' for running the setup and execute methods.
    #' @param dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    #' @param field_names Vector of field names to add or update in the database.
    #' @param field_farmer_names Vector of farmer names reflecting the
    #' owner of the field names supplied.
    #' @param field_path File path to the location of field boundary shapefiles.
    #' @return A new initialized 'AddFarms' object.
    initialize = function(dbCon, field_names, field_farmer_names, field_path) {
      stopifnot(
        !is.null(field_names),
        !is.character(field_names),
        !is.null(field_farmer_names),
        !is.character(field_farmer_names),
        !is.null(field_path),
        !is.character(field_path)
      )
      self$dbCon <- dbCon
      self$field_names <- field_names
      self$field_farmer_names <- field_farmer_names
      self$field_path <- field_path
    },
    #' @description
    #' Prepares the field data prior to uploading to the database. Imports
    #' shapefiles from the field path and puts them into a list with the
    #' corresponding farmer to be uploaded to the database.
    #' @param None No arguments needed because they are provided during class
    #' initialization.
    #' @return Prepared R6 class for uploading farmer information.
    setup = function() {


    },
    #' @description
    #' Executes the upload of prepared field information to the database in the
    #' 'all_farms' schema and the 'fields' table. If a field boundary is already
    #' present with the same field name and farmer that owns it, the entry in the
    #' database is replaced.
    #' @param None No arguments needed because they are provided during class
    #' initialization.
    #' @return Farm data uploaded into the 'all_farms.fields' table in the database.
    execute = function() {


    }
  )
)

