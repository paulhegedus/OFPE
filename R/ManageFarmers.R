#' @title R6 Class for adding farmer information in OFPE database.
#'
#' @description R6 class that takes a list with a vector of farmer names and
#' a database connection object. Adds a unique ID field for each farmer that
#' is used for internal relationships. If a farmer is already present in the
#' database, it will be skipped. This follows the ManageDB interface and
#' includes a setup and execute method that is called from the ManageDB class.
#' @export
ManageFarmers <- R6::R6Class(
  "ManageFarmers",
  public = list(
    #' @field dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    dbCon = NULL,
    #' @field farmers Vector of farmer names to add to the database.
    farmers = NULL,

    #' @description
    #' Create an ManageFarmers object. The database connection and a vector of
    #' farmer names are used to upload farmer information to the database.
    #' Instantiates the class for running the setup and execute methods.
    #' @param dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    #' @param action_item List of inputs to the ManageFarmers method, from the
    #' list passed into the ManageDB class. This includes a vector of farmer
    #' names to add to the 'all_farms.farmers' table of an OFPE formatted
    #' database.
    #' @return A new initialized 'ManageFarmers' object.
    initialize = function(dbCon, action_item) {
      stopifnot(
        !is.null(action_item),
        !is.null(action_item$farmers),
        is.character(action_item$farmers)
      )
      self$dbCon <- dbCon
      self$farmers <- action_item$farmers %>% tolower()
    },
    #' @description
    #' No setup needed for the ManageFarmers class. However is still present to
    #' abide by the ManageDB interface consisting of setup and execute methods.
    setup = function() {},
    #' @description
    #' Executes the upload of prepared farmer information to the database in the
    #' 'all_farms' schema and the 'farmers' table. If a farmer is already
    #' present with the same name and ID, it will be skipped.
    #' @param None No arguments needed because they are provided during class
    #' initialization.
    #' @return Farmer data uploaded into 'all_farms.farmers' table in the database.
    execute = function() {
      sapply(self$farmers, self$.uploadFarmers, self$dbCon$db)
    },
    #' @description
    #' Uploads a farmer to the database. Takes the farmer name and adds it to the
    #' 'all_farms.farmers' table. Makes sure there is no conflict defined in the
    #' .buildTables method of the BuildDB class. A farmeridx is automatically
    #' generated upon upload by PostgreSQL. The dot indicates that this function
    #' would be private if not for documentations sake.
    #' @param db Database connection.
    #' @param farmer Name of a farmer for upload into 'all_farms.farmers'.
    #' @return Farmer upload into database.
    .uploadFarmers = function(farmer, db) {
      DBI::dbSendQuery(
        db,
        paste0("INSERT INTO all_farms.farmers(farmer)
               VALUES ('", farmer, "')
               ON CONFLICT ON CONSTRAINT norepfarmers
               DO UPDATE SET farmer = EXCLUDED.farmer;")
      )
    }
  )
)

