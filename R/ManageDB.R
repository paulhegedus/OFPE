#' @title R6 Class for managing information in the OFPE database
#'
#' @description R6 class for for adding information to the OFPE database
#' such as new farmer names, farm names and boundaries, and experimental
#' field names and boundaries used in OFPE. Users can build and add their
#' own methods.
#' @export
ManageDB <- R6::R6Class(
  "ManageDB",
  public = list(
    #' @field db Database connection, such as PostgreSQLConnection.
    db = NULL,
    #' @field action_list List of lists with the names of the action methods
    #' to execute and their associated parameters.
    action_list = NULL,
    #' @field do_action List of action methods initialized for execution.
    do_action = NULL,

    #' @description
    #' Create a database manager object.
    #' @param db Database connection.
    #' @param action_list List of lists with the names of the action methods
    #' to execute and their associated parameters.
    #' @return A new 'ManageDB' object.
    initialize = function(db = NA, action_list = NA) {
      stopifnot(
        !is.null(action_list)
      )
      self$db <- db
      self$action_list <- action_list

      self$do_actions <- lapply(self$action_list, private$.loadActions)
    },
    #' @description
    #' Execute database manager action methods that the user specifies.
    #' @param None No arguments needed because they are provided during class
    #' initialization.
    executeAction = function() {
      lapply(self$action_list, private$.runActions)
    },
    #' @description
    #' Initializes the a
    #' @param db Connection to a database.
    #' @param postgis_version PostGIS version installed.
    #' @return Enabled database extensions.
    #' @source \url{https://trac.osgeo.org/postgis/wiki/UsersWikiCreateFishnet}
    initializeActions = function() {


    }
  ),
  private = list(
    .runActions = function(action) {
      action$setUp()
      action$upload()
    }
  )
)
#' Loads PostGIS tools and
#' a function for generating a net across an area of interest. From PostGIS website, see source.
