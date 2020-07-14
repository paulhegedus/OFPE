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
    #' Create a database manager object. The database connection and each
    #' action class list is passed to an internal method that initializes
    #' the specified action classes.
    #' @param db Database connection.
    #' @param action_list List of lists with the names of the action methods
    #' to execute and their associated parameters.
    #' @return A new 'ManageDB' object with initialized action classes.
    initialize = function(db = NA, action_list = NA) {
      stopifnot(
        !is.null(action_list)
      )
      self$db <- db
      self$action_list <- action_list
      self$do_actions <- lapply(self$action_list, private$.initializeActions)
    },
    #' @description
    #' Prepares the R6 action classes supplied by the user in the
    #' 'action_list' by running their setup methods that preprocess data
    #' prior to uploading to the OFPE database.
    #' @param None There are no arguments to this method as the list of
    #' actions was supplied upong initialization of the ManageDB class.
    #' @return Prepared R6 action classes specified by the user.
    setupActions = function() {
      lapply(self$do_actions, private$.setupActions)
    },
    #' @description
    #' Executes the upload method of each action class that was passed in by
    #' the user and has been initialzied and set up. These methods upload the
    #' data prepared by each action's setup method.
    #' @param None No arguments needed because they are provided during class
    #' initialization.
    #' @return Data uploaded into the database.
    executeActions = function() {
      lapply(self$action_list, private$.executeActions)
    }
  ),
  private = list(
    .initializeActions = function(action_item) {
      init_text <- "$new(self$db, action_item)"
      return(eval(parse(text = paste0(action_item$action, init_text))))
    },
    .setupActions = function(action) {
      action$setup()
    },
    .executeActions = function(action) {
      action$upload()
    }
  )
)
