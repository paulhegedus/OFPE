#' @title R6 Class for managing information in the OFPE database
#'
#' @description R6 class for for adding information to the OFPE database
#' such as new farmer names, farm names and boundaries, and experimental
#' field names and boundaries used in OFPE. Users can build and add their
#' own methods.
#' @seealso \code{\link{DBCon}} for database connection class,
#' \code{\link{ManageFarmers}} for the class that manages farmer data,
#' \code{\link{ManageFarms}} for the class that manages farm data, and
#' \code{\link{ManageFields}} for the class that manages field data.
#' @export
ManageDB <- R6::R6Class(
  "ManageDB",
  public = list(
    #' @field dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    dbCon = NULL,
    #' @field action_list List of lists with the names of the action methods
    #' to execute and their associated parameters.
    action_list = NULL,
    #' @field do_actions List of action methods initialized for setup and
    #' execution.
    do_actions = NULL,

    #' @description
    #' Create a database manager object. The database connection and each
    #' action class list is passed to an internal method that initializes
    #' the specified action classes.
    #' @param dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    #' @param action_list List of lists with the names of the action methods
    #' to execute and their associated parameters.
    #' @return A new 'ManageDB' object with initialized action classes.
    initialize = function(dbCon, action_list) {
      stopifnot(!is.null(action_list))
      self$dbCon <- dbCon
      self$action_list <- action_list
      self$do_actions <- lapply(self$action_list, private$.initializeActions)
    },
    #' @description
    #' Prepares the R6 action classes supplied by the user in the
    #' 'action_list' by running their setup methods that preprocess data
    #' prior to uploading to the OFPE database.
    #' @param None There are no arguments to this method as the list of
    #' actions was supplied upon initialization of the ManageDB class.
    #' @return Prepared R6 action classes specified by the user.
    setupActions = function() {
      lapply(self$do_actions, private$.setupActions)
    },
    #' @description
    #' Executes the upload method of each action class that was passed in by
    #' the user and has been initialized and set up. These methods upload the
    #' data prepared by each action's setup method.
    #' @param None No arguments needed because they are provided during class
    #' initialization.
    #' @return Data uploaded into the database.
    executeActions = function() {
      lapply(self$do_actions, private$.executeActions)
    }
  ),
  private = list(
    .initializeActions = function(action_item) {
      init_text <- "$new(self$dbCon, action_item)"
      return(eval(parse(text = paste0(action_item$action, init_text))))
    },
    .setupActions = function(action) {
      action$setup()
    },
    .executeActions = function(action) {
      action$execute()
    }
  )
)
