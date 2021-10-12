#' @title R6 Class for connecting to a database.
#'
#' @description R6 class for for connecting to your OFPE
#' database. The default database driver is PostgreSQL as that
#' is recommended for OFPE use and shown in the OFPE tutorials and
#' vignettes. However, any database driver can be supplied. This class
#' was created to consolidate database connections being supplied to
#' functions and other R6 classes in the OFPE data workflow.
#' 
#' Users can connect with any DBI compliant option. Using RPostgreSQL drivers on 
#' a Mac, users can provide the user, password, dbname, host, and port. If on a 
#' Windows computer the user just needs to set up a PostgreSQL ODBC driver, and 
#' supply the DSN, user, and password.
#' @export
DBCon <- R6::R6Class(
  "DBCon",
  public = list(
    #' @field db Database connection, such as PostgreSQL connection.
    db = NULL,

    #' @description
    #' Create a database connection inside of the DBCon class.
    #' @param user Username associated with the database owner.
    #' @param password Password associated with the database owner.
    #' @param dbname Name of the database to connect to.
    #' @param host Host name of the database location (typically localhost)
    #' unless user is connecting to a non-local database.
    #' @param port The port number of the database.
    #' @param dsn Optional. If ODBC database source name is setup. Required for 
    #' Windows users.
    #' @return An open database connection.
    initialize = function(user = NA,
                          password = NA,
                          dbname = NULL,
                          host = NULL,
                          port = NULL,
                          dsn = NULL) {
      stopifnot(
        !is.null(user),
        !is.null(password)
      )
      
      if (!is.null(dsn)) {
        self$db <- DBI::dbConnect(
          drv = odbc::odbc(),
          dsn = dsn,
          user = user,
          password = password,
          maxvarcharsize = 0
        )
      } else {
        stopifnot(
          !is.null(dbname),
          !is.null(host),
          !is.null(port)
        )
        self$db <- DBI::dbConnect(
          drv = RPostgreSQL::PostgreSQL(),
          user = user,
          password = password,
          dbname = dbname,
          host = host,
          port = port
        )
      }
    },
    #' @description
    #' Close a database connection in the DCCon class.
    #' @param None No parameters necessary. Simply call the method.
    #' @return A closed database connection.
    disconnect = function() {
      OFPE::removeTempTables(self$db) # removes temporary tables
      DBI::dbDisconnect(self$db)
    }
  )
)
