#' @title R6 Class for adding or updating field information in OFPE database.
#'
#' @description R6 class that takes a database connection object, path to
#' the folder with the field boundaries, and a list with field information to
#' upload. Uploads boundaries to the database. Adds a unique ID column for
#' each field that is used for internal relationships. If a field is already
#' present in the database, it's information will be updated. This
#' follows the ManageDB interface and includes a setup and execute method that
#' is called from the ManageDB class.
#' @export
ManageFields <- R6::R6Class(
  "ManageFields",
  public = list(
    #' @field dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    dbCon = NULL,
    #' @field field_info List of fields to update, with each element a list with
    #' a 'field_name' element for the name of the field, a 'field_shp_name'
    #' element with the name of the shapefile with the field boundary, and a
    #' element called  'farmer_name' with the name of the farmer who own the
    #' field.
    field_info = NULL,
    #' @field field_path File path to the location of field boundary shapefiles.
    field_path = NULL,
    #' @field fields List with data prepared for upload to the database. Contains
    #' shapefiles with a column indicating the associated farmer and farm name.
    fields = NULL,

    #' @description
    #' Create an ManageFields object. The database connection and a list with
    #' field information used to upload field information to the database with
    #' the appropriate owner information. The field_path argument indicates the
    #' file path in which to follow for the field boundary shapefiles that are
    #' to be uploaded to the database. Instantiates the class for running the
    #' setup and execute methods.
    #' @param dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    #' @param action_item List of inputs to the AddFields method, from the
    #' list passed into the ManageDB class. This includes a list of field
    #' information and a path to where the field boundaries are stored.
    #' @return A new initialized 'AddFields' object.
    initialize = function(dbCon, action_item) {
      stopifnot(
        !is.null(action_item),
        !is.null(action_item$fields),
        !is.null(action_item$field_path)
      )
      self$dbCon <- dbCon
      self$field_info <- action_item$fields %>%
        lapply(function(x) lapply(x, tolower))
      self$field_path <- field_path
    },
    #' @description
    #' Prepares the field data prior to uploading to the database. Imports
    #' shapefiles from the field path and puts them into a list with the
    #' corresponding farmer to be uploaded to the database.
    #' @param None No arguments needed because they are provided during class
    #' initialization.
    #' @return Prepared R6 class for uploading field information.
    setup = function() {
      self$fields <- lapply(self$field_info, self$.setupFields, self$field_path)
    },
    #' @description
    #' Executes the upload of prepared field information to the database in the
    #' 'all_farms' schema and the 'fields' table. If a field boundary is already
    #' present with the same field name and farmer that owns it, the entry in the
    #' database is replaced.
    #' @param None No arguments needed because they are provided during class
    #' initialization.
    #' @return Field data uploaded into the 'all_farms.fields' table in the database.
    execute = function() {
      lapply(self$fields, self$.uploadFields, self$dbCon$db)
    },
    #' @description
    #' Converts a list with information about the fields into a format ready for
    #' uploading to the database. Imports the shapefile using the path name
    #' in the list provided and adds a column for the farmer name. If fields
    #' contain multiple unconnected portions, the 'id' column associated with
    #' these sections is renamed 'wfid' for 'within field ID'.
    #' @param field_info List with information about the field to uplaod.
    #' @param field_path Path to the location of field boundary shapefiles.
    #' @return ESRI shapefile of field boundary.
    .setupFields = function(field_info, field_path = NULL) {
      field <- sf::st_read(paste0(self$field_path,
                                  field_info$field_shp_name,
                                 ".shp"),
                          quiet = TRUE) %>%
        sf::st_zm() %>%
        sf::st_cast()
      field$fieldname <- field_info$field_name
      field$farmer <- field_info$farmer_name
      if (any(grepl("id", names(field)))) {
        names(field)[which(names(field)=="id")] <- "wfid"
      } else {
        field$wfid <- 1:nrow(field)
      }
      if (any(grepl("geometry", names(field)))) {
        geom <- field$geometry
        field <- sf::st_drop_geometry(field)
        field$geom <- geom
        field <- sf::st_as_sf(field) %>%
          sf::st_cast()
      }
      return(field)
    },
    #' @description
    #' Uploads a field to the database. Takes the field shapefile and
    #' identifies the coresponding farmeridx, adds it as a column, and
    #' then uploads to the 'all_farms.fields' table. Makes sure there is
    #' no conflict defined in the .buildTables method of the BuildDB class.
    #' A fieldidx is automatically generated upon upload by PostgreSQL based
    #' on the 'wfid' and 'fieldname'. After uploading, PostGIS is used to
    #' identify the farm the field falls within and a the 'farmidx' column is
    #' filled. The dot indicates that this function would be private if not for
    #' documentations sake.
    #' @param field Field shapefile for upload into 'all_farms.fields'.
    #' @param db Database connection.
    #' @return Field upload into database.
    .uploadFields = function(field, db) {
      # field_ids upsert
      field_name <- unique(field$fieldname)
      DBI::dbSendQuery(
        db,
        paste0("INSERT INTO all_farms.field_ids(fieldname)
               VALUES ('", field_name, "')
               ON CONFLICT ON CONSTRAINT norepfieldids
               DO UPDATE SET fieldname = EXCLUDED.fieldname;")
      )
      # fields upsert
      field$farmeridx <- DBI::dbGetQuery(
        db,
        paste0("SELECT farmeridx
               FROM all_farms.farmers
               WHERE farmer = '", unique(field$farmer), "'")
      )$farmeridx
      field$fieldidx <- DBI::dbGetQuery(
        db,
        paste0("SELECT fieldidx
               FROM all_farms.field_ids
               WHERE fieldname = '", unique(field$fieldname), "'")
      )$fieldidx
      field <- field[, c("wfid", "fieldidx", "farmeridx", "fieldname", "geom")]
      suppressMessages(
        rpostgis::pgInsert(db,
                           c("all_farms","fields"),
                           sf::as_Spatial(field),
                           partial.match = TRUE,
                           upsert.using = c("wfid", "fieldname"))
      )
      DBI::dbGetQuery(
        db,
        "UPDATE all_farms.fields
        SET farmidx = farms.farmidx
        FROM all_farms.farms
        WHERE ST_INTERSECTS(farms.geom, fields.geom)"
      )
    }
  )
)

