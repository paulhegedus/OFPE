#' @description
#' Makes a grid across a field of a specified size. If the grid
#' option was selected observations will be aggregated onto
#' the centroids of 10m grid cells generated from the bounding box of
#' the field boundary. If the field boundary is from the database, the
#' centroid locations will be consistent for aggregted data across years.
#' If the user imported a field boundary, these points may not be consistent
#' across years unless the user uses the same field boundary each year.
#' Again, it is strongly encouraged that the user updates the database with
#' the field boundary they have imported, to assure the centroid location
#' remains constant year to year. To create the aggregation table, a 10m grid
#' is applied to the field using the bounding box of the field boundary. The
#' locations of the center of the grid cells are extracted, as well as the
#' grid ID. These create the basis of the aggregation table. If the observed
#' option was selected, a grid is still made to identify the grid cells,
#' however data is not aggregated to the grid cell centroid locations. The
#' dot indicates that this function would be private if not for
#' documentations sake.
#' @param db Connection to an OFPE formatted database.
#' @param boundary_import Whether the user uploaded their own field boundary
#' or are using a boundary stored in the database.
#' @param fieldname Name of the field for aggregation.
#' @param size Size of grid to make (meters), default = 10.
#' @param farmername Name of farmer that owns the field.
#' @return None.
makeXmGrid = function(db, boundary_import, fieldname, size = 10, farmername) {
  utm_epsg <- OFPE::findUTMzone(db, fieldname = fieldname[1])
  grids_exist <- FALSE
  field_exist <- FALSE
  # check if grids exist
  grids_exist <- as.logical(DBI::dbGetQuery(
    db,
    paste0("SELECT EXISTS (
               SELECT 1
               FROM information_schema.tables
               WHERE  table_schema = 'all_farms'
               AND table_name = 'grids')")
  ))
  # if grids exists check if field exists
  if (grids_exist) {
    # check if field has a grid
    field_exist <- as.logical(DBI::dbGetQuery(
      db,
      paste0("SELECT EXISTS (
                 SELECT 1
                 FROM all_farms.grids grids
                 WHERE grids.field = '", fieldname, "'
                 AND grids.size = ", size,")")
    ))
    # if it does copy to gridtemp
    if (field_exist) {
      invisible(DBI::dbSendQuery(
        db,
        paste0("CREATE TABLE all_farms.gridtemp AS
                SELECT *
                FROM all_farms.grids
                WHERE field = '", fieldname, "'
                AND size = ", size)
      ))
    }
  }
  # if field does not exists
  if (!field_exist) {
    BBOX <- sf::st_read(
      db,
      query = paste0("SELECT * FROM all_farms.fields 
                     WHERE fieldname = '", fieldname, "';"),
      geometry_column = "geom") %>%
      sf::st_transform(paste0("epsg:", utm_epsg)) %>%
      as("Spatial") %>%
      sp::bbox()
    NCOL <- ceiling((BBOX["x", "max"] - BBOX["x", "min"]) / size)
    NROW <- ceiling((BBOX["y", "max"] - BBOX["y", "min"]) / size)
    
    invisible(DBI::dbSendQuery(
      db,
      paste0("CREATE TABLE all_farms.gridtemp AS
               SELECT *
               FROM ST_CreateFishnet (", NROW, ", ", NCOL, ", ", size, ", ", size, ", ", BBOX["x", "min"], ", ", BBOX["y", "min"], ") AS cells;")
    ))
    invisible(DBI::dbSendQuery(
      db,
      paste0("ALTER TABLE all_farms.gridtemp
               ADD COLUMN cell_id VARCHAR,
               ADD COLUMN field VARCHAR,
               ADD COLUMN size double precision;")
    ))
    invisible(DBI::dbSendQuery(
      db,
      paste0("UPDATE all_farms.gridtemp SET
               cell_id = row::text ||'_'|| col::text,
               field = '", fieldname, "',
               size = ", size, ";")
    ))
    invisible(DBI::dbSendQuery(
      db,
      paste0("UPDATE all_farms.gridtemp SET geom = ST_SetSRID (geom, ", utm_epsg, ");")
    ))
    invisible(DBI::dbSendQuery(
      db,
      paste0("ALTER TABLE all_farms.gridtemp
               ADD COLUMN x double precision,
               ADD COLUMN y double precision;")
    ))
    invisible(DBI::dbSendQuery(
      db,
      paste0("UPDATE all_farms.gridtemp SET
               x = ST_X(ST_Centroid(geom)),
               y = ST_Y(ST_Centroid(geom));")
    ))
    invisible(DBI::dbSendQuery(
      db,
      paste0("ALTER TABLE all_farms.gridtemp
               ADD PRIMARY KEY (cell_id, field, size);")
    ))
    invisible(DBI::dbSendQuery(
      db,
      paste0("CREATE INDEX gridtemp_geom_idx
               ON all_farms.gridtemp
               USING gist (geom);")
    ))
  }
  # if boundary_import = no, field does not exist in grids
  if (boundary_import == "No" & !field_exist) {
    if (grids_exist) { # if grids_exists append
      invisible(DBI::dbSendQuery(
        db,
        paste0("INSERT INTO all_farms.grids
                 SELECT * FROM all_farms.gridtemp;")
      ))
    } else { # if not create grids
      invisible(DBI::dbSendQuery(
        db,
        paste0("CREATE TABLE all_farms.grids AS
                 SELECT * FROM all_farms.gridtemp;")
      ))
    }
    invisible(DBI::dbSendQuery(
      db,
      paste0("VACUUM ANALYZE all_farms.grids")
    ))
  }
  invisible(DBI::dbSendQuery(
    db,
    paste0("VACUUM ANALYZE all_farms.gridtemp")
  ))
}