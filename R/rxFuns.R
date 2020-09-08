#' @title Return a grid for creating experiments and prescriptions.
#'
#' @description Returns a grid of the user specified dimensions combined
#' with data passed in by the user. The data available for use as an
#' experiment is clipped to the grid, removing areas between the field edge
#' and the grid within the field. This is used to apply the experimental
#' or check rates.
#'
#' @param db Connection to an OFPE formatted database.
#' @param rx_dt Data frame that contains the coordinates of locations to apply
#' experimental inputs. For an experiment, these are the centroids of the grid
#' made to aggregate data for the field.
#' @param farmername If the user is creating a new experiment, provide or
#' select the name of the farmer that owns or manages the field(s) that
#' an experiment is going to be generated for.
#' @param fieldname If the user is creating a new experiment, provide or
#' select the fieldname of the field to use. The field list is from the
#' available fields in the database for experimentation.
#' @param trt_length Length, in meters, for which to apply treatments.
#' @param boom_width The width of the sprayer boom or spreader.
#' @param unq_fieldname Unique fieldname for the field(s) used for the experiment. This
#' concatenates multiple fields with an ampersand. Used for labelling.
#' @return NULL, grid data stored in database in 'rxgrids' and 'rxgridtemp'.
#' @export
getRxGrid <- function(db,
                      rx_dt,
                      farmername,
                      fieldname,
                      trt_length,
                      boom_width,
                      unq_fieldname) {
  utm_epsg <- OFPE::findUTMzone(farmername = farmername)
  # remove temp tables
  OFPE::removeTempTables(db)
  OFPE::removeTempFarmerTables(db, farmername)

  OFPE::makeTempBound(db, fieldname, farmername)

  # make a grid and add to db if not already present (rx_grids) saved in db
  OFPE::makeRXGrid(db,
                  fieldname,
                  trt_length,
                  boom_width,
                  farmername,
                  unq_fieldname)

  #Make rxDt spatial and add to database (not saved)
  rx_sdt <- rx_dt %>%
    `names<-`(names(rx_dt))
  rx_sdt$X <- rx_sdt$x
  rx_sdt$Y <- rx_sdt$y
  sp::coordinates(rx_sdt) <- c("X", "Y")
  rx_sdt <- sf::st_as_sf(rx_sdt, remove_coordinates = FALSE)
  if (is.na(raster::crs(rx_sdt))) {
    sf::st_crs(rx_sdt) <- utm_epsg
  }

  suppressMessages(rpostgis::pgInsert(
    db,
    c(paste0(farmername, "_a"), "temp"),
    as(rx_sdt, "Spatial"),
    geom = "geometry",
    new.id = "gid"
  ))
  #Add cell_id to temp table from rxgridtemp and get the mean ssopt from each cell
  # Get the cell_id for each point in the temporary table
  invisible(DBI::dbSendQuery(
    db,
    paste0("ALTER TABLE ", farmername, "_a.temp
          ADD COLUMN cell_id VARCHAR;
          UPDATE ", farmername, "_a.temp temp
          SET cell_id = rxgridtemp.cell_id
          FROM all_farms.rxgridtemp
          WHERE ST_Within(temp.geometry, rxgridtemp.geom);")
  ))

  # Add the mean response to the aggregated table by cell_id
  invisible(DBI::dbSendQuery(
    db,
    paste0("ALTER TABLE all_farms.rxgridtemp
          ADD COLUMN expRate REAL,
          ADD COLUMN applRate REAL;

          WITH vtemp AS (
          SELECT
            b.cell_id,
            to_char(
              AVG (b.exp_rate_opt),
              '9999999999999999999'
            ) AS exp_rate_opt
          FROM ", farmername, "_a.temp b
          INNER JOIN all_farms.rxgridtemp a ON a.cell_id = b.cell_id
          GROUP BY  b.cell_id
          )

          UPDATE all_farms.rxgridtemp rxgridtemp
          SET expRate = CAST ( vtemp.exp_rate_opt AS REAL )
          FROM vtemp
          WHERE rxgridtemp.cell_id = vtemp.cell_id;")
  ))

  # Clip rxgridtemp to field boundary & bring into R
  # clip raster
  invisible(DBI::dbSendQuery(
    db,
    paste0( "ALTER TABLE all_farms.rxgridtemp
          ADD COLUMN id SERIAL;

          DELETE FROM all_farms.rxgridtemp  AS rxgridtemp
          WHERE rxgridtemp.id IN (
            SELECT a.id
            FROM all_farms.rxgridtemp  a, (
              SELECT ST_Union(geometry) As geometry FROM all_farms.temp
            ) b
          WHERE NOT ST_Within(a.geom, b.geometry)
          );

          ALTER TABLE all_farms.rxgridtemp
          DROP COLUMN id;")
  ))
  rx_sdt <- sf::st_read(
    db,
    query = paste0("SELECT * FROM all_farms.rxgridtemp"),
    geometry_column = "geom") %>%
    #sf::st_transform(paste0("epsg:", utm_epsg)) %>%
    sf::st_cast("MULTIPOLYGON")
  # remove temp tables
  OFPE::removeTempTables(db)
  OFPE::removeTempFarmerTables(db, farmername)
  return(rx_sdt)
}
#' @title Make a temporary table of field boundaries
#'
#' @description Creates a temporary table in the 'all_farms'
#' schema containing one table with all of the field boundaries
#' for the fields selected by the user.
#'
#' @param db Connection to an OFPE formatted database.
#' @param fieldname If the user is creating a new experiment, provide or
#' select the fieldname of the field to use. The field list is from the
#' available fields in the database for experimentation.
#' @param farmername If the user is creating a new experiment, provide or
#' select the name of the farmer that owns or manages the field(s) that
#' an experiment is going to be generated for.
#' @return NULL, temporary field boundary table in 'all_farms'.
#' @export
makeTempBound <- function(db, fieldname, farmername) {
  utm_epsg <- OFPE::findUTMzone(farmername = farmername)
  for (i in 1:length(fieldname)) {
    if (i == 1) {
      invisible(DBI::dbSendQuery(
        db,
        paste0("CREATE TABLE all_farms.temp AS
              SELECT * FROM all_farms.fields fields
              WHERE fields.fieldname = '", fieldname[i], "';")
      ))
    } else {
      invisible(DBI::dbSendQuery(
        db,
        paste0("INSERT INTO all_farms.temp
              SELECT * FROM all_farms.fields fields
              WHERE fields.fieldname = '", fieldname[i], "';")
      ))
    }
  }
  invisible(DBI::dbSendQuery(
    db,
    paste0("ALTER TABLE all_farms.temp
          RENAME COLUMN geom TO geometry;
          ALTER TABLE all_farms.temp
          ALTER COLUMN geometry TYPE geometry(POLYGON, ", utm_epsg, ")
          USING ST_Transform(geometry, ", utm_epsg, ");")
  ))
}
#' @title Create a grid for creating experiments and prescriptions.
#'
#' @description Creates a grid of the user specified dimensions and
#' stores in the database for later use. The grid is used to place and
#' apply the experiment or prescription outputs and reflects the dimensions
#' of the user's equipment and desired treatment lengths. The 'boom_width',
#' or width of the applicator, is multiplied by two, to make treatments with
#' two passes of the equipment.
#'
#' @param db Connection to an OFPE formatted database.
#' @param fieldname If the user is creating a new experiment, provide or
#' select the fieldname of the field to use. The field list is from the
#' available fields in the database for experimentation.
#' @param trt_length Length, in meters, for which to apply treatments.
#' @param boom_width The width of the sprayer boom or spreader.
#' @param farmername If the user is creating a new experiment, provide or
#' select the name of the farmer that owns or manages the field(s) that
#' an experiment is going to be generated for.
#' @param unq_fieldname Unique fieldname for the field(s) used for the experiment. This
#' concatenates multiple fields with an ampersand. Used for labelling.
#' @return NULL, grid data stored in database in 'rxgrids' and 'rxgridtemp'.
#' @export
makeRXGrid <- function(db,
                       fieldname,
                       trt_length,
                       boom_width,
                       farmername,
                       unq_fieldname) {
  boom_width <- boom_width * 2
  utm_epsg <- OFPE::findUTMzone(farmername = farmername)
  size <- paste0(trt_length, " x ", boom_width)
  grids_exist <- FALSE
  field_exist <- FALSE
  # check if grids exist
  grids_exist <- as.logical(
    DBI::dbGetQuery(
      db,
      paste0("SELECT EXISTS (
               SELECT 1
               FROM information_schema.tables
               WHERE  table_schema = 'all_farms'
               AND table_name = 'rxgrids')")
    )
  )
  # if grids exists check if field exists
  if (grids_exist) {
    # check if field has a grid
    field_exist <- as.logical(
      DBI::dbGetQuery(
        db,
        paste0("SELECT EXISTS (
                 SELECT 1
                 FROM all_farms.rxgrids rxgrids
                 WHERE rxgrids.field = '", unq_fieldname, "'
                 AND rxgrids.size = '", size, "')")
      )
    )
    # if it does copy to gridtemp
    if (field_exist) {
      invisible(
        DBI::dbSendQuery(
          db,
          paste0("CREATE TABLE all_farms.rxgridtemp AS
                SELECT *
                FROM all_farms.rxgrids
                WHERE field = '", unq_fieldname, "'
                AND size = '", size, "'")
        )
      )
    }
  }
  # if field does not exists
  if (!field_exist) {
    BBOX <- sf::st_read(
      db,
      query = paste0("SELECT * FROM all_farms.temp"),
      geometry_column = "geometry") %>%
      sf::st_transform(paste0("epsg:", utm_epsg)) %>%
      as("Spatial") %>%
      sp::bbox()
    NCOL <- ceiling((BBOX["x", "max"] - BBOX["x", "min"]) / boom_width)
    NROW <- ceiling((BBOX["y", "max"] - BBOX["y", "min"]) / trt_length)

    invisible(DBI::dbSendQuery(
      db,
      paste0("CREATE TABLE all_farms.rxgridtemp AS
               SELECT *
               FROM ST_CreateFishnet (", NROW, ", ", NCOL, ", ", boom_width, ", ", trt_length, ", ", BBOX["x", "min"], ", ", BBOX["y", "min"], ") AS cells;
               ALTER TABLE all_farms.rxgridtemp
               ADD COLUMN cell_id VARCHAR,
               ADD COLUMN field VARCHAR,
               ADD COLUMN size VARCHAR,
               ADD COLUMN length double precision,
               ADD COLUMN width double precision,
               ADD COLUMN cell_type VARCHAR;
               UPDATE all_farms.rxgridtemp SET
               cell_id = row::text ||'_'|| col::text,
               field = '", unq_fieldname, "',
               size = '", size, "',
               length = ", trt_length, ",
               width = ", boom_width, ",
               cell_type = 'base';
               UPDATE all_farms.rxgridtemp SET geom = ST_SetSRID (geom, ", utm_epsg, ");
               ALTER TABLE all_farms.rxgridtemp
               ADD COLUMN x double precision,
               ADD COLUMN y double precision;
               UPDATE all_farms.rxgridtemp SET
               x = ST_X(ST_Centroid(geom)),
               y = ST_Y(ST_Centroid(geom));
               ALTER TABLE all_farms.rxgridtemp
               ADD PRIMARY KEY (cell_id, field);
               CREATE INDEX rxgridtemp_geom_idx
               ON all_farms.rxgridtemp
               USING gist (geom);")
    ))
  }
  # if boundary_import = no, field does not exist in grids
  if (!field_exist) {
    if (grids_exist) { # if grids_exists append
      invisible(DBI::dbSendQuery(
        db,
        paste0("INSERT INTO all_farms.rxgrids
                 SELECT * FROM all_farms.rxgridtemp;")
      ))
    } else { # if not create grids
      invisible(DBI::dbSendQuery(
        db,
        paste0("CREATE TABLE all_farms.rxgrids AS
                 SELECT * FROM all_farms.rxgridtemp;")
      ))
    }
    invisible(DBI::dbSendQuery(
      db, paste0("VACUUM ANALYZE all_farms.rxgrids")
    ))
  }
  invisible(DBI::dbSendQuery(
    db, paste0("VACUUM ANALYZE all_farms.rxgridtemp")
  ))
}



