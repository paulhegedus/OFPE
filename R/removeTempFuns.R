#' @title Remove temporary tables from 'all_farms' schema
#'
#' @description Removes any temporary tables from the 'all_farms' schema of an
#' OFPE formatted database.
#'
#' @param db Connection to an OFPE formatted database.
#' @return NULL, tables removed in database.
#' @export
removeTempTables <- function(db) {
  ## remove old temp bboxes (user input bboxes)
  geom_temp_exist <- as.logical(
    DBI::dbGetQuery(
      db,
      paste0("SELECT EXISTS (
             SELECT 1
             FROM   information_schema.tables
             WHERE  table_schema = 'all_farms'
             AND table_name = 'temp')")
    )
  )
  if(geom_temp_exist){
    invisible(
      DBI::dbSendQuery(
        db,
        paste0("DROP TABLE all_farms.temp")
      )
    )

  }
  ## remove any temporary grids
  grid_temp_exist <- as.logical(
    DBI::dbGetQuery(
      db,
      paste0("SELECT EXISTS (SELECT 1 FROM
             information_schema.tables
             WHERE table_schema = 'all_farms'
             AND table_name = 'gridtemp')")
    )
  )
  if(grid_temp_exist){
    invisible(
      DBI::dbSendQuery(
        db,
        paste0("DROP TABLE all_farms.gridtemp")
      )
    )
  }
  rxgrid_temp_exist <- as.logical(
    DBI::dbGetQuery(
      db,
      paste0("SELECT EXISTS (SELECT 1 FROM
             information_schema.tables
             WHERE table_schema = 'all_farms'
             AND table_name = 'rxgridtemp')")
    )
  )
  if(rxgrid_temp_exist){
    invisible(
      DBI::dbSendQuery(
        db,
        paste0("DROP TABLE all_farms.rxgridtemp")
      )
    )
  }
  ## remove any temporary geetemp
  gee_temp_exist <- as.logical(
    DBI::dbGetQuery(
      db,
      paste0("SELECT EXISTS (SELECT 1 FROM
             information_schema.tables
             WHERE table_schema = 'all_farms'
             AND table_name = 'geetemp')")
    )
  )
  if(gee_temp_exist){
    invisible(
      DBI::dbSendQuery(
        db,
        paste0("DROP TABLE all_farms.geetemp")
      )
    )
  }
}
#' @title Remove temporary tables from a farmer's schemas
#'
#' @description Removes any temporary tables from the schemas
#' in an OFPE formatted database of the specified farmer. .
#'
#' @param db Connection to an OFPE formatted database,
#' @param farmername Name of the farmer corresponding to the schemas to search
#' in for removal of temporary tables.
#' @return NULL, tables removed in database.
#' @export
removeTempFarmerTables <- function(db, farmername) {
  ## remove old temporary tables
  temp_exist <- as.logical(
    DBI::dbGetQuery(
      db,
      paste0("SELECT EXISTS (
             SELECT 1
             FROM information_schema.tables
             WHERE table_schema = '",farmername,"_a'
             AND table_name = 'temp')")
    )
  )
  if(temp_exist){
    invisible(
      DBI::dbGetQuery(
        db,
        paste0("DROP TABLE ",
               farmername,
               "_a.temp")
      )
    )
  }
  temp_exist <- as.logical(
    DBI::dbGetQuery(
      db,
      paste0("SELECT EXISTS (
             SELECT 1
             FROM information_schema.tables
             WHERE table_schema = '",farmername,"_a'
             AND table_name = 'temp2')")
    )
  )
  if(temp_exist){
    invisible(
      DBI::dbGetQuery(
        db,
        paste0("DROP TABLE ",
               farmername,
               "_a.temp2")
      )
    )
  }
  temp_exist <- as.logical(
    DBI::dbGetQuery(
      db,
      paste0("SELECT EXISTS (
             SELECT 1
             FROM information_schema.tables
             WHERE table_schema = '",farmername,"_a'
             AND table_name = 'aspect')")
    )
  )
  if(temp_exist){
    invisible(
      DBI::dbGetQuery(
        db,
        paste0("DROP TABLE ",
               farmername,
               "_a.aspect")
      )
    )
  }
  temp_exist <- as.logical(
    DBI::dbGetQuery(
      db,
      paste0("SELECT EXISTS (
             SELECT 1
             FROM information_schema.tables
             WHERE table_schema = '",farmername,"_r'
             AND table_name = 'temp')")
    )
  )
  if(temp_exist){
    invisible(
      DBI::dbGetQuery(
        db,
        paste0("DROP TABLE ",
               farmername,
               "_r.temp")
      )
    )
  }
  temp_exist <- as.logical(
    DBI::dbGetQuery(
      db,
      paste0("SELECT EXISTS (
             SELECT 1
             FROM information_schema.tables
             WHERE table_schema = '",farmername,"_r'
             AND table_name = 'temp2')")
    )
  )
  if(temp_exist){
    invisible(
      DBI::dbGetQuery(
        db,
        paste0("DROP TABLE ",
               farmername,
               "_r.temp2")
      )
    )
  }
  means_exist <- as.logical(
    DBI::dbGetQuery(
      db,
      paste0("SELECT EXISTS (
             SELECT 1
             FROM information_schema.tables
             WHERE table_schema = '",farmername,"_r'
             AND table_name = 'means')")
    )
  )
  if(means_exist){
    invisible(
      DBI::dbGetQuery(
        db,
        paste0("DROP TABLE ",
               farmername,
               "_r.means")
      )
    )
  }
  exp_grid_exist <- as.logical(
    DBI::dbGetQuery(
      db,
      paste0("SELECT EXISTS (
             SELECT 1
             FROM information_schema.tables
             WHERE table_schema = '",farmername,"_a'
             AND table_name = 'exp_grid')")
    )
  )
  if(exp_grid_exist){
    invisible(
      DBI::dbGetQuery(
        db,
        paste0("DROP TABLE ",
               farmername,
               "_a.exp_grid")
      )
    )
  }
  exp_box_exist <- as.logical(
    DBI::dbGetQuery(
      db,
      paste0("SELECT EXISTS (
             SELECT 1
             FROM information_schema.tables
             WHERE table_schema = '",farmername,"_a'
             AND table_name = 'exp_box')")
    )
  )
  if(exp_box_exist){
    invisible(
      DBI::dbGetQuery(
        db,
        paste0("DROP TABLE ",
               farmername,
               "_a.exp_box")
      )
    )
  }
  return(invisible())
}















