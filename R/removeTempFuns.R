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
  geom_temp_exist <- DBI::dbGetQuery(
    db,
    paste0("SELECT EXISTS (
             SELECT 1
             FROM   information_schema.tables
             WHERE  table_schema = 'all_farms'
             AND table_name = 'temp')")) %>% 
    as.numeric() %>% 
    as.logical()
  if(geom_temp_exist){
    tt <- invisible(
      DBI::dbSendQuery(
        db,
        paste0("DROP TABLE all_farms.temp")
      )
    )
    DBI::dbClearResult(tt)

  }
  ## remove any temporary grids
  grid_temp_exist <- DBI::dbGetQuery(
    db,
    paste0("SELECT EXISTS (SELECT 1 FROM
             information_schema.tables
             WHERE table_schema = 'all_farms'
             AND table_name = 'gridtemp')")) %>% 
    as.numeric() %>% 
    as.logical()
  if(grid_temp_exist){
    tt <- invisible(
      DBI::dbSendQuery(
        db,
        paste0("DROP TABLE all_farms.gridtemp")
      )
    )
    DBI::dbClearResult(tt)
  }
  rxgrid_temp_exist <- DBI::dbGetQuery(
    db,
    paste0("SELECT EXISTS (SELECT 1 FROM
             information_schema.tables
             WHERE table_schema = 'all_farms'
             AND table_name = 'rxgridtemp')")) %>% 
    as.numeric() %>% 
    as.logical()
  if(rxgrid_temp_exist){
    tt <- invisible(
      DBI::dbSendQuery(
        db,
        paste0("DROP TABLE all_farms.rxgridtemp")
      )
    )
    DBI::dbClearResult(tt)
  }
  ## remove any temporary geetemp
  gee_temp_exist <- DBI::dbGetQuery(
    db,
    paste0("SELECT EXISTS (SELECT 1 FROM
             information_schema.tables
             WHERE table_schema = 'all_farms'
             AND table_name = 'geetemp')")) %>% 
    as.numeric() %>% 
    as.logical()
  if(gee_temp_exist){
    tt <- invisible(
      DBI::dbSendQuery(
        db,
        paste0("DROP TABLE all_farms.geetemp")
      )
    )
    DBI::dbClearResult(tt)
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
  temp_exist <- DBI::dbGetQuery(
    db,
    paste0("SELECT EXISTS (
             SELECT 1
             FROM information_schema.tables
             WHERE table_schema = '",farmername,"_a'
             AND table_name = 'temp')")) %>% 
    as.numeric() %>% 
    as.logical()
  if(temp_exist){
    tt <- invisible(
      DBI::dbSendQuery(
        db,
        paste0("DROP TABLE ",
               farmername,
               "_a.temp")
      )
    )
    DBI::dbClearResult(tt)
  }
  temp_exist <- DBI::dbGetQuery(
    db,
    paste0("SELECT EXISTS (
             SELECT 1
             FROM information_schema.tables
             WHERE table_schema = '",farmername,"_a'
             AND table_name = 'temp2')")) %>% 
    as.numeric() %>% 
    as.logical()
  if(temp_exist){
    tt <- invisible(
      DBI::dbSendQuery(
        db,
        paste0("DROP TABLE ",
               farmername,
               "_a.temp2")
      )
    )
    DBI::dbClearResult(tt)
  }
  temp_exist <- DBI::dbGetQuery(
    db,
    paste0("SELECT EXISTS (
             SELECT 1
             FROM information_schema.tables
             WHERE table_schema = '",farmername,"_a'
             AND table_name = 'aspect')")) %>% 
    as.numeric() %>% 
    as.logical()
  if(temp_exist){
    tt <- invisible(
      DBI::dbSendQuery(
        db,
        paste0("DROP TABLE ",
               farmername,
               "_a.aspect")
      )
    )
    DBI::dbClearResult(tt)
  }
  temp_exist <- DBI::dbGetQuery(
    db,
    paste0("SELECT EXISTS (
             SELECT 1
             FROM information_schema.tables
             WHERE table_schema = '",farmername,"_r'
             AND table_name = 'temp')")) %>% 
    as.numeric() %>% 
    as.logical()
  if(temp_exist){
    tt <- invisible(
      DBI::dbSendQuery(
        db,
        paste0("DROP TABLE ",
               farmername,
               "_r.temp")
      )
    )
    DBI::dbClearResult(tt)
  }
  temp_exist <- DBI::dbGetQuery(
    db,
    paste0("SELECT EXISTS (
             SELECT 1
             FROM information_schema.tables
             WHERE table_schema = '",farmername,"_r'
             AND table_name = 'temp2')")) %>% 
    as.numeric() %>% 
    as.logical()
  if(temp_exist){
    tt <- invisible(
      DBI::dbSendQuery(
        db,
        paste0("DROP TABLE ",
               farmername,
               "_r.temp2")
      )
    )
    DBI::dbClearResult(tt)
  }
  means_exist <-  DBI::dbGetQuery(
    db,
    paste0("SELECT EXISTS (
             SELECT 1
             FROM information_schema.tables
             WHERE table_schema = '",farmername,"_r'
             AND table_name = 'means')")) %>% 
    as.numeric() %>% 
    as.logical()
  if(means_exist){
    tt <- invisible(
      DBI::dbSendQuery(
        db,
        paste0("DROP TABLE ",
               farmername,
               "_r.means")
      )
    )
    DBI::dbClearResult(tt)
  }
  exp_grid_exist <- DBI::dbGetQuery(
    db,
    paste0("SELECT EXISTS (
             SELECT 1
             FROM information_schema.tables
             WHERE table_schema = '",farmername,"_a'
             AND table_name = 'exp_grid')")) %>% 
    as.numeric() %>% 
    as.logical()
  if(exp_grid_exist){
    tt <- invisible(
      DBI::dbSendQuery(
        db,
        paste0("DROP TABLE ",
               farmername,
               "_a.exp_grid")
      )
    )
    DBI::dbClearResult(tt)
  }
  exp_box_exist <- DBI::dbGetQuery(
    db,
    paste0("SELECT EXISTS (
             SELECT 1
             FROM information_schema.tables
             WHERE table_schema = '",farmername,"_a'
             AND table_name = 'exp_box')")) %>% 
    as.numeric() %>% 
    as.logical()
  if(exp_box_exist){
    tt <- invisible(
      DBI::dbSendQuery(
        db,
        paste0("DROP TABLE ",
               farmername,
               "_a.exp_box")
      )
    )
    DBI::dbClearResult(tt)
  }
  return(invisible())
}















