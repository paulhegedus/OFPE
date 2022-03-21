#' @title R6 Class for generating water holding capacity classifications
#'
#' @description R6 class for for creating the water holding capacity classification
#' across fields based on remotely sensed NDVI data. This class facilitates the 
#' importing of the NDVI rasters from a database, calculating the mean and coefficient
#' of variation at each pixel, and adding the classification to a dataset.
#' 
#' Main initializing requirements are a database connection, and a field name present
#' in the database. The methods include gathering NDVI information for a set of 
#' years, calculating the mean and CV of a set of rasters on a pixel basis, and 
#' classifying a combination of mean and CV data into WHC classes.
#' 
#' Additional methods include adding the data to datasets either directly or in 
#' the database. 
#' @seealso \code{\link{DBCon}} for database connection class
#' @export
WHC <- R6::R6Class(
  "WHC",
  public = list(
    #' @field db Database connection, connected to an OFPE formatted
    #' database, see DBCon class.
    db = NULL,
    #' @field fieldname Name of the field to generate WHC classes for. 
    fieldname = NULL,
    #' @field farmidx Identifier for the farm boundary to gather remote sensing
    #' data from. 
    farmidx = NULL,
    
    #' @param dbCon Database connection object connected to an OFPE formatted
    #' database, see DBCon class.
    #' @param fieldname Name of the field to generate WHC classes for. 
    #' @return A new 'WHC' object.
    initialize = function(dbCon,
                          fieldname) {
      self$db <- dbCon$db
      self$fieldname <- fieldname
      
      # find farmidx name
      self$farmidx <- DBI::dbGetQuery(
        self$db, 
        paste0("SELECT farmidx FROM all_farms.fields a WHERE a.fieldname = '", self$fieldname,"'")
      ) %>% as.numeric()
      
    },
    
    #' @description
    #' Function for creating a stack of rasters from a set of years. Requires 
    #' a user to pass a vector of years as an argument. Also requires specification
    #' of a vegetation index. Must be present as a data type in the 'all_farms.gee' table 
    #' of raster data in the database. The vegetation index (default = NDVI) data from 
    #' each year is downloaded and clipped to the field boundary. 
    #' @param years Vector of years corresponding to years of historical NDVI 
    #' information in the database.
    #' @param vegindx Type of remote sensing data to use. Must be a data type present in 
    #' the database. 
    #' @param output Whether to return a list with the raster data.
    #' @return A list with rasters from each year, for the given field. 
    gatherVegIndx = function(years, vegindx = "ndvi", output = FALSE) {
      stopifnot(is.numeric(years) | is.character(years))
      
      # # get landsat data from full year for every year
      # # clip to boundary
      # types <- DBI::dbGetQuery(
      #   db,
      #   "SELECT DISTINCT type FROM all_farms.gee"
      # ) %>% 
      #   .[, 1] %>% 
      #   .[grep(vegindx, ., ignore.case = TRUE)]
      #   
      # sources <- DBI::dbGetQuery(
      #   db,
      #   "SELECT DISTINCT source FROM all_farms.gee"
      # ) %>% 
      #   .[, 1] %>% 
      #   .[grep("^L|^S", .)]
      # 
      # 
      # i = 1
      # # DBI::dbSendQuery(
      # #   db,
      # #   "CREATE INDEX ON all_farms.gee(farmidx, loy, type, source, year);"
      # # )
      # 
      # testFxn <- function(y, s, t, f) {
      #   ogf <- DBI::dbGetQuery(
      #     db,
      #     paste0("SELECT DISTINCT orig_file FROM all_farms.gee a
      #          WHERE a.farmidx = '", farmidx, "' 
      #          AND a.loy = 'full' 
      #          AND a.type = '", types, "' 
      #          AND a.source = '", sources[1], "' 
      #          AND a.year = '", years[1], "';")
      #   )
      #   return(ogf)
      # }
      # temp_ls <- as.list(sources)
      # temp_ls <- lapply(temp_ls, function(x, years) as.list(years), years)
      # for (i in 1:length(sources)) {
      #   for (j in 1:length(years)) {
      #     temp_ls[[i]][[j]] <- testFxn(years[j], sources[i], types, farmidx) 
      #   }
      # }
      # 
      # orig_files <- DBI::dbGetQuery(
      #   db,
      #   paste0("SELECT DISTINCT orig_file FROM all_farms.gee a
      #          WHERE a.farmidx = '", farmidx, "' 
      #          AND a.loy = 'full' 
      #          AND a.type = '", types, "' 
      #          AND a.source = '", sources[i], "' 
      #          AND a.year = '", years[i], "';")
      # )
      # 
      # # put them in a list & return
      # if (output) {
      #   return(rast_stack)
      # } 
      
    },
    #' @description
    #' Function for generating the WHC class
    #' @param rast_stack List of rasters to calculate the mean and CV for
    #' @return A list with a raster layer of the mean from each pixel and a raster 
    #' layer with the CV from each pixel across the field. 
    genWHCclass = function(rast_stack,
                           save2db = FALSE,
                           save2file = FALSE,
                           out_name = NULL) {
      
      # calculate the mean and CV across the stack of rasters
      metric_rast_ls <- calcWHCmetrics(rast_stack)
      
      # classify the mean and CV into H/M/L
      class_rast_ls <- lapply(metric_rast_ls, classRast)
      
      # classify into WHC
      whc_class <- classWHC(class_rast_ls)
      
      # return WHC raster
      # if save2db = TRUE put in DB & requires db connection
      if (save2db) {
        stopifnot(!is.null(self$db),
                  !is.null(self$fieldname)) # TODO add farmer name and farm name
        # add to db
      }
      # else if save2file = TRUE & requires a file path
      if (save2file) {
        stopifnot(!is.null(out_name))
        sf::st_write(whc_class, out_name)
      }
      return(whc_class)
    },
    
    #' @description
    #' Function for calculating the mean and CV for each pixel across a list of 
    #' rasters. 
    #' @param rast_stack List of rasters to calculate the mean and CV for from calcWHCmetrics().
    #' @return A list with a raster layer of the mean from each pixel and a raster 
    #' layer with the CV from each pixel across the field. 
    calcWHCmetrics = function(rast_stack) {
      
      
      # calculate the mean for each pixel
      
      # calculate the CV for each pixel
      
      
      # return a list of mean and CV rasters
      metric_out_ls <- list(mean_rast = mean_rast,
                            cv_rast = cv_rast)
      return(metric_out_ls)
    },
    
    #' @description
    #' Function for classifying a raster into H/M/L categories.
    #' @param rast Raster to classify.
    #' @param method Method used for classifying into three groups, default to "Jenks", 
    #' otherwise can use...
    #' @return A raster with classifications into H/M/L
    classRast = function(rast, method = "Jenks") {
      
      
      # calculate the H/M/L cutoffs
      
      # classify each observation
      
      # return classification raster
      return(class_rast)
    },
    
    #' @description
    #' Function for classifying mean and CV classifications into WHC zones.
    #' @param rast_stack List of mean and CV classified rasters from classRast()
    #' @return A raster with classifications of WHC
    classWHC = function(rast_stack) {
      
      # combine rasters 
      
      # identify WHC calss
      
      # return classification of WHC
      return(whc_class)
    }
  ),
  private = list()
)






