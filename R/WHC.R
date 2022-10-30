#' @title R6 Class for generating water holding capacity classifications
#'
#' @description R6 class for for creating the water holding capacity (WHC) classification
#' across fields based on remotely sensed data. This class facilitates the 
#' importing of the satellite from the OFPE database, calculating the mean 
#' and coefficient of variation (CV) of a user specified metric (e.g. NDVI, NDRE, CIRE) 
#' at each pixel of the satellite dataset, and generating a classification 
#' of WHC across a field (Low, Medium, High). 
#' 
#' The mean and CV of the specified metric at each pixel of the satellite data 
#' are classified into categories of high, medium, or low in terms of the mean to 
#' other means across the field, and into a high, medium, or low CV category 
#' based on the relation of the pixel's CV to the total range of other CV values across 
#' the field. Every location in the field has an assignment of H/M/L for the mean and CV, 
#' creating a 3x3 matrix of combinations of H/M/L mean and CV of the user specified 
#' metric.
#' 
#' If the mean and CV of a given pixel are high and low, respectively, the pixel would 
#' be categorized as a high WHC because in the given pixel, the metric specified 
#' (e.g. NDVI, NDRE, CIRE or other vegetation index) representing productivity is 
#' high on average across varying weather conditions while the pixel's low CV 
#' indicates that the pixel maintains a consistent productivity across weather conditions.
#' The high productivity with little variation indicates the crop is getting adequate 
#' moisture and nutrients to maintain high production in different weather conditions 
#' across time. First, we assume that to maintain a high productivity across time, without 
#' making assumptions about management, the soil in that pixel would retain water and 
#' nutrients well enough to minimize the resource limitation of a crop, even in adverse years. 
#' Second, we assume that, because moisture and nutrient retention are related to the WHC of 
#' a soil, areas of the field with a high mean productivity and low CV of productivity 
#' would have soils with a high WHC and minimized risk of water and nutrient inefficiency 
#' 
#' Similarly, if the mean and CV of a given pixel are both low, the pixel would be 
#' categorized as a low WHC because in a given pixel, the metric of productivity is 
#' consistently low over time, indicating that the crop is never receiving adequate 
#' moisture and nutrients, no matter the weather condition. This indicates that these
#' areas are intransigent to any prior management practice to increase production.  
#' 
#' Other combinations of H/M/L mean and CV valeus for productivity are used to generate
#' a WHC classification, laid out in Hegedus & Ewing, 2022. 
#' 
#' Main initializing requirements are a database connection, and a field name present
#' in the database. The methods include gathering remote sensing information 
#' for a set of years from the 'sat' table of a farmer's schema for aggregated 
#' data. Additionally, methods include generating classifications of WHC at the 
#' resolution of the satellite data based on the the mean and CV at each pixel across
#' a range of years specificed.
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
    #' @field farmername Name of the farmer that manages the specified field. 
    farmername = NULL,
    #' @field raw_dat List of raw raster fields from gatherDBdat method.
    raw_dat = NULL,
    #' @field dattype Type of data to use for making classifications from.
    dattype = NULL,
    #' @field whc_dat Data table with the classifications of mean, CV, and WHC.
    whc_dat = NULL,
    #' @field not_fallow Vector of years where the mean NDVI was greater than the 
    #' fallow cutoff specified in gatherDBdat().
    not_fallow = NULL,
    
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
        paste0("SELECT DISTINCT farmidx FROM all_farms.fields a WHERE a.fieldname = '", self$fieldname,"'")
      ) %>% as.numeric()
      
      # find farmername
      farmeridx <- DBI::dbGetQuery(
        self$db, 
        paste0("SELECT DISTINCT farmeridx FROM all_farms.fields a WHERE a.fieldname = '", self$fieldname,"'")
      ) %>% as.numeric()
      self$farmername <- DBI::dbGetQuery(
        self$db, 
        paste0("SELECT DISTINCT farmer FROM all_farms.farmers a WHERE a.farmeridx = '", farmeridx,"'")
      ) %>% as.character()
      
    },
    
    #' @description
    #' Function for creating a stack of rasters from a set of years. Requires 
    #' a user to pass a vector of years as an argument. Also requires specification
    #' of a vegetation index. Must be present as a data type in the 'all_farms.gee' table 
    #' of raster data in the database. The vegetation index (default = NDVI) data from 
    #' each year is downloaded and clipped to the field boundary. This function pulls from
    #' cleaned and aggregated satellite data (in a farmer's '_a.sat' table). Only
    #' retrieves Landsat data
    #' @param years Vector of years corresponding to years of historical NDVI 
    #' information in the database.
    #' @param dattype Type of remote sensing data to use. Must be a data type present in 
    #' the database. 
    #' @param output Whether to return a figure showing the trends in NDVI across 
    #' time.
    #' @param fallow_cutoff The value of the metric below which indicates a fallow 
    #' field and removed from list of years to calculate a WHC classification.
    #' @return An internal list object with data from each year stored in a 'raw_dat' object. 
    gatherDBdat = function(years, dattype = "ndvi", output = FALSE, fallow_cutoff = 0.3) {
      stopifnot(is.numeric(years) | is.character(years),
                is.numeric(fallow_cutoff))
      self$dattype <- dattype
    
      # get data from full year for every year
      self$raw_dat <- years %>% 
        as.list() %>% 
        `names<-`(years - 1) %>% 
        lapply(private$.getDBdat) %>% 
        lapply(private$.trimDat, dattype)
    
      # find and remove fallow years
      df <- lapply(self$raw_dat, sf::st_drop_geometry) %>%
        data.table::rbindlist() %>%
        tidyr::pivot_longer(-c(cell_id, field, x, y, farmer, year),
                            names_to = "source", values_to = dattype) %>%
        data.table::as.data.table() %>% 
        .[, list(mean = mean(na.omit(get(dattype))), sd = sd(get(dattype))), 
          by = c("year","source")]
        
      self$not_fallow <- df %>% 
        as.data.frame() %>% 
        .[grep(paste0(dattype, "_py_l"), df$source, ignore.case = TRUE), ] %>% 
        data.table::as.data.table() %>% 
        list() %>% 
        lapply(function(x) x[which(x$mean > fallow_cutoff), "year"]$year) %>% 
        unlist()
      
      if (output) {
        df$source <- ifelse(grepl("s",df$source),"Sentinel", "Landsat")
        df$source <- factor(df$source)
        p <- ggplot2::ggplot(df,ggplot2::aes(x=year,y=mean,col=source)) +
          ggplot2::geom_line(na.rm=T) +
          ggplot2::geom_point(na.rm=T) +
          ggplot2::theme_bw() +
          ggplot2::scale_color_discrete(name="Source") +
          ggplot2::scale_x_continuous(name="Year",
                             breaks=seq(min(df$year),max(df$year),1),
                             labels=seq(min(df$year),max(df$year),1)) +
          ggplot2::scale_y_continuous(name="Mean NDVI",
                             breaks=seq(0,1,0.1),
                             labels=seq(0,1,0.1),
                             limits=0:1) +
          ggplot2::theme(axis.text.x=ggplot2::element_text(angle=35,hjust=1)) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin=df$mean-(1.96*df$sd), 
                            ymax=df$mean+(1.96*df$sd)),
                        width=.2,
                        na.rm=T) +
          ggplot2::ggtitle(paste0("Mean NDVI for ", self$fieldname),
                  subtitle = "Error Bars = 95% CI")
        return(p)
      }
    },
    #' @description
    #' Function for generating the WHC classification from data in the database. Must 
    #' be supplied with a list of data from the 'sat' tables of an aggregated schema or 
    #' the gatherDBdat() method must be called prior to this. 
    #' @param METHOD Method to classify mean and CV values into High/Medium/Low 
    #' categories. Either "nb" for Jenks or "ei" for equal interval.
    #' @param save2db Logical, default to FALSE, on whether to save the WHC classification 
    #' dataset into the OFPE database in the all_farms.WHC table. Overwrites any 
    #' existing data for the same field.
    #' @param out_path Character path for a folder in which to save the WHC 
    #' classification data to. File name is 'whc_class_farmer_field.csv'. If NULL 
    #' does not save.
    #' @param output Whether to return a list with the raster data.
    #' @param epsg The coordinate reference system code that the satellite data is 
    #' in.
    #' @return A data frame with the WHC classification, if desired. 
    genWHCclass = function(METHOD = "nb", # "ei"
                           save2db = FALSE,
                           out_path = NULL,
                           output = FALSE,
                           epsg = 32612) {
      
      stopifnot(!is.null(self$raw_dat), !is.null(self$dattype))
      
      # put into one big table & subset out non-fallow year
      raw_dat <- self$raw_dat %>% 
        .[names(.) %in% self$not_fallow] %>%
        lapply(sf::st_drop_geometry) %>% 
        data.table::rbindlist() 
    
      # calculate the mean and CV across the stack of rasters
      self$whc_dat <- raw_dat %>%
        tidyr::pivot_longer(-c(cell_id, field, x, y, farmer, year), 
                     names_to = "source", values_to = self$dattype) %>%
        data.table::as.data.table() %>% 
        .[, list(mean = mean(na.omit(get(self$dattype))), sd = sd(na.omit(get(self$dattype)))),
           by = c("cell_id", "x", "y", "source")]
      self$whc_dat$CV <- self$whc_dat$sd / self$whc_dat$mean
      self$whc_dat <- self$whc_dat[grep(paste0(self$dattype, "_py_l"), self$whc_dat$source, ignore.case = TRUE), ]
      
      # classify the mean and CV into H/M/L
      if (METHOD == "ei") {
        private$.classifyEI()
      }
      if (METHOD == "nb") {
        private$.classifyNB()
      }
      
      # classify into WHC
      self$whc_dat <- self$classWHC(self$whc_dat)
      
      # add in field name, farmer, and method
      self$whc_dat$farmer <- self$farmername
      self$whc_dat$field <- self$fieldname
      self$whc_dat$method <- METHOD
      
      # make spatial
      self$whc_dat <- sf::st_as_sf(self$whc_dat,
                     coords = c("x", "y"), 
                     crs = epsg) %>% 
        sf::st_transform(4326)
      
      # return WHC raster
      # if save2db = TRUE put in DB & requires db connection
      if (save2db) {
        stopifnot(!is.null(self$db),
                  !is.null(self$fieldname),
                  !is.null(self$farmername)) # TODO add farmer name and farm name
        # add to db
        private$.addToDB(METHOD)
      }
      
      # else if save2file = TRUE & requires a file path
      if (!is.null(out_path)) {
        stopifnot(file.exists(out_path))
        sf::st_write(self$whc_dat, 
                     paste0(out_path, "/", METHOD, "_whc_class_", self$farmername, "_", self$fieldname, ".shp"),
                     delete_dsn = TRUE)
      }
      
      if (output) {
        return(self$whc_dat)
      }
    },
    
    
    #' @description
    #' Function for classifying mean and CV classifications into WHC zones.
    #' @param whc_dat Data.table or data.frame with the mean and CV for each row of the 
    #' satellite data.
    #' @return The user's dataset with classifications of WHC.
    classWHC = function(whc_dat) {
      stopifnot(is.data.frame(whc_dat) | data.table::is.data.table(whc_dat),
                any(grepl("meanClass", names(whc_dat))),
                any(grepl("cvClass", names(whc_dat))))
      whc_dat$whcClass <- ifelse(
        whc_dat$meanClass == "High" & whc_dat$cvClass == "Low",
        "High",
        ifelse(
          whc_dat$meanClass == "High" & whc_dat$cvClass == "Medium", 
          "High",
          ifelse(
            whc_dat$meanClass == "High" & whc_dat$cvClass == "High", 
            "Medium",
            ifelse(
              whc_dat$meanClass == "Medium" & whc_dat$cvClass == "Low",
              "High", #  "Medium",
              ifelse(
                whc_dat$meanClass == "Medium" & whc_dat$cvClass == "Medium",
                "Medium",
                ifelse(
                  whc_dat$meanClass == "Medium" & whc_dat$cvClass == "High",
                  "Low", # "Medium",
                  ifelse(
                    whc_dat$meanClass == "Low" & whc_dat$cvClass == "Low",
                    "Low",
                    ifelse(
                      whc_dat$meanClass == "Low" & whc_dat$cvClass == "Medium",
                      "Low",
                      ifelse(
                        whc_dat$meanClass == "Low" & whc_dat$cvClass == "High",
                        "Medium",
                        "Medium"
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
      whc_dat$whcClass <- factor(whc_dat$whcClass, levels = c("Low", "Medium", "High"))
    
      return(whc_dat)
    },
    
    #' @description
    #' Function for plotting the raw satellite data.
    #' @param out_path Character path for a folder in which to save the map to.
    #'  File name is 'fieldname_dattype_map_year.png'.
    #' @param epsg The coordinate reference system code that the satellite data is 
    #' in.
    #' @param years Vector of year(s) to save and return maps for. If NULL assumes all.
    #' @param output Logical, defaults to FALSE, on whether to return maps
    #' @return A data frame with the WHC classification, if desired. 
    plotRawMaps = function(out_path, epsg, years = NULL, output = FALSE) {
      stopifnot(file.exists(out_path), 
                is.numeric(epsg))
      if (is.null(years)) {
        years <- names(self$raw_dat)
      }
      map_ls <- as.list(years) %>% 
        `names<-`(years)
      for (i in 1:length(years)) {
        dat_col <- names(self$raw_dat[[i]])[grepl(self$dattype, names(self$raw_dat[[i]]), ignore.case = TRUE)]
        map_ls[[i]] <- as.list(dat_col) %>% 
          `names<-`(dat_col)
        for (j in 1:length(dat_col)) {
          if (!all(is.na(self$raw_dat[[i]][, grep(dat_col[j], names(self$raw_dat[[i]]))][[1]]))) {
            p <- OFPE::plotMaps(self$raw_dat[[i]],
                                dat_col[j],
                                self$dattype,
                                self$dattype,
                                self$fieldname,
                                self$farmername,
                                epsg) %>%
              suppressMessages() %>%
              suppressWarnings()
            
            try({dev.off()}, silent = TRUE)
            ggplot2::ggsave(
              file = paste0(out_path,
                            "/", self$fieldname, "_", dat_col[j],
                            "_map_", years[i], ".png"),
              plot = p, device = "png",
              width = 7.5, height = 7.5, units = "in"
            )
            map_ls[[i]][[j]] <- p
          }
        }
      }
    },
    
    #' @description
    #' Function for plotting the WHC classifications.
    #' @param out_path Character path for a folder in which to save the map to.
    #'  File name is 'fieldname_whc_map.png'.
    #' @param epsg The coordinate reference system code that the satellite data is 
    #' in.
    #' @param output Logical, defaults to FALSE, on whether to return maps
    #' @param METHOD Method to classify mean and CV values into High/Medium/Low 
    #' categories. Either "nb" for Jenks or "ei" for equal interval.
    #' @return A data frame with the WHC classification, if desired. 
    plotWHCmap = function(out_path, epsg, output = FALSE, METHOD = "nb") {
      stopifnot(file.exists(out_path), 
                is.numeric(epsg))
    
      coords <- whc$whc_dat %>% 
        sf::st_transform(epsg) %>% 
        sf::st_coordinates() %>% 
        as.data.frame() %>% 
        `names<-`(c("x", "y"))
      temp <- whc$whc_dat %>% 
        sf::st_drop_geometry() %>% 
        cbind(., coords)
      
      temp$whc_f <- ifelse(temp$whcClass == "High", 3, 
                           ifelse(temp$whcClass == "Medium", 2, 
                                  ifelse(temp$whcClass == "Low", 1, NA)))
      
      sp <- sp::SpatialPoints(coords = temp[, c("x", "y")])
      utm <- sp::SpatialPoints(sp, proj4string = sp::CRS("+proj=utm +zone=12"))
      longlat <- sp::spTransform(utm, sp::CRS("+proj=longlat +datum=WGS84"))
      llc <- as(longlat, "SpatialPointsDataFrame")
      llc <- as.data.frame(llc@coords)
      sp <- sp::SpatialPoints(coords = llc[, c("x", "y")])
      e <- raster::extent(llc[, c("x", "y")])
      
      rast <- raster::raster(ext = e, resolution = 0.00015)
      rastVar <- raster::rasterize(sp,
                                   rast,
                                   temp[, "whc_f"],
                                   fun = max,
                                   na.rm = TRUE)
      rSpdf <- as(rastVar, "SpatialPixelsDataFrame")
      rDf <- as.data.frame(rSpdf)
      rDf$WHC <- ifelse(rDf[, 1] == 3, "High",
                        ifelse(rDf[, 1] == 2, "Medium", "Low"))
      rDf$WHC <- factor(rDf$WHC, levels = c("Low", "Medium", "High"))
      
      map <- ggmap::get_map(location = c(lon = mean(sp::coordinates(longlat)[, 1]), 
                                         lat = mean(sp::coordinates(longlat)[, 2])), 
                            zoom = 14, # ifelse(any(grepl("BSE|sec1east", sub_title)), 15, 14), 
                            maptype = "satellite", source = "google")
      colors <- c("Red",  "Gold", "green4")
      
      p <- suppressMessages(
        ggmap::ggmap(map, extent = "panel") + 
          ggplot2::geom_tile(data = rDf,
                             ggplot2::aes(x = x,
                                          y = y,
                                          fill = rDf[, 4])) +
          ggplot2::scale_fill_manual(name = "WHC", 
                                     values = colors) +
          #geom_point(data = DF, aes(x = xLong, y = yLat, col = riskClass)) + 
          #scale_color_manual(name = "Risk Classification", values = colors) +
          
          ggplot2::scale_x_continuous(limits = c(e@xmin-0.001, e@xmax+0.001), 
                                      expand = c(0, 0), 
                                      breaks = c(e@xmin-0.001, e@xmax+0.001)) +
          ggplot2::scale_y_continuous(limits = c(e@ymin-0.001, e@ymax+0.001), 
                                      expand = c(0, 0)) +
          ggplot2::labs(title =  paste0(self$farmername, " ", self$fieldname, " WHC"), 
                        subtitle = paste0("Method = ", ifelse(METHOD == "nb", "Jenks", "Equal Interval")), 
                        x = "", 
                        y = "") +
          ggplot2::theme_bw() +
          ggplot2::theme(axis.text.x = ggplot2::element_blank(), 
                         axis.text.y = ggplot2::element_blank()) +
          OFPE::scale_bar(lon = e@xmin-0.0005, 
                          lat = e@ymin-0.00075, 
                          distance_lon = 0.2, 
                          distance_lat = .01, 
                          distance_legend = -.01, 
                          dist_unit = "km", 
                          orientation = TRUE, 
                          arrow_length = .05, 
                          arrow_distance =  .02)
      )
      
      try({dev.off()}, silent = TRUE)
      ggplot2::ggsave(
        file = paste0(out_path,
                      "/", self$fieldname, "_WHC_map_", METHOD, ".png"),
        plot = p, device = "png",
        width = 7.5, height = 7.5, units = "in"
      )
      
      if (output) {
        return(p)
      }
    }
  ),
  private = list(
    .trimDat = function(x, dattype) {
      # first trim out columns
      keep_cols <- c("cell_id", "field", "x", "y", "farmer", "year") %>% 
        c(., names(x)[grep(paste0(dattype, "_py"), names(x), ignore.case = TRUE)]) %>% 
        paste0("^", ., "$") %>% 
        paste(collapse = "|") %>% 
        grep(., names(x))
      x <- x[, keep_cols]
      # fix year because using (py) from these datasets (all of the previous year) 
      x$year <- x$year - 1
      
      return(x)
    },
    .getDBdat = function(x) {
      y <- sf::st_read(
        dsn = self$db, 
        query = paste0("SELECT * FROM ", self$farmername,"_a.sat a
                          WHERE a.field = '", self$fieldname, "'
                          AND a.year = '", x, "' 
                          AND a.datused = 'decision_point';"),
        geometry = "geometry"
      )
      return(y)
    },
    .classifyEI = function() {
      GROUPS <- 3 
      GROUPNAMES <- c("Low", "Medium", "High")
      
      ## classify mean
      mean_int <- (max(self$whc_dat$mean, na.rm = T) - min(self$whc_dat$mean, na.rm = T)) / GROUPS
      self$whc_dat$meanClass <- NA
      breaks <- rep(NA, GROUPS - 1)
      for (i in 1:(GROUPS - 1)) {
        breaks[i] <- min(self$whc_dat$mean, na.rm = T) + (i * mean_int)
        ## if first break
        if (i == 1) {
          self$whc_dat$meanClass <- ifelse(self$whc_dat$mean <= breaks[i], 
                                 GROUPNAMES[i], 
                                 self$whc_dat$meanClass) 
        } else {
          ## if last break
          if (i == (GROUPS - 1)) {
            self$whc_dat$meanClass <- ifelse(self$whc_dat$mean > breaks[i], 
                                   GROUPNAMES[i + 1], 
                                   self$whc_dat$meanClass) 
            self$whc_dat$meanClass <- ifelse(self$whc_dat$mean > breaks[i - 1] & self$whc_dat$mean <= breaks[i], 
                                   GROUPNAMES[i], 
                                   self$whc_dat$meanClass) 
          } else {
            self$whc_dat$meanClass <- ifelse(self$whc_dat$mean>breaks[i - 1] & self$whc_dat$mean <= breaks[i], 
                                   GROUPNAMES[i], 
                                   self$whc_dat$meanClass) 
          }
        }
      }
      self$whc_dat$meanClass <- factor(self$whc_dat$meanClass)
      
      ## classify cv
      cv_int <- (max(self$whc_dat$CV, na.rm = T) - min(self$whc_dat$CV, na.rm = T)) / GROUPS
      self$whc_dat$cvClass <- NA
      breaks <- rep(NA, GROUPS - 1)
      for (i in 1:(GROUPS - 1)) {
        breaks[i] <- min(self$whc_dat$CV, na.rm = T) + (i * cv_int)
        ## if first break
        if (i == 1) {
          self$whc_dat$cvClass <- ifelse(self$whc_dat$CV <= breaks[i],
                               GROUPNAMES[i], 
                               self$whc_dat$cvClass) 
        } else {
          ## if last break
          if (i == (GROUPS - 1)) {
            self$whc_dat$cvClass <- ifelse(self$whc_dat$CV > breaks[i],
                                 GROUPNAMES[i + 1],
                                 self$whc_dat$cvClass) 
            self$whc_dat$cvClass <- ifelse(self$whc_dat$CV > breaks[i - 1] & self$whc_dat$CV <= breaks[i],
                                 GROUPNAMES[i], 
                                 self$whc_dat$cvClass) 
          } else {
            self$whc_dat$cvClass <- ifelse(self$whc_dat$CV > breaks[i - 1] & self$whc_dat$CV <= breaks[i], 
                                 GROUPNAMES[i], 
                                 self$whc_dat$cvClass) 
          }
        }
      }
      self$whc_dat$cvClass <- factor(self$whc_dat$cvClass)
    },
    .classifyNB = function() {
      GROUPS <- 3 
      GROUPNAMES <- c("Low", "Medium", "High")
      
      ## classify mean
      mean_int <- BAMMtools::getJenksBreaks(self$whc_dat$mean, GROUPS + 1)
      mean_int <- mean_int[2:3]
      
      self$whc_dat$meanClass <- NA
      breaks <- rep(NA, GROUPS - 1)
      for (i in 1:(GROUPS - 1)) {
        breaks[i] <- mean_int[i]
        ## if first break
        if (i == 1) {
          self$whc_dat$meanClass <- ifelse(self$whc_dat$mean <= breaks[i], 
                                 GROUPNAMES[i], 
                                 self$whc_dat$meanClass) 
        } else {
          ## if last break
          if (i== (GROUPS - 1)) {
            self$whc_dat$meanClass <- ifelse(self$whc_dat$mean > breaks[i], 
                                   GROUPNAMES[i + 1],
                                   self$whc_dat$meanClass) 
            self$whc_dat$meanClass <- ifelse(self$whc_dat$mean > breaks[i - 1] & self$whc_dat$mean <= breaks[i], 
                                   GROUPNAMES[i], 
                                   self$whc_dat$meanClass) 
          } else {
            self$whc_dat$meanClass <- ifelse(self$whc_dat$mean > breaks[i - 1] & self$whc_dat$mean <= breaks[i],
                                   GROUPNAMES[i], 
                                   self$whc_dat$meanClass) 
          }
        }
      }
      self$whc_dat$meanClass <- factor(self$whc_dat$meanClass)
    
      ## classify cv
      cv_int <- BAMMtools::getJenksBreaks(self$whc_dat$CV, GROUPS + 1)
      cv_int <- cv_int[2:3]
      
      self$whc_dat$cvClass <- NA
      breaks <- rep(NA, GROUPS - 1)
      for (i in 1:(GROUPS - 1)) {
        breaks[i] <- cv_int[i]
        ## if first break
        if (i == 1) {
          self$whc_dat$cvClass <- ifelse(self$whc_dat$CV <= breaks[i],
                               GROUPNAMES[i],
                               self$whc_dat$cvClass) 
        } else {
          ## if last break
          if (i == (GROUPS - 1)) {
            self$whc_dat$cvClass <- ifelse(self$whc_dat$CV > breaks[i],
                                 GROUPNAMES[i + 1],
                                 self$whc_dat$cvClass) 
            self$whc_dat$cvClass <- ifelse(self$whc_dat$CV>breaks[i - 1] & self$whc_dat$CV <= breaks[i],
                                 GROUPNAMES[i],
                                 self$whc_dat$cvClass) 
          } else {
            self$whc_dat$cvClass <- ifelse(self$whc_dat$CV > breaks[i - 1] & self$whc_dat$CV <= breaks[i],
                                 GROUPNAMES[i],
                                 self$whc_dat$cvClass) 
          }
        }
      }
      self$whc_dat$cvClass <- factor(self$whc_dat$cvClass)
    },
    .addToDB = function(METHOD) {
      # check if table exists yet (after first upload it should)
      whc <- OFPE::tabExist(self$db, "all_farms", "whc")
      if (whc) { 
        ## if whc table exists
        # check if data with same fieldname exists
        db_check <- FALSE # assumes field not in db
        db_check <- DBI::dbGetQuery(
          self$db,
          paste0("SELECT EXISTS (SELECT 1
                  FROM all_farms.whc 
                  WHERE field = '", self$fieldname,"' 
                 AND method = '", METHOD,"')")) %>% 
          as.numeric() %>% 
          as.logical()
        # if so delete
        if (db_check) {
          tt <- DBI::dbSendQuery(
            self$db,
            paste0("DELETE FROM all_farms.whc
                   WHERE field = '", self$fieldname,"'
                   AND method = '", METHOD,"';")
          )
          DBI::dbClearResult(tt)
          db_check <- FALSE
        }
        # append existing data with new data for field
        OFPE::importDat(self$db, as(self$whc_dat, "Spatial"), "all_farms", "whc", c("gid", "field", "method"))
      } else {
        ## if whc table does not exist, create
        OFPE::importNewDat(self$db, as(self$whc_dat, "Spatial"), "all_farms", "whc")
        query <- paste0("ALTER TABLE all_farms.whc 
                        ADD PRIMARY KEY (gid, field, method)")
        tt <- invisible(DBI::dbSendQuery(self$db, query))
        DBI::dbClearResult(tt)
        geom_idx <- paste0("whc_geom_idx")
        OFPE::makeSpatialIndex(self$db, "whc_geom_idx", "all_farms", "whc")
      }
    }
  )
)






