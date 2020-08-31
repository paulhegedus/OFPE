#' @title Create a map of a variable of interest from provided data.
#'
#' @description Function for creating a map with a Google satellite basemap
#' and the rasterized map of a variable supplied by the user. The user supplies
#' the column name to search for in the data to map, the legend label, and the
#' main label. They also must provide the name of the field mapped and the farmer
#' that manages it.
#'
#' @param df Data.frame or data.table with data to be mapped. Must include columns
#' labeled 'x' and 'y' and for the data that is to be mapped.
#' @param var_col_name The name of the column of the variable in the
#' supplied data ('dat'). This can be a vector corresponding with
#' 'var_label' and 'var_main_label'.
#' @param var_label The label to be applied to the legend of the map
#' corresponding to the variable mapped. This can be a vector corresponding
#' with 'var_col_name' and 'var_main_label'.
#' @param var_main_label The main label to apply to the map. This can be a vector
#' corresponding with 'var_col_name' and 'var_label'.
#' @param fieldname Unique field name corresponding to all fields used in the simulation.
#' @param farmername Name of the farmer managing the field. Used to identify UTM zone and
#' for figure labeling.
#' @return If one variable for plotting is passed as an argument, a map of
#' selected variable is returned, otherwise a list with the map for each
#' variable the user passed in will be returned..
#' @export
plotMaps <- function(df,
                     var_col_name,
                     var_label,
                     var_main_label,
                     fieldname,
                     farmername) {
  stopifnot(
    length(var_col_name) == length(var_label),
    length(var_col_name) == length(var_main_label),
    length(var_label) == length(var_main_label),
    is.data.frame(df) | data.table::is.data.table(df),
    is.character(var_col_name),
    is.character(var_label),
    is.character(var_main_label),
    is.character(fieldname),
    length(fieldname) == 1,
    is.character(farmername),
    !is.null(df$x),
    !is.null(df$y),
    any(grepl(var_col_name, names(df)))
  )
  utm_zone <- OFPE::findUTMzone(farmername = farmername)
  df <- as.data.frame(df)
  dfOG <- df
  plot_list <- rep(list(NA), length(var_col_name))
  for (i in 1:length(var_col_name)) {
    df <- dfOG
    if (length(which(is.na(df[, var_col_name[i]]))) > 0) {
      df <- df[-which(is.na(df[, var_col_name[i]])), ]
    }

    sp <- sp::SpatialPoints(coords = df[, c("x", "y")])
    utm <- sf::st_as_sf(sp, remove_coordinates = FALSE)
    utm <- cbind(utm, sp@coords)
    if (is.na(raster::crs(utm))) {
      sf::st_crs(utm) <- utm_zone
    }
    utm <- sf::st_transform(utm, "epsg:4326")
    utm[, 1:2] <- sp::coordinates(as(utm, "Spatial"))
    llc <- sp::coordinates(as(utm, "Spatial")) %>%
      as.data.frame() %>%
      `names<-`(c("x", "y"))
    sp <- sp::SpatialPoints(coords = llc[, c("x", "y")])
    e <- raster::extent(llc[, c("x", "y")])
    rast <- raster::raster(ext = e, resolution = 0.00015)
    rastVar <- raster::rasterize(sp, rast, df[, var_col_name[i]], fun = mean, na.rm = TRUE)
    map <- ggmap::get_map(location = c(lon = mean(sp::coordinates(as(utm, "Spatial"))[, 1]),
                                       lat = mean(sp::coordinates(as(utm, "Spatial"))[, 2])),
                          zoom = 14, maptype = "satellite", source = "google")
    rSpdf <- as(rastVar, "SpatialPixelsDataFrame")
    rDf <- as.data.frame(rSpdf)

    color <- OFPE::getColorRamp(var_col_name)
    colnames(rDf)[1] <- var_label[i]
    main <- var_main_label[i]
    if (grepl("prev", var_col_name[i])) {
      sub_main <- df[1, "prev_yr"]
    } else {
      sub_main <- df[1, "year"]
    }
    var_map <-
      ggmap::ggmap(map, extent  =  "panel") +
      ggplot2::geom_tile(data = rDf, aes(x = x, y = y, fill = rDf[, 1])) +
      ggplot2::scale_fill_gradientn(
        limits = c(floor(min(rDf[, 1], na.m = TRUE)),
                   ceiling(max(rDf[, 1], na.rm = TRUE))),
        colours = color,
        breaks = seq(as.integer(floor(min(rDf[, 1], na.rm = TRUE))),
                     as.integer(ceiling(max(rDf[, 1], na.rm = TRUE))),
                     by = (ceiling(max(rDf[, 1], na.rm = TRUE)) -
                             floor(min(rDf[, 1], na.rm = TRUE))) / 5),
        name = colnames(rDf)[1]
      ) +
      ggplot2::scale_x_continuous(limits = c(e@xmin-0.001, e@xmax+0.001),
                                  expand = c(0, 0),
                                  breaks = c(e@xmin-0.001, e@xmax+0.001)) +
      ggplot2::scale_y_continuous(limits = c(e@ymin-0.001, e@ymax+0.001),
                                  expand = c(0, 0)) +
      ggplot2::labs(title = paste0(main),
                    subtitle = paste0(fieldname, " ",  sub_main),
                    x = "",
                    y = "") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = element_blank(),
                     axis.text.y = element_blank()) +
      OFPE::scale_bar(lon = e@xmin-0.0005,
                      lat = e@ymin-0.0005,
                      distance_lon = 0.2,
                      distance_lat = .01,
                      distance_legend = -.01,
                      dist_unit = "km",
                      orientation = TRUE,
                      arrow_length = .05,
                      arrow_distance = .02) %>%
      suppressMessages()
    plot_list[[i]] <- var_map
  }
  if (length(var_col_name) == 1) {
    return(plot_list[[1]])
  } else {
    return(plot_list)
  }
}
#' @title Create a color ramp from an OFPE variable.
#'
#' @description Function for returning a color ramp appropriate for
#' mapping a supplied variable. For example, yield is mapped from
#' red to green corresponding to low to high.
#'
#' @param var Variable for which to create a color ramp.
#' @return Color ramp appropriate for variable.
#' @export
getColorRamp <- function(var) {
  if(any(grepl("pro", var, ignore.case = T))){
    color <- rev(colorRamps::cyan2yellow(15))
  }else{
    if(any(grepl("yld", var, ignore.case = T))){
      color <- rev(colorRamps::green2red(15)) #color <- rev(grDevices::heat.colors(15))
    }else{
      if(any(grepl("NR", var))){
        color <- rev(topo.colors(15))
      }else{
        if(any(grepl("aa_n", var))|
           any(grepl("ndist", var))|
           any(grepl("aa_sr", var))|
           any(grepl("rate", var))){
          color <- rev(colorRamps::matlab.like2(15))
        }else{
          if(any(grepl("prec", var))){
            color <- rev(colorRamps::blue2green(15))
          }else{
            if(any(grepl("gdd", var))){
              color <- rev(grDevices::terrain.colors(15))
            }else{
              if(any(grepl("ndvi", var))|any(grepl("ndre", var))|any(grepl("cire", var))){
                color <- rev(colorRamps::green2red(15))
              }else{
                color <- rev(grDevices::topo.colors(15))
              }
            }
          }
        }
      }
    }
  }
  return(color)
}



