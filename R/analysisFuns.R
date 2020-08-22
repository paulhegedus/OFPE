#' @title Identify parameters inappropriate for use in the OFPE model fitting.
#'
#' @description Identifies whether the parameters listed in a data.frame ('parm_df$parms'),
#' meet the criteria to be omitted from the model, making it a 'bad_parm'. The criteria
#' for this is over 30% of data for a given year missing for a parameter or a
#' standard deviation of zero, indicating singularity. These will lead to errors in the
#' model fitting process and subsequent predictions. It is a conservative parameter
#' selection step that promotes the automation of the process.
#'
#' @param parm_df Data.frame with a column for the parameters to check, named 'parms',
#' a column called 'bad_parms' filled with logical responses on whether to omit the
#' parameter from the model fitting process, and columns labeld 'mean' and 'sd' to
#' populate in the 'bad_parm' identification process. Other columns may be included
#' but these columns and specific column names are required.
#' @param dat Data.table with the data to check validity of parameters in. Typically
#' the training data that the model will be fit on.
#' @return Updated 'parm_df' table.
#' @export
findBadParms <- function(parm_df, dat) {
  stopifnot(is.data.frame(parm_df),
            data.table::is.data.table(dat),
            any(grepl("parms", names(parm_df))),
            any(grepl("bad_parms", names(parm_df))),
            any(grepl("means", names(parm_df))),
            any(grepl("sd", names(parm_df))))

  obs_num <- by(dat, dat$year, nrow) %>%
    lapply(as.numeric) %>%
    unlist()
  for (i in 1:nrow(parm_df)) {
    dfNAs <- by(
      dat[which(names(dat) %in% as.character(parm_df$parms[i]))][[1]],
      dat$year,
      summary
    )
    # NA's held in 7th positiong of summary table
    dfNAs <- lapply(dfNAs, function(x) ifelse(length(x) == 7,
                                              as.numeric(x[7]),
                                              0)) %>%
      unlist() %>%
      as.numeric()
    # bad_parm if more than 30% missing
    for (j in 1:length(obs_num)) {
      if (any(dfNAs[j] / obs_num[j] > 0.3)) {
        parm_df$bad_parms[i] <- TRUE
      }
    }
    # bad_parm if standard deviation = 0
    sds <- by(
      dat[which(names(dat) %in% as.character(parm_df$parms[i]))][[1]],
      dat$year,
      sd,
      na.rm = TRUE
    )
    sds <- as.numeric(sds)
    if (any(is.na(sds))) {
      parm_df$bad_parms[i] <- TRUE
    } else {
      parm_df$bad_parms[i] <- ifelse(any(sds == 0),
                                          TRUE,
                                          FALSE)
    }
    if (!parm_df$bad_parms[i]) {
      parm_df$means[i] <- mean(
        dat[which(names(dat) %in% as.character(parm_df$parms[i]))][[1]],
        na.rm = TRUE
      )
      parm_df$sd[i] <- sd(
        dat[which(names(dat) %in% as.character(parm_df$parms[i]))][[1]],
        na.rm = TRUE
      )
    }
  }
  return(parm_df)
}
#' @title Prepare validation data for plotting.
#' @description Adds a column to the data.table/data.frame with a unique year and field
#' descriptor by concatenating year and field, separated by a '.', corrects the experimental
#' variable values back to the observed by reversing the centering. If the data was not centered
#' there is no change. Removes missing values from the response variable and predicted response
#' variable columns.
#' @param dat Data.table with the data to check validity of parameters in. Typically
#' the validation data that the model will be tested on.
#' @param respvar Character, the response variable of interest.
#' @param expvar Character, the experimental variable of interest.
#' @param num_means Named vector of the means for each numerical covariate, including
#' the experimental variable. This is used for converting centered data back to the
#' original form. The centering process does not center three numerical variables; the
#' x and y coordinates, and the response variable (yld/pro). This is for the data specified
#' from the analysis data inputs (grid specific).
#' @return Updated 'parm_df' table.
#' @export
valPrep <- function(dat, respvar, expvar, num_means) {
  stopifnot(data.table::is.data.table(dat),
            any(grepl("pred", names(dat))))

  dat$field <- as.character(dat$field)

  dat$year.field <- paste0(dat$year,  " ", dat$field)
  cent_correct <- num_means[which(names(num_means) == expvar)] %>%
    as.numeric()
  dat[which(names(dat) == expvar)] <-
    dat[which(names(dat) == expvar)] + cent_correct
  dat <-
    dat[-which(is.na(dat[which(names(dat) %in% respvar)][[1]]) | is.na(dat$pred)), ]
  return(dat)
}
#' @title Identify a unique fieldname from data.
#' @description Returns a character with the fieldname(s) present in the data. Unique name
#' for the field(s) analyzed. If multiple fields are used they are separated by an ampersand,
#' otherwise the singular field name is used. This is used for plotting.
#' @param dat Data.table with the data to to produce a unique fieldname from.
#' @return Character with unique field name.
#' @export
uniqueFieldname <- function(dat) {
  stopifnot(data.table::is.data.table(dat) | is.data.frame(dat),
            any(grepl("field", names(dat))))
  fieldname <- ifelse(length(unique(dat$field)) > 1,
                           paste(unique(dat$field),  collapse = " & "),
                           dat$field[1])
  return(fieldname)
}


