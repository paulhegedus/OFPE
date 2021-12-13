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
#' parameter from the model fitting process, and columns labeled 'mean' and 'sd' to
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
  # check each var
  obs_num <- by(dat, dat$year, nrow) %>%
    lapply(as.numeric) %>%
    unlist()
  for (i in 1:nrow(parm_df)) {
    if (!parm_df$bad_parms[i]) {
      dfNAs <- by(
        dat[, which(names(dat) %in% as.character(parm_df$parms[i])),
            with = FALSE][[1]],
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
        dat[, which(names(dat) %in% as.character(parm_df$parms[i])),
            with = FALSE][[1]],
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
          dat[, which(names(dat) %in% as.character(parm_df$parms[i])),
              with = FALSE][[1]],
          na.rm = TRUE
        )
        parm_df$sd[i] <- sd(
          dat[, which(names(dat) %in% as.character(parm_df$parms[i])),
              with = FALSE][[1]],
          na.rm = TRUE
        )
      }
    }
  }
  if (any(grepl("^yld$|^pro$", names(dat)))) {
    # check for aliased vars
    resp <- ifelse(any(grepl("yld", names(dat))), "yld", "pro")
    lm_form <- stats::as.formula(
      paste0(resp, " ~ ", paste(parm_df[!parm_df$bad_parms, "parms"],
                                collapse = " + "))
    )
    m0 <- stats::lm(lm_form, data = dat) %>%
      stats::alias()
    if (!is.null(m0$Complete)) {
      parm_df[parm_df$parms %in% row.names(m0$Complete), "bad_parms"] <- TRUE
    }
  }
  
  return(parm_df)
}

#' @title Remove NA values from covariate columns.
#' @description Function for removing missing values from columns of a data.frame
#' or data.table. Must pass in the data to remove missing values from and a character
#' vector of covariate names that correspond to column names in the data. Only observations 
#' from these columns are removed.
#' @param dat Data.table or data.frame to remove missing values from.
#' @param covars Character vector, the column names to look and remove missing observations
#' from.
#' @return Data.table with missing observations removed from specified columns.
#' @export
removeNAfromCovars <- function(dat, covars) {
  stopifnot(
    is.data.frame(dat) | data.table::is.data.table(dat),
    is.character(covars),
    all(covars %in% names(dat))
  )
  dat <- data.table::as.data.table(dat)
  for (i in 1:length(covars)) {
    covar_col <- covars[i]
    good_rows <- !is.na(dat[, covar_col, with = FALSE])
    dat <- dat[good_rows[, covar_col], ]
  }
  return(dat)
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
#' @return Updated 'parm_df' table.
#' @export
valPrep <- function(dat, respvar, expvar) {
  stopifnot(data.table::is.data.table(dat),
            any(grepl("pred", names(dat))))
  dat$field <- as.character(dat$field)
  dat$year.field <- paste0(dat$year,  " ", dat$field)
  NArows <- which(is.na(dat[, which(names(dat) %in% respvar), with = FALSE][[1]]) | is.na(dat$pred))
  if (length(NArows) > 0) {
    dat <- dat[-NArows, ]
  }
  return(dat)
}
#' @title Identify a unique fieldname from data.
#' @description Returns a character with the fieldname(s) present in the data. Unique name
#' for the field(s) analyzed. If multiple fields are used they are separated by an ampersand,
#' otherwise the singular field name is used. This is used for plotting. Can pass in a data.frame
#' with a column named 'field', or a vector of fieldnames.
#' @param dat data.frame with a column called 'field' to produce a unique fieldname from, or
#' a vector of fieldnames.
#' @return Character with unique field name.
#' @export
uniqueFieldname <- function(dat) {
  stopifnot(data.table::is.data.table(dat) | is.data.frame(dat) | is.character(dat))
  if (data.table::is.data.table(dat) | is.data.frame(dat)) {
    stopifnot(any(grepl("field", names(dat))))
    fieldname <- ifelse(length(unique(dat$field)) > 1,
                        paste(unique(dat$field),  collapse = " & "),
                        dat$field[1] %>% as.character())
  } else {
    fieldname <- ifelse(length(dat) > 1,
                        paste(unique(dat),  collapse = " & "),
                        dat %>% as.character())[1]
  }
  return(fieldname)
}

#' @title Take random subset of n samples.
#' @description Function for taking a random subset of the data. Used for 
#' top-down selection in model fitting to speed up processes. This creates
#' a representative sample across fields and years if they're present.
#' @param x data.frame or data.table.
#' @param respvar Character, the response variable column name.
#' @param sub_n Numeric, the number of random observations to take.
#' @return A random subset of the data.
#' @export
takeSubset <- function(x, respvar, sub_n = 1250) {
  stopifnot(any(grepl("field", names(x))),
            any(grepl("year", names(x))))
  
  nrows_x <- nrow(x)
  if (length(unique(x$field)) > 1) {
    if (length(unique(x$year)) > 1) {
      # multi field, multi years
      x <- split(x, list(x$field, x$year))
      pct_dat <- lapply(x, dim) %>% 
        lapply(function(x) x[1]) %>% 
        lapply(function(x) x / nrows_x)
      n_sub <- ceiling(unlist(pct_dat) * sub_n)
      
      for (i in 1:length(x)) {
        if (n_sub[i] != 0) {
          x[[i]]$fid <- 1:nrow(x[[i]])
          resp_col <- grep(paste0("^", respvar, "$"), names(x[[i]]))
          resp_rows <- na.omit(x[[i]], resp_col)
          sub_row_fids <- resp_rows[sample(nrow(resp_rows), n_sub[i])][["fid"]]
          x[[i]] <- x[[i]][sub_row_fids, ]
          x[[i]]$fid <- NULL 
        }
      }
      x <- data.table::rbindlist(x)
    }
  } else {
    # one field, multi years
    if (length(unique(x$year)) > 1) {
      x <- split(x, list(x$year))
      pct_dat <- lapply(x, dim) %>% 
        lapply(function(x) x[1]) %>% 
        lapply(function(x) x / nrows_x)
      n_sub <- ceiling(unlist(pct_dat) * sub_n)
      
      for (i in 1:length(x)) {
        if (n_sub[i] != 0) {
          x[[i]]$fid <- 1:nrow(x[[i]])
          resp_col <- grep(paste0("^", respvar, "$"), names(x[[i]]))
          resp_rows <- na.omit(x[[i]], resp_col)
          sub_row_fids <- resp_rows[sample(nrow(resp_rows), n_sub[i])][["fid"]]
          x[[i]] <- x[[i]][sub_row_fids, ]
          x[[i]]$fid <- NULL 
        }
      }
      x <- data.table::rbindlist(x)
    } else {
      # if one field or year
      x$fid <- 1:nrow(x)
      resp_col <- grep(paste0("^", respvar, "$"), names(x))
      resp_rows <- na.omit(x, resp_col)
      sub_row_fids <- resp_rows[sample(nrow(resp_rows), sub_n)][["fid"]]
      x <- x[sub_row_fids, ]
      x$fid <- NULL 
    }
  }
  return(x)
}

