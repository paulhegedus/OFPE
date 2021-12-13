#' @title R6 Class for GAM modeling of crop responses
#'
#' @description R6 class using Generalized Additive Models (GAM) to fit a crop
#' response model with the experimental variable and remotely sensed covariate
#' data. This class is initialized with a named list of training (named 'trn')
#' and validation (named 'val') datasets, the response variable, the experimental
#' variable, and the means of the centered data.
#'
#' The initialization creates a data frame ('parm_df') containing the parameter names,
#' the k value to use for the GAM, the mean and standard deviation, and whether it meets
#' the criteria to be omitted from the model, making it a 'bad_parm'. The criteria for
#' this is over 30% of data for a given year missing for a parameter or a standard
#' deviation of zero, indicating singularity.
#'
#' The process then creates a formula for a final model with all parameters that
#' are not considered 'bad'. This is used to fit the final model that is returned
#' to the user for use in the simulation to predict the response under varying
#' rates of the experimental variable.
#'
#' The 'saveDiagnostics' method include residuals vs. fitted, normal QQ-
#' plots, etc. The fitting process also prepares data for validation plots in the
#' 'ModClass' R6 class. This includes predicting observations in the validation
#' dataset, making a unique id using the year and fieldname, uncentering data, and
#' identifying a field name to use for plotting that reflects all fields in the
#' dataset.
#' @seealso \code{\link{ModClass}} for the class that calls the ModClass interface,
#' \code{\link{NonLinear_Logistic}}, \code{\link{RF}}, and \code{\link{BayesLinear}}
#' for alternative model classes.
#' @export
GAM <- R6::R6Class(
  "GAM",
  public = list(
    #' @field dat Named list of traning (named 'trn') and validation (named 'val')
    #' datasets with the response, experimental, and remotely sensed variables.
    dat = NULL,
    #' @field respvar Character, the response variable of interest.
    respvar = NULL,
    #' @field expvar Character, the experimental variable of interest.
    expvar = NULL,
    #' @field covars Character vector of covariates to use for training the model.
    covars = NULL,

    #' @field m Fitted GAM.
    m = NULL,
    #' @field form Final GAM formula.
    form = NULL,
    #' @field parm_df Data frame of parameter names, starting k for the GAM, and a column
    #' named 'bad_parms' to indicate whether to include in the model formula. Also includes
    #' columns for the mean and standard deviation of each parameter.
    parm_df = NULL,
    #' @field fieldname Unique name for the field(s) analyzed. If multiple fields are used
    #' they are separated by an ampersand, otherwise the singular field name is used. This
    #' is used for plottting.
    fieldname = NULL,
    #' @field mod_type Name of the model of this class, used for plotting.
    mod_type = "GAM",

    #' @description
    #' The initialization creates a data frame ('parm_df') containing the parameter names,
    #' the k value to use for the GAM, the mean and standard deviation, and whether it meets
    #' the criteria to be omitted from the model, making it a 'bad_parm'. The criteria for
    #' this is over 30% of data for a given year missing for a parameter or a standard
    #' deviation of zero, indicating singularity.
    #' @param dat Named list of training (named 'trn') and validation (named 'val')
    #' datasets with the response, experimental, and remotely sensed variables.
    #' @param respvar Character, the response variable of interest.
    #' @param expvar Character, the experimental variable of interest.
    #' @param covars Character vector of covariates to use for training the model.
    #' @param init_k Optional, provide an initial 'k' value to use for the GAM. If no selection
    #' automatically 50. K is the the dimension of the basis used to represent the smooth term.
    #' Multiple k values will be tested, consider this the upper limit and starting place.
    #' @return A instantiated 'GAM' object.
    initialize = function(dat, respvar, expvar, covars , init_k = -1) {
      stopifnot(any(grepl("trn", names(dat)), grepl("val", names(dat))),
                is.character(respvar),
                is.character(expvar),
                is.character(covars)
      )
      for (i in 1:length(dat)) {
        stopifnot(any(grepl(paste0("^", respvar, "$"), names(dat[[i]]))),
                  any(grepl(paste0("^", expvar, "$"), names(dat[[i]]))),
                  all(covars %in% names(dat[[i]])))
      }
      self$dat <- dat
      self$respvar <- respvar
      self$expvar <- expvar
      self$covars <- covars
      
      self$dat <- lapply(self$dat, 
                         OFPE::removeNAfromCovars, 
                         c(self$expvar, self$covars))
      
      init_k <- ifelse(nrow(self$dat$trn) < 1000 | nrow(self$dat$val) < 1000,
                       50,
                       init_k)
      if (expvar %in% covars) {
        covars <- covars[-grep(expvar, covars)]
      }
      self$parm_df <- data.frame(
        parms = c(expvar, covars),
        k = init_k,
        bad_parms = FALSE,
        means = NA,
        sd = NA
      )
    },
    #' @description
    #' Method for fitting the GAM to response variables using experimental and
    #' covariate data.
    #'
    #' The fitting begins by taking the data frame ('parm_df') containing the parameter names,
    #' the k value to use for the GAM, the mean and standard deviation, and identifying whether
    #' each parameter meets the criteria to be omitted from the model, making it a 'bad_parm'.
    #' The criteria for this is over 30% of data for a given year missing for a parameter or a
    #' standard deviation of zero, indicating singularity.
    #'
    #' This implements another method for identifying 'bad_parms' and fits an initial k value
    #' to pass into the GAM. it sequentially  tests a series of k values and if it does not
    #' converge after setting k = 1, it is considered a bad parm because it does not induce model
    #' convergence.
    #'
    #' The process then creates a formula for a final model with all parameters that
    #' are not considered 'bad'. This is used to fit the final model that is returned
    #' to the user for use in the simulation to predict the response under varying
    #' rates of the experimental variable.
    #'
    #' Finally, this method prepares the validation data for plotting by using the model to predict
    #' the response for each of the observations in the validation dataset, uncentering data if
    #' necessary, and identifying a unique field name from the data.
    #' @param None Parameters provided upon class instantiation.
    #' @return A fitted GAM.
    fitMod = function() {
      self$parm_df <- OFPE::findBadParms(self$parm_df, self$dat$trn)
      private$.findK()
      self$form <- private$.makeFormula()
      self$m <- mgcv::bam(as.formula(self$form),
                          data = self$dat$trn)

      self$dat$val$pred <- self$predResps(self$dat$val, self$m)
      self$dat$val <- OFPE::valPrep(self$dat$val,
                                    self$respvar,
                                    self$expvar)
      self$fieldname <- OFPE::uniqueFieldname(self$dat$val)
      return(self$m)
    },
    #' @description
    #' Method for predicting response variables using data and a model.
    #' @param dat Data for predicting response variables for.
    #' @param m The fitted model to use for predicting the response
    #' variable for each observation in 'dat'.
    #' @return Vector of predicted values for each location in 'dat'.
    predResps = function(dat, m) {
      pred <- mgcv::predict.bam(m, dat) %>% as.numeric()
      return(pred)
    },
    #' @description
    #' Method for saving diagnostic plots of the fitted model. These include residual
    #' vs. fitted values, normal QQ plots, etc.
    #' @param out_path The path to the folder in which to store and
    #' save outputs from the model fitting process
    #' @param SAVE Whether to save diagnostic plots.
    #' @return Diagnostic plots.
    saveDiagnostics = function(out_path, SAVE) {
      if (SAVE) {
        try({dev.off()}, silent = TRUE)
        ## general diagnostics
        png(paste0(out_path, "/Outputs/Diagnostics/", self$respvar, "_",
                   self$fieldname, "_GAM_diagnostics.png"),
            width = 10, height = 10, units = 'in', res = 100)
        par(mfrow = c(2, 2))
          mgcv::gam.check(self$m)
        dev.off()
      }
    }
  ),
  private = list(
    .findK = function() {
      ## brute force method for finding a reasonable 'k' estimate.
      good_parms <- self$parm_df[!self$parm_df$bad_parms, "parms"]
      for (i in 1:length(good_parms)) {
        unq_vals <- unique(
          self$dat$trn[, which(names(self$dat$trn) %in% as.character(self$parm_df$parms[i])),
              with = FALSE][[1]],
          na.rm = TRUE
        )
        if (length(unq_vals) < 5) {
          self$parm_df$k[i] <- 2
        } else {
          if (length(unq_vals) < 10) {
            self$parm_df$k[i] <- 5
          } else {
            # trgtK <- self$parm_df[grep(paste0("^", good_parms[i], "$"),
            #                            self$parm_df$parms), "k"]
            # tryK <- c(trgtK,
            #           ifelse(round(trgtK / 2) > 20,
            #                  round(trgtK / 2),
            #                  20),
            #           ifelse(round(trgtK / 4) > 15,
            #                  round(trgtK / 4),
            #                  15),
            #           ifelse(round(trgtK / 10) > 10,
            #                  round(trgtK / 10),
            #                  10),
            #           ifelse(round(trgtK / 20) > 5,
            #                  round(trgtK / 20),
            #                  5),
            #           3, 2, 1
            # )
            # for (j in 1:length(tryK)) {
            #   if (!exists("foundK")) { foundK <- FALSE }
            #   # if foundK  = FALSE (have not found a k that fits, keep trying)
            #   if (!foundK) {
            #     # set the k in the paramter table to the k estimate
            #     self$parm_df[grep(paste0("^", good_parms[i], "$"),
            #                       self$parm_df$parms), "k"] <- tryK[j]
            #     # make the function statement
            #     fxn <- private$.makeFormula(
            #       self$parm_df[grep(paste0("^", good_parms[i], "$"),
            #                         self$parm_df$parms), "parms"],
            #       self$parm_df[grep(paste0("^", good_parms[i], "$"),
            #                         self$parm_df$parms), "k"]
            #     )
            #     # fit model with the estimated k
            #     rand_rows <- runif(nrow(self$dat$trn) * 0.25, 1, nrow(self$dat$trn) + 1) %>%
            #       as.integer()
            #     tryCatch(
            #       {mgcv::bam(as.formula(fxn),
            #                  data = self$dat$trn[rand_rows, ])
            #         foundK <- TRUE },
            #       warning = function(w) {foundK <- FALSE },
            #       error = function(e) {foundK <- FALSE })
            #     # if the model was fit then foundK = T & k in self$parm_df table
            #     # otherwise is FALSE and will try the next k
            #   }
            # } # end tryK
            # # if no k found
            # if (!foundK) {
            #   if (grepl("^aa_n$", self$parm_df$parms[i]) |
            #       grepl("^aa_sr$", self$parm_df$parms[i])) {
            #     self$parm_df[grep(paste0("^", good_parms[i], "$"),
            #                       self$parm_df$parms), "bad_parms"] <- FALSE
            #     self$parm_df[grep(paste0("^", good_parms[i], "$"),
            #                       self$parm_df$parms), "k"] <- 5
            #   } else {
            #     self$parm_df[grep(paste0("^", good_parms[i], "$"),
            #                       self$parm_df$parms), "bad_parms"] <- TRUE
            #     self$parm_df[grep(paste0("^", good_parms[i], "$"),
            #                       self$parm_df$parms), "k"] <- NA
            #   }
            # }
            # rm(foundK) # remove the indicator for the next var in loop
          }
        }
      } # end parms
    },
    .makeFormula = function(parms = NULL, K = NULL, BS = "ts", xyK = 20) {
      if (is.null(parms)) {
        parms <- self$parm_df$parms[!self$parm_df$bad_parms]
      }
      if (is.null(K)) {
        K <- self$parm_df$k[!self$parm_df$bad_parms]
      }
      # x and y accounting for spatial autocorrelation
      fxn <- paste(
        c(paste0("s(x, y, bs = 'gp', k = ", xyK, ", m = 2)"),
          paste0("s(", parms, ", k = ",
                 K, ", bs = '", BS, "')")),
        collapse = " + ")
      fxn <- paste0(self$respvar, " ~ ", fxn)
      return(fxn)
    }
  )
)
