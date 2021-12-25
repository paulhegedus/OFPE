#' @title R6 Class for Random Forest modeling of crop responses
#'
#' @description R6 class using Random Forest (RF) to fit a crop
#' response model with the experimental variable and remotely sensed covariate
#' data. This class is initialized with a named list of training (named 'trn')
#' and validation (named 'val') datasets, the response variable, the experimental
#' variable, and the means of the centered data.
#'
#' The initialization creates a data frame ('parm_df') containing the parameter names,
#' the mean and standard deviation, and whether it meets
#' the criteria to be omitted from the model, making it a 'bad_parm'. The criteria for
#' this is over 30% of data for a given year missing for a parameter or a standard
#' deviation of zero, indicating singularity.
#'
#' The process then creates a formula for a final model with all parameters that
#' are not considered 'bad'. This is used to fit the final model that is returned
#' to the user for use in the simulation to predict the response under varying
#' rates of the experimental variable.
#'
#' The 'saveDiagnostics' method include residuals vs. fitted. The fitting process 
#' also prepares data for validation plots in the
#' 'ModClass' R6 class. This includes predicting observations in the validation
#' dataset, making a unique id using the year and fieldname, uncentering data, and
#' identifying a field name to use for plotting that reflects all fields in the
#' dataset.
#' @seealso \code{\link{ModClass}} for the class that calls the ModClass interface,
#' \code{\link{NonLinear_Logistic}}, \code{\link{GAM}}, and \code{\link{BayesLinear}}
#' for alternative model classes.
#' @export
RF <- R6::R6Class(
  "RF",
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
    
    #' @field m Fitted RF model.
    m = NULL,
    #' @field form Final RF formula.
    form = NULL,
    #' @field parm_df Data frame of parameter names, and a column
    #' named 'bad_parms' to indicate whether to include in the model formula. Also includes
    #' columns for the mean and standard deviation of each parameter.
    parm_df = NULL,
    #' @field fieldname Unique name for the field(s) analyzed. If multiple fields are used
    #' they are separated by an ampersand, otherwise the singular field name is used. This
    #' is used for plotting.
    fieldname = NULL,
    #' @field mod_type Name of the model of this class, used for plotting.
    mod_type = "RF",
    
    #' @description
    #' The initialization creates a data frame ('parm_df') containing the parameter names,
    #' the mean and standard deviation, and whether it meets
    #' the criteria to be omitted from the model, making it a 'bad_parm'. The criteria for
    #' this is over 30% of data for a given year missing for a parameter or a standard
    #' deviation of zero, indicating singularity.
    #' @param dat Named list of training (named 'trn') and validation (named 'val')
    #' datasets with the response, experimental, and remotely sensed variables.
    #' @param respvar Character, the response variable of interest.
    #' @param expvar Character, the experimental variable of interest.
    #' @param covars Character vector of covariates to use for training the model.
    #' @return A instantiated 'RF' object.
    initialize = function(dat, respvar, expvar, covars) {
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
      
      if (self$expvar %in% self$covars) {
        covars <- self$covars[-grep(self$expvar, self$covars)]
      }
      self$parm_df <- data.frame(
        parms = c(self$expvar, covars),
        bad_parms = FALSE,
        means = NA,
        sd = NA
      )
      
      self$parm_df <- OFPE::findBadParms(self$parm_df, self$dat$trn)
      self$dat <- lapply(self$dat, 
                         OFPE::removeNAfromCovars, 
                         self$parm_df$parms[!self$parm_df$bad_parms])
    },
    #' @description
    #' Method for fitting the RF to response variables using experimental and
    #' covariate data.
    #'
    #' The fitting begins by taking the data frame ('parm_df') containing the parameter names,
    #' the mean and standard deviation, and identifying whether
    #' each parameter meets the criteria to be omitted from the model, making it a 'bad_parm'.
    #' The criteria for this is over 30% of data for a given year missing for a parameter or a
    #' standard deviation of zero, indicating singularity.
    #' 
    #' A subset of 1000 observations is taken to perform top-down feature
    #' selection based on the RMSE from predictions to a holdout set. If a covariate
    #' lowers the RMSE when removed, it is flagged as 'bad'. Then it tunes the 
    #' model by iteratively testing the number of trees to use and the number of 
    #' variables to randomly select from at each node. Once optimums for each are 
    #' found they are used in the final model.
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
    #' @return A fitted RF.
    fitMod = function() {
      subdat <- split(
        self$dat$trn,
        sample(c(rep("trn", round(nrow(self$dat$trn) * 0.7)),
                 rep("val", round(nrow(self$dat$trn) * 0.3))))
      ) %>% 
        lapply(OFPE::takeSubset, self$respvar)
        
      form <- private$.makeFormula(parms = self$parm_df[!self$parm_df$bad_parms, "parms"],
                              respvar = self$respvar)
      m <- ranger::ranger(as.formula(form),
                                      data = subdat$trn,
                                      replace = TRUE)
      subdat$val$preds <- predict(m, subdat$val)$predictions
      get_cols <- c(self$respvar, "preds")
      preds <- subdat$val[!is.na(subdat$val$preds), get_cols, with = FALSE]
      init_rmse <- Metrics::rmse(preds[[1]], preds$preds) %>% round(4)
      
      ## do top-down RMSE based feature selection
      # for (i in 1:nrow(self$parm_df)) {
      #   if (self$parm_df$parms[i] != self$expvar) {
      #     self$parm_df$bad_parms[i] <- TRUE
      #     
      #     form <- private$.makeFormula(parms = self$parm_df$parms[!self$parm_df$bad_parms],
      #                             respvar = self$respvar)
      #     m <- ranger::ranger(as.formula(form),
      #                                     data = subdat$trn,
      #                                     replace = TRUE)
      #     subdat$val$preds <- predict(m, subdat$val)$predictions
      #     get_cols <- c(self$respvar, "preds")
      #     preds <- subdat$val[!is.na(subdat$val$preds), get_cols, with = FALSE]
      #     diff_rmse <- Metrics::rmse(preds[[1]], preds$preds) %>% round(4) - init_rmse
      #     if (diff_rmse < 0) {
      #       init_rmse <- Metrics::rmse(preds[[1]], preds$preds) %>% round(4)
      #     } else {
      #       self$parm_df$bad_parms[i] <- FALSE
      #     }
      #   }
      #   rm(m)
      # }
      gc()
      ## now tune the model, starting with the number of trees
      treeCheck <- data.frame(rftrees = seq.int(100, 3000, by = 100), rmse = NA)
      for (i in 1:nrow(treeCheck)) {
        form <- private$.makeFormula(parms = self$parm_df$parms[!self$parm_df$bad_parms],
                                respvar = self$respvar)
        m <- ranger::ranger(as.formula(form),
                                        data = subdat$trn,
                                        num.trees = treeCheck$rftrees[i],
                                        replace = TRUE)
        subdat$val$preds <- predict(m, subdat$val)$predictions
        get_cols <- c(self$respvar, "preds")
        preds <- subdat$val[!is.na(subdat$val$preds), get_cols, with = FALSE]
        treeCheck$rmse[i] <- Metrics::rmse(preds[[1]], preds$preds) %>% round(4)
        rm(m)
      }
      gc()
      subdat$val$preds <- NULL
      opt_n_tree <- treeCheck[which.min(treeCheck$rmse), "rftrees"]
      ## tune the model with the number of variables selected from at each tree
      maxmtry <- floor(length(self$parm_df$parms[!self$parm_df$bad_parms]) / 2) 
      mCheck <- data.frame(m = 1:maxmtry, rmse = NA)
      for (i in 1:nrow(mCheck)) {
        form <- private$.makeFormula(parms = self$parm_df$parms[!self$parm_df$bad_parms],
                                respvar = self$respvar)
        m <- ranger::ranger(as.formula(form),
                                        data = subdat$trn,
                                        num.trees = opt_n_tree,
                                        mtry = mCheck$m[i],
                                        replace = TRUE)
        subdat$val$preds <- predict(m, subdat$val)$predictions
        get_cols <- c(self$respvar, "preds")
        preds <- subdat$val[!is.na(subdat$val$preds), get_cols, with = FALSE]
        mCheck$rmse[i] <- Metrics::rmse(preds[[1]], preds$preds) %>% round(4)
        rm(m)
      }
      gc()
      subdat$val$preds <- NULL
      opt_mtry <- mCheck[which.min(mCheck$rmse), "m"]
      
      self$form <- private$.makeFormula(parms = self$parm_df$parms[!self$parm_df$bad_parms],
                              respvar = self$respvar)
      self$m <- ranger::ranger(as.formula(self$form),
                                      data = self$dat$trn,
                                      num.trees = opt_n_tree,
                                      mtry = opt_mtry,
                                      replace = TRUE)
      
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
      pred <- predict(m, dat)$predictions
      return(pred)
    },
    #' @description
    #' Method for saving diagnostic plots of the fitted model. This is 
    #' simply the residuals vs fitted values.
    #' @param out_path The path to the folder in which to store and
    #' save outputs from the model fitting process
    #' @param SAVE Whether to save diagnostic plots.
    #' @return Diagnostic plots.
    saveDiagnostics = function(out_path, SAVE) {
      if (SAVE) {
        try({dev.off()}, silent = TRUE)
        ## general diagnostics
        resp_col <- grep(paste0("^", self$respvar, "$"), names(self$dat$trn))
        obs <- self$dat$trn[, resp_col, with = FALSE][[1]]
        
        mod_diagnostics <- data.frame(
          fitted = self$m$predictions,
          residuals = obs - self$m$predictions
        )
        png(paste0(out_path, "/Outputs/Diagnostics/", self$respvar, "_",
                   self$fieldname, "_RF_diagnostics.png"),
            width = 7.5, height = 5, units = 'in', res = 100)
        plot(mod_diagnostics$fitted, 
             mod_diagnostics$residuals, 
             ylab = "Residuals", xlab = "Fitted")
        dev.off()
      }
    }
  ),
  private = list(
    .makeFormula = function(parms = NULL, respvar = NULL) {
      if (is.null(parms)) {
        parms <- self$parm_df$parms[!self$parm_df$bad_parms]
      }
      # x and y accounting for spatial autocorrelation
      parms <- c("x", "y", parms)
      fxn <- paste(
        parms,
        collapse = " + ")
      fxn <- paste0(self$respvar, " ~ ", fxn)
      return(fxn)
    }
  )
)
