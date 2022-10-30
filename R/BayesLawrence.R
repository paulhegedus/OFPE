#' @title R6 Class for Bayesian modeling of crop responses using modified model from Lawrence et al. 2015
#'
#' @description R6 class using Bayesian Non-Linear Regression to fit a crop
#' response model with the experimental variable, precipitation, and claycontent. 
#' This is a modified version of the model developed by Patrick Lawrence (2015) in
#' "A probabilistic Bayesian framework for progressively updating site-specific recommendations".
#' Rather than using EC data due to its unavailability, clay content is used because of 
#' its relation to EC. This class is initialized with a named list of training (named 'trn')
#' and validation (named 'val') datasets, the response variable, the experimental
#' variable, and the means of the centered data.
#'
#' Any covariates passed into the model are replaced by precipitation and claycontent.
#' This is used to fit the final model that is returned to the user for use in the 
#' simulation to predict the response under varying rates of the experimental variable.
#'
#' The 'saveDiagnostics' method include residuals vs. fitted. The fitting process 
#' also prepares data for validation plots in the
#' 'ModClass' R6 class. This includes predicting observations in the validation
#' dataset, making a unique id using the year and fieldname, uncentering data, and
#' identifying a field name to use for plotting that reflects all fields in the
#' dataset.
#' @seealso \code{\link{ModClass}} for the class that calls the ModClass interface,
#' \code{\link{NonLinear_Logistic}}, \code{\link{GAM}}, and \code{\link{RF}}
#' for alternative model classes.
#' @export
BayesLawrence <- R6::R6Class(
  "BayesLawrence",
  public = list(
    #' @field dat Named list of traning (named 'trn') and validation (named 'val')
    #' datasets with the response, experimental, and remotely sensed variables.
    dat = NULL,
    #' @field respvar Character, the response variable of interest.
    respvar = NULL,
    #' @field expvar Character, the experimental variable of interest.
    expvar = NULL,
    #' @field covars Character vector of covariates to use for training the model. Ignored,
    #' because only the experimental variable, precipitation, and claycontent are used.
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
    mod_type = "BayesLawrence",
    
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
    #' @param covars covars Character vector of covariates to use for training the model. Ignored,
    #' because only the experimental variable, precipitation, and claycontent are used.
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
      self$covars <- c("prec_py_g", "claycontent")
      
      if (self$expvar %in% self$covars) {
        self$covars <- self$covars[-grep(self$expvar, self$covars)]
      }
      self$parm_df <- data.frame(
        parms = c(self$expvar, self$covars),
        bad_parms = FALSE,
        means = NA,
        sd = NA
      )
      
      self$parm_df <- OFPE::findBadParms(self$parm_df, self$dat$trn)
      self$parm_df$bad_parms <- FALSE # force parms to be used
      self$dat <- lapply(self$dat, 
                         OFPE::removeNAfromCovars, 
                         self$parm_df$parms[!self$parm_df$bad_parms])
    },
    #' @description
    #' Method for fitting the modified Lawrence et al. 2015 Bayesian model to
    #' response variables using experimental input data, precipitation, and claycontent.
    #'
    #' A model is fit that is returned  to the user for use in the simulation to 
    #' predict the response under varying  rates of the experimental variable.
    #'
    #' Finally, this method prepares the validation data for plotting by using the model to predict
    #' the response for each of the observations in the validation dataset, uncentering data if
    #' necessary, and identifying a unique field name from the data.
    #' @param None Parameters provided upon class instantiation.
    #' @return A fitted BayesLawrence model.
    fitMod = function() {
      ## simultaneous autoregressive model 
      xy_sub <- self$dat$trn[, c("x", "y")]
      xy_sf <- sf::st_as_sf(xy_sub, coords = c("x", "y"))
      nn <- suppressWarnings(spdep::knn2nb(spdep::knearneigh(x = xy_sf, k = 4)))
      self$form <- private$.makeFormula(expvar = self$expvar,
                                        respvar = self$respvar)
      self$m <- invisible(suppressWarnings(suppressMessages(brms::brm(
        brms::brmsformula(as.formula(self$form),
                          Bmax ~ 1,
                          Bshp ~ 1,
                          B1 ~ 1,
                          B2 ~ 1,
                          B3 ~ 1,
                          nl = TRUE,
                          autocor = ~ brms::sar(nn),
                          decomp = "QR"),
        data = self$dat$trn, family = gaussian(),
        prior = c(
          brms::prior(normal(0, 1000), nlpar = "Bmax", lb = 0),
          brms::prior(normal(0, 1000), nlpar = "Bshp", lb = 0),
          brms::prior(normal(0, 1000), nlpar = "B1", lb = 0),
          brms::prior(normal(0, 1000), nlpar = "B2", lb = 0),
          brms::prior(normal(0, 1000), nlpar = "B3", lb = 0)
        ),
        control = list(adapt_delta = 0.99),
        iter = 6000,
        warmup = 2000,
        normalize = FALSE,
        cores = ifelse(parallel::detectCores() > 4, 4, parallel::detectCores())
      ))))
      
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
      pred_df <- predict(m, dat, ndraws = 100) 
      pred <- apply(pred_df, 1, function(x) rnorm(1, x[1], x[2]))
      gc()
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
        resp_col <- grep(paste0("^", self$respvar, "$"), names(self$dat$val))
        obs <- self$dat$val[, resp_col, with = FALSE][[1]]
        
        mod_diagnostics <- data.frame(
          fitted = self$dat$val$pred,
          residuals = obs - self$dat$val$pred
        )
        png(paste0(out_path, "/Outputs/Diagnostics/", self$respvar, "_",
                   self$fieldname, "_BayesLawrence_diagnostics.png"),
            width = 7.5, height = 5, units = 'in', res = 100)
        plot(mod_diagnostics$fitted, 
             mod_diagnostics$residuals, 
             ylab = "Residuals", xlab = "Fitted")
        dev.off()
      }
    }
  ),
  private = list(
    .makeFormula = function(expvar = NULL, respvar = NULL) {
      fxn <- paste0(respvar, " ~ ", "(Bmax * prec_py_g) / (1 + exp(Bshp - (B1 * ", expvar, ") - (B2 * claycontent) - (B3 * claycontent * ", expvar, ")))")
      return(fxn)
    }
  )
)
