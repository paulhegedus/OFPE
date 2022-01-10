#' @title R6 Class for NonLinear_Beta modeling of crop responses
#'
#' @description R6 class using non-linear regression (NonLinear_Beta) to
#' model a crop response model with the experimental variable and remotely sensed covariate
#' data. The model used in this class is a modified form of the sigmoidal 'Beta' function 
#' developed in the Yin et al. 2003 paper titled 'A Flexible Sigmoid Function of Determinate 
#' Growth'. This modified version uses the experimental input rate rather than time with a 
#' response of yield or protein rather than growth. Additionally, we added a term called 
#' 'Alpha' corresponding to the response when the experimental input was zero. This differs 
#' from the default where the function starts at zero. 
#' 
#' This class is initialized with a named list of traning (named 'trn')
#' and validation (named 'val') datasets, the response variable, the experimental
#' variable, and the means of the centered data.
#'
#' The initialization creates a data frame ('parm_df') containing the parameter names,
#' function component and id, the mean and standard deviation, and whether it meets
#' the criteria to be omitted from the model, making it a 'bad_parm'. The criteria for
#' this is over 30% of data for a given year missing for a parameter or a standard
#' deviation of zero, indicating singularity.
#'
#' The process then creates a formula for a final model with all parameters that
#' are not considered 'bad'. This is used to fit the final model that is returned
#' to the user for use in the simulation to predict the response under varying
#' rates of the experimental variable. This process is iterative and fits each
#' model component at a time, using default coefficient estimates and updating
#' as each component is fit and new coefficients are generated.
#'
#' The 'saveDiagnostics' method include residuals vs. fitted, normal QQ-
#' plots, etc. The fitting process also prepares data for validation plots in the
#' 'ModClass' R6 class. This includes predicting observations in the validation
#' dataset, making a unique id using the year and fieldname, uncentering data, and
#' identifying a field name to use for plotting that reflects all fields in the
#' dataset.
#' @seealso \code{\link{ModClass}} for the class that calls the ModClass interface,
#' \code{\link{GAM}}, \code{\link{RF}}, and \code{\link{BayesLinear}}
#' for alternative model classes. 
#' @export
NonLinear_Beta <- R6::R6Class(
  "NonLinear_Beta",
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
    
    #' @field m Fitted non-linear logistic model.
    m = NULL,
    #' @field form Final non-linear logistic formula.
    form = NULL,
    #' @field parm_df Data frame of parameter names, the function component and ID, and a column
    #' named 'bad_parms' to indicate whether to include in the model formula. Also includes
    #' columns for the mean and standard deviation of each parameter.
    parm_df = NULL,
    #' @field fieldname Unique name for the field(s) analyzed. If multiple fields are used
    #' they are separated by an ampersand, otherwise the singular field name is used. This
    #' is used for plottting.
    fieldname = NULL,
    #' @field mod_type Name of the model of this class, used for plotting.
    mod_type = "NonLinear_Beta",
    
    #' @description
    #' The initialization creates a data frame ('parm_df') containing the parameter names,
    #' the function component and id, the mean and standard deviation, and whether it meets
    #' the criteria to be omitted from the model, making it a 'bad_parm'. The criteria for
    #' this is over 30% of data for a given year missing for a parameter or a standard
    #' deviation of zero, indicating singularity. Removes data missing from covariates.
    #' @param dat Named list of traning (named 'trn') and validation (named 'val')
    #' datasets with the response, experimental, and remotely sensed variables.
    #' @param respvar Character, the response variable of interest.
    #' @param expvar Character, the experimental variable of interest.
    #' @param covars Character vector of covariates to use for training the model.
    #' @return A instantiated 'NonLinear_Beta' object.
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
      # covars <- c("x", "y", covars)
      self$parm_df <- data.frame(
        parms = c(self$expvar, covars),
        fxn_comp = NA,
        coef_id = NA,
        bad_parms = FALSE,
        means = NA,
        sd = NA
      )
      
      self$parm_df <- OFPE::findBadParms(self$parm_df, self$dat$trn)
      self$dat <- lapply(self$dat, 
                         OFPE::removeNAfromCovars, 
                         self$parm_df$parms[!self$parm_df$bad_parms])
      
      ## TEMP - NEEDS UPDATING BASED ON MODEL
      alpha_comps <- c("aspect", "slope", "elev", "tpi", "_py_", "_2py_", 
                       "bulkdensity", "claycontent", "sandcontent", 
                       "phw", "watercontent", "carboncontent") %>% 
        paste(collapse = "|")
      beta_comps <- c("_cy_") %>% 
        paste(collapse = "|")
      self$parm_df[grepl(alpha_comps, self$parm_df$parms), "fxn_comp"] <- "alpha"
      self$parm_df[which(self$parm_df$fxn_comp == "alpha"), "coef_id"] <-
        paste0("a", 1:length(which(self$parm_df$fxn_comp == "alpha")))
      self$parm_df[grepl(beta_comps, self$parm_df$parms), "fxn_comp"] <- "beta"
      self$parm_df[which(self$parm_df$fxn_comp == "beta"), "coef_id"] <-
        paste0("b", 1:length(which(self$parm_df$fxn_comp == "beta")))
      self$parm_df[which(self$parm_df$parms %in% self$expvar), "fxn_comp"] <- "EXP"
      self$parm_df[which(self$parm_df$parms %in% self$expvar), "coef_id"] <- "EXP"
    },
    #' @description
    #' Method for fitting the non-linear beta model to response variables
    #' using experimental and covariate data.
    #'
    #' The process first identifies parameters that will cause errors in the model fitting
    #' method, then creates some initial coefficient estimates to use for fitting an initial
    #' model. The estimates are based off of the mean to reduce the weight of large parameter
    #' values on the estimates. Then the alpha and beta components are estimated with these
    #' initial values and the means of each parameter. The intercepts of alpha and beta are 
    #' derived from the 1st and 3rd quantiles of the response while delta1 and delta2 are 
    #' derived from the 1st and 3rd quantiles of the experimental input.
    #'
    #' The model is fit iteratively one parameter at a time using error handling where 
    #' if a parameter causes convergence to fail it is omitted. In theory, a final model
    #' could result that only includes the alpha and beta intercepts, the deltas, and the 
    #' experimental variable.
    #'
    #' The fitting process also prepares data for validation plots in the
    #' 'ModClass' R6 class. This includes predicting observations in the validation
    #' dataset, making a unique id using the year and fieldname, uncentering data, and
    #' identifying a field name to use for plotting that reflects all fields in the
    #' dataset.
    #' @param None Put parameters here
    #' @return A fitted beta function model.
    fitMod = function() {
      # adjust coefficients to realistic values to setup the logistic curve using the
      # summary of the response variable
      self$parm_df$est <- ifelse(self$parm_df$means > 1000, 0.00001,
                                 ifelse(self$parm_df$means > 100, 0.0001,
                                        ifelse(self$parm_df$means > 10, 0.001, 0.01)))
      resp_col <- grep(paste0("^", self$respvar, "$"), names(self$dat$trn))
      exp_col <- grep(paste0("^", self$expvar, "$"), names(self$dat$trn))
      Alpha <- summary(self$dat$trn[, resp_col, with = FALSE][[1]])[2] %>% 
        as.numeric()
      Beta <- summary(self$dat$trn[, resp_col, with = FALSE][[1]])[5] %>%
        as.numeric()
      Delta <- summary(self$dat$trn[, exp_col, with = FALSE][[1]])[2] %>%
        as.numeric()
      Delta2 <- summary(self$dat$trn[, exp_col, with = FALSE][[1]])[5] %>% 
        as.numeric()
      
      self$parm_df[(nrow(self$parm_df) + 1), ] <- c("Delta", "Delta", "Delta", FALSE, NA, NA, Delta)
      self$parm_df[(nrow(self$parm_df) + 1), ] <- c("Delta2", "Delta2", "Delta2", FALSE, NA, NA, Delta2)
      
      self$parm_df <- self$parm_df[order(self$parm_df$fxn_comp), ]
      self$parm_df$bad_parms <- as.logical(self$parm_df$bad_parms)
      
      # save formula, return model
      private$.fitFinalMod(Alpha, Beta)
      
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
      pred <- predict(m, dat) %>% as.numeric()
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
        ## Save main diagnostics
        std_res <- self$m$residuals/sigma(self$m)
        png(paste0(out_path, "/Outputs/Diagnostics/", self$respvar, "_",
                   self$fieldname, "_NonLinear_Beta_diagnostics.png"),
            width = 10, height = 10, units = 'in', res = 100)
        par(mfrow=c(2,2))
        qqnorm(std_res, ylab = "Standardized Residuals")
        plot(self$m$fitted,
             self$m$residuals,
             pch=1,
             xlab = "Fitted Values",
             ylab = "Residuals",
             main = "Residuals vs Fitted Values")
        hist(residuals(self$m),
             xlab = "Residuals")
        plot(self$m$fitted,
             std_res,
             pch = 1,
             xlab = "Fitted Values",
             ylab = "Standardized Residuals",
             main = "Standardized Residuals vs Fitted Values")
        dev.off()
      }
    }
  ),
  private = list(
    .BetaFun = function(Alpha, Beta, delta1, delta2, EXP) {
      ###### Beta function ######
      #### Alpha => baseline effect (value when nitrogen=0)
      #### Beta => max fertilizer effect (asymptote)
      #### delta1 => inflection point 1
      #### delta2 => experimental rate where rate Beta found
      #### EXP => experimental input applied
      r <- Alpha + ((Beta - Alpha) * (1 + (delta2 - EXP) / (delta2 - delta1)) * (EXP / delta2)^(delta2 / (delta2 - delta1)))   
      return(r)
    },
    .fitFinalMod = function(Alpha, Beta) {
      for (i in 1:nrow(self$parm_df)) {
        if (!self$parm_df$bad_parms[i] & !grepl("EXP|Delta", self$parm_df$fxn_comp[i])) {
          new_parm_df <- self$parm_df[1:i, ]
          new_parm_df <- new_parm_df[!new_parm_df$bad_parms, ]
          
          tryCatch({
            self$form <- private$.makeFormula(new_parm_df)
            
            start_list <- list(
              a0 = Alpha,
              b0 = Beta,
              Delta = self$parm_df[self$parm_df$parms == "Delta", "est"] %>% as.numeric(),
              Delta2 = self$parm_df[self$parm_df$parms == "Delta2", "est"] %>% as.numeric()
            )
            parm_list <- as.list(new_parm_df[, "est"] %>% as.numeric) %>% 
              `names<-`(new_parm_df[, "coef_id"])
            start_list <- c(start_list, parm_list)
            m <- minpack.lm::nlsLM(as.formula(self$form),
                                 data = self$dat$trn,
                                 control = stats::nls.control(maxiter = 500, minFactor = 1e-10),
                                 start = start_list)
            self$m <- nlme::gnls(as.formula(self$form),
                            data = self$dat$trn,
                            start = coef(m),
                            control = nlme::gnlsControl(maxIter = 500, tolerance = 10000))
            self$m$call[["model"]] <- as.call(list(model = as.formula(self$form)))[[1]]
            Alpha <- coef(self$m)["a0"] %>% as.numeric()
            Beta <- coef(self$m)["b0"] %>% as.numeric()
            self$parm_df[self$parm_df$parms == "Delta", "est"] <- coef(self$m)["Delta"] %>%
              as.numeric()
            self$parm_df[self$parm_df$parms == "Delta2", "est"] <- coef(self$m)["Delta2"] %>%
              as.numeric()
            self$parm_df[self$parm_df$parms %in% new_parm_df$parms, "est"] <- coef(self$m)[5:(4+i)] %>% 
              as.numeric()
            
          }, 
          warning = function(w) {},
          error = function(e) {
            self$parm_df$bad_parms[i] <- TRUE
          })
        }
      }
      
      ## fit with spatial correlation structure on subset of data
      subdat_trn <- self$dat$trn[sample(nrow(self$dat$trn), floor(nrow(self$dat$trn) * 0.25)), ]
      subdat_trn <- subdat_trn[!duplicated(subdat_trn[, c("x", "y")]), ]
      tryCatch({self$m <- nlme::gnls(as.formula(self$form),
                                     data = subdat_trn,
                                     correlation = nlme::corGaus(form = ~ x+y),
                                     control = nlme::gnlsControl(tolerance = 10000),
                                     start = coef(self$m))
      self$m$call[["model"]] <- as.call(list(model = as.formula(self$form)))[[1]]}, 
               warning = function(w) {},
               error = function(e) {
                 print(paste0("Unable to fit spatial correlation structure for ", self$respvar, " model!"))
               })
      
    },
    .makeFormula = function(parm_df) {
      if (is.null(parm_df)) {
        parm_df <- self$parm_df
      }
      
      if (any(grepl("alpha", parm_df$fxn_comp))) {
        alpha <- paste0(parm_df[grepl("alpha", parm_df$fxn_comp), "coef_id"], 
                        " * ",
                        parm_df[grepl("alpha", parm_df$fxn_comp), "parms"])
        alpha <- c("a0", alpha)
      } else {
        alpha <- "a0"
      }
      if (any(grepl("beta", parm_df$fxn_comp))) {
        beta <- paste0(parm_df[grepl("beta", parm_df$fxn_comp), "coef_id"], 
                        " * ",
                        parm_df[grepl("beta", parm_df$fxn_comp), "parms"])
        beta <- c("b0", beta)
      } else {
        beta <- "b0"
      }
      
      # make function statement
      # fxn <- paste0("private$.BetaFun(", paste(alpha, collapse = " + "), ", ",
      #               paste(beta, collapse = " + "), ", ", "Delta, ", "Delta2, ",
      #               self$expvar, ")")
      Alpha <- paste0("(", paste(alpha, collapse = " + "), ")")
      Beta <- paste0("(", paste(beta, collapse = " + ") , ")")
      
      fxn <- paste0(Alpha, " + ((", Beta,  " - ", Alpha, ") * (1 + (Delta2 - ",  self$expvar, ") / (Delta2 - Delta)) * (", self$expvar, " / Delta2)^(Delta2 / (Delta2 - Delta)))") 
      
      fxn <- paste0(ifelse(self$respvar == "yld", "yld", "pro"), " ~ ", fxn)
      return(fxn)
    }
  )
)



