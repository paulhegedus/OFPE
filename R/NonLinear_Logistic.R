#' @title R6 Class for NonLinear_Logistic modeling of crop responses
#'
#' @description R6 class using non-linear logistic regression (NonLinear_Logistic) to
#' model a crop response model with the experimental variable and remotely sensed covariate
#' data. This class is initialized with a named list of traning (named 'trn')
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
#' @export
NonLinear_Logistic <- R6::R6Class(
  "NonLinear_Logistic",
  public = list(
    #' @field dat Named list of traning (named 'trn') and validation (named 'val')
    #' datasets with the response, experimental, and remotely sensed variables.
    dat = NULL,
    #' @field respvar Character, the response variable of interest.
    respvar = NULL,
    #' @field expvar Character, the experimental variable of interest.
    expvar = NULL,
    #' @field num_means Named vector of the means for each numerical covariate, including
    #' the experimental variable. This is used for converting centered data back to the
    #' original form. The centering process does not center three numerical variables; the
    #' x and y coordinates, and the response variable (yld/pro). This is for the data specified
    #' from the analysis data inputs (grid specific).
    num_means = NULL,

    #' @field mod Fitted GAM.
    mod = NULL,
    #' @field form Final GAM formula.
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
    mod_type = "NonLinear_Logistic",

    #' @description
    #' The initialization creates a data frame ('parm_df') containing the parameter names,
    #' the function component and id, the mean and standard deviation, and whether it meets
    #' the criteria to be omitted from the model, making it a 'bad_parm'. The criteria for
    #' this is over 30% of data for a given year missing for a parameter or a standard
    #' deviation of zero, indicating singularity.
    #' @param dat Named list of traning (named 'trn') and validation (named 'val')
    #' datasets with the response, experimental, and remotely sensed variables.
    #' @param respvar Character, the response variable of interest.
    #' @param expvar Character, the experimental variable of interest.
    #' @param num_means Named vector of the means for each numerical covariate, including
    #' the experimental variable. This is used for converting centered data back to the
    #' original form. The centering process does not center three numerical variables; the
    #' x and y coordinates, and the response variable (yld/pro). This is for the data specified
    #' from the analysis data inputs (grid specific).
    #' @return A instantiated 'NonLinear_Logistic' object.
    initialize = function(dat, respvar, expvar, num_means) {
      stopifnot(any(grepl("trn", names(dat)), grepl("val", names(dat))),
                is.character(respvar),
                is.character(expvar),
                is.numeric(num_means)
      )
      for (i in 1:length(dat)) {
        stopifnot(any(grepl(paste0("^", respvar, "$"), names(dat[[i]]))),
                  any(grepl(paste0("^", expvar, "$"), names(dat[[i]]))),
                  all(names(num_means) %in% names(dat[[i]])))
      }
      self$dat <- dat
      self$respvar <- respvar
      self$expvar <- expvar
      self$num_means <- num_means

      self$parm_df <- data.frame(
        parms = c(expvar, names(self$num_means)[-which(names(num_means) %in% expvar)]),
        fxn_comp = NA,
        coef_id = NA,
        bad_parms = FALSE,
        means = NA,
        sd = NA
      )
      alpha_comps <- c("a0", paste0("prev_", self$expvar), paste0("prev_", self$respvar),
                       "aspect_rad", "slope", "elev", "tpi")
      beta_comps <- c("b0", "prec_cy", "prec_py", "gdd_cy", "gdd_py",
                      "veg_2py", "veg_py", "veg_cy")

      self$parm_df[which(self$parm_df$parms %in% alpha_comps), "fxn_comp"] <- "alpha"
      self$parm_df[which(self$parm_df$parms %in% alpha_comps), "coef_id"] <-
        paste0("a", 1:length(which(self$parm_df$parms %in% alpha_comps)))
      self$parm_df[which(self$parm_df$parms %in% beta_comps), "fxn_comp"] <- "beta"
      self$parm_df[which(self$parm_df$parms %in% beta_comps), "coef_id"] <-
        paste0("b", 1:length(which(self$parm_df$parms %in% beta_comps)))
      self$parm_df[which(self$parm_df$parms %in% self$expvar), "fxn_comp"] <- "EXP"
      self$parm_df[which(self$parm_df$parms %in% self$expvar), "coef_id"] <- "EXP"
    },

    #' @description
    #' Method for fitting the non-linear logistic regression model to response variables
    #' using experimental and covariate data.
    #'
    #' The process first identifies parameters that will cause errors in the model fitting
    #' method, then creates some initial coefficient estimates to use for fitting an initial
    #' model. The estimates are based off of the mean to reduce the weight of large parameter
    #' values on the estiamtes. Then the alpha and beta components are estimated with these
    #' initial values and the means of each parameter. If alpha and beta fall outside of half a
    #' standard deviation from the 1st and 3rd quartiles of the data the intercept for each
    #' component of alpha and beta (a0 and b0) are set to the 1st and 3rd quantiles, respectively.
    #' This sets up the intitial model to follow a logistic curve from the 1st to the 3rd quantile
    #' levels of the response variable, mimicking the expected response of crop yield and protein
    #' to look.
    #'
    #' Then, an initial model only using data from the lowest experimental rates is used to fit
    #' the coefficients for the alpha component. Beta components are held constant in this process.
    #' Once the alpha coefficeints are fitted, these are locked in and another model fitting step
    #' uses the estiamted alpha component in a model to fit the beta, delta, and gamma components.
    #' If this fails, delta is set at the mean of the experimenal value and the fit is tried again.
    #' This usually works, if not there are deeper issues in the data. This final model  is returned
    #' to the user for use in the simulation to predict the response under varying
    #' rates of the experimental variable.
    #'
    #' The fitting process also prepares data for validation plots in the
    #' 'ModClass' R6 class. This includes predicting observations in the validation
    #' dataset, making a unique id using the year and fieldname, uncentering data, and
    #' identifying a field name to use for plotting that reflects all fields in the
    #' dataset.
    #' @param None Put parameters here
    #' @return A fitted logistic model.
    fitMod = function() {
      self$parm_df <- OFPE::findBadParms(self$parm_df, self$dat$trn)
      self$parm_df <- rbind(subset(self$parm_df, self$parm_df$fxn_comp == "EXP"),
                            data.frame(parms = "a0", fxn_comp = "alpha", coef_id = "a0",
                                       bad_parms = "FALSE", means = 1, sd = 1),
                            subset(self$parm_df, self$parm_df$fxn_comp == "alpha"),
                            subset(self$parm_df, is.na(self$parm_df$fxn_comp)),
                            data.frame(parms = "b0", fxn_comp = "beta", coef_id = "b0",
                                       bad_parms = "FALSE", means = 1, sd = 1),
                            subset(self$parm_df, self$parm_df$fxn_comp == "beta"))
      # adjust coefficients to realistic values to setup the logistic curve using the
      # summary of the response variable
      private$.makeInitCoefEsts()

      # fit alpha coefficients with only 0 rate data, store alpha coefs to use
      private$.lockAlpha()

      ## fit final model with beta, delta, gamma after alpha locked
      # save formula, return model
      private$.fitFinalMod()

      self$dat$val$pred <- predict(self$mod, self$dat$val) %>% as.numeric()
      self$dat$val <- OFPE::valPrep(self$dat$val,
                                    self$respvar,
                                    self$expvar,
                                    self$num_means)
      self$fieldname <- OFPE::uniqueFieldname(self$dat$val)
      return(self$mod)
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
        ## Save main diagnostics
        std_res <- self$mod$m$resid()/sigma(self$mod)
        png(paste0(out_path, "/Outputs/Diagnostics/", self$respvar, "_",
                   self$fieldname, "_NonLinear_Logistic_diagnostics.png"),
            width = 10, height = 10, units = 'in', res = 100)
        par(mfrow=c(2,2))
        qqnorm(std_res, ylab = "Standardized Residuals")
        plot(self$mod$m$fitted(),
             self$mod$m$resid(),
             pch=1,
             xlab = "Fitted Values",
             ylab = "Residuals",
             main = "Residuals vs Fitted Values")
        hist(residuals(self$mod),
             xlab = "Residuals")
        plot(self$mod$m$fitted(),
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
    .makeInitCoefEsts = function() {
      # make initial estimate based on means
      self$parm_df$est <- ifelse(self$parm_df$means > 1000, 0.001,
                                 ifelse(self$parm_df$means > 100, 0.01,
                                        ifelse(self$parm_df$means > 10, 0.1, 1)))
      # calculate initial alpha and beta estimates
      alpha <- sum(
        as.numeric(na.omit(self$parm_df[self$parm_df$fxn_comp == "alpha", "est"])) *
          as.numeric(na.omit(self$parm_df[self$parm_df$fxn_comp == "alpha", "means"]))
      )
      beta <- sum(
        as.numeric(na.omit(self$parm_df[self$parm_df$fxn_comp == "beta", "est"])) *
          as.numeric(na.omit(self$parm_df[self$parm_df$fxn_comp == "beta", "means"]))
      )
      # adjust estimates to match to 1st and 3rd quantile of resp (sets up initial logistic curve)
      resp_smry <- summary(self$dat$trn[which(names(self$dat$trn) %in% self$respvar)][[1]]) %>%
        as.numeric()
      resp_sd <- sd(self$dat$trn[which(names(self$dat$trn) %in% self$respvar)][[1]])
      est_alpha <- resp_smry[2] - ((resp_smry[2] - resp_smry[1]) / 2)
      est_beta <- resp_smry[5]
      if(alpha < est_alpha - 0.5 * resp_sd | alpha > est_alpha + 0.5 * resp_sd | alpha <= 0){
        self$parm_df[which(self$parm_df$coef_id %in% "a0"), "est"] <- alpha + (est_alpha - alpha)
      }
      if(beta < est_beta - 0.5 * resp_sd | beta > est_beta + 0.5 * resp_sd | beta <= 0){
        self$parm_df[which(self$parm_df$coef_id %in% "b0"), "est"] <- beta + (est_beta - beta)
      }
      self$parm_df <- self$parm_df[order(self$parm_df$coef_id), ]
      row.names(self$parm_df) <- 1:nrow(self$parm_df)
      self$parm_df[which(self$parm_df$coef_id %in% "EXP"), "bad_parms"] <- TRUE
    },
    .lockAlpha = function() {
      min_exp <- min(self$dat$trn[which(names(self$dat$trn) %in% self$expvar)][[1]], na.rm = TRUE)
      sd_exp <- sd(self$dat$trn[which(names(self$dat$trn) %in% self$expvar)][[1]], na.rm = TRUE)
      d_zero <- self$dat$trn[self$dat$trn[which(names(self$dat$trn) %in% self$expvar)][[1]] <= min_exp, ]
      if (nrow(d_zero) < min_exp + 10) {
        d_zero <- self$dat$trn[self$dat$trn[which(names(self$dat$trn) %in% self$expvar)][[1]] <= (min_exp - sd_exp), ]
      }
      fxn <- private$.makeFormula(TRUE)
      # make parm start list
      start_list <- as.list(self$parm_df$est[self$parm_df$fxn_comp == "alpha"]) %>%
        `names<-`(self$parm_df$coef_id[self$parm_df$fxn_comp == "alpha"])
      start_list <- start_list[!is.na(start_list)]
      delta <- self$parm_df[self$parm_df$fxn_comp == "EXP", "means"] %>% na.omit() %>% as.numeric()
      gamma <- 0.1
      b0 <- self$parm_df[self$parm_df$coef_id == "b0", "est"] %>% na.omit() %>% as.numeric()
      mod0 <- minpack.lm::nlsLM(as.formula(fxn),
                  data = d_zero,
                  control = nls.control(maxiter = 500, minFactor = 1e-10),
                  start = start_list)
      self$parm_df[self$parm_df$coef_id %in% names(summary(mod0)$coefficients[, 1]), "est"] <-
        as.numeric(summary(mod0)$coefficients[, 1])
    },
    logistic2 = function(alpha, beta, delta, gamma, EXP) {
      ### Logistic function ###
      ## alpha => baseline effect (value when nitrogen=0)
      ## beta => max fertilizer effect (asymptote)
      ## gamma => fertilizer efficiency (how quickly the curve rises)
      ## delta => inflection point
      ## EXP => nitrogen/seed applied
      r <- alpha + ((beta - alpha)/(1 + exp(-gamma * (EXP - delta))))
      return(r)
    },
    .fitFinalMod = function() {
      ## lock alpha and fit beta delta gamma
      self$form <- private$.makeFormula()
      delta <- self$parm_df[self$parm_df$fxn_comp == "EXP", "means"] %>% na.omit() %>% as.numeric()
      # make parm start list & fit model
      self$mod <- tryCatch(
        {start_list <- as.list(self$parm_df$est[self$parm_df$fxn_comp == "beta"]) %>%
          `names<-`(self$parm_df$coef_id[self$parm_df$fxn_comp == "beta"])
        start_list <- start_list[!is.na(start_list)]
        start_list$delta <- delta
        gamma <- 0.1
        minpack.lm::nlsLM(as.formula(self$form),
            data = self$dat$trn,
            control = nls.control(maxiter = 500, minFactor = 1e-10),
            start = start_list)},
      warning = function(w) {},
      error = function(e) {
        start_list <- as.list(self$parm_df$est[self$parm_df$fxn_comp == "beta"]) %>%
          `names<-`(self$parm_df$coef_id[self$parm_df$fxn_comp == "beta"])
        start_list <- start_list[!is.na(start_list)]
        delta <- delta
        gamma <- 0.1
        minpack.lm::nlsLM(as.formula(self$form),
            data = self$dat$trn,
            control = nls.control(maxiter = 500, minFactor = 1e-10),
            start = start_list)
      })

      self$parm_df <- rbind(self$parm_df, c("delta", "delta", "delta", FALSE, 1, 1, delta))
      #self$parm_df$bad_parms <- as.logical(self$parm_df$bad_parms)
      self$parm_df[self$parm_df$coef_id %in% names(summary(self$mod)$coefficients[, 1]), "est"] <-
        as.numeric(summary(self$mod)$coefficients[,1])
    },
    .makeFormula = function(fit_alpha = FALSE) {
      stopifnot(is.logical(fit_alpha))

      if (fit_alpha) {
        alpha <- c(paste0(self$parm_df$coef_id[self$parm_df$fxn_comp == "alpha"] %>% na.omit() %>% as.character(), "*",
                          self$parm_df$parms[self$parm_df$fxn_comp == "alpha"] %>% na.omit() %>% as.character()))
        alpha <- c("a0", alpha[2:length(alpha)])
        beta <- "b0"
      } else {
        alpha <- c(paste0(self$parm_df$est[self$parm_df$fxn_comp == "alpha"] %>% na.omit() %>% as.character(), "*",
                          self$parm_df$parms[self$parm_df$fxn_comp == "alpha"] %>% na.omit() %>% as.character()))
        alpha <- c(self$parm_df$est[self$parm_df$coef_id == "a0"] %>% na.omit() %>% as.character(), alpha[2:length(alpha)])
        beta <- c(paste0(self$parm_df$coef_id[self$parm_df$fxn_comp == "beta"] %>% na.omit() %>% as.character(), "*",
                         self$parm_df$parms[self$parm_df$fxn_comp == "beta"] %>% na.omit() %>% as.character()))
        beta <- c("b0", beta[2:length(beta)])
        if (any(as.logical(self$parm_df[self$parm_df$fxn_comp == "beta", "bad_parms"]))) {
          beta <- beta[-grep(paste(self$parm_df$parms[which(as.logical(self$parm_df$bad_parms))], collapse = "|"), beta)]
        }
      }
      if (any(as.logical(self$parm_df[self$parm_df$fxn_comp == "alpha", "bad_parms"]))) {
        alpha <- alpha[-grep(paste(self$parm_df$parms[which(as.logical(self$parm_df$bad_parms))], collapse = "|"), alpha)]
      }
      # make function statement
      fxn <- paste0("private$logistic2(", paste(alpha, collapse = " + "), ", ",
                    paste(beta, collapse = " + "), ", ", "delta, ", "gamma, ",
                    self$expvar, ")")
      fxn <- paste0(ifelse(self$respvar == "yld", "yld", "pro"), " ~ ", fxn)
      return(fxn)
    }
  )
)



