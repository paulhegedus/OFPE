#' @title R6 Class for working with nitrogen use efficiency
#'
#' @description R6 class for fitting the NUE model based on a mass balance 
#' approach using intensive soil and plant biomass sampling outlined in 
#' Hegedus & Ewing, 2022. NUE is calculated as the ratio of total N taken up 
#' by a crop over the amount of N available from fertilization, atmospheric 
#' deposition (negligible), and mineralized N. Mineralized N is estimated from 
#' a general model in Vigil et al. 2002. 
#' 
#' The NUE model is fit using open-source covariates aggregated per the 
#' OFPE framework and coinciding with the data used for crop responses models
#' and in data in the 'sat' tables of farmer's aggregated schemas. The NUE 
#' model is used when the optimization method in 'SimClass' is set to 'ecol'. 
#' 
#' Upon initialization of this class, the training data with NUE observations are 
#' used to train a support vector regression model characterizing NUE. There are 
#' methods for feeding in data for which to predict NUE on. Additionally, there 
#' is a method for plotting NUE across a covariate, such as N fertilizer.
#' 
#' When used in the 'SimClass', the NUE is calculated at each point in the simulation 
#' dataset for every N rate. When net-return is calculated, a method in this class
#' can be called to calculate the cost of inefficiency (AN x CN) - (AN x NUE x CN) 
#' which is subtracted from net-return. Then the profit maximizing N rate is found 
#' because it is the rate that maximizes profit and minimizes the cost of N inefficiency.
#' @seealso \code{\link{SimClass}} for use of the NUE class in the simulation and 
#' derivation of optimized rates.
#' @export
NUE <- R6::R6Class(
  "NUE",
  public = list(
    #' @field trn_dat Data used for training, built into package.
    trn_dat = NULL,
    #' @field mod_type Type of model to use for NUE.
    mod_type = NULL,
    #' @field m Trained NUE model used to make predictions of NUE in new 
    #' datasets.
    m = NULL,
    #' @field utm_epsg The code for the UTM zone that the field falls within (e.g. 32612).
    utm_epsg = NULL,
    
    #' @param mod_type Type of model to use for NUE. Defaults to support vector 
    #' regresssion "SVR" but user has options for simple linear regression "SLR", 
    #' multiple linear regression "MLR", polynomial regression with a cubic 
    #' term on N "PLR", non-linear exponential decay "NLR", generalized additive 
    #' modeling "GAM", or random forest regression "RF". 
    #' @param utm_epsg The code for the UTM zone that the field falls within (e.g. 32612).
    #' @return A NUE model is fit and initialized in the class.
    initialize = function(mod_type = "SVR", utm_epsg) {
      stopifnot(is.character(mod_type),
                length(mod_type) == 1, 
                grepl("SVR|SLR|MLR|PLR|NLR|RF|GAM", mod_type))
      self$mod_type <- mod_type
      self$utm_epsg <- utm_epsg
      # format data
      self$trn_dat <- split(OFPE::NUE_TrnDat, OFPE::NUE_TrnDat$field) %>% 
        lapply(function(x) {x$X <- x$x; x$Y <- x$y; return(x)}) %>% 
        lapply(sf::st_as_sf, coords = c("X", "Y"), crs = self$utm_epsg) %>% 
        do.call(rbind, .) 
      # remove na points
      self$trn_dat <- self$trn_dat[!is.na(self$trn_dat$NUE_noTN), ]
      names(self$trn_dat)[grep("aa_N_kgha", names(self$trn_dat))] <- "EXP"
      
      dat.coords <- sf::st_coordinates(self$trn_dat)
      dat.nb <- spdep::dnearneigh(dat.coords, 0, 400, longlat = FALSE) # Finds neighbours between 0m and 100 away based on their coordinates
      attr(dat.nb, "region.id") <- as.character(1:length(dat.nb))
      dat.W <- spdep::nb2listw(dat.nb, style = "W", zero.policy = TRUE)
      
      if (mod_type == "SLR") {
        m <- spatialreg::errorsarlm(log(NUE_noTN) ~ EXP, 
                                         data = self$trn_dat,
                                         listw = dat.W,
                                         zero.policy = TRUE)
      }
      
      if (mod_type == "MLR") {
        parm_list <- list(respvar = "NUE_noTN", 
                          expvar = "EXP",
                          tryvars = c("WHC", 
                                      "elev", "slope", "tpi", "aspect_sin", "aspect_cos",
                                      "prec_cy_g", "prec_py_g", # "gdd_py_g", 
                                      "ndvi_cy_l", "ndvi_py_l", "ndvi_2py_l",
                                      "ndwi_cy_l", "ndwi_py_l", "ndwi_2py_l",
                                      "bulkdensity", "claycontent", "watercontent", 
                                      "phw", "carboncontent"))
        mlr.sp <- private$.mlr_feat_select(self$trn_dat, parm_list)
        mlr.sp$feat_aic$use <- as.logical(mlr.sp$feat_aic$use)
        good_covars <- paste(mlr.sp$feat_aic$covar[mlr.sp$feat_aic$use], collapse = " + ")
        mlr_form <- paste0("log(", parm_list$respvar, ")", " ~ ", good_covars) %>% 
          as.formula() 
        m <- spatialreg::errorsarlm(formula = mlr_form,
                                         data = self$trn_dat,
                                         listw = dat.W,
                                         zero.policy = TRUE)
        m$call$formula <- mlr_form
      }
      
      if (mod_type == "PLR") {
        m <- spatialreg::errorsarlm(NUE_noTN ~ EXP^3, 
                                         data = self$trn_dat,
                                         listw = dat.W,
                                         zero.policy = TRUE)
      }
      
      if (mod_type == "NLR") {
        m <- tryCatch({
          init_fit_log <- stats::nls(NUE_noTN ~ stats::SSasymp(EXP, yf, y0, log_alpha), 
                              data = self$trn_dat,
                              control = stats::nls.control(maxiter = 1000,
                                                    minFactor = 1/10240))
          nlme::gnls(NUE_noTN ~ stats::SSasymp(EXP, yf, y0, log_alpha),
                     data = self$trn_dat,
                     correlation = nlme::corGaus(form = ~ x + y),
                     control = nlme::gnlsControl(tolerance = 10000),
                     start = stats::coef(init_fit_log))
        }, 
        warning = function(w){},
        error = function(e) {
          spatialreg::errorsarlm(NUE_noTN ~ EXP^3, 
                                 data = self$trn_dat,
                                 listw = dat.W,
                                 zero.policy = TRUE)
        })
      }
      
      if (mod_type == "GAM") {
        parm_list <- list(respvar = "NUE_noTN", 
                          expvar = "EXP",
                          tryvars = c("elev",  "tpi", "aspect_sin", "aspect_cos", # "slope",
                                      "prec_cy_g", "prec_py_g", # "gdd_py_g", 
                                      "ndvi_cy_l", "ndvi_py_l", "ndvi_2py_l",
                                      "ndwi_cy_l", "ndwi_py_l", "ndwi_2py_l",
                                      "bulkdensity", "claycontent", "watercontent", 
                                      "phw", "carboncontent", "WHC"))
        parm_list$cat <- c(rep(FALSE, length(parm_list$tryvars) - 1), TRUE)
        gam.sp <- private$.gam_feat_select(self$trn_dat, parm_list)
        gam.sp$feat_aic$use <- as.logical(gam.sp$feat_aic$use)
        gam.sp$feat_aic$cat <- as.logical(gam.sp$feat_aic$cat)
        parms <- gam.sp$feat_aic[gam.sp$feat_aic$use, ]
        cat_parms <- parms[parms$cat, ]
        num_parms <- parms[!parms$cat, ]
        form <- .makeFormula(paste0("log(", parm_list$respvar, ")"), num_parms$covar, num_parms$k)
        form <- c(form, cat_parms$covar) %>% 
          paste(collapse = " + ")
        m <- suppressWarnings(gam(as.formula(form), data = self$trn_dat))
      }
      
      if (mod_type == "RF") {
        parm_list <- list(respvar = "NUE_noTN", 
                          expvar = "EXP",
                          tryvars = c("WHC", # "yld", "pro",
                                      "elev", "slope", "tpi", "aspect_sin", "aspect_cos",
                                      "prec_cy_g", "prec_py_g", # "gdd_py_g", 
                                      "ndvi_cy_l", "ndvi_py_l", "ndvi_2py_l",
                                      "ndwi_cy_l", "ndwi_py_l", "ndwi_2py_l",
                                      "bulkdensity", "claycontent", "watercontent", 
                                      "phw", "carboncontent"))
        rf.sp <- private$.rf_feat_select(self$trn_dat, parm_list, seed = 2542)
        rf.sp$feat_rmse$use <- as.logical(rf.sp$feat_rmse$use)
        good_covars <- paste(rf.sp$feat_rmse$covar[rf.sp$feat_rmse$use], collapse = " + ")
        rf_form <- paste0("log(", parm_list$respvar, ")", " ~ ", good_covars, " + x + y") %>% 
          as.formula() 
        m <- ranger::ranger(formula = rf_form,
                                data = self$trn_dat %>% sf::st_drop_geometry(),
                                num.trees = rf.sp$m$num.trees,
                                mtry = rf.sp$m$mtry,
                                replace = TRUE)
      }
      
      if (mod_type == "SVR") {
        parm_list <- list(respvar = "NUE_noTN", 
                          expvar = "EXP",
                          tryvars = c("WHC", # "yld", "pro",
                                      "elev", "slope", "tpi", "aspect_sin", "aspect_cos",
                                      "prec_cy_g", "prec_py_g", # "gdd_py_g", 
                                      "ndvi_cy_l", "ndvi_py_l", "ndvi_2py_l",
                                      "ndwi_cy_l", "ndwi_py_l", "ndwi_2py_l",
                                      "bulkdensity", "claycontent", "watercontent", 
                                      "phw", "carboncontent"))
        svr.sp <- private$.svr_feat_select(self$trn_dat, parm_list, seed = 2542)
        m <- svr.sp$m
      }
      self$m <- m
    },
    
    #' @param new_dat New data to make predictions of NUE in. 
    #' @return Predictions of NUE for each observation in the new data.
    nuePred = function(new_dat) {
      stopifnot(any(grepl("WHC", names(new_dat))),
                any(grepl("EXP", names(new_dat))),
                any(grepl("^x$", names(new_dat))),
                any(grepl("^y$", names(new_dat))))
      
      new_dat$X <- new_dat$x
      new_dat$Y <- new_dat$y
      new_dat <- sf::st_as_sf(new_dat, coords = c("X", "Y"), crs = self$utm_epsg)
      
      
      if (self$mod_type == "SLR") {
        preds <- predict(self$m, new_dat)[,1] %>% exp()
      }
      
      if (self$mod_type == "MLR") {
        preds <- predict(self$m, new_dat)[,1] %>% exp()
      }
      
      if (self$mod_type == "PLR") {
        preds <- predict(self$m, new_dat)[,1]
      }
      
      if (self$mod_type == "NLR") {
        preds <- predict(self$m, new_dat) %>% as.numeric()
      }
      
      if (self$mod_type == "GAM") {
        preds <- predict(self$m, new_dat) %>% exp()
      }
      
      if (self$mod_type == "RF") {
        preds <- predict(self$m, new_dat %>% sf::st_drop_geometry())$predictions %>% exp()
      }
      
      if (self$mod_type == "SVR") {
        preds <- predict(self$m, new_dat)
      }
      return(preds)
    }
  ),
  private = list(
    .split_sample = function(x, n, seed = NULL) {
      if (!is.null(seed)) {
        # set.seed(2542)
        set.seed(seed)
      }
      ids <- sample(1:nrow(x), n)
      x$split <- "test"
      x[ids, "split"] <- "train"
      return(x)
    },
    .mlr_feat_select = function(trn, parm_list) {
      parm_df <- data.frame(
        parms = parm_list$tryvars[-grep("WHC", parm_list$tryvars)],
        bad_parms = FALSE,
        means = NA,
        sd = NA
      )
      parm_df <- OFPE::findBadParms(parm_df, trn)
      parm_list$tryvars <- c("WHC", parm_list$tryvars[-grep(paste(parm_df$parms[parm_df$bad_parms], collapse = "|"), parm_list$tryvars)])
      
      # aic table
      feat_aic <- data.frame(covar = c(parm_list$expvar, parm_list$tryvars))
      feat_aic$AIC <- NA
      feat_aic$deltaAIC <- NA
      feat_aic$use <- TRUE
      
      # spatial stuff
      trn.coords <- sf::st_coordinates(trn)
      trn.nb <- spdep::dnearneigh(trn.coords, 0, 500, longlat = FALSE) # Finds neighbours between 0m and 500 away based on their coordinates
      attr(trn.nb, "region.id") <- as.character(1:length(trn.nb))
      trn.W <- spdep::nb2listw(trn.nb, style = "W", zero.policy = TRUE)
      
      # fit initial model
      good_covars <- paste(feat_aic$covar[feat_aic$use], collapse = " + ")
      full_form <- paste0(parm_list$respvar, " ~ ", good_covars)
      full_mod <- spatialreg::errorsarlm(as.formula(full_form), 
                                         data = trn,
                                         listw = trn.W,
                                         zero.policy = TRUE)
      full_aic <- round(AIC(full_mod), 2)
      init_aic <- full_aic
      # check each covar
      for (i in 1:nrow(feat_aic)) {
        feat_aic[i, "use"] <- FALSE
        good_covars <- paste(feat_aic$covar[feat_aic$use], collapse = " + ")
        form <- paste0(parm_list$respvar, " ~ ", good_covars)
        m <- spatialreg::errorsarlm(as.formula(form), 
                                    data = trn,
                                    listw = trn.W,
                                    zero.policy = TRUE)
        diff_aic <- round((AIC(m) %>% round(4)) - init_aic, 4)
        if (diff_aic <= -2) {
          init_aic <- AIC(m) %>% round(4)
        } else {
          feat_aic$use[i] <- TRUE
        }
        feat_aic$AIC[i] <- AIC(m)
        feat_aic$deltaAIC[i] <- diff_aic
        feat_aic$use <- as.logical(feat_aic$use)
        rm(m)
      }
      # fit final model
      good_covars <- paste(feat_aic$covar[feat_aic$use], collapse = " + ")
      form <- paste0(parm_list$respvar, " ~ ", good_covars)
      m <- spatialreg::errorsarlm(as.formula(form), 
                                  data = trn,
                                  listw = trn.W,
                                  zero.policy = TRUE)
      feat_aic <- rbind(c("Full Model", full_aic, NA, FALSE),
                        feat_aic)
      return(list(m = m, 
                  feat_aic = feat_aic))
    },
    .makeFormula = function(respvar, parms = NULL, K = -1, BS = "ts", xyK = 3) {
      # x and y accounting for spatial autocorrelation
      fxn <- paste(
        c(paste0("s(x, y, bs = 'gp', k = ", xyK, ", m = 2)"),
          paste0("s(", parms, ", k = ",
                 K, ", bs = '", BS, "')")),
        collapse = " + ")
      fxn <- paste0(respvar, " ~ ", fxn)
      return(fxn)
    },
    .findK = function(feat_aic, trn) {
      ## brute force method for finding a reasonable 'k' estimate.
      good_covar <- feat_aic[feat_aic$use & !feat_aic$cat, "covar" ]
      
      for (i in 1:length(good_covar)) {
        unq_vals <- unique(
          trn[, which(names(trn) %in% as.character(good_covar[i]))],
          na.rm = TRUE
        )
        if (length(unq_vals) <= 10) {
          tryK <- c(9, 7, 5, 3, 2, 1)
          for (j in 1:length(tryK)) {
            if (!exists("foundK")) { foundK <- FALSE }
            # if foundK  = FALSE (have not found a k that fits, keep trying)
            if (!foundK) {
              # set the k in the paramter table to the k estimate
              feat_aic[grep(paste0("^", good_covar[i], "$"),
                            feat_aic$covar), "k"] <- tryK[j]
              # make the function statement
              fxn <- .makeFormula(respvar = "NUE_noTN", 
                                  parms = feat_aic[grep(paste0("^", good_covar[i], "$"), feat_aic$covar), "covar"],
                                  K = feat_aic[grep(paste0("^", good_covar[i], "$"), feat_aic$covar), "k"])
              
              # fit model with the estimated k
              # rand_rows <- runif(nrow(trn) * 0.25, 1, nrow(trn) + 1) %>%
              #   as.integer()
              tryCatch(
                {suppressWarnings(gam(as.formula(fxn), data = trn))
                  foundK <- TRUE },
                warning = function(w) {foundK <- FALSE },
                error = function(e) {foundK <- FALSE })
              # if the model was fit then foundK = T & k in feat_aic table
              # otherwise is FALSE and will try the next k
            }
          } # end tryK
          # if no k found
          if (!foundK) {
            if (grepl("^aa_n$", feat_aic$covar[i])) {
              feat_aic[grep(paste0("^", good_covar[i], "$"),
                            feat_aic$covar), "use"] <- TRUE
              feat_aic[grep(paste0("^", good_covar[i], "$"),
                            feat_aic$covar), "k"] <- length(unq_vals) - 1
            } else {
              feat_aic[grep(paste0("^", good_covar[i], "$"),
                            feat_aic$covar), "use"] <- FALSE
              feat_aic[grep(paste0("^", good_covar[i], "$"),
                            feat_aic$covar), "k"] <- NA
            }
          }
          rm(foundK) # remove the indicator for the next var in loop
        }
      }
      return(feat_aic)
    },
    .gam_feat_select = function(trn, parm_list) {
      parm_df <- data.frame(
        parms = parm_list$tryvars[-grep("WHC", parm_list$tryvars)],
        bad_parms = FALSE,
        means = NA,
        sd = NA
      )
      parm_df <- OFPE::findBadParms(parm_df, trn)
      parm_list$tryvars <- c("WHC", parm_list$tryvars[-grep(paste(parm_df$parms[parm_df$bad_parms], collapse = "|"), parm_list$tryvars)])
      
      feat_aic <- data.frame(covar = c(parm_list$expvar, parm_list$tryvars))
      feat_aic$AIC <- NA
      feat_aic$deltaAIC <- NA
      feat_aic$use <- TRUE
      feat_aic$cat <- FALSE
      feat_aic$cat[grep("WHC", feat_aic$covar)] <- TRUE
      feat_aic$k <- 10
      feat_aic <- private$.findK(feat_aic, trn)
      
      # full model
      parms <- feat_aic[feat_aic$use, ]
      cat_parms <- parms[parms$cat, ]
      num_parms <- parms[!parms$cat, ]
      full_form <- private$.makeFormula(parm_list$respvar, num_parms$covar, num_parms$k)
      full_form <- c(full_form, cat_parms$covar) %>% 
        paste(collapse = " + ")
      full_mod <- suppressWarnings(gam(as.formula(full_form), data = trn))
      full_aic <- round(AIC(full_mod), 4)
      init_aic <- full_aic
      
      # feat_aic <- feat_aic[sample(1:nrow(feat_aic)), ]
      
      for (i in 1:nrow(feat_aic)) {
        feat_aic[i, "use"] <- FALSE
        parms <- feat_aic[feat_aic$use, ]
        cat_parms <- parms[parms$cat, ]
        num_parms <- parms[!parms$cat, ]
        form <- private$.makeFormula(parm_list$respvar, num_parms$covar, num_parms$k)
        form <- c(form, cat_parms$covar) %>% 
          paste(collapse = " + ")
        
        m <- suppressWarnings(gam(as.formula(form), data = trn))
        diff_aic <- round((AIC(m) %>% round(4)) - init_aic, 4)
        if (diff_aic <= -2) {
          init_aic <- AIC(m) %>% round(4)
        } else {
          feat_aic$use[i] <- TRUE
        }
        feat_aic$AIC[i] <- AIC(m) %>% round(4)
        feat_aic$deltaAIC[i] <- diff_aic
        feat_aic$use <- as.logical(feat_aic$use)
      }
      
      # fit final model
      parms <- feat_aic[feat_aic$use, ]
      cat_parms <- parms[parms$cat, ]
      num_parms <- parms[!parms$cat, ]
      form <- private$.makeFormula(parm_list$respvar, num_parms$covar, num_parms$k)
      form <- c(form, cat_parms$covar) %>%
        paste(collapse = " + ")
      m <- suppressWarnings(gam(as.formula(form), data = trn))
      
      feat_aic <- rbind(c("Full Model", full_aic, NA, FALSE, FALSE, NA),
                        feat_aic)
      return(list(m = m, 
                  feat_aic = feat_aic))
    },
    .rf_feat_select = function(trn, parm_list, seed = NULL) {
      parm_df <- data.frame(
        parms = parm_list$tryvars[-grep("WHC", parm_list$tryvars)],
        bad_parms = FALSE,
        means = NA,
        sd = NA
      )
      parm_df <- OFPE::findBadParms(parm_df, trn)
      parm_list$tryvars <- c("WHC", parm_list$tryvars[-grep(paste(parm_df$parms[parm_df$bad_parms], collapse = "|"), parm_list$tryvars)])
      
      # make table for tracking covariate selection
      feat_rmse <- data.frame(covar = c(parm_list$expvar, parm_list$tryvars))
      feat_rmse$RMSE <- NA
      feat_rmse$deltaRMSE <- NA
      feat_rmse$use <- TRUE
      
      # remove NA's
      for (i in 1:length(feat_rmse$covar[feat_rmse$use])) {
        trn_col <- grep(feat_rmse$covar[feat_rmse$use][i], names(trn))
        trn <- trn[!is.na(trn[, trn_col][[1]]), ]
      }
      
      # make trn and test for feature selection
      dat <- trn %>% 
        sf::st_drop_geometry() %>% 
        as.data.frame() %>% 
        split(trn$field)
      dat_n <- lapply(dat, nrow) %>% 
        unlist() %>% 
        lapply(function(x) {round(x * 0.7)})
      if (!is.null(seed)) {
        dat <- mapply(private$.split_sample, dat, dat_n, seed, SIMPLIFY = FALSE)
      } else {
        dat <- mapply(private$.split_sample, dat, dat_n, SIMPLIFY = FALSE)
      }
      dat <- dat %>% 
        lapply(function(x) {x$X <- x$x; x$Y <- x$y; return(x)}) %>% 
        lapply(sf::st_as_sf, coords = c("X", "Y"), crs = self$utm_epsg) %>% 
        do.call(rbind, .) %>% 
        sf::st_drop_geometry()
      trn <- dat[dat$split == "train", ]
      val <- dat[dat$split == "test", ]
      rm(dat)
      
      # fit initial model
      good_covars <- paste(feat_rmse$covar[feat_rmse$use], collapse = " + ")
      full_form <- paste0(parm_list$respvar, " ~ ", good_covars, " + x + y")
      full_mod <- ranger::ranger(as.formula(full_form), data = trn, replace = TRUE)
      val$preds <- predict(full_mod, val)$predictions
      get_cols <- c(parm_list$respvar, "preds")
      preds <- val[!is.na(val$preds), get_cols]
      full_rmse <- Metrics::rmse(preds[[1]], preds$preds) %>% round(4)
      init_rmse <- full_rmse
      
      # check each covar, repeat to reduce uncertainty due to variable omission order
      covar_list <- as.list(1:3)
      for (j in 1:3) {
        feat_rmse$RMSE <- NA
        feat_rmse$deltaRMSE <- NA
        feat_rmse$use <- TRUE
        init_rmse <- full_rmse
        val$preds <- NULL
        for (i in 1:nrow(feat_rmse)) {
          feat_rmse[i, "use"] <- FALSE
          good_covars <- paste(feat_rmse$covar[feat_rmse$use], collapse = " + ")
          form <- paste0(parm_list$respvar, " ~ ", good_covars, " + x + y")
          m <- ranger::ranger(as.formula(form), data = trn, replace = TRUE)
          val$preds <- predict(m, val)$predictions
          get_cols <- c(parm_list$respvar, "preds")
          preds <- val[!is.na(val$preds), get_cols]
          new_rmse <- Metrics::rmse(preds[[1]], preds$preds) %>% round(4)
          diff_rmse <- (new_rmse - init_rmse) %>% round(4)
          
          if (new_rmse < init_rmse) {
            init_rmse <- new_rmse
          } else {
            feat_rmse$use[i] <- TRUE
          }
          feat_rmse$RMSE[i] <- new_rmse
          feat_rmse$deltaRMSE[i] <- diff_rmse
          feat_rmse$use <- as.logical(feat_rmse$use)
          rm(m)
        }
        covar_list[[j]] <- feat_rmse
      }
      
      # select covars that came in any iteration
      temp <- lapply(covar_list, function(x) x$covar[x$use]) %>% 
        do.call(c, .) %>% 
        unique()
      feat_rmse <- covar_list[[3]]
      feat_rmse[grepl(paste(temp, collapse = "|"), feat_rmse$covar), "use"] <- TRUE
      
      ## now tune the model, starting with the number of trees
      treeCheck <- data.frame(rftrees = seq.int(100, 3000, by = 100), rmse = NA)
      for (i in 1:nrow(treeCheck)) {
        form <- paste0(parm_list$respvar, " ~ ", paste(feat_rmse$covar[feat_rmse$use], collapse = " + "), " + x + y")
        m <- ranger::ranger(as.formula(form),
                            data = trn,
                            num.trees = treeCheck$rftrees[i],
                            replace = TRUE)
        val$preds <- predict(m, val)$predictions
        get_cols <- c(parm_list$respvar, "preds")
        preds <- val[!is.na(val$preds), get_cols]
        treeCheck$rmse[i] <- Metrics::rmse(preds[[1]], preds$preds) %>% round(4)
        rm(m)
      }
      gc()
      val$preds <- NULL
      opt_n_tree <- treeCheck[which.min(treeCheck$rmse), "rftrees"]
      ## tune the model with the number of variables selected from at each tree
      maxmtry <- floor(length(feat_rmse$covar[!feat_rmse$use]) / 2) 
      mCheck <- data.frame(m = 1:length(feat_rmse$covar[feat_rmse$use]), rmse = NA)
      for (i in 1:nrow(mCheck)) {
        form <- paste0(parm_list$respvar, " ~ ", paste(feat_rmse$covar[feat_rmse$use], collapse = " + "), " + x + y")
        m <- ranger::ranger(as.formula(form),
                            data = trn,
                            num.trees = opt_n_tree,
                            mtry = mCheck$m[i],
                            replace = TRUE)
        val$preds <- predict(m, val)$predictions
        get_cols <- c(parm_list$respvar, "preds")
        preds <- val[!is.na(val$preds), get_cols]
        mCheck$rmse[i] <- Metrics::rmse(preds[[1]], preds$preds) %>% round(4)
        rm(m)
      }
      gc()
      opt_mtry <- mCheck[which.min(mCheck$rmse), "m"]
      
      # fit final model
      val$preds <- NULL
      trn <- rbind(trn, val)
      form <- paste0(parm_list$respvar, " ~ ", paste(feat_rmse$covar[feat_rmse$use], collapse = " + "), " + x + y")
      m <- ranger::ranger(as.formula(form),
                          data = trn,
                          num.trees = opt_n_tree,
                          mtry = opt_mtry,
                          replace = TRUE)
      feat_rmse <- rbind(c("Full Model", full_rmse, NA, FALSE, FALSE, NA),
                         feat_rmse)
      
      return(list(m = m, 
                  feat_rmse = feat_rmse))
    },
    .svr_feat_select = function(trn, parm_list, seed = NULL) {
      parm_df <- data.frame(
        parms = parm_list$tryvars[-grep("WHC", parm_list$tryvars)],
        bad_parms = FALSE,
        means = NA,
        sd = NA
      )
      parm_df <- OFPE::findBadParms(parm_df, trn)
      parm_list$tryvars <- c("WHC", parm_list$tryvars[-grep(paste(parm_df$parms[parm_df$bad_parms], collapse = "|"), parm_list$tryvars)])
      
      # make table for tracking covariate selection
      feat_rmse <- data.frame(covar = c(parm_list$expvar, parm_list$tryvars))
      feat_rmse$RMSE <- NA
      feat_rmse$deltaRMSE <- NA
      feat_rmse$use <- TRUE
      
      # remove NA's
      for (i in 1:length(feat_rmse$covar[feat_rmse$use])) {
        trn_col <- grep(feat_rmse$covar[feat_rmse$use][i], names(trn))
        trn <- trn[!is.na(trn[, trn_col][[1]]), ]
      }
      
      # make trn and test for feature selection
      dat <- trn %>% 
        sf::st_drop_geometry() %>% 
        as.data.frame() %>% 
        split(trn$field)
      dat_n <- lapply(dat, nrow) %>% 
        unlist() %>% 
        lapply(function(x) {round(x * 0.7)})
      if (!is.null(seed)) {
        dat <- mapply(private$.split_sample, dat, dat_n, seed, SIMPLIFY = FALSE)
      } else {
        dat <- mapply(private$.split_sample, dat, dat_n, SIMPLIFY = FALSE)
      }
      dat <- dat %>% 
        lapply(function(x) {x$X <- x$x; x$Y <- x$y; return(x)}) %>% 
        lapply(sf::st_as_sf, coords = c("X", "Y"), crs = self$utm_epsg) %>% 
        do.call(rbind, .) %>% 
        sf::st_drop_geometry()
      trn <- dat[dat$split == "train", ]
      val <- dat[dat$split == "test", ]
      rm(dat)
      
      # fit initial model
      good_covars <- paste(feat_rmse$covar[feat_rmse$use], collapse = " + ")
      full_form <- paste0(parm_list$respvar, " ~ ", good_covars, " + x + y")
      full_mod <- caret::train(form = as.formula(full_form),
                               data = trn,
                               method = 'svmRadial',
                               preProcess = c("center", "scale"))
      val$preds <- predict(full_mod, val)
      get_cols <- c(parm_list$respvar, "preds")
      preds <- val[!is.na(val$preds), get_cols]
      full_rmse <- Metrics::rmse(preds[[1]], preds$preds) %>% round(4)
      init_rmse <- full_rmse
      
      # check each covar, repeat to reduce uncertainty due to variable omission order
      covar_list <- as.list(1:3)
      for (j in 1:3) {
        feat_rmse$RMSE <- NA
        feat_rmse$deltaRMSE <- NA
        feat_rmse$use <- TRUE
        init_rmse <- full_rmse
        val$preds <- NULL
        for (i in 1:nrow(feat_rmse)) {
          feat_rmse[i, "use"] <- FALSE
          good_covars <- paste(feat_rmse$covar[feat_rmse$use], collapse = " + ")
          form <- paste0(parm_list$respvar, " ~ ", good_covars, " + x + y")
          m <-caret::train(form = as.formula(form),
                           data = trn,
                           method = 'svmRadial',
                           preProcess = c("center", "scale"))
          val$preds <- predict(m, val)
          get_cols <- c(parm_list$respvar, "preds")
          preds <- val[!is.na(val$preds), get_cols]
          new_rmse <- Metrics::rmse(preds[[1]], preds$preds) %>% round(4)
          diff_rmse <- (new_rmse - init_rmse) %>% round(4)
          if (new_rmse < init_rmse) {
            init_rmse <- new_rmse
          } else {
            feat_rmse$use[i] <- TRUE
          }
          feat_rmse$RMSE[i] <- new_rmse
          feat_rmse$deltaRMSE[i] <- diff_rmse
          feat_rmse$use <- as.logical(feat_rmse$use)
          rm(m)
        }
        covar_list[[j]] <- feat_rmse
      }
      
      # select covars that came in any iteration
      temp <- lapply(covar_list, function(x) x$covar[x$use]) %>% 
        do.call(c, .) %>% 
        unique()
      feat_rmse <- covar_list[[3]]
      feat_rmse[grepl(paste(temp, collapse = "|"), feat_rmse$covar), "use"] <- TRUE
      
      # fit final model
      val$preds <- NULL
      trn <- rbind(trn, val)
      form <- paste0(parm_list$respvar, " ~ ", paste(feat_rmse$covar[feat_rmse$use], collapse = " + "), " + x + y")
      m <- caret::train(form = as.formula(form),
                        data = trn,
                        method = 'svmRadial',
                        preProcess = c("center", "scale"))
      feat_rmse <- rbind(c("Full Model", full_rmse, NA, FALSE, FALSE, NA),
                         feat_rmse)
      return(list(m = m, 
                  feat_rmse = feat_rmse))
    }
  )
)