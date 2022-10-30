#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(Rcpp)]]

// Select remote sensing data for analysis.

//' Function for subsetting remotely sensed data from the user
//' selected source. This function takes a matrix with all the
//' remotely sensed data and returns a matrix with the selected
//' data. This function exists because data such as precipitation and
//' vegetation indices are collected from multiple sources, and
//' the user can select the source based on their data preferences.
//'
//' The user also must pass in the column index of the data passed
//' in that corresponds to the primary and secondary source for each
//' variable.
//'
//' @param dat A matrix with all of the remotely sensed data to
//' select from.
//' @param rr The number of rows in dat and df (must be the same).
//' @param prec_cy_1 The column index of the preferred precipitation source for
//' the current year.
//' @param prec_cy_2 The column index of the secondary precipitation source for
//' the current year.
//' @param prec_py_1 The column index of the preferred precipitation source for
//' the previous year.
//' @param prec_py_2 The column index of the secondary precipitation source for
//' the previous year.
//' @param gdd_cy_1 The column index of the preferred growing degree day source
//' for the current year.
//' @param gdd_cy_2 The column index of the secondary growing degree day source
//' for the current year.
//' @param gdd_py_1 The column index of the preferred growing degree day source
//' for the previous year.
//' @param gdd_py_2 The column index of the secondary growing degree day source
//' for the previous year.
//' @param veg_cy_1 The column index of the preferred vegetation index source
//' for the current year.
//' @param veg_cy_2 The column index of the secondary vegetation index source
//' for the current year.
//' @param veg_py_1 The column index of the preferred vegetation index source
//' for the previous year.
//' @param veg_py_2 The column index of the secondary vegetation index source
//' for the previous year.
//' @param veg_2py_1 The column index of the preferred vegetation index source
//' for two years prior.
//' @param veg_2py_2 The column index of the secondary vegetation index source
//' for two years prior.
//' @return df Returns a completed table with selected data.
//' @export
// [[Rcpp::export]]
arma::mat selectDatCpp(arma::mat dat,
                        int rr,
                        int prec_cy_1,
                        int prec_cy_2,
                        int prec_py_1,
                        int prec_py_2,
                        int gdd_cy_1,
                        int gdd_cy_2,
                        int gdd_py_1,
                        int gdd_py_2,
                        int veg_cy_1,
                        int veg_cy_2,
                        int veg_py_1,
                        int veg_py_2,
                        int veg_2py_1,
                        int veg_2py_2) {
  arma::mat df(rr, 7);
  for (int ii = 0; ii < rr; ii++) {
    // prec_cy
    if (R_IsNA(dat(ii, prec_cy_1))) {
      df(ii, 0) = dat(ii, prec_cy_2);
    } else {
      df(ii, 0) = dat(ii, prec_cy_1);
    }
    // prec_py
    if (R_IsNA(dat(ii, prec_py_1))) {
      df(ii, 1) = dat(ii, prec_py_2);
    } else {
      df(ii, 1) = dat(ii, prec_py_1);
    }
    // gdd_cy
    if (R_IsNA(dat(ii, gdd_cy_1))) {
      df(ii, 2) = dat(ii, gdd_cy_2);
    } else {
      df(ii, 2) = dat(ii, gdd_cy_1);
    }
    // gdd_py
    if (R_IsNA(dat(ii, gdd_py_1))) {
      df(ii, 3) = dat(ii, gdd_py_2);
    } else {
      df(ii, 3) = dat(ii, gdd_py_1);
    }
    // veg_cy
    if (R_IsNA(dat(ii, veg_cy_1))) {
      df(ii, 4) = dat(ii, veg_cy_2);
    } else {
      df(ii, 4) = dat(ii, veg_cy_1);
    }
    // veg_py
    if (R_IsNA(dat(ii, veg_py_1))) {
      df(ii, 5) = dat(ii, veg_py_2);
    } else {
      df(ii, 5) = dat(ii, veg_py_1);
    }
    // veg_2py
    if (R_IsNA(dat(ii, veg_2py_1))) {
      df(ii, 6) = dat(ii, veg_2py_2);
    } else {
      df(ii, 6) = dat(ii, veg_2py_1);
    }
  }
  return df;
}

