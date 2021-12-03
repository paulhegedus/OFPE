#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]

// Identify the full-field optimum experimental rate using derivatives.

//' Function for identifying the optimum full-field experimental rate based on
//' profit maximization. This function selects the optimum rate as
//' the rate where the difference in full-field net-return between
//' experimental rates (first derivitave of the net-return vs. experimental rate
//' function) does not exceed the cost of applying one more unit of
//' the experimental rate across the entire field.
//'
//' This function calculates the optimum rate from the sum of the
//' net-return for a uniform rate applied across the field to
//' identify the full-field optimum rate.
//'
//' See the documentation in OFPE::derivNRoptCpp for a site-specific
//' example of how the optimum is identified compared to when the
//' optimization method is set to 'max'.
//'
//' @param NRff A matrix with rows for each experimental rate and
//' columns for the rate and the full-field net-return calculated
//' under a uniform application of the corresponding rate.
//' @param rr Integer of the number of observations (rows of NRdf).
//' @param fieldsize Double, the size of the field(s) that the
//' experimental input would be applied to.
//' @param CEXP Double, the cost of the experimental input.
//' @return NRffmax, table with the full-field optimum rate
//' and the full-field net-return.
//' @export
// [[Rcpp::export]]
arma::mat derivFFoptCpp(arma::mat NRff,
                        int rr,
                        double fieldsize,
                        double CEXP) {
  arma::mat NRffmax(1, 2);
  double diff;

  CEXP = CEXP * fieldsize;
  for (int ii = 0; ii < rr; ii++) {
    if (ii != 0) {
      diff = NRff(ii, 1)-NRff(ii - 1, 1);
      if ((diff > CEXP) && (NRff(ii, 1) > NRffmax(0, 1))) {
        NRffmax(0, 0) = NRff(ii, 0);
        NRffmax(0, 1) = NRff(ii, 1);
      }
    } else {
      NRffmax(0, 0) = NRff(ii, 0);
      NRffmax(0, 1) = NRff(ii, 1);
    }
  }
  return NRffmax;
}

