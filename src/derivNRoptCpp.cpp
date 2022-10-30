#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]

// Identify site-specific optimum experimental rates using derivatives.

//' Function for identifying the optimum site-specific experimental rate
//' based on profit maximization. This function selects the optimum rate as
//' the rate where the difference in net-return between experimental
//' rates (first derivitave of the net-return vs. experimental rate
//' function) does not exceed the cost of applying one more unit of
//' the experimental rate.
//'
//' This function calculates the optimum rate for every location in
//' the field to identify the site-specific optimum rate.
//'
//' This differs from selecting the rate that yields the highest net-
//' return (selecting 'max' for the optimization method) is highlighted
//' in the following example. The net-return at a point for a rate of X
//' was 120 and the net-return at the point for a rate of X+1 was 121
//' and the cost of one unit of the experimental input was $3.
//' The optimum rate identified in this function would be X, whereas
//' when 'max' is selected X+1 would be the optimum rate because the
//' net-return was higher despite earning less than the cost of the
//' added input.
//'
//' @param NRdf A matrix with rows for each location in the field
//' and columns for the experimental rate applied. Values are the
//' calculated net-returns.
//' @param NRoptDat A matrix with columns for the optimum experimental
//' rate and the associated net-return for each location in the field.
//' This is filled in below.
//' @param Nrates A matrix with one column with each experimental rate
//' in the range specified by the user.
//' @param rr Integer of the number of observations (rows of NRdf).
//' @param cc Integer of the number of experimental rates (rows of Nrates =
//' columns of NRdf).
//' @param CEXP Double, the cost of the experimental input.
//' @return NRoptDat.
//' @export
// [[Rcpp::export]]
arma::mat derivNRoptCpp(arma::mat NRdf,
                        arma::mat NRoptDat,
                        arma::mat Nrates,
                        int rr,
                        int cc,
                        double CEXP) {
  double diff;
  for (int ii = 0; ii < rr; ii++) {
    for (int jj = 0; jj < cc; jj++) {
      if (jj != 0) {
        diff = NRdf(ii, jj) - NRdf(ii, jj - 1);
        if((diff > CEXP) && (NRdf(ii, jj) > NRoptDat(ii, 1))){
          NRoptDat(ii, 0) = Nrates(jj, 0);
          NRoptDat(ii, 1) = NRdf(ii, jj);
        }
      } else {
        NRoptDat(ii, 0) = Nrates(jj, 0);
        NRoptDat(ii, 1) = NRdf(ii, jj);
      }
    }
  }
  return NRoptDat;
}
