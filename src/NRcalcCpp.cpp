#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]

// Calculates net-return for each point for every experimental rate.

//' Function for calculating the net-return for different
//' management scenarios under the selected economic conditions
//' for each location in the data frame. For each point the
//' net-retrun is calculated with the base price, and the
//' opposite system type's base price. If the rate is equal
//' to the farmer selected or the minimum experimental rates, the
//' net-return for those economic conditions are calculated.
//'
//' @param df A matrix with the experimental rate and predicted
//' responses for each data point in the field.
//' @param Bp The base price corresponding to the price for the system
//' type selected by the user (i.e. conventional or organic).
//' @param B0pd The intercept for the protein premium/dockage equation.
//' @param B1pd The coefficient for protein in the protein premium/dockage
//' equation.
//' @param B2pd The coefficient for protein squared for the protein
//' premium/dockage equation.
//' @param CEXP The cost of the experimental input.
//' @param BpOpp The base price corresponding to the price for the opposite
//' system type selected by the user (i.e. conventional or organic).
//' @param FC The fixed costs associated with production per acre, not including
//' the experimental input.
//' @param fs The farmer selected rate or the rate of the experimental
//' input that the farmer would have applied uniformly to the field.
//' @param ssAC The cost of site-specific technology or variable rate
//' application.
//' @param predInd Indicator for whether protein is included or not
//' in the net-return calculations. 1 = yes, 0 = no.
//' @param rr Number of rows in df.
//' @param expCol Indicator for the position of the experimental
//' variable column in df.
//' @param yldCol Indicator for the position of the predicted yield
//' variable column in df.
//' @param proCol Indicator for the position of the predicted protein
//' variable column in df.
//' @param NRcol Indicator for the position of the net-return column
//' in df.
//' @param NRminCol Indicator for the position of the net-return column
//' in df for the minimum experimental rate applied.
//' @param NRoppCol Indicator for the position of the net-return column
//' in df when net-return calculated with the opposite base price as for the
//' system.
//' @param NRfsCol Indicator for the position of the net-return column
//' in df for the farmer selected rate.
//' @param AAmin The minimum experimental rate in the range of rates to
//' simulate in (i.e. 0 lbs N/ac).
//' @return df Matrix filled in with net-returns.
//' @export
// [[Rcpp::export]]
arma::mat NRcalcCpp(arma::mat df,
                        double Bp,
                        double B0pd,
                        double B1pd,
                        double B2pd,
                        double CEXP,
                        double BpOpp,
                        double FC,
                        double fs,
                        double ssAC,
                        int predInd,
                        int rr,
                        int expCol,
                        int yldCol,
                        int proCol,
                        int NRcol,
                        int NRminCol,
                        int NRoppCol,
                        int NRfsCol,
                        int AAmin) {
  double prosq;
  double P;

  for (int ii = 0; ii < rr; ii++) {
    // Rcpp::checkUserInterrupt();
    if (predInd == 1) {
      double float_pro = df(ii, proCol);
      prosq = std::pow(float_pro, 2);
      P = Bp + (B0pd + B1pd * df(ii, proCol) + B2pd * prosq);
      df(ii, NRcol) = (df(ii, yldCol) * P) - CEXP * df(ii, expCol) - FC;
    } else {
      df(ii, NRcol) = (df(ii, yldCol) * Bp) - CEXP * df(ii, expCol) - FC;
    }
    if (df(ii, expCol) == AAmin) {
      df(ii, NRminCol) = df(ii, NRcol);
    }
    if (df(ii, expCol) == fs) {
      df(ii, NRfsCol) = df(ii, NRcol);
    }
    df(ii, NRoppCol) = ((df(ii, yldCol) * 0.6) * BpOpp) - FC;
    df(ii, NRcol) = df(ii, NRcol) - ssAC;
  }
  return df;
}
