

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::cube get_tvg(arma::mat obs, arma::uvec lags) {

  // Get dimensions from the input observation matrix
  int ns = obs.n_rows; // Number of spatial locations
  int nt = obs.n_cols; // Number of time points
  int nlags = lags.n_elem; // Number of lags

  arma::cube gmat(ns, ns, nlags);
  gmat.fill(arma::datum::nan);

  // Loop over the lags
  for(int k_idx = 0; k_idx < nlags; ++k_idx) {
    int k = lags(k_idx); // Get the current lag value

    // Check if the lag is valid for the time series length
    if(k > nt) {
      continue; // Skip if lag is too large
    }

    for(int i = 0; i < ns; ++i) {
      for(int l = 0; l < ns; ++l) {

        double sum = 0.0;
        double count = 0.0; // Counter for valid pairs

        for(int j = 0; j < (nt - k); ++j) {
          // Check for NaN (missing values)
          if(!std::isnan(obs(i, j + k)) && !std::isnan(obs(l, j))) {
            sum += pow(obs(i, j + k) - obs(l, j), 2);
            count += 1.0;
          }
        }

        // Calculate variogram only if there are valid pairs
        if(count > 0.0) {
          double vario_val = (1.0 / (2.0 * count)) * sum;
          gmat(i, l, k_idx) = vario_val;
        } else {
          // If no valid pairs, set to NaN
          gmat(i, l, k_idx) = arma::datum::nan;
        }
      }
    }


  }


  return gmat;
}
