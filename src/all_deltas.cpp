#include <Rcpp.h>
using namespace Rcpp;

inline bool is_na(double x) { return NumericVector::is_na(x); }


// [[Rcpp::export]]
IntegerVector all_deltas(NumericVector value, int k, double delta) {
  int n = value.size();
  IntegerVector out(n, NA_INTEGER);

  for (int i = k; i < n; ++i) {
    double window_max = value[i - k];
    double window_min = value[i - k];
    bool any_valid = false;
    for (int j = i - k; j <= i; ++j) {
      double vj = value[j];
      if (!NumericVector::is_na(vj)) {
        if (!any_valid) {
          window_max = window_min = vj;
          any_valid = true;
        } else {
          if (vj > window_max) window_max = vj;
          if (vj < window_min) window_min = vj;
        }
      }
    }
    if (!any_valid) continue;

    double d = window_max - window_min;
    if (d >= delta) {
      out[i] = 1;
    } else if (d <= -delta) {
      out[i] = 0;
    }
    // else remains NA
  }
  return out;
}