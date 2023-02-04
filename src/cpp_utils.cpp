#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
IntegerVector unique_id(CharacterVector x, int n) {
  std::map<String, int> counts;
  IntegerVector idx = no_init_vector(n);

  for (int i = 0; i < n; i++) {
    idx[i] = ++counts[x[i]];
  }

  for (int i=0; i < n; i++) {
    if (counts[x[i]] == 1) idx[i] = 0;
  }

  return idx;
}


