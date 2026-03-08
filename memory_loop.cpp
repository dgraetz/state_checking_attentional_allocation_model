#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List compute_memory_check(CharacterVector sequence, 
                          NumericVector rand,
                          double cr_a, double cr_b,
                          IntegerVector new_iteration) {
  int n = sequence.size();
  CharacterVector memory(n);
  IntegerVector check(n);
  
  for (int i = 0; i < n; i++) {
    if (new_iteration[i] == 1) {
      check[i] = 0;
      memory[i] = sequence[i];
    } else {
      String prev = memory[i - 1];
      double cr = (prev == "A") ? cr_a : cr_b;
      if (rand[i] <= cr) {
        check[i] = 1;
        memory[i] = sequence[i];
      } else {
        check[i] = 0;
        memory[i] = prev;
      }
    }
  }
  
  return List::create(Named("memory") = memory,
                      Named("check") = check);
}