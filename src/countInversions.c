#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

 SEXP do_countInversions(SEXP s_x) {
  double invs;
 /* Unpack the 's_x' vector:
  * n_x - length of the 's_x'
  * x - pointer to the values
  */
  const R_len_t n_x = length(s_x);
  int *x = REAL(s_x);
  
  /* the -loop*/
  size_t i;
  size_t j;
  invs = 0;
  for (i=0; i < n_x; i++) {
    for (j=i+1; j < n_x; j++) {
      if (x[i] > x[j]) {
        invs++;
      }
    }
  }
  /* And finally return the value stored in 'invs' */
  return ScalarReal(invs);
}