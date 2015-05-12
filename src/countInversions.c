#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

int count_inversion_merge(double array[], int first, int last) {
  int mid = floor((first + last) / 2);
  int ai = first;
  int bi = mid + 1;
  double final[last - first + 1];
  int finali = 0;
  int inversion = 0, i;
  
  while (ai <= mid && bi <= last) {
    if (array[ai] <= array[bi]) {
      final[finali++] = array[ai++];
    } else {
      final[finali++] = array[bi++];
      inversion += mid - ai + 1;
    }
  }
  
  while (ai <= mid)
    final[finali++] = array[ai++];
    
  while (bi <= last)
    final[finali++] = array[bi++];
    
  for (i=0 ; i < last - first + 1 ; i++)
    array[i + first] = final[i];
    
  return inversion;
}

int count_inversion(double array[], int a, int b) {
  int x, y, z, mid;
  if (a >= b)
    return 0;
  mid = floor((a + b) / 2);
  x = count_inversion(array, a, mid);
  y = count_inversion(array, mid + 1, b);
  z = count_inversion_merge(array, a, b);
  return x + y + z;
}

SEXP do_countInversions(SEXP s_x) {
  const R_len_t n_x = length(s_x);
  double *x = REAL(s_x);
  
  int count = 0;
  count = count_inversion(x, 0, n_x - 1);
  return ScalarInteger(count);
}