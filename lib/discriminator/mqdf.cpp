#include <cmath>
#include <iostream>
#include <algorithm>
#include "mqdf.h"

using namespace std;


double mqdf(VectorXd &X, VectorXd &M, VectorXd &D, MatrixXd &V,
	     double alpha, int k, double sigma) {

  double first_term = (X-M).squaredNorm();
  
  // Âè2¹à¤Î·×»»
  
  double second_term = 0;
  double third_term = 0;
  for (int i = 0; i < k; i++){
    second_term += (((1 - alpha) * D.coeff(i)) / 
		    ((1 - alpha) * D.coeff(i) + alpha * sigma)) *
      (V.col(i).transpose() * (X - M)) * (V.col(i).transpose() * (X - M));
    
    third_term += log((1 - alpha) * D.coeff(i) + alpha * sigma);
    
  }
  return (1.0 / (alpha * sigma)) * (first_term - second_term) + third_term;
}
