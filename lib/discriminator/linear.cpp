#include <cmath>
#include <iostream>
#include <algorithm>
#include "linear.h"

using namespace std;


double linear(VectorXd &X, VectorXd &W, double w) {
  return W.transpose() * X + w;
}
