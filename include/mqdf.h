#pragma once
#include <string>
#include <iostream>
#include <fstream>
#include <vector>
#include <limits.h>
#include <omp.h>
#include "Eigen/Dense"

#ifdef _OPENMP
#include <omp.h>
#endif

class MQDF {
 public:
  // static const
  // static double ALPHA;
  // static unsigned int K;

  
  // constructor
 MQDF(): dimension(0), power(1), max_k(UINT_MAX), ALPHA(0.1), K(0){}; 
 MQDF( unsigned int k, double alpha ): dimension(0), power(1), max_k(UINT_MAX),
    ALPHA(alpha), K(k){};
  
  // destructor
  
  // method
  void train_vv(std::vector<std::vector<double> > _features, int in_label);
  void train(Eigen::MatrixXd features, int in_label, unsigned int _dimension, unsigned long int samples);

  void loadDictionary(const char* dir_name);
  void saveDictionarySet(const char* dir_name);
  void saveDictionary(const char* file_name, int _label);
  
  void readDictionary(std::string dir_name);

  
  void classify(Eigen::VectorXd feature_vector,
		unsigned int &_class, double &similarity);

  unsigned int getNumCategory(void) {return num_category;}
  unsigned int getDimension(void) {return dimension;}
  float getPow(void){return power;};

  void setPow(float _power);
  
  
  // data member
  std::vector<unsigned short> codes;

 private:
  double function(Eigen::VectorXd &X, unsigned int idx);

  
  unsigned int dimension;
  unsigned int num_category = 0;
  float power;
  unsigned int max_k;

  // MQDF parameter 
  double ALPHA;
  unsigned int K;
  
  // dictionary data of each classes
  std::vector<int> labels;
  std::vector<Eigen::VectorXd> mean_vectors;
  std::vector<Eigen::MatrixXd> covariance_matrix;
  std::vector<Eigen::VectorXd> eigen_values;
  std::vector<Eigen::MatrixXd> eigen_vectors;

  double sigma;
};
