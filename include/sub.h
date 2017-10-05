#include <fstream>
#include <sstream>
#include <string>
#include <iostream>
#include <vector>
#include <unistd.h>
#include <sys/time.h>
#include <cstdio>
#include <dirent.h>

#include "Eigen/Dense"
#include "Eigen/Eigenvalues"

#ifdef _OPENMP
#include <omp.h>
#endif


void loadDictionaryDirectry(MQDF *mqdf, const char* dir_name);

void loadPredictFile(const char* dir_name, unsigned int dimension,
		     std::vector<unsigned int> &label,
		     std::vector<Eigen::VectorXd> &features);


void loadFeatures_Dir(const char* dir_name, MQDF *mqdf);
void loadFeatures(const char* file_name, MQDF *mqdf);
