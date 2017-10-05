#include <vector>
using namespace std;
void WeightFilter14641(const vector<double> input_hist, const int size, 
			const int input_dir, 
			vector<double> &output_hist);
void WeightFilter121(const vector<double> input_hist, const int size,
		     const int input_dir, 
		     vector<double> &output_hist);

void GaussianFilter(const vector<double> input_hist,
		     const int input_w, const int input_h, const int dir,
		    vector<double> &output_hist);
void mean_filter_2x2(const int *input_image, const int w, const int h,
		     int *output_image);

void hexagonal_filter_test(const vector<double> src, const int dir, vector<double> &dst) ;
