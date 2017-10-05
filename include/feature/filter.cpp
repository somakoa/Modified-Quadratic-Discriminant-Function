#include "filter.h"
void mean_filter_2x2(const int *input_image, const int w, const int h,
		int *output_image){

	for (int x = 0; x < w+1; ++x) output_image[x] = 0;
	for (int y = 1; y < h; ++y) {
		output_image[y*(w+1)] = 0;
		for (int x = 1; x < w; ++x) {
			output_image[y*(w+1) + x] =
				input_image[y*w + x]     + input_image[y*w + x-1] +
				input_image[(y-1)*w + x] + input_image[(y-1)*w + x-1];	
		}
		output_image[y*(w+1)+w] = 0;
	}
	for (int x = 0; x < w+1; ++x) output_image[h*(w+1)+x] = 0;
}
void WeightFilter14641(const vector<double> input_hist, const int size, 
		const int input_dir, 
		vector<double> &output_hist) {
	const int block = size / input_dir;
	for (int d = 0; d < input_dir/2; ++d) {
		int dir[5];
		for (int i = 0; i < 5; ++i) dir[i] = (2*d + i - 2 + input_dir) % input_dir;
		for (int p = 0; p < block; ++p) {
			output_hist[d*block + p] = 
				6 * input_hist[dir[2]*block + p] +
				4 *(input_hist[dir[1]*block + p] + input_hist[dir[3]*block + p]) +
				(input_hist[dir[0]*block + p] + input_hist[dir[4]*block + p]);
		}
	}
}
void WeightFilter121(const vector<double> input_hist, const int size,
		const int input_dir, 
		vector<double> &output_hist) {
	const int block = size / input_dir;
	for (int d = 0; d < input_dir/2; ++d) {
		int dir[3];
		for (int i = 0; i < 3; ++i) dir[i] = (2*d + i - 1 + input_dir) % input_dir;
		for (int p = 0; p < block; ++p) {
			output_hist[d*block + p] = 
				2*input_hist[dir[1]*block + p] +
				(input_hist[dir[0]*block + p] + input_hist[dir[2]*block + p]);
		}
	}
}
static const double GAUSS[5][5] = 
{{0.0000, 0.0092, 0.0169, 0.0092, 0.0000},
	{0.0092, 0.0573, 0.1054, 0.0573, 0.0092},
	{0.0169, 0.1054, 0.1939, 0.1054, 0.0169},
	{0.0092, 0.0573, 0.1054, 0.0573, 0.0092},
	{0.0000, 0.0092, 0.0169, 0.0092, 0.0000}
};
void GaussianFilter(const vector<double> input_hist,
		const int input_w, const int input_h, const int dir,
		vector<double> &output_hist) {
	const int output_w = (input_w + 1) / 2;
	const int output_h = (input_h + 1) / 2;

	for (int d = 0; d < dir; ++d) {
		for (int output_y = 0; output_y < output_h; ++output_y) {
			int input_y = output_y * 2;
			for (int output_x = 0; output_x < output_w; ++output_x) {
				int input_x = output_x * 2;
				// filter
				for (int gy = -2; gy <= 2; ++gy) {
					if ( input_y + gy < 0 || input_y + gy >= input_h ) continue;
					for (int gx = -2; gx <= 2; ++gx) {
						if ( input_x + gx < 0 || input_x + gx >= input_w ) continue;

						output_hist[d*output_h*output_w + output_y*output_w + output_x] +=
							GAUSS[gy+2][gx+2] * input_hist[d*input_h*input_w + 
							(input_y + gy)*input_w + input_x + gx];
					}
				}	
			}
		}
	}
}
const int HLUT[14]={
	8,10,12,14,
	24,26,28,
	38,40,32,44,
	54,56,58};
void hexagonal_filter_test(const vector<double> src, const int dir, vector<double> &dst) {
	for (int d = 0; d < dir; ++d) {
		for (int i = 0; i < 2; ++i) {// 0 7
			dst[d*14 + 0 + 7*i] =
				2*src[d*68 + 8 + 30*i] + 
				src[d*68 + 0  + 30*i] + src[d*68 + 1  + 30*i] + 
				src[d*68 + 9  + 30*i] + 
				src[d*68 + 15 + 30*i] + src[d*68 + 16 + 30*i];
		}
		for (int i = 0; i < 2; ++i) {// 3 10
			dst[d*14 + 3 + 7*i] =
				2*src[d*68 + 14 + 30*i] + 
				src[d*68 + 6  + 30*i] + src[d*68 + 7 + 30*i] + 
				src[d*68 + 13 + 30*i] + 
				src[d*68 + 21 + 30*i] + src[d*68 + 22 + 30*i];
		}
		for (int i = 0; i < 14; ++i) {
			if ( i == 0 || i == 7 || i == 3 || i == 10) continue;
			dst[d*14 + i] = 
				2*src[d*68 + HLUT[i]] + 
				src[d*68+HLUT[i]-8] + src[d*68+HLUT[i]-7] +
				src[d*68+HLUT[i]-1] + src[d*68+HLUT[i]+1] +
				src[d*68+HLUT[i]+7] + src[d*68+HLUT[i]+8];
		}
	}
}
