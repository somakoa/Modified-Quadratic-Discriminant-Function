#include <cmath>
#include <string>
#include <iostream>
#include <vector>
#include <iomanip>
#include <algorithm>
#include <numeric>

#include <opencv2/core/core.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include <opencv2/highgui/highgui.hpp>
#include "filter.h"
#include "lup_4.h"
// #include "lup_3x3_square.h"
#include "lup_6.h"

#define A //printf("%s-LINE=%d\n",__FILE__,__LINE__)

using namespace std;

const int R = 4;
const int NORMALIZE_SIZE = 52;
// const int R = 4;
// const int NORMALIZE_SIZE = 82 - R;


void bounding(cv::InputArray _src, cv::OutputArray _dst) {
	cv::Mat src = _src.getMat();

	int top = 0;
	for (int y = 0; src.rows; ++y) {
		for (int x = 0; x < src.cols; ++x) {
			if (src.data[src.cols*y + x] != 0) {
				top = y;
				goto BOTTOM;
			}
		}
	}
BOTTOM:
	int bottom = 0;
	for (int y = src.rows-1; y >= 0; --y) {
		for (int x = 0; x < src.cols; ++x) {
			if (src.data[src.cols*y + x] != 0) {
				bottom = y;
				goto LEFT;
			}
		}
	}
LEFT:
	int left = 0;
	for (int x = 0; x < src.cols; ++x) {
		for (int y = 0; y < src.rows; ++y) {
			if (src.data[src.cols*y + x] != 0) {
				left = x;
				goto RIGHT;
			}
		}
	}
RIGHT:
	int right = 0;
	for (int x = src.cols-1; x >= 0; --x) {
		for (int y = 0; y < src.rows; ++y) {
			if (src.data[src.cols*y + x] != 0) {
				right = x;
				goto END;
			}
		}
	}
END:
	int w = right - left + 1;
	int h = bottom - top + 1;
	_dst.create( cv::Size(w,h), src.type() );
	src(cv::Rect(left, top, w, h)).copyTo(_dst);
}
void pre_image_process(cv::InputArray _src, cv::OutputArray _dst) {

}
bool gradient_feature_4(const cv::Mat &src_img, double *feature) {
//const int NUM_ZONE = 49;
  //const int NUM_SAMPLED_ZONE = 16;
  const int GRID = 7;
  const int NUM_ZONE = GRID * GRID;
  const int NUM_SAMPLED_ZONE = 4*4;
  
  const int DIRECTION = 8;
  const int DIMENSION = NUM_SAMPLED_ZONE * DIRECTION;
  
  const int FIRST_DIRECTION  = 4 * DIRECTION;
  const int SECOND_DIRECTION = 2 * DIRECTION;
  
  const int FIRST_HISTOGRAM_SIZE  = NUM_ZONE * FIRST_DIRECTION;
  const int SECOND_HISTOGRAM_SIZE = NUM_ZONE * SECOND_DIRECTION;
  const int THIRD_HISTOGRAM_SIZE  = NUM_ZONE * DIRECTION;
  
  cv::Mat roi_img;
  bounding(src_img, roi_img);
  
  // normalize image size  
  double scale;
  if( roi_img.cols > roi_img.rows) {
    scale = static_cast<double>(NORMALIZE_SIZE)/roi_img.cols;
  }else {
    scale = static_cast<double>(NORMALIZE_SIZE)/roi_img.rows;
  }
  
  cv::Mat norm_img(cv::Size(NORMALIZE_SIZE, NORMALIZE_SIZE), CV_8U, cv::Scalar(0));
  cv::Mat resize_img;
  cv::resize(roi_img, resize_img, cv::Size(), scale, scale, cv::INTER_NEAREST);
  
  int blank_width  = (norm_img.cols - resize_img.cols) / 2;
  int blank_height = (norm_img.rows - resize_img.rows) / 2;
  cv::Mat roi = norm_img(cv::Rect(blank_width, blank_height, 
				  resize_img.cols, resize_img.rows));
  resize_img.copyTo(roi);
  
  const int norm_w = NORMALIZE_SIZE;
  const int norm_h = NORMALIZE_SIZE;
  int norm_image[norm_w*norm_h];
  for (int p = 0; p < norm_w*norm_h; ++p) norm_image[p] = norm_img.data[p];
  
  // apply 2x2 mean filter for "R" times
  int mean_images[R][(NORMALIZE_SIZE+R)*(NORMALIZE_SIZE+R)];
  int mean_w = norm_w;
  int mean_h = norm_h;
  mean_filter_2x2(norm_image, norm_w, norm_h, mean_images[0]);
  for (int r = 1; r < R; ++r) {
    mean_w++; mean_h++;
    mean_filter_2x2(mean_images[r-1], mean_w, mean_h, mean_images[r]);
  }
  mean_w++; mean_h++;
  const int gray_w = NORMALIZE_SIZE+R+1;
  const int gray_h = NORMALIZE_SIZE+R+1;
  int gray_image[gray_w*gray_h];
  
  // normalize gray scale level
  int max_gray_level = 
    *std::max_element(mean_images[R-1], mean_images[R-1] + mean_w*mean_h);
  for (int y = 0; y < gray_h-1; ++y) {
    for (int x = 0; x < gray_w-1; ++x) {
      gray_image[y*gray_w + x] = 
	255*mean_images[R-1][y*(gray_w-1) + x]/max_gray_level;
    }
    gray_image[y*gray_w+gray_w-1] = 0;
  }
  for (int x = 0; x < gray_w; ++x) gray_image[(gray_h-1)*gray_w + x] = 0;
  
  // apply roberts filter and obtain local gradient histogram
  vector<double> first_histogram(FIRST_HISTOGRAM_SIZE, 0.0);
  for (int y = 0; y < (gray_h-1); ++y) {
    for (int x = 0; x < (gray_w-1); ++x) {
			double du =
			  gray_image[(y+1)*gray_w + x+1] - gray_image[y*gray_w + x];
			double dv =
			  gray_image[y*gray_w + x+1] - gray_image[(y+1)*gray_w + x];
			double magnitude = sqrt(du*du + dv*dv);
			int dir = static_cast<int>(
						   (atan2(dv,du) * (180/M_PI) * (FIRST_DIRECTION / 360.0) +
						    FIRST_DIRECTION + 0.5)) % FIRST_DIRECTION;
			first_histogram[dir*NUM_ZONE + LUP4[y][x]] += magnitude;
    }
  }
  
  vector<double> second_histogram(SECOND_HISTOGRAM_SIZE, 0.0);
  WeightFilter14641(first_histogram, FIRST_HISTOGRAM_SIZE, FIRST_DIRECTION,
		    second_histogram);
  
  vector<double> third_histogram(THIRD_HISTOGRAM_SIZE, 0.0);
  WeightFilter121(second_histogram, SECOND_HISTOGRAM_SIZE, SECOND_DIRECTION, third_histogram);
  
  vector<double> histogram(DIMENSION, 0.0);
  GaussianFilter( third_histogram, GRID, GRID, DIRECTION, histogram);
  
  int mean_gray_level = 
    accumulate(gray_image, gray_image + gray_w*gray_h, 0) / (gray_w*gray_h);
  
  double k = 1.0/(255 - mean_gray_level);
  for (int i = 0 ; i < DIMENSION; i++) feature[i] = histogram[i]*k;
  
  return true;
} 
bool gradient_feature_6(const cv::Mat &src_img, double *feature) {
  const int NUM_ZONE = 68;
  const int NUM_SAMPLED_ZONE = 14;
  
  const int DIRECTION = 8;
  const int DIMENSION = NUM_SAMPLED_ZONE * DIRECTION;
  
  const int FIRST_DIRECTION  = 4 * DIRECTION;
  const int SECOND_DIRECTION = 2 * DIRECTION;
  
  const int FIRST_HISTOGRAM_SIZE  = NUM_ZONE * FIRST_DIRECTION;
  const int SECOND_HISTOGRAM_SIZE = NUM_ZONE * SECOND_DIRECTION;
  const int THIRD_HISTOGRAM_SIZE  = NUM_ZONE * DIRECTION;
  
  cv::Mat roi_img;
  bounding(src_img, roi_img);
  
  // normalize image size  
	double scale;
	if( roi_img.cols > roi_img.rows) {
	  scale = static_cast<double>(NORMALIZE_SIZE)/roi_img.cols;
	}else {
	  scale = static_cast<double>(NORMALIZE_SIZE)/roi_img.rows;
	}
	
	cv::Mat norm_img(cv::Size(NORMALIZE_SIZE, NORMALIZE_SIZE), CV_8U, cv::Scalar(0));
	cv::Mat resize_img;
	cv::resize(roi_img, resize_img, cv::Size(), scale, scale, cv::INTER_NEAREST);
	
	int blank_width  = (norm_img.cols - resize_img.cols) / 2;
	int blank_height = (norm_img.rows - resize_img.rows) / 2;
	cv::Mat roi = norm_img(cv::Rect(blank_width, blank_height, 
					resize_img.cols, resize_img.rows));
	resize_img.copyTo(roi);
	
	const int norm_w = NORMALIZE_SIZE;
	const int norm_h = NORMALIZE_SIZE;
	int norm_image[norm_w*norm_h];
	for (int p = 0; p < norm_w*norm_h; ++p) norm_image[p] = norm_img.data[p];
	
	// apply 2x2 mean filter for "R" times
	int mean_images[R][(NORMALIZE_SIZE+R)*(NORMALIZE_SIZE+R)];
	int mean_w = norm_w;
	int mean_h = norm_h;
	mean_filter_2x2(norm_image, norm_w, norm_h, mean_images[0]);
	for (int r = 1; r < R; ++r) {
	  mean_w++; mean_h++;
	  mean_filter_2x2(mean_images[r-1], mean_w, mean_h, mean_images[r]);
	}
	mean_w++; mean_h++;
	const int gray_w = NORMALIZE_SIZE+R+1;
	const int gray_h = NORMALIZE_SIZE+R+1;
	int gray_image[gray_w*gray_h];
	
	// normalize gray scale level
	int max_gray_level = 
	  *std::max_element(mean_images[R-1], mean_images[R-1] + mean_w*mean_h);
	for (int y = 0; y < gray_h-1; ++y) {
	  for (int x = 0; x < gray_w-1; ++x) {
		  gray_image[y*gray_w + x] = 
		    255*mean_images[R-1][y*(gray_w-1) + x]/max_gray_level;
	  }
	  gray_image[y*gray_w+gray_w-1] = 0;
	}
	for (int x = 0; x < gray_w; ++x) gray_image[(gray_h-1)*gray_w + x] = 0;
	
	// apply roberts filter and obtain local gradient histogram
	vector<double> first_histogram(FIRST_HISTOGRAM_SIZE, 0.0);
	for (int y = 0; y < (gray_h-1); ++y) {
	  for (int x = 0; x < (gray_w-1); ++x) {
	    double du =
	      gray_image[(y+1)*gray_w + x+1] - gray_image[y*gray_w + x];
	    double dv =
	      gray_image[y*gray_w + x+1] - gray_image[(y+1)*gray_w + x];
	    double magnitude = sqrt(du*du + dv*dv);
	    int dir = static_cast<int>(
				       (atan2(dv,du) * (180/M_PI) * (FIRST_DIRECTION / 360.0) +
					FIRST_DIRECTION + 0.5)) % FIRST_DIRECTION;
	    first_histogram[dir*NUM_ZONE + LUP6[y][x]] += magnitude;
	  }
	}
	
	vector<double> second_histogram(SECOND_HISTOGRAM_SIZE, 0.0);
	WeightFilter14641(first_histogram, FIRST_HISTOGRAM_SIZE, FIRST_DIRECTION,
			  second_histogram);
	
	vector<double> third_histogram(THIRD_HISTOGRAM_SIZE, 0.0);
	WeightFilter121(second_histogram, SECOND_HISTOGRAM_SIZE, SECOND_DIRECTION, third_histogram);
	
	vector<double> histogram(DIMENSION, 0.0);
	// 	GaussianFilter( third_histogram, FIRST_GRID, FIRST_GRID, direction, histogram);
	hexagonal_filter_test(third_histogram, DIRECTION, histogram);
	
	int mean_gray_level = 
	  accumulate(gray_image, gray_image + gray_w*gray_h, 0) / (gray_w*gray_h);
	
	double k = 1.0/(255 - mean_gray_level);
	for (int i = 0 ; i < DIMENSION; i++) feature[i] = histogram[i]*k;
	
	return true;
}
bool gradient_feature_4_old_g(const cv::Mat &src_img, double *feature,const int g){
  const int GRID_SIZE = g;
  const int GRID = 2*GRID_SIZE - 1;
  const int NUM_ZONE = GRID * GRID;
  const int NUM_SAMPLED_ZONE = GRID_SIZE*GRID_SIZE;
  const int DIRECTION = 8;
  const int DIMENSION = NUM_SAMPLED_ZONE * DIRECTION;
  
  const int FIRST_DIRECTION  = 4 * DIRECTION;
  const int SECOND_DIRECTION = 2 * DIRECTION;
  
  const int FIRST_HISTOGRAM_SIZE  = NUM_ZONE * FIRST_DIRECTION;
  const int SECOND_HISTOGRAM_SIZE = NUM_ZONE * SECOND_DIRECTION;
  const int THIRD_HISTOGRAM_SIZE  = NUM_ZONE * DIRECTION;
  
  cv::Mat roi_img;
  bounding(src_img, roi_img);
  
  // normalize image size
  double scale;
  if( roi_img.cols > roi_img.rows) {
    scale = static_cast<double>(NORMALIZE_SIZE)/roi_img.cols;
  }else {
    scale = static_cast<double>(NORMALIZE_SIZE)/roi_img.rows;
  }
  double scaleC=scale,scaleR=scale;
  
  // double scaleC,scaleR;
  // if( roi_img.cols > roi_img.rows) {
  //   scaleC = static_cast<double>(NORMALIZE_SIZE)/roi_img.cols;
  // 	scaleR = scaleC;
  // 	if (scaleR*roi_img.rows < 1)
  // 		scaleR = 1.0000 / roi_img.rows;
  // }else {
  //   scaleR = static_cast<double>(NORMALIZE_SIZE)/roi_img.rows;
  // 	scaleC = scaleR;
  // 	if (scaleC*roi_img.cols < 1)
  // 		scaleC = 1.0000 / roi_img.cols;
  // }
  
  cv::Mat resize_img;
  cv::resize(roi_img, resize_img, cv::Size(), scaleC, scaleR, cv::INTER_NEAREST);
  
  
  cv::Mat norm_img(cv::Size(NORMALIZE_SIZE, NORMALIZE_SIZE), CV_8U, cv::Scalar(0));
  int blank_width  = (norm_img.cols - resize_img.cols) / 2;
  int blank_height = (norm_img.rows - resize_img.rows) / 2;
  cv::Mat roi = norm_img(cv::Rect(blank_width, blank_height, 
				  resize_img.cols, resize_img.rows));
  resize_img.copyTo(roi);
  
  int norm_image[(NORMALIZE_SIZE+2)*(NORMALIZE_SIZE+2)];
  for (int p = 0; p < norm_img.rows*norm_img.cols; ++p) norm_image[p] = norm_img.data[p];
  
  // cv::Mat norm_img(cv::Size(resize_img.cols + 2, resize_img.rows + 2), CV_8U, cv::Scalar(0));
  // 
  // cv::Mat roi = norm_img(cv::Rect(1, 1, resize_img.cols, resize_img.rows));
  // 
  // resize_img.copyTo(roi);
  // 
  // int norm_image[(NORMALIZE_SIZE+2)*(NORMALIZE_SIZE+2)];
  // for (int p = 0; p < norm_img.rows*norm_img.cols; ++p) {
  //   norm_image[p] = norm_img.data[p];
  // }
  
  // apply 2x2 mean filter for "R" times
  int mean_images[R][(NORMALIZE_SIZE+R+2)*(NORMALIZE_SIZE+R+2)];
  int mean_w = norm_img.cols;
  int mean_h = norm_img.rows;
  mean_filter_2x2(norm_image, mean_w, mean_h, mean_images[0]);
  for (int r = 1; r < R; ++r) {
    mean_w++; mean_h++;
    mean_filter_2x2(mean_images[r-1], mean_w, mean_h, mean_images[r]);
  }
  mean_w++; mean_h++;
  const int gray_w = mean_w+1;
  const int gray_h = mean_h+1;
  std::vector<int> gray_image(gray_w*gray_h);
  
  // normalize gray scale level
  int max_gray_level = 
    *std::max_element(mean_images[R-1], mean_images[R-1] + mean_w*mean_h);
  
  // if(max_gray_level ==0)
  //   max_gray_level = 1;
  // 
  for (int y = 0; y < gray_h-1; ++y) {
    for (int x = 0; x < gray_w-1; ++x) {
      gray_image[y*gray_w + x] = 
        255*mean_images[R-1][y*(gray_w-1) + x]/max_gray_level;
    }
    gray_image[y*gray_w+gray_w-1] = 0;
  }
  
  for (int x = 0; x < gray_w; ++x) {
    gray_image[(gray_h-1)*gray_w + x] = 0;
  }
  
  // apply roberts filter and obtain local gradient histogram
  std::vector<double> first_histogram(FIRST_HISTOGRAM_SIZE, 0.0);
  for (int y = 0; y < (gray_h-1); ++y) {
    for (int x = 0; x < (gray_w-1); ++x) {
      double du =
        gray_image[(y+1)*gray_w + x+1] - gray_image[y*gray_w + x];
      double dv =
        gray_image[y*gray_w + x+1] - gray_image[(y+1)*gray_w + x];
      double magnitude = sqrt(du*du + dv*dv);
      int dir = static_cast<int>(
				 (atan2(dv,du) * (180/M_PI) * (FIRST_DIRECTION / 360.0) +
				  FIRST_DIRECTION + 0.5)) % FIRST_DIRECTION;
      first_histogram[dir*GRID*GRID + 
		      (y*GRID)/(gray_h-1) * GRID +
		      (x*GRID)/(gray_w-1) ]+= magnitude;//*********
    }
  }
  
  std::vector<double> second_histogram(SECOND_HISTOGRAM_SIZE, 0.0);
  WeightFilter14641(first_histogram, FIRST_HISTOGRAM_SIZE, FIRST_DIRECTION, second_histogram);
  
  std::vector<double> third_histogram(THIRD_HISTOGRAM_SIZE, 0.0);
  WeightFilter121(second_histogram, SECOND_HISTOGRAM_SIZE, SECOND_DIRECTION, third_histogram);
  
  std::vector<double> histogram(DIMENSION, 0.0);
  GaussianFilter( third_histogram, GRID, GRID, DIRECTION, histogram);//********
  //GaussianFilter( third_histogram, 5, 5, DIRECTION, histogram);//********
  
  int mean_gray_level = 
    std::accumulate(gray_image.begin(), gray_image.end(), 0) / (gray_w*gray_h);
  
  double k = 1.0/(255 - mean_gray_level);
  for (int i = 0 ; i < DIMENSION; i++) feature[i] = histogram[i]*k;
  
  return true;
}

bool gradient_feature_4_old(const cv::Mat &src_img, double *feature) {
  //n==2
	const int GRID = 3;
	const int NUM_ZONE = GRID * GRID;
	const int NUM_SAMPLED_ZONE = 2*2;

	const int DIRECTION = 8;
	const int DIMENSION = NUM_SAMPLED_ZONE * DIRECTION;

	const int FIRST_DIRECTION  = 4 * DIRECTION;
	const int SECOND_DIRECTION = 2 * DIRECTION;

	const int FIRST_HISTOGRAM_SIZE  = NUM_ZONE * FIRST_DIRECTION;
	const int SECOND_HISTOGRAM_SIZE = NUM_ZONE * SECOND_DIRECTION;
	const int THIRD_HISTOGRAM_SIZE  = NUM_ZONE * DIRECTION;

	cv::Mat roi_img;
	bounding(src_img, roi_img);

    // normalize image size  
	double scale;
	if( roi_img.cols > roi_img.rows) {
		scale = static_cast<double>(NORMALIZE_SIZE)/roi_img.cols;
	}else {
		scale = static_cast<double>(NORMALIZE_SIZE)/roi_img.rows;
	}

	cv::Mat resize_img;
	cv::resize(roi_img, resize_img, cv::Size(), scale, scale, cv::INTER_NEAREST);
	cv::Mat norm_img(cv::Size(resize_img.cols+2, resize_img.rows+2), CV_8U, cv::Scalar(0));
	cv::Mat roi = norm_img(cv::Rect(1, 1, resize_img.cols, resize_img.rows));
	resize_img.copyTo(roi);
	
	int norm_image[(NORMALIZE_SIZE+2)*(NORMALIZE_SIZE+2)];
	for (int p = 0; p < norm_img.rows*norm_img.cols; ++p) norm_image[p] = norm_img.data[p];

	// apply 2x2 mean filter for "R" times
	int mean_images[R][(NORMALIZE_SIZE+R+2)*(NORMALIZE_SIZE+R+2)];
	int mean_w = norm_img.cols;
	int mean_h = norm_img.rows;
	mean_filter_2x2(norm_image, mean_w, mean_h, mean_images[0]);
	for (int r = 1; r < R; ++r) {
		mean_w++; mean_h++;
		mean_filter_2x2(mean_images[r-1], mean_w, mean_h, mean_images[r]);
	}	
	mean_w++; mean_h++;
	int gray_w = mean_w+1;
	int gray_h = mean_h+1;
	std::vector<int> gray_image(gray_w*gray_h);

	// normalize gray scale level
	int max_gray_level = 
		*std::max_element(mean_images[R-1], mean_images[R-1] + mean_w*mean_h);
	for (int y = 0; y < gray_h-1; ++y) {
		for (int x = 0; x < gray_w-1; ++x) {
			gray_image[y*gray_w + x] = 
				255*mean_images[R-1][y*(gray_w-1) + x]/max_gray_level;
		}
		gray_image[y*gray_w+gray_w-1] = 0;
	}

	for (int x = 0; x < gray_w; ++x) gray_image[(gray_h-1)*gray_w + x] = 0;

	// apply roberts filter and obtain local gradient histogram
	vector<double> first_histogram(FIRST_HISTOGRAM_SIZE, 0.0);
	for (int y = 0; y < (gray_h-1); ++y) {
		for (int x = 0; x < (gray_w-1); ++x) {
			double du =
				gray_image[(y+1)*gray_w + x+1] - gray_image[y*gray_w + x];
			double dv =
				gray_image[y*gray_w + x+1] - gray_image[(y+1)*gray_w + x];
			double magnitude = sqrt(du*du + dv*dv);
			int dir = static_cast<int>(
				(atan2(dv,du) * (180/M_PI) * (FIRST_DIRECTION / 360.0) +
				FIRST_DIRECTION + 0.5)) % FIRST_DIRECTION;
			first_histogram[dir * GRID*GRID +
				(y*GRID)/(gray_h-1) * GRID +
				(x*GRID)/(gray_w-1)] += magnitude;
		}
	}

	vector<double> second_histogram(SECOND_HISTOGRAM_SIZE, 0.0);
	WeightFilter14641(first_histogram, FIRST_HISTOGRAM_SIZE, FIRST_DIRECTION,
			second_histogram);

	vector<double> third_histogram(THIRD_HISTOGRAM_SIZE, 0.0);
	WeightFilter121(second_histogram, SECOND_HISTOGRAM_SIZE, SECOND_DIRECTION, third_histogram);

	vector<double> histogram(DIMENSION, 0.0);
	GaussianFilter( third_histogram, GRID, GRID, DIRECTION, histogram);
 
	int mean_gray_level = 
	  accumulate(gray_image.begin(), gray_image.end(), 0) / (gray_w*gray_h);     
	double k = 1.0/(255 - mean_gray_level);
	for (int i = 0 ; i < DIMENSION; i++) feature[i] = histogram[i]*k;

	return true;
} 

bool gradient_feature_4_old_2x2x16(const cv::Mat &src_img, double *feature) {

	const int GRID = 3;
	const int NUM_ZONE = GRID * GRID;
	const int NUM_SAMPLED_ZONE = 2*2;

	const int DIRECTION = 16;
	const int DIMENSION = NUM_SAMPLED_ZONE * DIRECTION;

	const int FIRST_DIRECTION  = 4 * DIRECTION;
	const int SECOND_DIRECTION = 2 * DIRECTION;

	const int FIRST_HISTOGRAM_SIZE  = NUM_ZONE * FIRST_DIRECTION;
	const int SECOND_HISTOGRAM_SIZE = NUM_ZONE * SECOND_DIRECTION;
	const int THIRD_HISTOGRAM_SIZE  = NUM_ZONE * DIRECTION;

	cv::Mat roi_img;
	bounding(src_img, roi_img);

    // normalize image size  
	double scale;
	if( roi_img.cols > roi_img.rows) {
		scale = static_cast<double>(NORMALIZE_SIZE)/roi_img.cols;
	}else {
		scale = static_cast<double>(NORMALIZE_SIZE)/roi_img.rows;
	}

	cv::Mat resize_img;
	cv::resize(roi_img, resize_img, cv::Size(), scale, scale, cv::INTER_NEAREST);
	cv::Mat norm_img(cv::Size(resize_img.cols+2, resize_img.rows+2), CV_8U, cv::Scalar(0));
	cv::Mat roi = norm_img(cv::Rect(1, 1, resize_img.cols, resize_img.rows));
	resize_img.copyTo(roi);
	
	int norm_image[(NORMALIZE_SIZE+2)*(NORMALIZE_SIZE+2)];
	for (int p = 0; p < norm_img.rows*norm_img.cols; ++p) norm_image[p] = norm_img.data[p];

	// apply 2x2 mean filter for "R" times
	int mean_images[R][(NORMALIZE_SIZE+R+2)*(NORMALIZE_SIZE+R+2)];
	int mean_w = norm_img.cols;
	int mean_h = norm_img.rows;
	mean_filter_2x2(norm_image, mean_w, mean_h, mean_images[0]);
	for (int r = 1; r < R; ++r) {
		mean_w++; mean_h++;
		mean_filter_2x2(mean_images[r-1], mean_w, mean_h, mean_images[r]);
	}	
	mean_w++; mean_h++;
	int gray_w = mean_w+1;
	int gray_h = mean_h+1;
	std::vector<int> gray_image(gray_w*gray_h);

	// normalize gray scale level
	int max_gray_level = 
		*std::max_element(mean_images[R-1], mean_images[R-1] + mean_w*mean_h);
	for (int y = 0; y < gray_h-1; ++y) {
		for (int x = 0; x < gray_w-1; ++x) {
			gray_image[y*gray_w + x] = 
				255*mean_images[R-1][y*(gray_w-1) + x]/max_gray_level;
		}
		gray_image[y*gray_w+gray_w-1] = 0;
	}

	for (int x = 0; x < gray_w; ++x) gray_image[(gray_h-1)*gray_w + x] = 0;

	// apply roberts filter and obtain local gradient histogram
	vector<double> first_histogram(FIRST_HISTOGRAM_SIZE, 0.0);
	for (int y = 0; y < (gray_h-1); ++y) {
		for (int x = 0; x < (gray_w-1); ++x) {
			double du =
				gray_image[(y+1)*gray_w + x+1] - gray_image[y*gray_w + x];
			double dv =
				gray_image[y*gray_w + x+1] - gray_image[(y+1)*gray_w + x];
			double magnitude = sqrt(du*du + dv*dv);
			int dir = static_cast<int>(
				(atan2(dv,du) * (180/M_PI) * (FIRST_DIRECTION / 360.0) +
				FIRST_DIRECTION + 0.5)) % FIRST_DIRECTION;
			first_histogram[dir * GRID*GRID +
				(y*GRID)/(gray_h-1) * GRID +
				(x*GRID)/(gray_w-1)] += magnitude;
		}
	}

	vector<double> second_histogram(SECOND_HISTOGRAM_SIZE, 0.0);
	WeightFilter14641(first_histogram, FIRST_HISTOGRAM_SIZE, FIRST_DIRECTION,
			second_histogram);

	vector<double> third_histogram(THIRD_HISTOGRAM_SIZE, 0.0);
	WeightFilter121(second_histogram, SECOND_HISTOGRAM_SIZE, SECOND_DIRECTION, third_histogram);

	vector<double> histogram(DIMENSION, 0.0);
	GaussianFilter( third_histogram, GRID, GRID, DIRECTION, histogram);
 
	int mean_gray_level = 
	  accumulate(gray_image.begin(), gray_image.end(), 0) / (gray_w*gray_h);     
	double k = 1.0/(255 - mean_gray_level);
	for (int i = 0 ; i < DIMENSION; i++) feature[i] = histogram[i]*k;

	return true;
} 
bool gradient_feature_4_old_fixed_size(const cv::Mat &src_img, double *feature) {

	const int GRID = 3;
	const int NUM_ZONE = GRID * GRID;
	const int NUM_SAMPLED_ZONE = 2*2;

	const int DIRECTION = 8;
	const int DIMENSION = NUM_SAMPLED_ZONE * DIRECTION;

	const int FIRST_DIRECTION  = 4 * DIRECTION;
	const int SECOND_DIRECTION = 2 * DIRECTION;

	const int FIRST_HISTOGRAM_SIZE  = NUM_ZONE * FIRST_DIRECTION;
	const int SECOND_HISTOGRAM_SIZE = NUM_ZONE * SECOND_DIRECTION;
	const int THIRD_HISTOGRAM_SIZE  = NUM_ZONE * DIRECTION;

	cv::Mat roi_img;
	bounding(src_img, roi_img);

    // normalize image size  
	double scale;
	if( roi_img.cols > roi_img.rows) {
		scale = static_cast<double>(NORMALIZE_SIZE)/roi_img.cols;
	}else {
		scale = static_cast<double>(NORMALIZE_SIZE)/roi_img.rows;
	}

	cv::Mat resize_img;
	cv::resize(roi_img, resize_img, cv::Size(), scale, scale, cv::INTER_NEAREST);
	cv::Mat norm_img(cv::Size(NORMALIZE_SIZE, NORMALIZE_SIZE), CV_8U, cv::Scalar(0));
	int blank_width  = (norm_img.cols - resize_img.cols) / 2;
	int blank_height = (norm_img.rows - resize_img.rows) / 2;
	cv::Mat roi = norm_img(cv::Rect(blank_width, blank_height, 
				resize_img.cols, resize_img.rows));
	resize_img.copyTo(roi);
	
	int norm_image[(NORMALIZE_SIZE+2)*(NORMALIZE_SIZE+2)];
	for (int p = 0; p < norm_img.rows*norm_img.cols; ++p) norm_image[p] = norm_img.data[p];

	// apply 2x2 mean filter for "R" times
	int mean_images[R][(NORMALIZE_SIZE+R+2)*(NORMALIZE_SIZE+R+2)];
	int mean_w = norm_img.cols;
	int mean_h = norm_img.rows;
	mean_filter_2x2(norm_image, mean_w, mean_h, mean_images[0]);
	for (int r = 1; r < R; ++r) {
		mean_w++; mean_h++;
		mean_filter_2x2(mean_images[r-1], mean_w, mean_h, mean_images[r]);
	}	
	mean_w++; mean_h++;
	int gray_w = mean_w+1;
	int gray_h = mean_h+1;
	std::vector<int> gray_image(gray_w*gray_h);

	// normalize gray scale level
	int max_gray_level = 
		*std::max_element(mean_images[R-1], mean_images[R-1] + mean_w*mean_h);
	for (int y = 0; y < gray_h-1; ++y) {
		for (int x = 0; x < gray_w-1; ++x) {
			gray_image[y*gray_w + x] = 
				255*mean_images[R-1][y*(gray_w-1) + x]/max_gray_level;
		}
		gray_image[y*gray_w+gray_w-1] = 0;
	}

	for (int x = 0; x < gray_w; ++x) gray_image[(gray_h-1)*gray_w + x] = 0;

	// apply roberts filter and obtain local gradient histogram
	vector<double> first_histogram(FIRST_HISTOGRAM_SIZE, 0.0);
	for (int y = 0; y < (gray_h-1); ++y) {
		for (int x = 0; x < (gray_w-1); ++x) {
			double du =
				gray_image[(y+1)*gray_w + x+1] - gray_image[y*gray_w + x];
			double dv =
				gray_image[y*gray_w + x+1] - gray_image[(y+1)*gray_w + x];
			double magnitude = sqrt(du*du + dv*dv);
			int dir = static_cast<int>(
				(atan2(dv,du) * (180/M_PI) * (FIRST_DIRECTION / 360.0) +
				FIRST_DIRECTION + 0.5)) % FIRST_DIRECTION;
			first_histogram[dir * GRID*GRID +
				(y*GRID)/(gray_h-1) * GRID +
				(x*GRID)/(gray_w-1)] += magnitude;
		}
	}

	vector<double> second_histogram(SECOND_HISTOGRAM_SIZE, 0.0);
	WeightFilter14641(first_histogram, FIRST_HISTOGRAM_SIZE, FIRST_DIRECTION,
			second_histogram);

	vector<double> third_histogram(THIRD_HISTOGRAM_SIZE, 0.0);
	WeightFilter121(second_histogram, SECOND_HISTOGRAM_SIZE, SECOND_DIRECTION, third_histogram);

	vector<double> histogram(DIMENSION, 0.0);
	GaussianFilter( third_histogram, GRID, GRID, DIRECTION, histogram);
 
	int mean_gray_level = 
	  accumulate(gray_image.begin(), gray_image.end(), 0) / (gray_w*gray_h);     
	double k = 1.0/(255 - mean_gray_level);
	for (int i = 0 ; i < DIMENSION; i++) feature[i] = histogram[i]*k;

	return true;
} 
bool gradient_feature_4_old_plus_aspect(const cv::Mat &src_img, double *feature) {

	const int GRID = 3;
	const int NUM_ZONE = GRID * GRID;
	const int NUM_SAMPLED_ZONE = 2*2;

	const int DIRECTION = 8;
	const int DIMENSION = NUM_SAMPLED_ZONE * DIRECTION;

	const int FIRST_DIRECTION  = 4 * DIRECTION;
	const int SECOND_DIRECTION = 2 * DIRECTION;

	const int FIRST_HISTOGRAM_SIZE  = NUM_ZONE * FIRST_DIRECTION;
	const int SECOND_HISTOGRAM_SIZE = NUM_ZONE * SECOND_DIRECTION;
	const int THIRD_HISTOGRAM_SIZE  = NUM_ZONE * DIRECTION;

	cv::Mat roi_img;
	bounding(src_img, roi_img);

    // normalize image size  
	double scale;
	if( roi_img.cols > roi_img.rows) {
		scale = static_cast<double>(NORMALIZE_SIZE)/roi_img.cols;
	}else {
		scale = static_cast<double>(NORMALIZE_SIZE)/roi_img.rows;
	}
	double aspect = static_cast<double>(roi_img.cols)/roi_img.rows;

	cv::Mat resize_img;
	cv::resize(roi_img, resize_img, cv::Size(), scale, scale, cv::INTER_NEAREST);
	cv::Mat norm_img(cv::Size(resize_img.cols+2, resize_img.rows+2), CV_8U, cv::Scalar(0));
	cv::Mat roi = norm_img(cv::Rect(1, 1, resize_img.cols, resize_img.rows));
	resize_img.copyTo(roi);
	
	int norm_image[(NORMALIZE_SIZE+2)*(NORMALIZE_SIZE+2)];
	for (int p = 0; p < norm_img.rows*norm_img.cols; ++p) norm_image[p] = norm_img.data[p];

	// apply 2x2 mean filter for "R" times
	int mean_images[R][(NORMALIZE_SIZE+R+2)*(NORMALIZE_SIZE+R+2)];
	int mean_w = norm_img.cols;
	int mean_h = norm_img.rows;
	mean_filter_2x2(norm_image, mean_w, mean_h, mean_images[0]);
	for (int r = 1; r < R; ++r) {
		mean_w++; mean_h++;
		mean_filter_2x2(mean_images[r-1], mean_w, mean_h, mean_images[r]);
	}	
	mean_w++; mean_h++;
	int gray_w = mean_w+1;
	int gray_h = mean_h+1;
	std::vector<int> gray_image(gray_w*gray_h);

	// normalize gray scale level
	int max_gray_level = 
		*std::max_element(mean_images[R-1], mean_images[R-1] + mean_w*mean_h);
	for (int y = 0; y < gray_h-1; ++y) {
		for (int x = 0; x < gray_w-1; ++x) {
			gray_image[y*gray_w + x] = 
				255*mean_images[R-1][y*(gray_w-1) + x]/max_gray_level;
		}
		gray_image[y*gray_w+gray_w-1] = 0;
	}

	for (int x = 0; x < gray_w; ++x) gray_image[(gray_h-1)*gray_w + x] = 0;

	// apply roberts filter and obtain local gradient histogram
	vector<double> first_histogram(FIRST_HISTOGRAM_SIZE, 0.0);
	for (int y = 0; y < (gray_h-1); ++y) {
		for (int x = 0; x < (gray_w-1); ++x) {
			double du =
				gray_image[(y+1)*gray_w + x+1] - gray_image[y*gray_w + x];
			double dv =
				gray_image[y*gray_w + x+1] - gray_image[(y+1)*gray_w + x];
			double magnitude = sqrt(du*du + dv*dv);
			int dir = static_cast<int>(
				(atan2(dv,du) * (180/M_PI) * (FIRST_DIRECTION / 360.0) +
				FIRST_DIRECTION + 0.5)) % FIRST_DIRECTION;
			first_histogram[dir * GRID*GRID +
				(y*GRID)/(gray_h-1) * GRID +
				(x*GRID)/(gray_w-1)] += magnitude;
		}
	}

	vector<double> second_histogram(SECOND_HISTOGRAM_SIZE, 0.0);
	WeightFilter14641(first_histogram, FIRST_HISTOGRAM_SIZE, FIRST_DIRECTION,
			second_histogram);

	vector<double> third_histogram(THIRD_HISTOGRAM_SIZE, 0.0);
	WeightFilter121(second_histogram, SECOND_HISTOGRAM_SIZE, SECOND_DIRECTION, third_histogram);

	vector<double> histogram(DIMENSION, 0.0);
	GaussianFilter( third_histogram, GRID, GRID, DIRECTION, histogram);
 
	int mean_gray_level = 
	  accumulate(gray_image.begin(), gray_image.end(), 0) / (gray_w*gray_h);     
	double k = 1.0/(255 - mean_gray_level);
	for (int i = 0 ; i < DIMENSION; i++) feature[i] = histogram[i]*k;

	feature[32] = static_cast<double>(norm_img.cols)/norm_img.rows;

	return true;
} 
