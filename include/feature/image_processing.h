#ifndef IMAGE_PROCESSING_H
#define IMAGE_PROCESSING_H
#include <opencv2/opencv.hpp>
void ExtractCC(const int *input_image,
	       const int input_width, const int input_height,
	       int *output_image, int &output_width, int &output_height );
void NormalizeImage(int *input_image,  int input_width,  int input_height,
		    int *output_image, int output_width, int output_height );
void Rotate(const uchar *input_image,
	    const int input_width, const int input_height,
	    const int angle_x, const int angle_y, const int angle_z,
	    uchar *output_image, const int output_width, const int output_height,
	    const double view_z);
void TrackContour(const int *input_image, 
		  const int input_width, const int input_height,
		  int );
void rotateImage(const cv::Mat &input, cv::Mat &output, double alpha, double beta, double gamma, double f);
#endif
