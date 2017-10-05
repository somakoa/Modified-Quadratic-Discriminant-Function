bool gradient_feature_6(const cv::Mat &src_img, double *feature);
bool gradient_feature_4(const cv::Mat &src_img, double *feature);
bool gradient_feature_4_old_g(const cv::Mat &src_img, double *feature, const int g);
bool gradient_feature_4_old(const cv::Mat &src_img, double *feature);
bool gradient_feature_4_old_plus_aspect(const cv::Mat &src_img, double *feature);
bool gradient_feature_4_old_2x2x16(const cv::Mat &src_img, double *feature);
bool gradient_feature_4_old_fixed_size(const cv::Mat &src_img, double *feature);
void GradientFeature(const uchar *input_image,
		const int input_w, const int input_h,
		double *feature, const int grid_size, const int direction);


void bounding(cv::InputArray _src, cv::OutputArray _dst);
