/********************************************
 *
 * GFV.cpp
 *
 * 濃度こう配特徴を抽出する関数．
 *
 * 引数：
 * int *inputImage  : 入力画像
 * int inputW       : 入力画像の幅
 * int inputH       : 入力画像の高さ
 * int divideNum    : ブロック分割数（最終的な）
 *
 * 返り値：なし
 *
 *　Create by Kanta Kuramoto 2013/5/31
 *********************************************/
#include <cmath>
#include <string>
#include <iostream>
#include <vector>
#include <iomanip>
#include <algorithm>

#include "filter.h"
#include "image_processing.h"

using namespace std;

// 2x2平均値フィルタの回数
const int R = 4;

// 正規化サイズ
const int NORMALIZE_SIZE = 52;

/**
 * 濃度こう配
 **/
void GradientFeature(const uchar *input_image,
		     const int input_w, const int input_h,
		     double *feature, const int grid_size, const int direction)
{
  // 最終的なヒストグラムのサイズ
  const int HISTOGRAM_SIZE = grid_size * grid_size * direction;

  // 計算過程の領域分割数と方向量子化数
  const int FIRST_GRID = 2 * grid_size - 1;
  const int FIRST_DIRECTION = 4 * direction;
  const int SECOND_DIRECTION = 2 * direction;

  // 計算過程のヒストグラムのサイズ
  const int FIRST_HISTOGRAM_SIZE = FIRST_GRID * FIRST_GRID * FIRST_DIRECTION;
  const int SECOND_HISTOGRAM_SIZE = FIRST_GRID * FIRST_GRID * SECOND_DIRECTION;
  const int THIRD_HISTOGRAM_SIZE = FIRST_GRID * FIRST_GRID * direction;

  int *input_img = new int[input_w*input_h];
  for (int i = 0; i < input_w*input_h; ++i) {
	  input_img[i] = input_image[i];
  }
  // 文字切り出し
  int char_w;
  int char_h;
  int *char_image(NULL);
  char_image = new int[(input_w + 2) * (input_h + 2)]; // 枠用に余分に確保
  ExtractCC(input_img,input_w,input_h,char_image,char_w,char_h);

  // 正規化
  int tmpW2 = NORMALIZE_SIZE;
  int tmpH2 = NORMALIZE_SIZE;
  int tmpImage2[NORMALIZE_SIZE*NORMALIZE_SIZE];
  NormalizeImage(char_image, char_w, char_h, tmpImage2, tmpW2, tmpH2);
  delete [] char_image;
  //------------------------
  // 正規化した画像を切り出す
  //------------------------
  int cutW;
  int cutH;
  int cutImage[(NORMALIZE_SIZE+2)*(NORMALIZE_SIZE+2)];
  ExtractCC(tmpImage2,tmpW2,tmpH2,cutImage,cutW,cutH);
  //--------------------
  // 2x2の平均値filter 
  //
  //--------------------
  //int meanImages[R][(NORMALIZE_SIZE+2+R)*(NORMALIZE_SIZE+2+R)];
  //int meanW = tmpW2 + 1;
  //int meanH = tmpH2 + 1;
  int meanImages[R][(NORMALIZE_SIZE+2+R)*(NORMALIZE_SIZE+2+R)];
  int meanW = cutW + 1;
  int meanH = cutH + 1;
  fill(meanImages[0], meanImages[0]+meanW*meanH,0);
  mean_filter_2x2(cutImage, cutW, cutH,meanImages[0]);
  //mean_filter_2x2(tmpImage2, tmpW2, tmpH2, meanImages[0]);

  for( int i = 1; i < R; i++ ){
    int nextW = meanW + 1;
    int nextH = meanH + 1;
    fill(meanImages[i], meanImages[i]+nextW*nextH,0);
    mean_filter_2x2(meanImages[i-1], meanW, meanH, meanImages[i]);
    meanW++;
    meanH++;
  }
  //----------------------------------
  // 濃淡画像の濃度値の平均が0，
  // 最大値が1となるように画像を正規化
  //----------------------------------
  int gray_width = meanW;
  int gray_height = meanH;
  int gray_image[(NORMALIZE_SIZE+2+R)*(NORMALIZE_SIZE+2+R)];
  int gray_max = 0;

  // 濃度値の最大を調べる．
  for( int i = 0; i < gray_width*gray_height; i++){
    gray_max = max(meanImages[R-1][i],gray_max);
  }
  for( int i = 0; i < gray_width*gray_height; i++){
    gray_image[i] = 255 * (double)meanImages[R-1][i]/(double)gray_max;
  }

  int meanPixel=0;
  int maxPixel = -1;
  for (int i = 0; i < meanW*meanH; ++i) {
    maxPixel = max(maxPixel,gray_image[i]);
    meanPixel += gray_image[i];
  }
  meanPixel = (double)meanPixel / (meanW*meanH) + 0.5;
 
  // ロバーツフィルターを用いてこう配ヒストグラム作成
  vector<double> first_histogram(FIRST_HISTOGRAM_SIZE, 0.0);
  for (int y = 0; y < (gray_height-1); ++y) {
    for (int x = 0; x < (gray_width-1); ++x) {
      double du =
	gray_image[(y+1)*gray_width + x+1] - gray_image[y*gray_width + x];
      double dv =
	gray_image[y*gray_width + x+1] - gray_image[(y+1)*gray_width + x];
      // こう配強度
      double magnitude = sqrt(du*du + dv*dv);
      // こう配方向
      int direction = static_cast<int>((atan2(dv,du)*(180/M_PI) *
					(FIRST_DIRECTION / 360.0) +
					FIRST_DIRECTION ) + 0.5) % FIRST_DIRECTION ;
      first_histogram[direction * FIRST_GRID*FIRST_GRID +
		      (y*FIRST_GRID)/(gray_height-1) * FIRST_GRID +
		      (x*FIRST_GRID)/(gray_width-1)] += magnitude;
    }
  }

  // 1 4 6 4 1の加重フィルタ
  vector<double> second_histogram(SECOND_HISTOGRAM_SIZE, 0.0);
  WeightFilter14641(first_histogram, FIRST_HISTOGRAM_SIZE, FIRST_DIRECTION,
  		    second_histogram);

  // 121の加重フィルタ
  vector<double> third_histogram(THIRD_HISTOGRAM_SIZE, 0.0);
  WeightFilter121(second_histogram, SECOND_HISTOGRAM_SIZE, SECOND_DIRECTION, third_histogram);

  // ガウシアンフィルタ
  vector<double> histogram(HISTOGRAM_SIZE, 0.0);
  GaussianFilter( third_histogram, FIRST_GRID, FIRST_GRID, direction, histogram);
  
  maxPixel = maxPixel - meanPixel;

  double k = 1.0/maxPixel;

  for( int i = 0 ; i < HISTOGRAM_SIZE; i++){
    feature[i] = histogram[i]*k;
  }
}
