/**
 *  binary.cpp 
 *
 *
 *
 *　Create by kanta kuramoto　2013/10/03
 */ 
#include "binary.h"
int otsu(const unsigned char *pixels, const unsigned int number){
  // ヒストグラムを求める
  double hist[256] = {0};
  for(unsigned int p = 0; p < number; p++){
    hist[pixels[p]]++; 
  }
  // 正規化ヒストグラム・全体平均を求める
  double hist2[256];
  double mt = 0.0;
  for(unsigned int t = 0; t < 256; t++){
    hist2[t] = hist[t] / number ;
    mt += t*hist2[t];
  }

  double w1 = 0.0,w2 = 1.0;
  double m1 = 0.0,m2 = mt;

  // 判別分析法
  double max_num = -1;
  unsigned int max_t = 0;
  for(unsigned int t = 1; t < 255; t++){
    double wt1 = w1;
    double wt2 = w2;
    w1 = w1 + hist2[t-1];
    w2 = w2 - hist2[t-1];

    if( w1 != 0){
      m1 = (wt1 * m1 + (t-1)*hist2[t-1]) / w1;
    }
    if( w2 != 0){
      m2 = (wt2 * m2 - (t-1)*hist2[t-1]) / w2;
    }
    if( w1*w2*(m1-m2)*(m1-m2) > max_num){
      max_num = w1*w2*(m1-m2)*(m1-m2);
      max_t = t;
    }
  }

  return max_t;
}
void niblack(unsigned char *pixels,
	      const unsigned int width,
	      const unsigned int height,
	      const int windowWidth,
	      const int windowHeight,
	      const double k){
  
  const int R = 128;
  //
  // calcuate integral image.
  //
  /*
  unsigned int *integral = new int[imageWidth*imageHeight];
  
  intergral[0] = pixels[0];
  for( unsigned int x = 1; x < imageWidth; x++){
    integral[x] = integral[x-1] + pixels[x];
  }

  for( unsigned int y = 1; y < imageHeight; y++){
    unsigned int rowSum = 0;
    for( unsigned int x = 0; x < imageWidth; x++){
      rowSum += pixels[imageWidth*y+x];
      integral[imageWidth*y+x] = rowSum + integral[imageWidth*(y-1)+x];
    }
  }
  

  */

  // binarization
  int hw = windowWidth/2;
  int hh = windowHeight/2;

  // left border and right border
  /*
  for( unsigned int y = halfWindowHeight; y < imageHeight-halfWindowHeight-1; y++){
  }

  for( unsigned int y = ; y < height; y++){
    for( unsigned int x = 0; x < width; x++){
    }
  }
  
  */

  unsigned int *t = new unsigned int[width*height];
  for( unsigned int y = 0; y < height; y++){
    for( unsigned int x = 0; x < width; x++){
      int m = 0;
      int cnt = 0;
      for(int i = (-1)*hh; i < windowHeight - hh; ++i ){
	int yi = y+i;
	if( yi < 0 || yi > height) continue;
	for(int j = (-1)*hw; j < windowWidth - hw; ++j ){
	  int xj = x+j;
	  if( xj < 0 || xj > width) continue;
	  int p = pixels[yi*width + xj];
	  m += p;
	  cnt++;
	}
      }
      m /= cnt;
      int s = 0;
      for(int i = (-1)*hh; i < windowHeight - hh; ++i ){
	int yi = y+i;
	if( yi < 0 || yi > height) continue;
	for(int j = (-1)*hw; j < windowWidth - hw; ++j ){
	  int xj = x+j;
	  if( xj < 0 || xj > width) continue;
	  int p = pixels[yi*width + xj];
	  s += (m-p)*(m-p);
	}
      }
      s /= cnt;
      s = sqrt(s);
      if(pixels[y*width + x] < m * (1 + k*(s/R - 1))){
	t[y*width + x] = 0;
      }else{
	t[y*width + x] = 255;
      }
    }
  }
  for( unsigned int p = 0; p < height*width; p++){
    pixels[p] = t[p];
  }
}
