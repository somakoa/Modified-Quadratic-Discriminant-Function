#include <cmath>
#include <iostream>
int otsu(const unsigned char *pixels, unsigned int number);
void niblack(unsigned char *pixels,
	      const unsigned int width,
	      const unsigned int height,
	      const int windowWidth,
	      const int windowHeight,
	     const double k);
unsigned int labeling(unsigned int *pixels,
		      unsigned int w, unsigned int h);
