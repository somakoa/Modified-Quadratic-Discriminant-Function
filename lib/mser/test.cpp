//--------------------------------------------------------------------------------------------------
// Linear time Maximally Stable Extremal Regions implementation as described in D. Nistè´±r and H.
// Stewè´±nius. Linear Time Maximally Stable Extremal Regions. Proceedings of the European Conference
// on Computer Vision (ECCV), 2008.
//
// Copyright (c) 2012 Idiap Research Institute, http://www.idiap.ch/.
// Written by Charles Dubout <charles.dubout@idiap.ch>.
//
// MSER is free software: you can redistribute it and/or modify it under the terms of the GNU
// General Public License version 3 as published by the Free Software Foundation.
//
// MSER is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
// the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
// Public License for more details.
//
// You should have received a copy of the GNU General Public License along with MSER. If not, see
// <http://www.gnu.org/licenses/>.
//--------------------------------------------------------------------------------------------------

#include <algorithm>
#include <cmath>
#include <ctime>
#include <cstdio>
#include <iostream>

#include <Magick++.h>

#include "mser.h"

using namespace std;
using namespace Magick;

int main(int argc, const char * argv[]) {
  // Check for correct usage of the command line
  if (argc != 3) {
    cerr << "Usage: " << argv[0] << " input_image output_image" << endl;
    return -1;
  }
  // Try to load the image

  Image master(argv[1]);
  master.modifyImage();

  int width = master.columns();
  int height = master.rows();
  int depth = master.depth();

  // Create a grayscale image
  vector<unsigned char> grayscale(width * height);
  PixelPacket *pixel = master.getPixels(0, 0, width, height);
  for (int p = 0; p < width * height; ++p) {
    grayscale[p] = ColorGray(pixel[p]).shade() * 255;
  }

  // Extract MSER
  clock_t start = clock();

  /// Constructor.
  /// @param[in] delta DELTA parameter of the MSER algorithm. Roughly speaking, the stability of a
  /// region is the relative variation of the region area when the intensity is changed by delta.
  /// @param[in] minArea Minimum area of any stable region relative to the image domain area.
  /// @param[in] maxArea Maximum area of any stable region relative to the image domain area.
  /// @param[in] maxVariation Maximum variation (absolute stability score) of the regions.
  /// @param[in] minDiversity Minimum diversity of the regions. When the relative area of two
  /// nested regions is below this threshold, then only the most stable one is selected.
  /// @param[in] eight Use 8-connected pixels instead of 4-connected.
  MSER mser8(2, 0.0005, 0.1, 0.5, 0.5, true);
  //MSER mser4(2, 0.0005, 0.1, 0.5, 0.5, false);
  
  vector<MSER::Region> regions;
  
  mser8(&grayscale[0], width, height, regions);
  
  // Invert the pixel values
  /*
  for (int i = 0; i < width * height; ++i)
    grayscale[i] = ~grayscale[i];
  
  mser4(&grayscale[0], width, height, regions[1]);
  */
  clock_t stop = clock();
  
  cout << "Extracted " << regions.size() << " regions from " << argv[1]
       << " (" << width << 'x' << height << ") in "
       << (static_cast<double>(stop - start) / CLOCKS_PER_SEC) << "s." << endl;
  
  // Draw ellipses in the original image
  //const uint8_t colors[2][3] = {{127, 127, 127}, {255, 255, 255}};
  
  for (int i = 0; i <  regions.size(); ++i) {
    master.strokeColor("red"); 
    master.strokeWidth(5);

    double x = regions[i].moments_[0] / regions[i].area_;
    double y = regions[i].moments_[1] / regions[i].area_;

    master.draw( DrawablePoint(x, y, rx, ry, as, ae) );
  // Save the original image
  }

  master.display();
  
  
}

