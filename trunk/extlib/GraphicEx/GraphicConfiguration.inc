//----------------------------------------------------------------------------------------------------------------------
// This file is part of the image library GraphicEx (www.lischke-online.de/Graphics.html).
//
// In this configuratin file you can determine which parts of the library are to be compiled.
// This is in particular important for LZW compression code for which Unisys owns a patent.
// If you want to use LZW decoding then you have to make a deal with Unisys. Don't make me
// responsible for using the code.
// Other configuration options include to limit the number of available image formats to
// save memory, if necessary. 
//
// (c) Copyright 1999, 2000  Dipl. Ing. Mike Lischke (public@lischke-online.de). All rights reserved.
//
// This package is freeware for non-commercial use only.
// Contact author for licenses (shareware@lischke-online.de) and see License.txt which comes with the package.
//----------------------------------------------------------------------------------------------------------------------

{.$define UseLZW} // if this is not defined (default) then neither the GIF format nor the
                  // LZW compression decoder (e.g. for TIFF LZW compression) is available

// Pick your preferred image formats here or leave them all enabled. By disabling
// certain formats which are not of interest you can save some memory.
{$define SGIGraphic}             // *.bw, *.rgb, *.rgba, *.sgi images are supported
{$define AutodeskGraphic}        // *.cel, *.pic images
{$define TIFFGraphic}            // *.tif, *.tiff images
{$define TargaGraphic}           // *.tga, *.vst, *.icb, *.vda, *.win images
{$define PCXGraphic}             // *.pcx, *.pcc, *.scr images
{$define PCDGraphic}             // *.pcd images
{$define PortableMapGraphic}     // *.ppm, *.pgm, *.pbm images
{$define CUTGraphic}             // *.cut (+ *.pal) images
{$define GIFGraphic}             // *.gif images
{$define RLAGraphic}             // *.rla, *.rpf images
{$define PhotoshopGraphic}       // *.psd, *.pdd images
{$define PaintshopProGraphic}    // *.psp images
{$define PortableNetworkGraphic} // *.png images
{$define EPSGraphic}             // *.eps images

// adjust dependent definitions
{$ifndef UseLZW} {$undef GIFGraphic} {$endif}
{$ifndef TIFFGraphic} {$undef EPSGraphic} {$endif}

