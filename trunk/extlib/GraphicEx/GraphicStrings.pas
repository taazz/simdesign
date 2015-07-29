unit GraphicStrings;

// This file is part of the image library GraphicEx (www.lischke-online.de/Graphics.html).
//
// GraphicStrings contains the strings used in GraphicEx which could be localized.
//
// (c) Copyright 1999, 2000  Dipl. Ing. Mike Lischke (public@lischke-online.de). All rights reserved.
//
// This package is freeware for non-commercial use only.
// Contact author for licenses (shareware@lischke-online.de) and see License.txt which comes with the package.

interface

{$I GraphicConfiguration.inc}

resourcestring
  // image file descriptions
  gesAllImages = 'All images';
  gesRegistration = 'Attempt to register %s twice.';

  gesBitmaps = 'Windows bitmaps';
  gesRLEBitmaps = 'Run length encoded Windows bitmaps';
  gesDIBs = 'Device independant Windows bitmaps';
  gesEPS = 'Encapsulated Postscript images';
  gesIcons = 'Windows icons';
  gesMetaFiles = 'Windows metafiles';
  gesEnhancedMetaFiles = 'Windows enhanced meta files';
  gesJPGImages = 'JPG images';
  gesJPEGImages = 'JPEG images';
  gesJPEImages = 'JPE images';
  gesJFIFImages = 'JFIF images';
  gesTruevision = 'Truevision images';
  gesTIFF = 'Tagged image file format images';
  gesMacTIFF =  'Macintosh TIFF images';
  gesPCTIF = 'PC TIF images';
  gesGFIFax = 'GFI fax images';
  gesSGI = 'SGI images';
  gesSGITrueColor = 'SGI true color images';
  gesZSoft = 'ZSoft Paintbrush images';
  gesZSoftWord = 'Word 5.x screen capture images';
  gesAliasWaveFront = 'Alias/Wavefront images';
  gesSGITrueColorAlpha = 'SGI true color images with alpha';
  gesSGIMono = 'SGI black/white images';
  gesPhotoshop = 'Photoshop images';
  gesPortable = 'Portable map images';
  gesPortablePixel = 'Portable pixel map images';
  gesPortableGray = 'Portable gray map images';
  gesPortableMono = 'Portable bitmap images';
  gesAutoDesk = 'Autodesk images';
  gesKodakPhotoCD = 'Kodak Photo-CD images';
  gesCompuserve = 'CompuServe images';
  gesHalo = 'Dr. Halo images';
  gesPaintShopPro = 'Paintshop Pro images';
  gesPortableNetworkGraphic = 'Portable network graphic images';

  // image specific error messages
  gesInvalidImage = 'Cannot load image. Invalid or unexpected %s image format.';
  gesInvalidColorFormat = 'Invalid color format in %s file.';
  gesStreamReadError = 'Stream read error in %s file.';
  gesUnsupportedImage = 'Cannot load image. Unsupported %s image format.';
  gesUnsupportedFeature = 'Cannot load image. %s not supported for %s files.';
  gesInvalidCRC = 'Cannot load image. CRC error found in %s file.';
  gesCompression = 'Cannot load image. Compression error found in %s file.';
  gesExtraCompressedData = 'Cannot load image. Extra compressed data found in %s file.';
  gesInvalidPalette = 'Cannot load image. Palette in %s file is invalid.';
  gesUnknownCriticalChunk = 'Cannot load PNG image. Unexpected but critical chunk detected.';

  // features (usually used together with unsupported feature string)
  gesCompressionScheme = 'The compression scheme is';
  gesRLAPixelFormat = 'Image formats other than RGB and RGBA are';
  gesPSPFileType = 'File versions other than 3 or 4 are';

  // color manager error messages
  gesIndexedNotSupported = 'Conversion between indexed and non-indexed pixel formats is not supported.';
  gesConversionUnsupported = 'Color conversion failed. Could not find a proper method.';
  gesInvalidSampleDepth = 'Color depth is invalid. Bits per sample must be 1, 2, 4, 8 or 16.';
  gesInvalidPixelDepth = 'Sample count per pixel does not correspond to the given color scheme.';
  gesInvalidSubSampling = 'Subsampling value is invalid. Allowed are 1, 2 and 4.';
  gesVerticalSubSamplingError = 'Vertical subsampling value must be <= horizontal subsampling value.';

  // progress strings
  gesPreparing = 'Preparing...';
  gesLoadingData = 'Loading data...';
  gesUpsampling = 'Upsampling...';
  gesTransfering = 'Transfering...';

  // compression errors
  gesLZ77Error = 'LZ77 decompression error.';
  gesJPEGEOI = 'JPEG decompression error. Unexpected end of input.';
  gesJPEGStripSize = 'Improper JPEG strip/tile size.';
  gesJPEGComponentCount = 'Improper JPEG component count.';
  gesJPEGDataPrecision = 'Improper JPEG data precision.';
  gesJPEGSamplingFactors = 'Improper JPEG sampling factors.';
  gesJPEGBogusTableField = 'Bogus JPEG tables field.';
  gesJPEGFractionalLine = 'Fractional JPEG scanline unsupported.';

  // miscellaneous
  gesWarning = 'Warning';

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

end.
