// This file contains general defines for the pg project. Functionality can be
// adapted by in- or outcommenting certain defines.

//  Author: Nils Haeck
//  Copyright (c) 2004 - 2005 by SimDesign B.V.

// Vector format support

{$DEFINE IMPORTSVG} // Import of SVG vector files (requires pgSvgImport.pas)
{$DEFINE IMPORTEMF} // Import of EMF vector files (requires pgEmfImport.pas)
{.$DEFINE IMPORTPDF} // Import of PDF files (requires pgPdfImport.pas and PDF library)

{.$DEFINE EXPORTSVG} // Export of SVG vector files (requires pgSvgExport.pas)
{.$DEFINE EXPORTEMF} // Export of EMF vector files (requires pgEmfExport.pas)
{.$DEFINE EXPORTPDF} // Export of PDF files (requires pgPdfExport.pas and PDF library)

// External lib support

{$DEFINE LCMS}      // Support for ICC color profiles through LCMS lib

// Raster format support

{$DEFINE SUPPORTBMP}  // Requires pgRasterBmp.pas
{$DEFINE SUPPORTJPG}  // Requires pgRasterJpg.pas and uses Delphi unit Jpeg
{$DEFINE SUPPORTPNG}  // Requires pgRasterPng.pas and uses TPngImage by Gustavo Daud
{$DEFINE SUPPORTGIF}  // Requires pgRasterGif.pas and uses TGifImage by Anders Melander
{.$DEFINE SUPPORTTIF}  // Requires pgRasterTif.pas and uses LibTiff.dll 
{.$DEFINE SUPPORTTGA}  // Requires pgRasterTga.pas

