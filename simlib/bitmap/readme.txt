This folder contains image processing code
==========================================

sdBitmap.pas + sdbitmapPlatform.pas
===================================
sdBitmap.pas contains the platform-independent TsdBitmap. A bitmap is a collection of pixels, each pixel containing N bits, e.g. 1bit, 2bit, 4bit, 8bit, 24bit, 32bit, up to 64bit for now. 

The pixels are organised in rows, where row 0 is the first one. Each row (scanline) is aligned to a memory location that is usually 4 or 8 bytes aligned (see ScanAlign).

One pixel usually is (but not necessarily) byte-aligned. Examples of non-bytealigned pixels are 1bit, 2bit and 4bit. Pixels in all other formats use byte sizes bigger or equal than needed with the number of bits for the pixel. Eg a pixel with 30bits/pixel uses 4 bytes / 32 bits.

The *standard* palette entry used in a TsdBitmap is a palette with 4 bytes:

  TsdPaletteEntryRec = packed record
    Red: byte;
    Green: byte;
    Blue: byte;
    Alpha: byte;
  end;

sdByteMap.pas
=============
This unit implements a byte map (or: matrix of bytes). It tries to encapsulate all previous
code (so many sdByteMaps around) into this definitive unit.

The TsdByteMap class is used in a lot of other code (e.g. segmentation, affine transforms, etc).

