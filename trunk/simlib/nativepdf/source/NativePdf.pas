{ Copyright (c) 2008 By Nils Haeck M.Sc. - SimDesign
  More information: www.simdesign.nl or n.haeck@simdesign.nl

  This source code may NOT be used or replicated without prior permission
  from the abovementioned author.
}
unit NativePdf;

interface

uses
  Graphics;

type

  TsdPdfGraphic = class(TGraphic)
  end;

implementation

initialization

  TPicture.RegisterFileFormat('pdf', 'Portable Document Format', TsdPdfGraphic);

finalization

  TPicture.UnregisterGraphicClass(TsdPdfGraphic);

end.
