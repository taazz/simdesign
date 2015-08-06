{ Project: Pyro
  Module: Pyro Core

  Description:
  Text element class

  Author: Nils Haeck (n.haeck@simdesign.nl)
  Copyright (c) 2006 - 2011 SimDesign BV
}
unit pgText;

interface

uses
  pgShape, pgViewPort, pgDocument, Pyro;

type

  TpgTextSpan = class(TpgGraphic)
  private
    function GetDx: TpgHLengthListProp;
    function GetDy: TpgVLengthListProp;
    function GetX: TpgHLengthListProp;
    function GetY: TpgVLengthListProp;
    function GetText: TpgStringProp;
  public
    property X: TpgHLengthListProp read GetX;
    property Y: TpgVLengthListProp read GetY;
    property Dx: TpgHLengthListProp read GetDx;
    property Dy: TpgVLengthListProp read GetDy;
    property Text: TpgStringProp read GetText;
  end;

  TpgText = class(TpgTextSpan)
  protected
    constructor CreateInternal(AOwner: TpgDocument; AParent: TpgElement); override;
  end;

implementation

{ TpgTextSpan }

function TpgTextSpan.GetDx: TpgHLengthListProp;
begin
  Result := TpgHLengthListProp(PropById(piTxtDx));
end;

function TpgTextSpan.GetDy: TpgVLengthListProp;
begin
  Result := TpgVLengthListProp(PropById(piTxtDy));
end;

function TpgTextSpan.GetText: TpgStringProp;
begin
  Result := TpgStringProp(PropById(piText));
end;

function TpgTextSpan.GetX: TpgHLengthListProp;
begin
  Result := TpgHLengthListProp(PropById(piTxtX));
end;

function TpgTextSpan.GetY: TpgVLengthListProp;
begin
  Result := TpgVLengthListProp(PropById(piTxtY));
end;

{ TpgText }

constructor TpgText.CreateInternal(AOwner: TpgDocument; AParent: TpgElement);
begin
  inherited CreateInternal(AOwner, AParent);
  Flags := Flags + [efAllowElements];
end;

initialization

  RegisterElement(eiText, TpgText, 'Text');
  RegisterElement(eiTextSpan, TpgTextSpan, 'TextSpan');

  RegisterProp(piTxtX, TpgHLengthListProp, 'X', TpgTextSpan, [pfStored]);
  RegisterProp(piTxtY, TpgVLengthListProp, 'Y', TpgTextSpan, [pfStored]);
  RegisterProp(piTxtDx, TpgHLengthListProp, 'Dx', TpgTextSpan, [pfStored]);
  RegisterProp(piTxtDy, TpgVLengthListProp, 'Dy', TpgTextSpan, [pfStored]);
  RegisterProp(piText, TpgStringProp, 'Text', TpgTextSpan, [pfStored]);


end.
