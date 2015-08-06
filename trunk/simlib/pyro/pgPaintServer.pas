{ Project: Pyro
  Module: Pyro Core

  Description:
  Generic paint server

  Creation Date:
  17Oct2005

  Author: Nils Haeck (n.haeck@simdesign.nl)
  Copyright (c) 2005 - 2011 SimDesign BV
}
unit pgPaintServer;

{$i simdesign.inc}

interface

uses
  pgViewPort, pgTransform, Pyro, pgSampler, pgDocument;

type

  TpgUsageUnitsProp = class(TpgIntProp)
  private
    function GetValue: TpgUsageUnits;
    procedure SetValue(const Value: TpgUsageUnits);
  public
    property Value: TpgUsageUnits read GetValue write SetValue;
  end;

  // A Paint Server is a general notion of an element responsible to apply paint
  // to a fill or stroke area. It is the basis for gradient and pattern paint
  // servers, and perhaps others in future.
  TpgPaintServer = class(TpgGroup)
  public
    function CreateSampler(ATransformList: TpgTransformList; const BoundingBox: TpgBox): TpgSampler; virtual; abstract;
  end;

implementation

{ TpgUsageUnitsProp }

function TpgUsageUnitsProp.GetValue: TpgUsageUnits;
begin
  Result := TpgUsageUnits(inherited GetValue);
end;

procedure TpgUsageUnitsProp.SetValue(const Value: TpgUsageUnits);
begin
  inherited SetValue(integer(Value));
end;

end.
