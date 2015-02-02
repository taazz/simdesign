{ <b>Project</b>: Pyro<p>
  <b>Module</b>: Pyro Edit<p>

  <b>Description:</b><p>
  Viewport class working with projective transform

  <b>Author</b>: Nils Haeck (n.haeck@simdesign.nl)<p>
  Copyright (c) 2006 SimDesign BV
}
unit pgProjectiveViewPort;

interface

uses
  SysUtils, pgSizeable, pgViewPort, pgTransform, pgTypes, pgProperties, pgConstants,
  pgElement, pgProjectiveTransform;

type

  TpgProjectiveViewPort = class(TpgViewPort)
  private
    FLocalTransform: TpgProjectiveTransform;
    function GetPoints: TpgFloatListProp;
  protected
    function GetLocalTransform(const AInfo: TpgDeviceInfo): TpgTransform; override;
  public
    property Points: TpgFloatListProp read GetPoints;
  end;

resourcestring
  sPointListIncorrect              = 'Pointlist incorrect in transform';

implementation

{ TpgProjectiveViewPort }

function TpgProjectiveViewPort.GetLocalTransform(const AInfo: TpgDeviceInfo): TpgTransform;
var
  i: integer;
  PA: TpgPreserveAspect;
  VBMinX, VBMinY, VBWidth, VBHeight: double;
  List: TpgFloatProtectList;
begin
  if not assigned(FLocalTransform) or not FLocalTransform.IsValid then begin
    // Create it
    FreeAndNil(FLocalTransform);
    FLocalTransform := TpgProjectiveTransform.Create;
    // Make sure to use PreserveAspect = XMidYMid if no value available
    if PreserveAspect.ExistsLocal then
      PA := PreserveAspect.Value
    else
      PA := paXMidYMid;

    // Set viewbox of projective transform
    GetViewBoxProps(VBMinX, VBMinY, VBWidth, VBHeight);
    FLocalTransform.MinX := VBMinX;
    FLocalTransform.MinY := VBMinY;
    FLocalTransform.Width := VBWidth;
    FLocalTransform.Height := VBHeight;

    // Now setup the points
    List := Points.Values;
    if not assigned(List) or (List.Count <> 8) then
      raise Exception.Create(sPointListIncorrect);
    for i := 0 to 3 do begin
      FLocalTransform.Points[i].X := List[i * 2    ].Value;
      FLocalTransform.Points[i].Y := List[i * 2 + 1].Value;
    end;
  end;
  Result := FLocalTransform;
end;

function TpgProjectiveViewPort.GetPoints: TpgFloatListProp;
begin
  Result := TpgFloatListProp(PropById(piPVPPoints));
end;

initialization

  RegisterElement(eiProjectiveViewPort, TpgProjectiveViewPort, 'ProjectiveViewPort');
  RegisterProp(piPVPPoints, TpgFloatListProp, 'Points', TpgProjectiveViewPort, [pfStored]);


end.
