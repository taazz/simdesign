{ Project: Pyro
  Module: Pyro Core

  Description:
  Container descendant which is used by application, and defines a scene. It
  provides methods for synchronisation of element trees.

  Author: Nils Haeck (n.haeck@simdesign.nl)
  Copyright (c) 2006 - 2011 SimDesign BV

  Modified:
  19may2011: string > Utf8String
}
unit pgScene;

{$i simdesign.inc}

interface

uses
  Classes, SysUtils, Contnrs, pgViewPort, pgDocument, Pyro;

type

  TpgSceneListener = class(TPersistent)
  private
    FRef: TObject;
    FOnBeforeChange: TpgSceneChangeEvent;
    FOnAfterChange: TpgSceneChangeEvent;
  public
    constructor Create(ARef: TObject);
    property OnBeforeChange: TpgSceneChangeEvent read FOnBeforeChange write FOnBeforeChange;
    property OnAfterChange: TpgSceneChangeEvent read FOnAfterChange write FOnAfterChange;
  end;

  TpgSceneListenerList = class(TObjectList)
  private
    function GetItems(Index: integer): TpgSceneListener;
  public
    function AddRef(ARef: TObject): TpgSceneListener;
    procedure DeleteRef(ARef: TObject);
    property Items[Index: integer]: TpgSceneListener read GetItems; default;
  end;

  TpgScene = class(TpgStoredDocument)
  private
    FViewport: TpgViewPort;
    FListeners: TpgSceneListenerList;
    function GetViewPort: TpgViewPort;
  protected
    procedure DoBeforeChange(AElementId, APropId: longword; AChange: TpgChangeType); override;
    procedure DoAfterChange(AElementId, APropId: longword; AChange: TpgChangeType); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    function ElementByName(const AName: Utf8String): TpgElement;
    property ViewPort: TpgViewPort read GetViewPort;
    property Listeners: TpgSceneListenerList read FListeners;
  end;

implementation

{ TpgSceneListener }

constructor TpgSceneListener.Create(ARef: TObject);
begin
  inherited Create;
  FRef := ARef;
end;

{ TpgSceneListenerList }

function TpgSceneListenerList.AddRef(ARef: TObject): TpgSceneListener;
begin
  Result := TpgSceneListener.Create(ARef);
  Add(Result);
end;

procedure TpgSceneListenerList.DeleteRef(ARef: TObject);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].FRef = Aref then
    begin
      Delete(i);
      exit;
    end;
end;

function TpgSceneListenerList.GetItems(Index: integer): TpgSceneListener;
begin
  if (Index >= 0) and (Index < Count) then
    Result := Get(Index)
  else
    Result := nil;
end;

{ TpgScene }

procedure TpgScene.Clear;
begin
  inherited;
  FViewPort := nil;
end;

constructor TpgScene.Create(AOwner: TComponent);
begin
  inherited;
  FListeners := TpgSceneListenerList.Create;
end;

destructor TpgScene.Destroy;
begin
  FreeAndNil(FListeners);
  inherited;
end;

procedure TpgScene.DoAfterChange(AElementId, APropId: longword; AChange: TpgChangeType);
var
  i: integer;
begin
  if UpdateCount > 0 then exit;
  for i := 0 to FListeners.Count - 1 do
    if assigned(FListeners[i].FOnAfterChange) then
      FListeners[i].FOnAfterChange(Self, AElementId, APropId, AChange);
end;

procedure TpgScene.DoBeforeChange(AElementId, APropId: longword;
  AChange: TpgChangeType);
var
  i: integer;
begin
  if UpdateCount > 0 then exit;
  for i := 0 to FListeners.Count - 1 do
    if assigned(FListeners[i].FOnBeforeChange) then
      FListeners[i].FOnBeforeChange(Self, AElementId, APropId, AChange);
end;

function TpgScene.ElementByName(const AName: Utf8String): TpgElement;
var
  i: integer;
  AElement: TpgElement;
begin
  Result := nil;
  for i := 0 to Elements.Count - 1 do
  begin
    AElement := Elements[i];
    if (AElement is TpgStyleable) and (TpgStyleable(AElement).Name.Value = AName) then
    begin
      Result := AElement;
      exit;
    end;
  end;
end;

function TpgScene.GetViewPort: TpgViewPort;
var
  i: integer;
begin
  if not assigned(FViewPort) then
  begin
    // If we do not have a viewport, try to find it in the list, the first
    // element of that type which has parent = nil.
    for i := 0 to Elements.Count - 1 do
      if (Elements[i].Parent = nil) and (Elements[i].ClassType = TpgViewPort) then
      begin
        FViewPort := TpgViewPort(Elements[i]);
        break;
      end;
    // If we still don't have it, we create a new viewport
    if not assigned(FViewPort) then
    begin
      FViewPort := TpgViewPort(NewElement(TpgViewPort));
      FViewPort.EditorOptions.Value := [eoDenySelect];
    end;
  end;
  Result := FViewPort;
end;

end.
