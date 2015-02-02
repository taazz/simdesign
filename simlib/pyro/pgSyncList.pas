{ Project: Pyro
  Module: Pyro Edit

  Description:
  Synchronisation between scene and editor

  Author: Nils Haeck (n.haeck@simdesign.nl)
  Contributors:
  BE: Bob Evens (bob.evens@btinternet.com)

  Copyright (c) 2006 - 2011 SimDesign BV
}
unit pgSyncList;

interface

uses
  Classes, sdSortedLists, pgDocument, pgScene, Pyro;

type

  TpgSyncList = class;

  TpgSyncChildList = class;

  TpgSyncItem = class(TPersistent)
  private
    FOwner: TpgSyncList;
    FId: longword;
    function GetElement: TpgElement;
  protected
    procedure Invalidate; virtual;
    function PreventDrop: boolean; virtual;
    // Create a list with references to children of this syncitem. If children
    // do not yet exist, they'll be created
    function GetChildList: TpgSyncChildList;
    property Owner: TpgSyncList read FOwner;
  public
    constructor Create(AOwner: TpgSyncList; AId: longword); virtual;
    property Id: longword read FId write FId;
    property Element: TpgElement read GetElement;
  end;
  TpgSyncItemClass = class of TpgSyncItem;

  // A list with reference to SyncItems.
  TpgSyncChildList = class(TList)
  end;

  TpgSyncOption = (
    soUpdateChildren,  // An update causes syncitem children to get updated too
    soDropChildren,    // An update causes syncitem children to be dropped (deleted)
    soBeforeChange,    // Tie into the scene's beforechange event
    soAfterChange      // Tie into the scene's afterchange event
  );
  TpgSyncOptions = set of TpgSyncOption;

  // A TpgSyncList is a list that automatically stays in sync with a scene's element
  // list. TpgSyncList owns the syncitems. A SyncList stores additional info about
  // elements from a scene. This info is used by e.g. an editor to store bounding
  // boxes etc. The list stays in sync with the scene by tying into the scene's
  // listeners.
  TpgSyncList = class(TCustomSortedList)
  private
    FItemClass: TpgSyncItemClass;
    FFilterClass: TpgElementClass;
    FScene: TpgScene;
    FOptions: TpgSyncOptions;
    procedure SetItemClass(const Value: TpgSyncItemClass);
    procedure SetScene(const Value: TpgScene);
    procedure SetFilterClass(const Value: TpgElementClass);
    function GetItems(Index: integer): TpgSyncItem;
    function GetElement(AId: longword): TpgElement;
  protected
    procedure Rebuild; virtual;
    procedure SceneChange(Sender: TObject; AElementId, APropId: longword;
      AChange: TpgChangeType);
    function DoCompare(Item1, Item2: TObject): integer; override;
    function IndexById(AId: longword; var AIndex: integer): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function ById(AId: longword): TpgSyncItem;
    function AddItem(AId: longword): TpgSyncItem;
    procedure RemoveItem(AId: longword);
    procedure UpdateItem(AId: longword);
    // The default SyncItem class to create when adding items
    property ItemClass: TpgSyncItemClass read FItemClass write SetItemClass;
    // Elements synced must be or descend from FilterClass in order to get added. Set
    // this to e.g. TpgGraphic to only add graphic elements.
    property FilterClass: TpgElementClass read FFilterClass write SetFilterClass;
    property Scene: TpgScene read FScene write SetScene;
    property Options: TpgSyncOptions read FOptions write FOptions;
    property Items[Index: integer]: TpgSyncItem read GetItems;
  end;

implementation

type

  TSceneAccess = class(TpgScene);

{ TpgSyncItem }

constructor TpgSyncItem.Create(AOwner: TpgSyncList; AId: longword);
begin
  inherited Create;
  FOwner := AOwner;
  FId := AId;
end;

function TpgSyncItem.GetChildList: TpgSyncChildList;
var
  i: integer;
  ChildId: longword;
  Parent: TpgElement;
  Child: TpgSyncItem;
begin
  Result := TpgSyncChildList.Create;
  Parent := Element;
  if not assigned(Parent) then
    exit;
  for i := 0 to Parent.ElementCount - 1 do
  begin
    ChildId := Parent.Elements[i].Id;
    Child := FOwner.ById(ChildId);
    if not assigned(Child) then
      Child := FOwner.AddItem(ChildId);
    if assigned(Child) then
      Result.Add(Child);
  end;
end;

function TpgSyncItem.GetElement: TpgElement;
begin
  if assigned(FOwner) then
    Result := FOwner.GetElement(FId)
  else
    Result := nil;
end;

procedure TpgSyncItem.Invalidate;
begin
// default does nothing
end;

function TpgSyncItem.PreventDrop: boolean;
begin
  Result := False;
end;

{ TpgSyncList }

function TpgSyncList.AddItem(AId: longword): TpgSyncItem;
var
  Element: TpgElement;
begin
  Result := nil;
  Element := GetElement(AId);
  if assigned(Element) then begin
    Result := FItemClass.Create(Self, AId);
    Add(Result);
  end;
end;

function TpgSyncList.ById(AId: longword): TpgSyncItem;
var
  Index: integer;
begin
  if IndexById(AId, Index) then
    Result := Get(Index)
  else
    Result := nil;
end;

constructor TpgSyncList.Create;
begin
  inherited Create;
  FItemClass := TpgSyncItem;
  FFilterClass := TpgElement;
end;

destructor TpgSyncList.Destroy;
begin
  SetScene(nil);
  inherited;
end;

function TpgSyncList.DoCompare(Item1, Item2: TObject): integer;
begin
  Result := CompareInteger(TpgSyncItem(Item1).FId, TpgSyncItem(Item2).FId);
end;

function TpgSyncList.GetElement(AId: longword): TpgElement;
begin
  Result := nil;
  if assigned(FScene) then
    Result := FScene.ElementById(AId);
  if not (Result is FFilterClass) then
    Result := nil;
end;

function TpgSyncList.GetItems(Index: integer): TpgSyncItem;
begin
  Result := Get(Index);
end;

function TpgSyncList.IndexById(AId: longword;
  var AIndex: integer): boolean;
var
  AMin, AMax: integer;
begin
  AIndex := 0;
  Result := False;
  // Find position for insert - binary method
  AMin := 0;
  AMax := Count;
  while AMin < AMax do begin
    AIndex := (AMin + AMax) div 2;
    case CompareInteger(Items[AIndex].FId, AId) of
    -1: AMin := AIndex + 1;
     0: begin
          Result := True;
          exit;
        end;
     1: AMax := AIndex;
    end;
  end;
  AIndex := AMin;
end;

procedure TpgSyncList.Rebuild;
var
  i: integer;
  Item: TpgSyncItem;
  Element: TpgElement;
begin
  Clear;
  if assigned(FScene) then
  begin
    for i := 0 to TSceneAccess(FScene).Elements.Count - 1 do
    begin
      Element := TSceneAccess(FScene).Elements[i];
      if Element is FFilterClass then
      begin
        Item := FItemClass.Create(Self, Element.Id);
        // Add as last item
        Insert(Count, Item);
      end;
    end;
  end;
end;

procedure TpgSyncList.RemoveItem(AId: longword);
var
  Index: integer;
begin
  if IndexById(AId, Index) then
    Delete(Index);
end;

procedure TpgSyncList.SceneChange(Sender: TObject; AElementId,
  APropId: longword; AChange: TpgChangeType);
begin
  case AChange of
  ctListClear: Clear;
  ctListUpdate: Rebuild;
  ctElementAdd:
    AddItem(AElementId);
  ctElementRemove:
    RemoveItem(AElementId);
  ctElementListAdd,
  ctElementListRemove,
  ctPropAdd,
  ctPropRemove,
  ctPropUpdate:
    UpdateItem(AElementId);
  end;
end;

procedure TpgSyncList.SetFilterClass(const Value: TpgElementClass);
begin
  if FFilterClass <> Value then
  begin
    FFilterClass := Value;
    Rebuild;
  end;
end;

procedure TpgSyncList.SetItemClass(const Value: TpgSyncItemClass);
begin
  if FItemClass <> Value then
  begin
    FItemClass := Value;
    Rebuild;
  end;
end;

procedure TpgSyncList.SetScene(const Value: TpgScene);
var
  L: TpgSceneListener;
begin
  if FScene <> Value then
  begin
    if assigned(FScene) then
    begin
      // BE: remove does not appear to free memory
      // whereas DeleteRef does
      // FScene.Listeners.Remove(Self);
      FScene.Listeners.DeleteRef(Self);
    end;
    FScene := Value;
    Rebuild;
    if assigned(FScene) then
    begin
      L := FScene.Listeners.AddRef(Self);
      if soBeforeChange in FOptions then
        L.OnBeforeChange := SceneChange;
      if soAfterChange in FOptions then
        L.OnAfterChange := SceneChange;
    end;
  end;
end;

procedure TpgSyncList.UpdateItem(AId: longword);
var
  i: integer;
  ChildId: longword;
  Item: TpgSyncItem;
  Parent: TpgElement;
  Child: TpgSyncItem;
begin
  Item := ById(AId);
  if not assigned(Item) then
    exit;
  Item.Invalidate;
  Parent := GetElement(AId);
  if [soUpdateChildren, soDropChildren] * FOptions <> [] then
  begin
    for i := 0 to Parent.ElementCount - 1 do
    begin
      ChildId := Parent.Elements[i].Id;
      Child := ById(ChildId);
      if not assigned(Child) then
        continue;
      if (soDropChildren in FOptions) and not Child.PreventDrop then
      begin
        RemoveItem(ChildId);
        continue;
      end;
      // Recursive call
      UpdateItem(ChildId);
    end;
  end;
end;

end.
