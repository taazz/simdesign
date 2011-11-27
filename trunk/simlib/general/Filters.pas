{
  Unit Filters

  This unit provides helper routines for working with filtered lists.

  Author: Nils Haeck M.Sc.
  Original date: 04-Nov-2003

  Copyright (c) 2003 Simdesign, Nils Haeck

  It is NOT allowed under ANY circumstances to publish or copy this code
  without prior written permission of the Author!

  This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied.

  Please visit http://www.simdesign.nl for more information.

}
unit Filters;

interface

uses
  Classes, ComCtrls, Contnrs, SysUtils, NativeXml;

type

  TFilterEvent = procedure(Sender: TObject; AItem, AFilterInfo: Pointer;
    var Accept: boolean) of object;

  TCompareEvent = function(Sender: TObject; Item1, Item2, Info: Pointer): integer of object;

  { TFilter is used to filter data in a source list TFilter.Source using event
    OnFilterItem. The resulting list will be sorted through OnCompareItem. The
    list can be updated by calling Execute.
  }
  TFilter = class(TList)
  private
    FCompareInfo: Pointer;
    FFilterInfo: Pointer;
    FSource: TList;
    FOnChanged: TNotifyEvent;
    FOnCompareItem: TCompareEvent;
    FOnFilterItem: TFilterEvent;
    procedure SetSource(const Value: TList);
    procedure QuickSort(L, R: integer);
  protected
    procedure DoChanged;
    procedure DoFilterItem(AItem, AFilterInfo: Pointer; var Accept: boolean);
    function DoCompareItem(Item1, Item2, Info: Pointer): integer;
  public
    procedure Execute; virtual;
    procedure Sort; virtual;
    property FilterInfo: Pointer read FFilterInfo write FFilterInfo;
    property CompareInfo: Pointer read FCompareInfo write FCompareInfo;
    property Source: TList read FSource write SetSource;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnCompareItem: TCompareEvent read FOnCompareItem write FOnCompareItem;
    property OnFilterItem: TFilterEvent read FOnFilterItem write FOnFilterItem;
  end;

  PIntArray = ^TIntArray;
  TIntArray = array[0..MaxInt div 4 - 1] of integer;

  TStringRefList = class;

  TStringRef = class
  private
    FId: integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ClearValue(ARefList: TStringRefList);
    function GetValue(ARefList: TStringRefList): string;
    procedure SetValue(ARefList: TStringRefList; const AValue: string);
    procedure LoadFromStream(S: TStream); virtual;
    procedure SaveToStream(S: TStream); virtual;
    property Id: integer read FId write FId;
  end;

  TStringElem = class
  private
    FValue: string;
    FId: integer;
    FRefCount: integer;
  public
    procedure LoadFromStream(S: TStream); virtual;
    procedure SaveToStream(S: TStream); virtual;
    property Value: string read FValue write FValue;
    property Id: integer read FId write FId;
    property RefCount: integer read FRefCount write FRefCount;
  end;

  { TStringRefList and TStringRef implement a special object designed to hold strings
    in an efficient way and to provide for a very fast compare method.

    Example:
    TMyObject = class
      property Name: TStringRef;
    end;
    var
      Names: TStringRefList;

    initialisation:
      Names := TStringRefList.Create;
    new object
      MyObj1.Name := TStringRef.Create;
      MyObj1.Name.SetValue(Names, 'My Name');
    compare:
      // Result =  0 means MyObj's value equals MyObj2's value
      // Result = -1 means MyObj's value < MyObj2's value
      //Result = +1 means MyObj's value > MyObj2's value
      if CompareStringRef(MyObj1, MyObj2, Names) = 0 then ...
    load/save
      Names.LoadFromStream(S);  // this loads the strings
      MyObj1.LoadFromStream(S); // this just the reference
  }
  TStringRefList = class(TList)
  private
    FElems: TObjectList; // Sorted list of elements
    FByIds: PIntArray;
    FByIdCount: integer;
    FByIdCapacity: integer;
    FCaseSensitive: boolean;
    function CompareStrings(const AValue1, AValue2: string): integer;
    function GetElemCount: integer;
    function GetElems(Index: integer): TStringElem;
    function GetByIds(Index: integer): integer;
    procedure SetByIdCapacity(ACapacity: integer);
    procedure SetByIDs(Index: integer; const Value: integer);
  protected
    function ElemFind(const AValue: string; var Index: integer): boolean;
    procedure ElemInsert(Index: integer; AElem: TStringElem);
    property ByIdCount: integer read FByIdCount;
    property Elems[Index: integer]: TStringElem read GetElems;
    property ElemCount: integer read GetElemCount;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function ByIDAdd(AInt: integer): integer;
    procedure Clear; override;
    procedure LoadFromStream(S: TStream); virtual;
    procedure SaveToStream(S: TStream); virtual;
    function ValueAdd(const AValue: string): integer;
    function ValueById(AId: integer): string;
    procedure ValueRemove(const AValue: string);
    property ByIDs[Index: integer]: integer read GetByIds write SetByIDs;
    property CaseSensitive: boolean read FCaseSensitive write FCaseSensitive;
  end;

  // Simple overridden TList type with integers instead of pointers
  TIntegerList = class(TList)
  private
    function GetList: PIntArray;
  protected
    function Get(Index: Integer): integer;
    procedure Put(Index: Integer; Item: integer);
  public
    function Add(Item: integer): integer;
    function Extract(Item: integer): integer;
    function First: integer;
    function IndexOf(Item: integer): integer;
    procedure Insert(Index: Integer; Item: integer);
    function Last: integer;
    function Remove(Item: integer): integer;
    property Items[Index: Integer]: integer read Get write Put; default;
    property List: PIntArray read GetList;
  end;

  TlvColumn = class(TPersistent)
  private
    FFieldIndex: integer;
    FWidth: integer;
    FCaption: string;
    FAlignment: TAlignment;
    FImageIndex: integer;
  public
    procedure LoadFromXml(ANode: TXmlNode);
    procedure SaveToXml(ANode: TXmlNode);
    property Caption: string read FCaption write FCaption;
    property Width: integer read FWidth write FWidth;
    property FieldIndex: integer read FFieldIndex write FFieldIndex;
    property Alignment: TAlignment read FAlignment write FAlignment;
    property ImageIndex: integer read FImageIndex write FImageIndex;
  end;

  { TlvManager
  }
  TlvManager = class(TComponent)
  private
    FColumns: TObjectList;
    FListview: TListview;
    FFilter: TFilter;
    FUpdateCount: integer;
    procedure SetListview(const Value: TListview);
    function GetColumns(Index: integer): TlvColumn;
    function GetColumnCount: integer;
    procedure SetFilter(const Value: TFilter);
    function IsUpdating: boolean;
    procedure BeginUpdate;
    procedure EndUpdate;
  protected
    procedure UpdateColumns;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ColumnAdd(AColumn: TlvColumn): integer;
    procedure ColumnsClear;
    procedure LoadFromXml(ANode: TXmlNode);
    procedure SaveToXml(ANode: TXmlNode);
    property Listview: TListview read FListview write SetListview;
    property ColumnCount: integer read GetColumnCount;
    property Columns[Index: integer]: TlvColumn read GetColumns;
    property Filter: TFilter read FFilter write SetFilter;
  end;


// Selection and listview helpers
function CreateSelectionFromListview(AListview: TListview): TIntegerList;

// Compare two stringref objects
function CompareStringRef(ARef1, ARef2: TStringRef; AList: TStringRefList): integer;

// Utility routines
function CompareInt(AInt1, AInt2: integer): integer;

implementation

function CompareInt(AInt1, AInt2: integer): integer;
begin
  if AInt1 < AInt2 then Result := -1 else
    if AInt1 > AInt2 then Result := 1 else
      Result := 0;
end;

procedure ReadString(S: TStream; var Value: string);
var
  ACount: integer;
begin
  S.Read(ACount, SizeOf(ACount));
  SetLength(Value, ACount);
  if ACount > 0 then
    S.Read(Value[1], ACount);
end;

procedure WriteString(S: TStream; const Value: string);
var
  ACount: integer;
begin
  ACount := Length(Value);
  S.write(ACount, SizeOf(ACount));
  if ACount > 0 then
    S.Write(Value[1], ACount);
end;

function CreateSelectionFromListview(AListview: TListview): TIntegerList;
var
  i: integer;
  ASelected: TListitem;
begin
  Result := TIntegerList.Create;
  if not assigned(AListview) then exit;
  with AListview do
    if SelCount <= Items.Count div 2 then begin
      // Method 1 - use GetNextItem
      ASelected := Selected;
      while assigned(ASelected) do begin
        Result.Add(ASelected.Index);
        ASelected := GetNextItem(ASelected, sdAll, [isSelected]);
      end;
    end else begin
      // Method 2 - loop through items
      for i := 0 to Items.Count - 1 do
        if Items[i].Selected then
          Result.Add(i);
    end;
end;

function CompareStringRef(ARef1, ARef2: TStringRef; AList: TStringRefList): integer;
begin
  Result := 0;
  if assigned(AList) then begin
    // We must merely compare the positions in the list
    Result := CompareInt(AList.ByIDs[ARef1.ID], AList.ByIDs[ARef2.ID]);
  end;
end;

{ TFilter }

procedure TFilter.DoChanged;
begin
  if assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TFilter.DoCompareItem(Item1, Item2, Info: Pointer): integer;
begin
  Result := 0;
  if assigned(FOnCompareItem) then
    Result := FOnCompareItem(Self, Item1, Item2, Info);
end;

procedure TFilter.DoFilterItem(AItem, AFilterInfo: Pointer;
  var Accept: boolean);
begin
  if assigned(FOnFilterItem) then
    FOnFilterItem(Self, AItem, AFilterInfo, Accept);
end;

procedure TFilter.Execute;
var
  i: integer;
  Accept: boolean;
begin
  Clear;
  if assigned(Source) then begin
    // Filter the list
    for i := 0 to Source.Count - 1 do begin
      Accept := False;
      DoFilterItem(Source[i], FilterInfo, Accept);
      if Accept then
        Add(Source[i]);
    end;
    // Sort the list
    Sort;
  end;
  DoChanged;
end;

procedure TFilter.QuickSort(L, R: integer);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := Items[(L + R) shr 1];
    repeat
      while DoCompareItem(Items[I], P, CompareInfo) < 0 do
        Inc(I);
      while DoCompareItem(Items[J], P, CompareInfo) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := Items[I];
        Items[I] := Items[J];
        Items[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TFilter.SetSource(const Value: TList);
begin
  FSource := Value;
  Execute;
end;

procedure TFilter.Sort;
begin
  if assigned(OnCompareItem) and (Count > 0) then begin
    QuickSort(0, Count - 1);
    DoChanged;
  end;
end;

{ TStringRef }

procedure TStringRef.ClearValue(ARefList: TStringRefList);
begin
  // This will remove the string from the reference list
  SetValue(ARefList, '');
end;

constructor TStringRef.Create;
begin
  inherited Create;
  FId := -1; // Indicate no ID yet
end;

destructor TStringRef.Destroy;
begin
  inherited;
end;

function TStringRef.GetValue(ARefList: TStringRefList): string;
begin
  Result := '';
  if assigned(ARefList) then
    Result := ARefList.ValueById(Id);
end;

procedure TStringRef.LoadFromStream(S: TStream);
begin
  S.Read(FId, SizeOf(FId));
end;

procedure TStringRef.SaveToStream(S: TStream);
begin
  S.Write(FId, SizeOf(FId));
end;

procedure TStringRef.SetValue(ARefList: TStringRefList; const AValue: string);
begin
  if assigned(ARefList) and (GetValue(ARefList) <> AValue) then begin
    ARefList.ValueRemove(GetValue(ARefList));
    Id := ARefList.ValueAdd(AValue);
  end;
end;

{ TStringRefList }

function TStringRefList.ByIDAdd(AInt: integer): integer;
begin
  Result := FByIdCount;
  if Result = FByIdCapacity then
    SetByIdCapacity(FByIdCount + 1024);
  FByIDs^[Result] := AInt;
  Inc(FByIdCount);
end;

procedure TStringRefList.Clear;
begin
  FElems.Clear;
  FByIdCount := 0;
  SetByIdCapacity(0);
end;

function TStringRefList.CompareStrings(const AValue1,
  AValue2: string): integer;
begin
  if CaseSensitive then
    Result := AnsiCompareStr(AValue1, AValue2)
  else
    Result := AnsiCompareText(AValue1, AValue2)
end;

constructor TStringRefList.Create;
begin
  inherited Create;
  FElems := TObjectList.Create;
  // Defaults
  FCaseSensitive := False;
end;

destructor TStringRefList.Destroy;
begin
  Clear;
  FreeAndNil(FElems);
  inherited;
end;

function TStringRefList.ElemFind(const AValue: string;
  var Index: integer): boolean;
var
  AMin, AMax: integer;
begin
  // Find position for insert - binary method
  Result := False;
  Index := 0;
  AMin := 0;
  AMax := ElemCount;
  while AMin < AMax do begin
    Index := (AMin + AMax) div 2;
    case CompareStrings(Elems[Index].Value, AValue) of
    -1: AMin := Index + 1;
     0: begin
          Result := True;
          exit;
        end;
     1: AMax := Index;
    end;
  end;
  Index := AMin;
end;

procedure TStringRefList.ElemInsert(Index: integer; AElem: TStringElem);
begin
  if assigned(FElems) then
    FElems.Insert(Index, AElem);
end;

function TStringRefList.GetByIds(Index: integer): integer;
begin
  Result := -1;
  if (Index >= 0) and (Index < ByIDCount) then
    Result := integer(FByIDs[Index]);
end;

function TStringRefList.GetElemCount: integer;
begin
  Result := 0;
  if assigned(FElems) then
    Result := FElems.Count;
end;

function TStringRefList.GetElems(Index: integer): TStringElem;
begin
  Result := nil;
  if (Index >= 0) and (Index < ElemCount) then
    Result := TStringElem(FElems[Index]);
end;

procedure TStringRefList.LoadFromStream(S: TStream);
var
  i, ACount, AInt: integer;
  AElem: TStringElem;
begin
  if not assigned(FElems) then exit;
  Clear;
  S.Read(ACount, SizeOf(ACount));
  for i := 0 to ACount - 1 do begin
    AElem := TStringElem.Create;
    AElem.LoadFromStream(S);
    FElems.Add(AElem);
  end;
  S.Read(ACount, SizeOf(ACount));
  for i := 0 to ACount - 1 do begin
    S.Read(AInt, SizeOf(AInt));
    ByIDAdd(AInt)
  end;
end;

procedure TStringRefList.SaveToStream(S: TStream);
var
  i, ACount, AInt: integer;
begin
  ACount := ElemCount;
  S.Write(ACount, SizeOf(ACount));
  for i := 0 to ACount - 1 do
    Elems[i].SaveToStream(S);
  ACount := ByIDCount;
  S.Write(ACount, SizeOf(ACount));
  for i := 0 to ACount - 1 do begin
    AInt := integer(FByIds[i]);
    S.Write(AInt, SizeOf(AInt));
  end;
end;

procedure TStringRefList.SetByIdCapacity(ACapacity: integer);
begin
  if ACapacity <> FByIdCapacity then begin
    ReallocMem(FByIds, ACapacity * SizeOf(integer));
    FByIdCapacity := ACapacity;
  end;
end;

procedure TStringRefList.SetByIDs(Index: integer; const Value: integer);
begin
  if (Index >= 0) and (Index < ByIdCount) then
    FByIds^[Index] := Value;
end;

function TStringRefList.ValueAdd(const AValue: string): integer;
// Add a string value, and return the id
var
  i: integer;
  AElem: TStringElem;
  AIndex: integer;
begin
  Result := -1;
  if not assigned(FElems) or (Length(AValue) = 0) then exit;

  // Try to find it first
  if ElemFind(AValue, AIndex) then begin

    // Return this element's ID and increase the refcount
    AElem := Elems[AIndex];
    AElem.RefCount := AElem.RefCount + 1;
    Result := AElem.ID;

  end else begin

    // We did not find it, so we add a new ID
    AElem := TStringElem.Create;
    AElem.Value := AValue;
    Result := ByIDCount;
    AElem.RefCount := 1;

    // Insert it at AIndex
    FElems.Insert(AIndex, AElem);

    // The list with ID's must be incremented for all ID's pointing to AIndex and up
    if ByIDCount > 0 then
      for i := 0 to ByIDcount - 1 do begin
        if FByIds^[i] >= AIndex then
          inc(FByIds^[i])
        else
          // Try spotting empty ID
          if FByIds^[i] = - 1 then
            Result := i;
      end;

    // And finally, add the new ID reference
    AElem.ID := Result;
    if Result = ByIDCount then
      ByIDAdd(AIndex)
    else
      ByIDs[Result] := AIndex;
  end;
end;

function TStringRefList.ValueById(AId: integer): string;
var
  AIndex: integer;
begin
  Result := '';
  if (AId >= 0) and (AId < ByIdCount) then begin
    AIndex := integer(FByIds[AId]);
    if (AIndex >= 0) and (AIndex < ElemCount) then
      Result := Elems[AIndex].Value;
  end;
end;

procedure TStringRefList.ValueRemove(const AValue: string);
var
  i, AIndex: integer;
  AElem: TStringElem;
begin
  if not assigned(FElems) or not assigned(FByIDs) or (Length(AValue) = 0) then exit;

  // Find the element
  if ElemFind(AValue, AIndex) then begin
    // We found it - so decrement its reference count
    AElem := Elems[AIndex];
    AElem.RefCount := AElem.RefCount - 1;

    // Check if this was the last reference
    if AElem.RefCount = 0 then begin

      // We remove this element
      if (AElem.ID >= 0) and (AElem.ID < ByIDCount) then begin
        // first set its ID in the list to -1
        ByIDs[AElem.ID] := -1;

        // Check if we can shorten the list
        while (ByIDCount > 0) and (ByIDs[ByIDCount - 1] = -1) do
          dec(FByIDCount);

      end;

      // Remove it from the list
      FElems.Delete(AIndex); // this will also free AElem

      // Now move up all references > AIndex
      for i := 0 to ByIDCount - 1 do
        if FByIDs^[i] > AIndex then
          dec(FByIds^[i]);

    end;
  end;
end;

{ TStringElem }

procedure TStringElem.LoadFromStream(S: TStream);
begin
  ReadString(S, FValue);
  S.Read(FId, SizeOf(FId));
  S.Read(FRefCount, SizeOf(FRefCount));
end;

procedure TStringElem.SaveToStream(S: TStream);
begin
  WriteString(S, FValue);
  S.Write(FId, SizeOf(FId));
  S.Write(FRefCount, SizeOf(FRefCount));
end;

{ TIntegerList }

function TIntegerList.Add(Item: integer): integer;
begin
  Result := inherited Add(pointer(Item));
end;

function TIntegerList.Extract(Item: integer): integer;
begin
  Result := integer(inherited Extract(pointer(Item)));
end;

function TIntegerList.First: integer;
begin
  Result := integer(inherited First);
end;

function TIntegerList.Get(Index: Integer): integer;
begin
  Result := integer(inherited Get(Index));
end;

function TIntegerList.GetList: PIntArray;
begin
  Result := PIntArray(inherited List);
end;

function TIntegerList.IndexOf(Item: integer): integer;
begin
  Result := inherited IndexOf(pointer(Item));
end;

procedure TIntegerList.Insert(Index, Item: integer);
begin
  inherited Insert(Index, pointer(Item));
end;

function TIntegerList.Last: integer;
begin
  Result := integer(inherited Last);
end;

procedure TIntegerList.Put(Index, Item: integer);
begin
  inherited Put(Index, pointer(Item));
end;

function TIntegerList.Remove(Item: integer): integer;
begin
  Result := inherited Remove(pointer(Item));
end;

{ TlvManager }

procedure TlvManager.BeginUpdate;
begin
  inc(FUpdateCount);
end;

function TlvManager.ColumnAdd(AColumn: TlvColumn): integer;
begin
  if assigned(AColumn) and assigned(FColumns) then
  begin
    Result := FColumns.Add(AColumn);
    UpdateColumns;
  end else
    Result := -1;
end;

procedure TlvManager.ColumnsClear;
begin
  FColumns.Clear;
end;

constructor TlvManager.Create(AOwner: TComponent);
begin
  inherited;
  FColumns := TObjectlist.Create;
end;

destructor TlvManager.Destroy;
begin
  // Remove event references in listview
  Listview := nil;
  FreeAndNil(FColumns);
  inherited;
end;

procedure TlvManager.EndUpdate;
begin
  dec(FUpdateCount);
end;

function TlvManager.GetColumnCount: integer;
begin
  Result := 0;
  if assigned(FColumns) then Result := FColumns.Count;
end;

function TlvManager.GetColumns(Index: integer): TlvColumn;
begin
  Result := nil;
  if (Index >= 0) and (Index < ColumnCount) then
    Result := TlvColumn(FColumns[Index]);
end;

function TlvManager.IsUpdating: boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TlvManager.LoadFromXml(ANode: TXmlNode);
var
  i: integer;
  AList: TList;
  AColumn: TlvColumn;
begin
  BeginUpdate;
  try
    ColumnsClear;
    AList := TList.Create;
    try
      ANode.NodesByName('Column', AList);
      for i := 0 to AList.Count - 1 do begin
        AColumn := TlvColumn.Create;
        ColumnAdd(AColumn);
        AColumn.LoadFromXml(TXmlNode(AList[i]));
      end;
    finally
      AList.Free;
    end;
  finally
    EndUpdate;
    UpdateColumns;
  end;
end;

procedure TlvManager.SaveToXml(ANode: TXmlNode);
var
  i: integer;
begin
  for i := 0 to ColumnCount - 1 do
    Columns[i].SaveToXml(ANode.NodeNew('Column'));
end;

procedure TlvManager.SetFilter(const Value: TFilter);
begin
  FFilter := Value;
end;

procedure TlvManager.SetListview(const Value: TListview);
// Set reference to listview and the events
begin
  if assigned(FListview) then with FListview do begin
    // Clear old events
  end;
  FListview := Value;
  if assigned(FListview) then with FListview do begin
    // Set props
    OwnerData := True;
    RowSelect := True;
    HideSelection := False;
    // Set new events
    // Set columns
    UpdateColumns;
  end;
end;

procedure TlvManager.UpdateColumns;
var
  i: integer;
  AColumn: TListColumn;
  AVisible: boolean;
begin
  if IsUpdating then exit;
  if not assigned(Listview) then exit;
  // Update the columns in the listview based on the Columns list

  // Temporarily turn off visibility
  AVisible := Listview.Visible;
  Listview.Visible := False;

  Listview.Columns.BeginUpdate;
  try
    // Columns
    for i := 0 to ColumnCount - 1 do begin
      // Re-use as many and create new ones
      if i < Listview.Columns.Count then
        AColumn := Listview.Columns[i]
      else
        AColumn := Listview.Columns.Add;
      AColumn.Caption    := Columns[i].Caption;
      AColumn.Width      := Columns[i].Width;
      AColumn.ImageIndex := Columns[i].ImageIndex;
      AColumn.Alignment  := Columns[i].Alignment;
    end;
    // Remove excess
    for i := Listview.Columns.Count - 1 downto ColumnCount do
      Listview.Columns.Delete(i);

  finally
    Listview.Columns.EndUpdate;
    // Turn vis back on
    Listview.Visible := AVisible;
  end;
end;

{ TlvColumn }

procedure TlvColumn.LoadFromXml(ANode: TXmlNode);
begin
  FFieldIndex := ANode.ReadInteger('FieldIndex');
  FImageIndex := ANode.ReadInteger('ImageIndex');
  FWidth :=  ANode.ReadInteger('Width');
  FCaption := ANode.ReadString('Caption');
  FAlignment := TAlignment(ANode.ReadInteger('Alignment'));
end;

procedure TlvColumn.SaveToXml(ANode: TXmlNode);
begin
  ANode.WriteInteger('FieldIndex', FFieldIndex);
  ANode.WriteInteger('ImageIndex', FImageIndex);
  ANode.WriteInteger('Width', FWidth);
  ANode.WriteString('Caption', FCaption);
  ANode.WriteInteger('Alignment', integer(FAlignment));
end;

end.
