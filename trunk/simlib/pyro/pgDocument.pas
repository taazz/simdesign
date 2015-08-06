{ Project: Pyro
  Module: Pyro Core

  Description:
  Core document class. The TpgElement class is the base class for
  all element types. TpgDocument holds a list of elements, and is the base
  class for scenes.

  Author: Nils Haeck (n.haeck@simdesign.nl)<p>
  Copyright (c) 2006 - 2011 SimDesign BV

  Modified:
  19may2011: string > Utf8String
}
unit pgDocument;

{$i simdesign.inc}

interface

uses
  Classes, SysUtils, sdSortedLists, sdDebug, pgPlatform, Pyro;

type

  // fwd declarations
  TpgDocument = class;
  TpgPropInfo = class;
  TpgElement = class;
  TpgElementList = class;
  TpgStorage = class;
  TpgStyle = class;

  // Basic property class, all properties descend from TpgProp.
  TpgProp = class(TPersistent)
  private
    FId: longword;
    procedure DoSetAsString(const Value: Utf8String);
  protected
    // Do not override any of the DoXXXX methods
    procedure DoBeforeChange(ACaller: TpgElement);
    procedure DoAfterChange(ACaller: TpgElement);
    constructor CreateInternal(AId: longword); virtual;
    function GetDocument: TpgDocument;
    procedure GetCaller(var ACaller: TpgElement; var MustWrite: boolean);
    // CallerProperty creates or returns a reference to a property of this type
    // depending on if it exists in ACaller yet. If it doesn't exist it will
    // be created.
    function CallerProperty(ACaller: TpgElement): TpgProp;
    // Override GetAsString to get the property value to a UTF8 string
    function GetAsString: Utf8String; virtual;
    // Override SetAsString to set the property value from a UTF8 string
    procedure SetAsString(const Value: Utf8String); virtual;
    // Override this method to copy all data of the property from AProp
    procedure CopyFrom(AProp: TpgProp); virtual;
  public
    // Do not override this method, override CopyFrom instead
    procedure Assign(ASource: TPersistent); override;
    // Call delete to utterly remove the property from the local props list of the
    // element it belongs to.
    procedure Delete;
    // Returns true if the property exists in the local property list of the
    // element
    function ExistsLocal: boolean;
    // Property Id, this Id identifies the property type, function and name and
    // should be unique within a project. When registering a property, you must
    // choose a unique positive number here. If the Id is 0, the property
    // is "stand-alone" and created outside of the element tree.
    property Id: longword read FId;
    // property representation as UTF8 string
    property AsString: Utf8String read GetAsString write DoSetAsString;
  end;

  TpgPropClass = class of TpgProp;

  TpgPropList = class(TCustomSortedList)
  private
    function GetItems(Index: integer): TpgProp;
  protected
    function DoCompare(Item1, Item2: TObject): integer; override;
    function IndexById(AId: longword; var AIndex: integer): boolean;
  public
    function ById(AId: longword): TpgProp;
    property Items[Index: integer]: TpgProp read GetItems; default;
  end;

  // Basic element type which supports a list of properties, and inheritance
  // of properties through the parent.
  TpgElement = class(TPersistent)
  private
    FId: longword;
    FDocument: TpgDocument;
    FParent: TpgElement;
    FLocalProps: TpgPropList;
    FFlags: TpgElementFlags;
    function GetLocalElementList: TpgElementList;
    procedure DoBeforeChange(AElementId, APropId: longword; AChange: TpgChangeType);
    procedure DoAfterChange(AElementId, APropId: longword; AChange: TpgChangeType);
    function GetElementIndex: integer;
    procedure SetElementIndex(const Value: integer);
  protected
    procedure DoDebugOut(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: Utf8String); virtual;
    procedure SetId(AId: longword);
    procedure SetDocument(ADocument: TpgDocument);
    procedure SetParent(AParent: TpgElement);
    // Copy all information except the Id from AElement. Do not override this method!
    procedure CopyFrom(AElement: TpgElement);
    // Check for location of a property with AId in own LocalProps list, references
    // (clones and styles), parent and eventually defaults of the container.
    function CheckPropLocations(AId: longword; var AInfo: TpgPropInfo; var AOwner: TpgElement): TpgProp;
    // CreateInternal creates the element and adds it to the ADocument (if specified).
    // Override this method to do any specific initialization. Always call inherited CreateInternal.
    constructor CreateInternal(ADocument: TpgDocument; AParent: TpgElement); virtual;
    // Check for referenced properties and return AOwner of the property if any. By
    // default this method does nothing, but in the TpgStyleable it returns properties
    // from clones and styles.
    function CheckReferenceProps(AId: longword; AInfo: TpgPropInfo; var AOwner: TpgElement): TpgProp; virtual;
    // Check if this element allows a property with info AInfo.
    function CheckPropertyClass(AInfo: TpgPropInfo): boolean; virtual;
    procedure DoElementAdd(AElement: TpgElement); virtual;
    procedure DoElementRemove(AElement: TpgElement); virtual;
    // DoBeforePropChange/DoAfterPropChange get called from the property whenever
    // it changes/gets added
    procedure DoBeforePropChange(APropId: longword; AChange: TpgChangeType); virtual;
    procedure DoAfterPropChange(APropId: longword; AChange: TpgChangeType); virtual;
    // DoAfterCreate is called after the element is created, and can be used to add
    // default props or subelements
    procedure DoAfterCreate; virtual;
    // DoBeforeSave is called before the element is saved to storage and can be
    // used to do any specific processing. The default does nothing
    procedure DoBeforeSave; virtual;
    // DoAfterLoad is called after the element is loaded from storage, and can
    // be used to do any specific processing based on the loaded prop or subelement
    // information
    procedure DoAfterLoad; virtual;
    // A list of properties owned by this element
    property LocalProps: TpgPropList read FLocalProps;
    function GetElementCount: integer; virtual;
    function GetElements(Index: integer): TpgElement; virtual;
    // Pointer to the local element list that is contained in the TpgElementList property.
    // If the element does not allow sub-elements, accessing this list will raise
    // an exception.
    property LocalElementList: TpgElementList read GetLocalElementList;
  public
    // Create a new element with Parent = AParent. It will automatically be
    // added to the container in which AParent also resides.
    constructor Create(AParent: TpgElement);
    // Create a new element as a copy of AElement. All local properties of
    // AElement are copied. AElement must be of the same class or an ancestor class.
    // The new element doesn't have a parent. It must be set through code.
    constructor CreateCopyFrom(AElement: TpgElement; AParent: TpgElement);
    destructor Destroy; override;
    // Returns a property with AId or nil if none found, also checks for inheritance
    // as well as clones and styles. Do not override this method!
    function PropById(AId: longword): TpgProp;
    // Clear all properties in this element
    procedure Clear; virtual;
    // Unique Id of this element within the container it is owned by. The container
    // automatically assigns a new unique Id when the element is created.
    property Id: longword read FId;
    // Reference to document that owns this element.
    property Document: TpgDocument read FDocument;
    // Reference to the parent of this element.
    property Parent: TpgElement read FParent write SetParent;
    // Set of flags for element
    property Flags: TpgElementFlags read FFlags write FFlags;
    // Returns the number of sub elements in this element
    property ElementCount: integer read GetElementCount;
    // Returns the sub element at Index in the sub element list. Warning: this
    // will raise an exception if the element doesn't allow storage of sub-elements
    property Elements[Index: integer]: TpgElement read GetElements;
    // Get or set the index in the parent element's array. Setting it to 0 means
    // the element will be first in the chain when rendered (bottom-most)
    property ElementIndex: integer read GetElementIndex write SetElementIndex;
  end;

  TpgElementClass = class of TpgElement;

  TpgElementList = class(TCustomSortedList)
  private
    function GetItems(Index: integer): TpgElement;
  protected
    function DoCompare(Item1, Item2: TObject): integer; override;
    function IndexById(AId: longword; var AIndex: integer): boolean;
  public
    procedure Remove(AElement: TpgElement);
    function ById(AId: longword): TpgElement;
    property Items[Index: integer]: TpgElement read GetItems; default;
  end;

  TpgElementListProp = class(TpgProp)
  private
    FElements: TpgElementList;
    function GetElements: TpgElementList;
  protected
    constructor CreateInternal(AId: longword); override;
  public
    destructor Destroy; override;
    property Elements: TpgElementList read GetElements;
  end;

  // Document
  TpgDocument = class(TsdDebugComponent)
  private
    FElements: TpgElementList;
    FLastElementId: longword;
    FDefaultElement: TpgElement;
    procedure DoElementAdd(AElement: TpgElement);
    procedure DoElementExtract(AElement: TpgElement);
  protected
    property LastElementId: longword read FLastElementId write FLastElementId;
    property Elements: TpgElementList read FElements;
    procedure DoBeforeChange(AElementId, APropId: longword; AChange: TpgChangeType); virtual;
    procedure DoAfterChange(AElementId, APropId: longword; AChange: TpgChangeType); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; virtual;
    function NewElement(AElementClass: TpgElementClass): TpgElement;
    function ElementById(AId: longword): TpgElement;
  end;

  TpgPropInfo = class(TPersistent)
  private
    FId: longword;
    FMinElementClass: TpgElementClass;
    FFlags: TpgPropInfoFlags;
    FName: Utf8String;
    FDefaultValue: Utf8String;
    FPropClass: TpgPropClass;
  public
    function NewProp: TpgProp;
    property Id: longword read FId;
    property PropClass: TpgPropClass read FPropClass;
    property Name: Utf8String read FName;
    property MinElementClass: TpgElementClass read FMinElementClass;
    property Flags: TpgPropInfoFlags read FFlags;
    property DefaultValue: Utf8String read FDefaultValue;
  end;

  TpgElementInfo = class(TPersistent)
  private
    FId: longword;
    FName: Utf8String;
    FElementClass: TpgElementClass;
  public
    function New: TpgElement;
    property Id: longword read FId;
    property ElementClass: TpgElementClass read FElementClass;
    property Name: Utf8String read FName;
  end;

{ former pgProperties.pas }

  TpgStoredDocument = class(TpgDocument)
  private
    FId: longword;
    FUpdateCount: integer;
  protected
    procedure InternalAddElement(AElement: TpgElement);
    property UpdateCount: integer read FUpdateCount;
  public
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure Clear; override;
    procedure ReadFromStorage(AStorage: TpgStorage);
    procedure WriteToStorage(AStorage: TpgStorage);
    function ProduceDocumentId(ADocument: TpgDocument): longword;
    function ResolveDocumentId(AId: longword): TpgStoredDocument;
    property Id: longword read FId;
  end;

  TpgStorage = class(TPersistent)
  private
    FDocument: TpgStoredDocument;
    FStream: TStream;
    FFileVersion: integer;
    FOnWarning: TpgMessageEvent;
  protected
    property Stream: TStream read FStream;
    procedure DoWarning(const AMessage: Utf8String);
    procedure StartLoad; virtual;
    procedure CloseLoad; virtual;
    procedure StartSave; virtual;
    procedure CloseSave; virtual;
    function ReadMarker: TpgStorageMarker; virtual; abstract;
    function PeekMarker: TpgStorageMarker; virtual;
    procedure RewindMarker; virtual; abstract;
    procedure WriteMarker(AMarker: TpgStorageMarker); virtual; abstract;
    procedure ReadFileVersion; virtual;
    procedure WriteFileVersion; virtual;
    procedure ReadElementInfo(var AInfoId, AElementId: longword); virtual; abstract;
    procedure WriteElementInfo(AInfoId, AElementId: longword); virtual; abstract;
    procedure SkipElement; virtual;
    procedure SkipUntil(AMarker: TpgStorageMarker); virtual;
    procedure ReadPropInfo(var AInfoId: longword); virtual; abstract;
    procedure SkipProp; virtual;
    procedure WritePropInfo(AInfoId: longword); virtual; abstract;
    procedure ReadPropFields(AProp: TpgProp); virtual;
    procedure WritePropFields(AProp: TpgProp); virtual;
    // Informational
    function GetPositionInfo: Utf8String; virtual;
    function LastElementClassInfo: Utf8String; virtual;
    function LastPropClassInfo: Utf8String; virtual;
  public
    constructor Create(ADocument: TpgStoredDocument; AStream: TStream); virtual;
    function ReadBool: boolean; virtual; abstract;
    procedure WriteBool(const Value: boolean); virtual; abstract;
    function ReadInt: integer; virtual; abstract;
    procedure WriteInt(const Value: integer); virtual; abstract;
    function ReadFloat: double; virtual; abstract;
    procedure WriteFloat(const Value: double); virtual; abstract;
    function ReadString: Utf8String; virtual; abstract;
    procedure WriteString(const Value: Utf8String); virtual; abstract;
    function ReadIntList(var List: TDynIntArray): integer; virtual;
    procedure WriteIntList(const List: array of integer; Count: integer); virtual;
    function ReadFloatList(var List: TDynFloatArray): integer; virtual;
    procedure WriteFloatList(const List: array of double; Count: integer); virtual;
    function ReadBinary: RawByteString; virtual;
    procedure WriteBinary(const AData: RawByteString); virtual;
    procedure ReadProp(AElement: TpgElement);
    procedure WriteProp(AProp: TpgProp); virtual;
    procedure ReadElement(AParent: TpgElement);
    procedure WriteElement(AElement: TpgElement); virtual;
    function ProduceDocumentId(ADocument: TpgDocument): longword;
    function ResolveDocumentId(ADocumentId: longword): TpgStoredDocument;
    // FileVersion should be set in Create and should be increased every time
    // the property readers/writers change for a certain property
    property FileVersion: integer read FFileVersion;
    property OnWarning: TpgMessageEvent read FOnWarning write FOnWarning;
    // Get or set the Document for this storage. This is the stored Document, or
    // descendant, eg. a scene.
    property Document: TpgStoredDocument read FDocument write FDocument;
  end;

{ former pgStorage.pas }

  // A TpgStoredProp supports methods Read and Write to store its fields to
  // a TpgStorage class.
  TpgStoredProp = class(TpgProp)
  protected
    procedure Read(AStorage: TpgStorage); virtual;
    procedure Write(AStorage: TpgStorage); virtual;
    // Override this method if the property contains pointers to other props
    // or elements. It should turn the ID of the prop/element into a pointer, the
    // ID is stored in the pointer as longword after loading. See TpgRefProp
    // for an implementation example.
    procedure ResolveReferences(AStorage: TpgStorage); virtual;
  public
    procedure BeforeChange;
    procedure AfterChange;
  end;

  TpgBoolProp = class(TpgStoredProp)
  private
    FValue: boolean;
  protected
    function GetValue: boolean;
    procedure SetValue(const Value: boolean);
    procedure Read(AStorage: TpgStorage); override;
    procedure Write(AStorage: TpgStorage); override;
    procedure CopyFrom(AProp: TpgProp); override;
  public
    property Value: boolean read GetValue write SetValue;
  end;

  TpgIntProp = class(TpgStoredProp)
  private
    FValue: integer;
  protected
    function GetValue: integer;
    procedure SetValue(const Value: integer);
    procedure Read(AStorage: TpgStorage); override;
    procedure Write(AStorage: TpgStorage); override;
    procedure CopyFrom(AProp: TpgProp); override;
    function GetAsString: Utf8String; override;
    procedure SetAsString(const Value: Utf8String); override;
  public
    property Value: integer read GetValue write SetValue;
  end;

  TpgFloatProp = class(TpgStoredProp)
  private
    FValue: double;
  protected
    function GetValue: double;
    procedure SetValue(const Value: double);
    procedure Read(AStorage: TpgStorage); override;
    procedure Write(AStorage: TpgStorage); override;
    procedure CopyFrom(AProp: TpgProp); override;
    function GetAsString: Utf8String; override;
    procedure SetAsString(const Value: Utf8String); override;
  public
    property Value: double read GetValue write SetValue;
  end;

  TpgStringProp = class(TpgStoredProp)
  private
    FValue: Utf8String;
  protected
    function GetValue: Utf8String;
    procedure SetValue(const Value: Utf8String);
    function GetAsString: Utf8String; override;
    procedure SetAsString(const Value: Utf8String); override;
    procedure Read(AStorage: TpgStorage); override;
    procedure Write(AStorage: TpgStorage); override;
    procedure CopyFrom(AProp: TpgProp); override;
  public
    property Value: Utf8String read GetValue write SetValue;
  end;

  // TpgBinaryProp stores binary information in a string
  TpgBinaryProp = class(TpgStringProp)
  protected
    procedure Read(AStorage: TpgStorage); override;
    procedure Write(AStorage: TpgStorage); override;
  public
    procedure SetBinary(First: pointer; Count: integer);
  end;

  TpgRefProp = class(TpgStoredProp)
  private
    FReference: TpgElement;
    FDocument: TpgDocument;
    function GetReference: TpgElement;
    procedure SetReference(const Value: TpgElement);
  protected
    procedure DoSetReference(const Value: TpgElement); virtual;
    procedure Read(AStorage: TpgStorage); override;
    procedure Write(AStorage: TpgStorage); override;
    procedure CopyFrom(AProp: TpgProp); override;
    procedure ResolveReferences(AStorage: TpgStorage); override;
  public
    property Reference: TpgElement read GetReference write SetReference;
  end;

  TpgRefElement = class(TpgElement)
  private
    FRefCount: integer;
  protected
    procedure IncRef;
    procedure DecRef;
  end;

  TpgCountedRefProp = class(TpgRefProp)
  private
    FReferenceId: integer;
  protected
    procedure DoSetReference(const Value: TpgElement); override;
    procedure ResolveReferences(AStorage: TpgStorage); override;
  public
    destructor Destroy; override;
  end;

  TpgStyleProp = class(TpgRefProp)
  protected
    function GetReference: TpgStyle;
    procedure SetReference(const Value: TpgStyle);
  public
    property Reference: TpgStyle read GetReference write SetReference;
  end;

  // TpgStyleable is a TpgElement descendant that can be cloned from another
  // element, or contain reference to a style element
  TpgStyleable = class(TpgElement)
  private
    function GetClone: TpgRefProp;
    function GetStyle: TpgStyleProp;
    function GetName: TpgStringProp;
  protected
    function IntPropById(AId: longword): TpgIntProp;
    function FloatPropById(AId: longword): TpgFloatProp;
    function StringPropById(AId: longword): TpgStringProp;
    function RefPropById(AId: longword): TpgRefProp;
    function CheckReferenceProps(AId: longword; AInfo: TpgPropInfo; var AOwner: TpgElement): TpgProp; override;
  public
    property Clone: TpgRefProp read GetClone;
    property Style: TpgStyleProp read GetStyle;
    property Name: TpgStringProp read GetName;
  end;

  // TpgStyle is an element that can store style properties. These are properties
  // with property info flag pfStyle set.
  TpgStyle = class(TpgStyleable)
  protected
    function CheckPropertyClass(AInfo: TpgPropInfo): boolean; override;
  end;

function GetPropInfo(AId: longword): TpgPropInfo;
function GetPropInfoByName(const AName: Utf8String; AElementClass: TpgElementClass): TpgPropInfo;
function GetElementInfoById(AId: longword): TpgElementInfo;
function GetElementInfoByClass(AClass: TpgElementClass): TpgElementInfo;
function GetElementInfoByName(const AName: Utf8String): TpgElementInfo;

procedure RegisterElement(AId: longword; AElementClass: TpgElementClass; const AName: Utf8String);

procedure RegisterProp(AId: longword; APropClass: TpgPropClass; const AName: Utf8String;
  AMinElementClass: TpgElementClass; AFlags: TpgPropInfoFlags; const ADefault: Utf8String = '');


implementation

type

  TThreadInfo = packed record
    ThreadId: longword;
    Caller: TpgElement;
    Owner: TpgElement;
  end;
  PThreadInfo = ^TThreadInfo;

var

  glThreads: array of TThreadInfo = nil;

function GetThread: PThreadInfo;
var
  i, Count: integer;
  ThreadId: longword;
begin
  Count := length(glThreads);
  ThreadId := pgGetCurrentThreadId;
  for i := 0 to Count - 1 do
    if glThreads[i].ThreadId = ThreadId then
    begin
      Result := @glThreads[i];
      exit;
    end;

  // if still here, add
  SetLength(glThreads, Count + 1);
  Result := @glThreads[Count];
  Result.ThreadId := ThreadId;
  Result.Caller := nil;
  Result.Owner := nil;
end;

type

  TpgPropInfoList = class(TCustomSortedList)
  private
    function GetItems(Index: integer): TpgPropInfo;
  protected
    function DoCompare(Item1, Item2: TObject): integer; override;
  public
    class function NewDefault(AInfo: TpgPropInfo): TpgProp;
    function ById(AId: longword): TpgPropInfo;
    property Items[Index: integer]: TpgPropInfo read GetItems; default;
  end;

  TpgElementInfoList = class(TCustomSortedList)
  private
    function GetItems(Index: integer): TpgElementInfo;
  protected
    function DoCompare(Item1, Item2: TObject): integer; override;
  public
    function ById(AId: longword): TpgElementInfo;
    property Items[Index: integer]: TpgElementInfo read GetItems; default;
  end;

var

  glPropInfoList: TpgPropInfoList = nil;
  glElementInfoList: TpgElementInfoList = nil;

procedure RegisterElement(AId: longword; AElementClass: TpgElementClass; const AName: Utf8String);
var
  ElementInfo: TpgElementInfo;
begin
  if assigned(glElementInfoList.ById(AId)) then
    raise Exception.CreateFmt(sDuplicateElementRegistered, [AId]);
  ElementInfo := TpgElementInfo.Create;
  ElementInfo.FId := AId;
  ElementInfo.FElementClass := AElementClass;
  ElementInfo.FName := AName;
  glElementInfoList.Add(ElementInfo);
end;

procedure RegisterProp(AId: longword; APropClass: TpgPropClass; const AName: Utf8String;
  AMinElementClass: TpgElementClass; AFlags: TpgPropInfoFlags; const ADefault: Utf8String);
var
  PropInfo: TpgPropInfo;
begin
  if assigned(glPropInfoList.ById(AId)) then
    raise Exception.CreateFmt(sDuplicatePropertyRegistered, [AId]);
  PropInfo := TpgPropInfo.Create;
  PropInfo.FId := AId;
  PropInfo.FPropClass := APropClass;
  PropInfo.FName := AName;
  PropInfo.FMinElementClass := AMinElementClass;
  PropInfo.FFlags := AFlags;
  PropInfo.FDefaultValue := ADefault;
  glPropInfoList.Add(PropInfo);
end;

function GetPropInfo(AId: longword): TpgPropInfo;
begin
  Result := glPropInfoList.ById(AId)
end;

function GetPropInfoByName(const AName: Utf8String; AElementClass: TpgElementClass): TpgPropInfo;
var
  i: integer;
  Info: TpgPropInfo;
begin
  Result := nil;
  if not assigned(AElementClass) then
    exit;
  for i := 0 to glPropInfoList.Count - 1 do
  begin
    Info := glPropInfoList[i];
    if (Info.Name = AName) and AElementClass.InheritsFrom(Info.MinElementClass) then
    begin
      Result := Info;
      exit;
    end;
  end;
end;

function GetElementInfoById(AId: longword): TpgElementInfo;
begin
  Result := glElementInfoList.ById(AId)
end;

function GetElementInfoByClass(AClass: TpgElementClass): TpgElementInfo;
var
  i: integer;
  Info: TpgElementInfo;
begin
  for i := 0 to glElementInfoList.Count - 1 do
  begin
    Info := glElementInfoList[i];
    if Info.ElementClass = AClass then
    begin
      Result := Info;
      exit;
    end;
  end;
  Result := nil;
end;

function GetElementInfoByName(const AName: Utf8String): TpgElementInfo;
var
  i: integer;
  Info: TpgElementInfo;
begin
  for i := 0 to glElementInfoList.Count - 1 do
  begin
    Info := glElementInfoList[i];
    if Info.Name = AName then
    begin
      Result := Info;
      exit;
    end;
  end;
  Result := nil;
end;

{ TpgProp }

procedure TpgProp.Assign(ASource: TPersistent);
var
  Caller: TpgElement;
  MustWrite: boolean;
begin
  if ASource is TpgProp then
  begin
    GetCaller(Caller, MustWrite);
    DoBeforeChange(Caller);
    CallerProperty(Caller).CopyFrom(ASource as TpgProp);
    DoAfterChange(Caller);
  end else
    inherited;
end;

function TpgProp.CallerProperty(ACaller: TpgElement): TpgProp;
begin
  if FId = 0 then
  begin
    Result := Self;
    exit;
  end;
  Result := ACaller.FLocalProps.ById(FId);
  if not assigned(Result) then
  begin
    Result := TpgPropClass(ClassType).CreateInternal(FId);
    ACaller.FLocalProps.Add(Result);
  end;
end;

procedure TpgProp.CopyFrom(AProp: TpgProp);
begin
// default does nothing
end;

constructor TpgProp.CreateInternal(AId: longword);
begin
  inherited Create;
  FId := AId;
end;

procedure TpgProp.DoAfterChange(ACaller: TpgElement);
begin
  if assigned(ACaller) then
    ACaller.DoAfterPropChange(FId, ctPropUpdate);
end;

procedure TpgProp.DoBeforeChange(ACaller: TpgElement);
begin
  if assigned(ACaller) then
    ACaller.DoBeforePropChange(FId, ctPropUpdate);
end;

procedure TpgProp.DoSetAsString(const Value: Utf8String);
var
  Caller: TpgElement;
  MustWrite: boolean;
begin
  GetCaller(Caller, MustWrite);

  // Do we have to write?
  if (GetAsString <> Value) or MustWrite then
  begin
    DoBeforeChange(Caller);
    CallerProperty(Caller).SetAsString(Value);
    DoAfterChange(Caller);
  end;
end;

procedure TpgProp.Delete;
var
  Caller: TpgElement;
  MustWrite: boolean;
  List: TpgPropList;
  Index: integer;
begin
  GetCaller(Caller, MustWrite);

  // If we must write it means the property doesn't exist so we dont have to
  // do a thing.
  if MustWrite then
    exit;

  if assigned(Caller) then
  begin
    Caller.DoBeforePropChange(FId, ctPropRemove);
    List := Caller.LocalProps;
    if List.IndexById(FId, Index) then
      List.Delete(Index);
    Caller.DoAfterPropChange(FId, ctPropRemove);
  end;
end;

function TpgProp.ExistsLocal: boolean;
var
  Caller: TpgElement;
  MustWrite: boolean;
begin
  GetCaller(Caller, MustWrite);
  Result := not MustWrite;
end;

function TpgProp.GetAsString: Utf8String;
begin
  // Default is empty string
  Result := '';
end;

procedure TpgProp.GetCaller(var ACaller: TpgElement; var MustWrite: boolean);
var
  Thread: PThreadInfo;
begin
  if FId = 0 then
  begin
    ACaller := nil;
    MustWrite := False;
    exit;
  end;

  Thread := GetThread;
  ACaller := Thread.Caller;

  if not assigned(ACaller) then
    raise Exception.Create(sInternalThreadException);
  MustWrite := ACaller <> Thread.Owner;
end;

function TpgProp.GetDocument: TpgDocument;
var
  Caller: TpgElement;
  MustWrite: boolean;
begin
  GetCaller(Caller, MustWrite);
  if assigned(Caller) then
    Result := Caller.Document
  else
    Result := nil;
end;

procedure TpgProp.SetAsString(const Value: Utf8String);
begin
// default does nothing
end;

{ TpgPropList }

function TpgPropList.ById(AId: longword): TpgProp;
var
  Index: integer;
begin
  if IndexById(AId, Index) then
    Result := Items[Index]
  else
    Result := nil;
end;

function TpgPropList.DoCompare(Item1, Item2: TObject): integer;
begin
  Result := CompareInteger(TpgProp(Item1).FId, TpgProp(Item2).FId);
end;

function TpgPropList.GetItems(Index: integer): TpgProp;
begin
  if (Index >= 0) and (Index < Count) then
    Result := Get(Index)
  else
    Result := nil;
end;

function TpgPropList.IndexById(AId: longword; var AIndex: integer): boolean;
var
  AMin, AMax: integer;
begin
  // Shortcut for small Id's of often-used properties
  if AId <= 4 then
  begin
    AIndex := 0;
    while AIndex < Count do
    begin
      case CompareInteger(Items[AIndex].Id, AId) of
      0:
        begin
          Result := True;
          exit;
        end;
      1: break; // We missed it, so this is our index
      end;
      inc(AIndex);
    end;
    Result := False;
    exit;
  end;

  // Find position for insert - binary method
  AIndex := 0;
  AMin := 0;
  AMax := Count;
  while AMin < AMax do
  begin
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
  Result := False;
end;

{ TpgElement }

function TpgElement.CheckPropertyClass(AInfo: TpgPropInfo): boolean;
begin
  if not InheritsFrom(AInfo.MinElementClass) then
  begin
    DoDebugOut(Self, wsFail, Format(sNoSuchPropertyForClass, [ClassName]));
    Result := False;
    exit;
  end;
  // In other cases it is ok
  Result := True;
end;

function TpgElement.CheckPropLocations(AId: longword; var AInfo: TpgPropInfo;
  var AOwner: TpgElement): TpgProp;
begin
  if not assigned(Self) then
  begin
    //raise Exception.Create('"Self" not assigned!');
    Result := nil;
    exit;
  end;

  // Check our own list
  Result := FLocalProps.ById(AId);
  if assigned(Result) then
  begin
    AOwner := Self;
    exit;
  end;

  // Get the info now
  if not assigned(AInfo) then
  begin
    AInfo := glPropInfoList.ById(AId);
    if not assigned(AInfo) then
      raise Exception.CreateFmt(sUknownPropertyType, [AId]);
  end;

  // Check correct property class
  if not CheckPropertyClass(AInfo) then
    exit;

  // Reference?
  Result := CheckReferenceProps(AId, AInfo, AOwner);
  if assigned(Result) then
    exit;

  // Parent?
  if (pfInherit in AInfo.Flags) and assigned(FParent)
    and (FParent is AInfo.MinElementClass) then
    Result := FParent.CheckPropLocations(AId, AInfo, AOwner);
  if assigned(Result) then
    exit;

  // Last resort: default property
  if assigned(FDocument) then
  begin
    Result := FDocument.FDefaultElement.FLocalProps.ById(AId);
    if assigned(Result) then
      AOwner := FDocument.FDefaultElement;
  end;
end;

function TpgElement.CheckReferenceProps(AId: longword;
  AInfo: TpgPropInfo; var AOwner: TpgElement): TpgProp;
begin
  // This behaviour is implemented in TpgStyleable
  Result := nil;
end;

procedure TpgElement.Clear;
begin
  FLocalProps.Clear;
end;

procedure TpgElement.CopyFrom(AElement: TpgElement);
var
  i: integer;
  NewProp, RefProp: TpgProp;
begin
  FLocalProps.Clear;
  for i := 0 to AElement.LocalProps.Count - 1 do
  begin
    RefProp := AElement.LocalProps[i];
    NewProp := TpgPropClass(RefProp.ClassType).CreateInternal(RefProp.Id);
    NewProp.CopyFrom(RefProp);
    FLocalProps.Add(NewProp);
  end;
end;

constructor TpgElement.Create(AParent: TpgElement);
var
  Document: TpgDocument;
begin
  if assigned(AParent) then
    Document := AParent.Document
  else
    Document := nil;
  CreateInternal(Document, AParent);
  DoAfterCreate;
end;

constructor TpgElement.CreateCopyFrom(AElement: TpgElement; AParent: TpgElement);
begin
  if not assigned(AElement) or not InheritsFrom(AElement.ClassType) then
    raise Exception.Create(sWrongClassForCopyOperation);
  CreateInternal(AElement.Document, AParent);
  CopyFrom(AElement);
end;

constructor TpgElement.CreateInternal(ADocument: TpgDocument; AParent: TpgElement);
begin
  inherited Create;
  FDocument := ADocument;
  FParent := AParent;
  if assigned(FDocument) then
    FDocument.DoElementAdd(Self);
  if assigned(FParent) then
    FParent.DoElementAdd(Self);
  FLocalProps := TpgPropList.Create;
  FFlags := [efStored];
end;

destructor TpgElement.Destroy;
begin
  if assigned(FParent) then
    FParent.DoElementRemove(Self);
  if assigned(FDocument) then
    FDocument.DoElementExtract(Self);
  FreeAndNil(FLocalProps);
  inherited;
end;

procedure TpgElement.DoAfterChange(AElementId, APropId: longword; AChange: TpgChangeType);
begin
  if assigned(FDocument) then
    FDocument.DoAfterChange(AElementId, APropId, AChange);
end;

procedure TpgElement.DoAfterCreate;
begin
// default does nothing
end;

procedure TpgElement.DoAfterLoad;
begin
// default does nothing
end;

procedure TpgElement.DoAfterPropChange(APropId: longword; AChange: TpgChangeType);
begin
  DoAfterChange(Id, APropId, AChange)
end;

procedure TpgElement.DoBeforeChange(AElementId, APropId: longword; AChange: TpgChangeType);
begin
  if assigned(FDocument) then
    FDocument.DoBeforeChange(AElementId, APropId, AChange);
end;

procedure TpgElement.DoBeforePropChange(APropId: longword; AChange: TpgChangeType);
begin
  DoBeforeChange(Id, APropId, AChange)
end;

procedure TpgElement.DoBeforeSave;
begin
// default does nothing
end;

procedure TpgElement.DoDebugOut(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: Utf8String);
begin
  if assigned(Self) and (FDocument is TsdDebugComponent) then
    TsdDebugComponent(FDocument).DoDebugOut(Sender, WarnStyle, AMessage);
end;

procedure TpgElement.DoElementAdd(AElement: TpgElement);
begin
  DoBeforeChange(FId, AElement.FId, ctElementListAdd);
  LocalElementList.Add(AElement);
  DoAfterChange(FId, AElement.FId, ctElementListAdd);
end;

procedure TpgElement.DoElementRemove(AElement: TpgElement);
begin
  DoBeforeChange(FId, AElement.FId, ctElementListRemove);
  LocalElementList.Remove(AElement);
  DoAfterChange(FId, AElement.FId, ctElementListRemove);
end;

function TpgElement.GetElementCount: integer;
var
  Prop: TpgProp;
begin
  Result := 0;
  if efAllowElements in FFlags then
  begin
    // Check if we have the property
    Prop := LocalProps.ById(piElementList);
    if not assigned(Prop) then
      exit;
    Result := TpgElementListProp(Prop).Elements.Count;
  end;
end;

function TpgElement.GetElementIndex: integer;
var
  LIst: TpgElementList;
begin
  Result := 0;
  if not assigned(FParent) or (FParent.ElementCount = 0) then
    exit;
  List := Parent.GetLocalElementList;
  List.IndexById(FId, Result);
end;

function TpgElement.GetElements(Index: integer): TpgElement;
begin
  Result := GetLocalElementList[Index]
end;

function TpgElement.GetLocalElementList: TpgElementList;
var
  Prop: TpgProp;
begin
  if not (efAllowElements in FFlags) then
    raise Exception.Create(sSubElementsNotAllowed);

  Prop := FLocalProps.ById(piElementList);
  if not assigned(Prop) then
  begin
    Prop := TpgElementListProp.CreateInternal(piElementList);
    FLocalProps.Add(Prop);
  end;
  Result := TpgElementListProp(Prop).Elements;
end;

function TpgElement.PropById(AId: longword): TpgProp;
var
  Thread: PThreadInfo;
  AOwner: TpgElement;
  AInfo: TpgPropInfo;
begin
  if not assigned(Self) then
  begin
    //raise Exception.Create('"Self" not assigned!');
    Result := nil;
    exit;
  end;

  // Set us as the owner
  Thread := GetThread;
  Thread.Caller := Self;

  AInfo := nil;
  AOwner := nil;

  // Check if the property can be found somewhere
  Result := CheckPropLocations(AId, AInfo, AOwner);
  if assigned(Result) then
  begin
    // Found? we can stop
    Thread.Owner := AOwner;
    exit;
  end;

  // Info?
  if not assigned(AInfo) then
    raise Exception.CreateFmt(sUknownPropertyType, [AId]);

  // Last resort: create a default property in container
  if assigned(FDocument) then
  begin
    Result := glPropInfoList.NewDefault(AInfo);
    FDocument.FDefaultElement.FLocalProps.Add(Result);
    Thread.Owner := FDocument.FDefaultElement;
  end;
end;

procedure TpgElement.SetElementIndex(const Value: integer);
var
  OldIndex: integer;
  List: TpgElementList;
begin
  OldIndex := GetElementIndex;
  if OldIndex <> Value then
  begin
    if not assigned(FParent) then
      raise Exception.Create(sIllegalElementIndex);

    if Value >= FParent.ElementCount then
      raise Exception.Create(sIllegalElementIndex);

    List := FParent.GetLocalElementList;
    List.Exchange(OldIndex, Value);
  end;
end;

procedure TpgElement.SetId(AId: longword);
begin
  FId := AId;
end;

procedure TpgElement.SetDocument(ADocument: TpgDocument);
begin
  FDocument := ADocument;
end;

procedure TpgElement.SetParent(AParent: TpgElement);
begin
  if AParent <> FParent then
  begin
    if assigned(FParent) then
      FParent.DoElementRemove(Self);

    FParent := AParent;
    if assigned(FParent) then
      FParent.DoElementAdd(Self);
  end;
end;

{ TpgElementList }

function TpgElementList.ById(AId: longword): TpgElement;
var
  Index: integer;
begin
  if IndexById(AId, Index) then
    Result := Items[Index]
  else
    Result := nil;
end;

function TpgElementList.DoCompare(Item1, Item2: TObject): integer;
begin
  Result := CompareInteger(TpgElement(Item1).FId, TpgElement(Item2).FId);
end;

function TpgElementList.GetItems(Index: integer): TpgElement;
begin
  if (Index >= 0) and (Index < Count) then
    Result := Get(Index)
  else
    Result := nil;
end;

function TpgElementList.IndexById(AId: longword; var AIndex: integer): boolean;
var
  i, AMin, AMax: integer;
begin
  AIndex := 0;
  Result := False;
  if Sorted then
  begin
    // Find position for insert - binary method
    AMin := 0;
    AMax := Count;
    while AMin < AMax do
    begin
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
  end else
    // linear search
    for i := 0 to Count - 1 do
      if Items[i].FId = AId then
      begin
        AIndex := i;
        Result := True;
      end;
end;

procedure TpgElementList.Remove(AElement: TpgElement);
var
  Index: integer;
begin
  if not assigned(AElement) then
    exit;
  if IndexById(AElement.FId, Index) then
    Delete(Index);
end;

{ TpgElementListProp }

constructor TpgElementListProp.CreateInternal(AId: longword);
begin
  inherited CreateInternal(AId);
  FElements := TpgElementList.Create(False);
  FElements.Sorted := False;
end;

destructor TpgElementListProp.Destroy;
begin
  FreeAndNil(FElements);
  inherited;
end;

function TpgElementListProp.GetElements: TpgElementList;
begin
  Result := FElements;
end;

{ TpgDocument }

procedure TpgDocument.Clear;
var
  i: integer;
  AElement: TpgElement;
begin
  for i := 0 to FElements.Count - 1 do
  begin
    AElement := FElements[i];
    AElement.FParent := nil;
    AElement.FDocument := nil;
  end;
  FElements.Clear;
  FDefaultElement.Clear;
  FLastElementId := 0;
end;

constructor TpgDocument.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultElement := TpgElement.CreateInternal(nil, nil);
  FElements := TpgElementList.Create;
end;

destructor TpgDocument.Destroy;
var
  i: integer;
  AElement: TpgElement;
begin
  // This loop assures we don't go through a lengthy deallocation
  for i := 0 to FElements.Count - 1 do
  begin
    AElement := FElements[i];
    AElement.FParent := nil;
    AElement.FDocument := nil;
  end;
  FElements.Clear;
  FreeAndNil(FElements);
  FreeAndNil(FDefaultElement);
  inherited;
end;

procedure TpgDocument.DoAfterChange(AElementId, APropId: longword; AChange: TpgChangeType);
begin
// default does nothing
end;

procedure TpgDocument.DoBeforeChange(AElementId, APropId: longword; AChange: TpgChangeType);
begin
// default does nothing
end;

procedure TpgDocument.DoElementAdd(AElement: TpgElement);
var
  Index: integer;
  ParentId: longword;
begin
  inc(FLastElementId);
  AElement.FId := FLastElementId;
  if assigned(AElement.FParent) then
    ParentId := AElement.FParent.FId
  else
    ParentId := 0;
  DoBeforeChange(AElement.FId, ParentId, ctElementAdd);
  if FElements.Find(AElement, Index) then
    raise Exception.Create(sDuplicateElementAdded);
  FElements.Insert(Index, AElement);
  DoAfterChange(AElement.FId, ParentId, ctElementAdd);
end;

procedure TpgDocument.DoElementExtract(AElement: TpgElement);
var
  ParentId: longword;
begin
  if assigned(AElement.FParent) then
    ParentId := AElement.FParent.FId
  else
    ParentId := 0;
  DoBeforeChange(AElement.FId, ParentId, ctElementRemove);
  FElements.Extract(AElement);
  if FLastElementId = AElement.FId then
    dec(FLastElementId);
  DoAfterChange(AElement.FId, ParentId, ctElementRemove);
end;

function TpgDocument.ElementById(AId: longword): TpgElement;
begin
  Result := FElements.ById(AId);
end;

function TpgDocument.NewElement(AElementClass: TpgElementClass): TpgElement;
begin
  Result := AElementClass.CreateInternal(Self, nil);
end;

{ TpgPropInfo }

function TpgPropInfo.NewProp: TpgProp;
begin
  if assigned(FPropClass) then
  begin
    Result := FPropClass.CreateInternal(FId);
    if length(FDefaultValue) > 0 then
      Result.SetAsString(FDefaultValue);
  end else
    Result := nil;
end;

{ TpgPropInfoList }

function TpgPropInfoList.ById(AId: longword): TpgPropInfo;
var
  Index, AMin, AMax: integer;
begin
  // Find position for insert - binary method
  AMin := 0;
  AMax := Count;
  while AMin < AMax do
  begin
    Index := (AMin + AMax) div 2;
    case CompareInteger(Items[Index].FId, AId) of
    -1: AMin := Index + 1;
     0: begin
          Result := Items[Index];
          exit;
        end;
     1: AMax := Index;
    end;
  end;
  Result := nil;
end;

function TpgPropInfoList.DoCompare(Item1, Item2: TObject): integer;
begin
  Result := CompareInteger(TpgPropInfo(Item1).FId, TpgPropInfo(Item2).FId);
end;

function TpgPropInfoList.GetItems(Index: integer): TpgPropInfo;
begin
  if (Index >= 0) and (Index < Count) then
    Result := Get(Index)
  else
    Result := nil;
end;

class function TpgPropInfoList.NewDefault(AInfo: TpgPropInfo): TpgProp;
begin
  if assigned(AInfo) and assigned(AInfo.PropClass) then
  begin
    Result := AInfo.PropClass.CreateInternal(AInfo.Id);
    if length(AInfo.DefaultValue) > 0 then
      Result.SetAsString(AInfo.DefaultValue);
  end else
    Result := nil;
end;

{ TpgElementInfo }

function TpgElementInfo.New: TpgElement;
begin
  if assigned(FElementClass) then
    Result := FElementClass.CreateInternal(nil, nil)
  else
    Result := nil;
end;

{ TpgElementInfoList }

function TpgElementInfoList.ById(AId: longword): TpgElementInfo;
var
  Index, AMin, AMax: integer;
begin
  // Find position for insert - binary method
  AMin := 0;
  AMax := Count;
  while AMin < AMax do
  begin
    Index := (AMin + AMax) div 2;
    case CompareInteger(Items[Index].FId, AId) of
    -1: AMin := Index + 1;
     0: begin
          Result := Items[Index];
          exit;
        end;
     1: AMax := Index;
    end;
  end;
  Result := nil;
end;

function TpgElementInfoList.DoCompare(Item1, Item2: TObject): integer;
begin
  Result := CompareInteger(TpgElementInfo(Item1).FId, TpgElementInfo(Item2).FId);
end;

function TpgElementInfoList.GetItems(Index: integer): TpgElementInfo;
begin
  if (Index >= 0) and (Index < Count) then
    Result := Get(Index)
  else
    Result := nil;
end;

{ TpgStoredDocument }

procedure TpgStoredDocument.BeginUpdate;
begin
  if FUpdateCount = 0 then
    DoBeforeChange(0, 0, ctListUpdate);
  inc(FUpdateCount);
end;

procedure TpgStoredDocument.Clear;
begin
  DoBeforeChange(0, 0, ctListClear);
  inherited;
  DoAfterChange(0, 0, ctListClear);
end;

procedure TpgStoredDocument.EndUpdate;
begin
  dec(FUpdateCount);
  if FUpdateCount < 0 then
    raise Exception.Create(sUpdateBeginEndMismatch);
  if FUpdateCount = 0 then
    DoAfterChange(0, 0, ctListUpdate);
end;

procedure TpgStoredDocument.InternalAddElement(AElement: TpgElement);
begin
  Elements.AddUnique(AElement, True);
  if LastElementId < AElement.Id then
    LastElementId := AElement.Id;
end;

function TpgStoredDocument.ProduceDocumentId(ADocument: TpgDocument): longword;
begin
  if ADocument = Self then
    Result := 0
  else
    if ADocument is TpgStoredDocument then
      Result := TpgStoredDocument(ADocument).Id // Some unique id within the document
    else
      Result := 0;
end;

procedure TpgStoredDocument.ReadFromStorage(AStorage: TpgStorage);
var
  i, j: integer;
  Marker: TpgStorageMarker;
  Element: TpgElement;
  Prop: TpgProp;
begin
  Clear;
  if not assigned(AStorage) then
    exit;
  BeginUpdate;
  AStorage.StartLoad;
  try
    Marker := AStorage.ReadMarker;
    while Marker <> smEndOfFile do
    begin
      case Marker of
      smFileVersion:  AStorage.ReadFileVersion;
      smElementStart: AStorage.ReadElement(nil);
      else
        AStorage.DoWarning(Format(sUnexpectedMarker,
          [cMarkerNames[marker], AStorage.GetPositionInfo]));
      end;
      Marker := AStorage.ReadMarker;
    end;

    // Resolving
    for i := 0 to Elements.Count - 1 do
    begin
      Element := Elements[i];
      // Resolve any prop references in the element's props
      for j := 0 to Element.LocalProps.Count - 1 do
      begin
        Prop := Element.LocalProps[j];
        if Prop is TpgStoredProp then
          TpgStoredProp(Prop).ResolveReferences(AStorage);
      end;
    end;

  finally
    AStorage.CloseLoad;
    EndUpdate;
  end;
end;

function TpgStoredDocument.ResolveDocumentId(AId: longword): TpgStoredDocument;
begin
  Result := nil;
  if AId = 0 then
    Result := Self;
end;

procedure TpgStoredDocument.WriteToStorage(AStorage: TpgStorage);
var
  i: integer;
  Element: TpgElement;
begin
  if not assigned(AStorage) then
    exit;

  AStorage.StartSave;
  try

    for i := 0 to Elements.Count - 1 do
    begin
      Element := Elements[i];
      if (Element.Parent = nil) and (efStored in Element.Flags) then
        AStorage.WriteElement(Element);
    end;

    AStorage.WriteMarker(smEndOfFile);

  finally
    AStorage.CloseSave;
  end;
end;

{ TpgStorage }

procedure TpgStorage.CloseLoad;
begin
// default does nothing
end;

procedure TpgStorage.CloseSave;
begin
// default does nothing
end;

constructor TpgStorage.Create(ADocument: TpgStoredDocument; AStream: TStream);
begin
  inherited Create;
  FDocument := ADocument;
  FStream := AStream;
end;

procedure TpgStorage.DoWarning(const AMessage: Utf8String);
begin
  if assigned(FOnWarning) then
    FOnWarning(Self, AMessage);
end;

function TpgStorage.GetPositionInfo: Utf8String;
begin
  Result := IntToStr(Stream.Position);
end;

function TpgStorage.LastElementClassInfo: Utf8String;
begin
  Result := 'unknown';
end;

function TpgStorage.LastPropClassInfo: Utf8String;
begin
  Result := 'unknown';
end;

function TpgStorage.PeekMarker: TpgStorageMarker;
begin
  Result := ReadMarker;
  RewindMarker;
end;

function TpgStorage.ProduceDocumentId(ADocument: TpgDocument): longword;
begin
  Result := FDocument.ProduceDocumentId(ADocument);
end;

function TpgStorage.ReadBinary: RawByteString;
begin
  Result := RawByteString(ReadString);
end;

procedure TpgStorage.ReadElement(AParent: TpgElement);
var
  InfoId, ElementId: longword;
  ElementInfo: TpgElementInfo;
  Element: TpgElement;
  Marker: TpgStorageMarker;
begin
  // We already saw the ElementStart marker, we dont read it here
  ReadElementInfo(InfoId, ElementId);
  ElementInfo := GetElementInfoById(InfoId);

  // do we find the element's class?
  if not assigned(ElementInfo) then
  begin
    DoWarning(Format(sUnknownElement, [LastElementClassInfo, InfoId]));
    SkipElement;
    exit;
  end;

  // Create the element
  Element := ElementInfo.New;

  // Set Element Id
  Element.SetId(ElementId);

  // Add to our object list. This method makes sure that LastElementId of
  // the container is set correctly
  FDocument.InternalAddElement(Element);

  // Set Parent
  Element.SetParent(AParent);
  // Set Document
  Element.SetDocument(FDocument);

  // Call DoAfterCreate
  Element.DoAfterCreate;

  // Read rest of element
  Marker := ReadMarker;
  while Marker <> smElementClose do
  begin

    case Marker of
    smElementStart:
      // Load a sub element
      ReadElement(Element);
    smPropStart:
      ReadProp(Element);
    smPropClose:; // just continue
    else
      DoWarning(Format(sUnexpectedMarker,
        [cMarkerNames[marker], GetPositionInfo]));
      RewindMarker;
      SkipElement;
      break;
    end;

    Marker := ReadMarker;
  end;

  // call DoAfterLoad
  Element.DoAfterLoad;
end;

procedure TpgStorage.ReadFileVersion;
begin
// default does nothing
end;

function TpgStorage.ReadFloatList(var List: TDynFloatArray): integer;
var
  i, Count: integer;
begin
  Count := ReadInt;
  if length(List) < Count then
    SetLength(List, Count);
  for i := 0 to Count - 1 do
    List[i] := ReadFloat;
  Result := Count;
end;

function TpgStorage.ReadIntList(var List: TDynIntArray): integer;
var
  i, Count: integer;
begin
  Count := ReadInt;
  if length(List) < Count then
    SetLength(List, Count);
  for i := 0 to Count - 1 do
    List[i] := ReadInt;
  Result := Count;
end;

procedure TpgStorage.ReadProp(AElement: TpgElement);
var
  InfoId: longword;
  PropInfo: TpgPropInfo;
  Prop: TpgProp;
begin
  ReadPropInfo(InfoId);
  PropInfo := GetPropInfo(InfoId);

  // Do we find the property's class?
  if not assigned(PropInfo) then
  begin
    DoWarning(Format(sUnknownProp, [LastPropClassInfo, InfoId]));
    SkipProp;
    exit;
  end;

  // Create the property
  Prop := PropInfo.NewProp;

  // Read it from the storage
  ReadPropFields(Prop);

  // Add it; in case the property exists we simply overwrite it
  AElement.LocalProps.AddUnique(Prop, False);
end;

procedure TpgStorage.ReadPropFields(AProp: TpgProp);
begin
  // By default we use the Read method of the property
  if AProp is TpgStoredProp then
    TpgStoredProp(AProp).Read(Self);
end;

function TpgStorage.ResolveDocumentId(ADocumentId: longword): TpgStoredDocument;
begin
  Result := FDocument.ResolveDocumentId(ADocumentId);
end;

procedure TpgStorage.SkipElement;
begin
  SkipUntil(smElementClose);
end;

procedure TpgStorage.SkipProp;
begin
  SkipUntil(smPropClose);
end;

procedure TpgStorage.SkipUntil(AMarker: TpgStorageMarker);
var
  Level: integer;
  M: TpgStorageMarker;
  IL: TDynIntArray;
  FL: TDynFloatArray;
begin
  Level := 1;
  repeat
    M := PeekMarker;
    if M = AMarker then
    begin
      dec(Level);
      if Level = 0 then
      begin
        ReadMarker;
        exit;
      end;
    end;

    if (M = smElementStart) and (AMarker = smElementClose) then
      inc(Level);

    case M of
    smIntValue: ReadInt;
    smFloatValue: ReadFloat;
    smStringValue: ReadString;
    smIntListValue: ReadIntList(IL);
    smFloatListValue: ReadFloatList(FL);
    else
      ReadMarker;
    end;
  until M = smEndOfFile;
end;

procedure TpgStorage.StartLoad;
begin
  // default rewinds stream
  FStream.Position := 0;
end;

procedure TpgStorage.StartSave;
begin
  // default rewinds stream
  FStream.Position := 0;
  FStream.Size := 0;
end;

procedure TpgStorage.WriteBinary(const AData: RawByteString);
begin
  WriteString(Utf8String(AData));
end;

procedure TpgStorage.WriteElement(AElement: TpgElement);
var
  i: integer;
  ElementInfo: TpgElementInfo;
  Sub: TpgElement;
begin
  ElementInfo := GetElementInfoByClass(TpgElementClass(AElement.ClassType));
  if not assigned(ElementInfo) then
  begin
    DoWarning(Format(sUnregisteredElement, [AElement.ClassName]));
    exit;
  end;

  AElement.DoBeforeSave;

  WriteMarker(smElementStart);
  WriteElementInfo(ElementInfo.Id, AElement.Id);

  // Save properties
  for i := 0 to AElement.LocalProps.Count - 1 do
    WriteProp(AElement.LocalProps[i]);

  // Save subelements
  for i := 0 to AElement.ElementCount - 1 do
  begin
     Sub := AElement.Elements[i];
     if efStored in Sub.Flags then
       WriteElement(Sub);
  end;

  WriteMarker(smElementClose);
end;

procedure TpgStorage.WriteFileVersion;
begin
// default does nothing
end;

procedure TpgStorage.WriteFloatList(const List: array of double; Count: integer);
var
  i: integer;
begin
  WriteInt(Count);
  for i := 0 to Count - 1 do
    WriteFloat(List[i]);
end;

procedure TpgStorage.WriteIntList(const List: array of integer;
  Count: integer);
var
  i: integer;
begin
  WriteInt(Count);
  for i := 0 to Count - 1 do
    WriteInt(List[i]);
end;

procedure TpgStorage.WriteProp(AProp: TpgProp);
var
  PropInfo: TpgPropInfo;
begin
  PropInfo := GetPropInfo(AProp.Id);
  // Checks
  if not assigned(PropInfo) then
  begin
    DoWarning(Format(sUnregisteredProp, [AProp.ClassName]));
    exit;
  end;
  if not (pfStored in PropInfo.Flags) then
    exit;

  WriteMarker(smPropStart);
  WritePropInfo(AProp.Id);
  WritePropFields(AProp);
  WriteMarker(smPropClose);
end;

procedure TpgStorage.WritePropFields(AProp: TpgProp);
begin
  // By default we use the Write method of the property
  if AProp is TpgStoredProp then
    TpgStoredProp(AProp).Write(Self);
end;

{ TpgStoredProp }

procedure TpgStoredProp.AfterChange;
var
  Caller: TpgElement;
  MustWrite: boolean;
begin
  GetCaller(Caller, MustWrite);
  DoAfterChange(Caller);
end;

procedure TpgStoredProp.BeforeChange;
var
  Caller: TpgElement;
  MustWrite: boolean;
begin
  GetCaller(Caller, MustWrite);
  DoBeforeChange(Caller);
end;

procedure TpgStoredProp.Read(AStorage: TpgStorage);
begin
// default does nothing
end;

procedure TpgStoredProp.ResolveReferences(AStorage: TpgStorage);
begin
// default does nothing
end;

procedure TpgStoredProp.Write(AStorage: TpgStorage);
begin
// default does nothing
end;

{ TpgBoolProp }

procedure TpgBoolProp.CopyFrom(AProp: TpgProp);
begin
  inherited;
  if AProp is TpgBoolProp then
    FValue := TpgBoolProp(AProp).FValue;
end;

function TpgBoolProp.GetValue: boolean;
begin
  Result := FValue;
end;

procedure TpgBoolProp.Read(AStorage: TpgStorage);
begin
  FValue := AStorage.ReadBool;
end;

procedure TpgBoolProp.SetValue(const Value: boolean);
var
  Caller: TpgElement;
  MustWrite: boolean;
begin
  GetCaller(Caller, MustWrite);
  // Do we have to write?
  if (FValue <> Value) or MustWrite then
  begin
    DoBeforeChange(Caller);
    TpgBoolProp(CallerProperty(Caller)).FValue := Value;
    DoAfterChange(Caller);
  end;
end;

procedure TpgBoolProp.Write(AStorage: TpgStorage);
begin
  AStorage.WriteBool(FValue);
end;

{ TpgIntProp }

procedure TpgIntProp.CopyFrom(AProp: TpgProp);
begin
  inherited;
  if AProp is TpgIntProp then
    FValue := TpgIntProp(AProp).FValue;
end;

function TpgIntProp.GetAsString: Utf8String;
begin
  Result := IntToStr(FValue);
end;

function TpgIntProp.GetValue: integer;
begin
  Result := FValue;
end;

procedure TpgIntProp.Read(AStorage: TpgStorage);
begin
  FValue := AStorage.ReadInt;
end;

procedure TpgIntProp.SetAsString(const Value: Utf8String);
begin
  FValue := StrToIntDef(Value, 0);
end;

procedure TpgIntProp.SetValue(const Value: integer);
var
  Caller: TpgElement;
  MustWrite: boolean;
begin
  GetCaller(Caller, MustWrite);

  // Do we have to write?
  if (FValue <> Value) or MustWrite then
  begin
    DoBeforeChange(Caller);
    TpgIntProp(CallerProperty(Caller)).FValue := Value;
    DoAfterChange(Caller);
  end;
end;

procedure TpgIntProp.Write(AStorage: TpgStorage);
begin
  AStorage.WriteInt(FValue);
end;

{ TpgFloatProp }

procedure TpgFloatProp.CopyFrom(AProp: TpgProp);
begin
  inherited;
  if AProp is TpgFloatProp then
    FValue := TpgFloatProp(AProp).FValue;
end;

function TpgFloatProp.GetAsString: Utf8String;
begin
  Result := FloatToStr(FValue);
end;

function TpgFloatProp.GetValue: double;
begin
  Result := FValue;
end;

procedure TpgFloatProp.Read(AStorage: TpgStorage);
begin
  FValue := AStorage.ReadFloat;
end;

procedure TpgFloatProp.SetAsString(const Value: Utf8String);
begin
  FValue := StrToFloatDef(Value, 0);
end;

procedure TpgFloatProp.SetValue(const Value: double);
var
  Caller: TpgElement;
  MustWrite: boolean;
begin
  GetCaller(Caller, MustWrite);

  // Do we have to write?
  if (FValue <> Value) or MustWrite then
  begin
    DoBeforeChange(Caller);
    TpgFloatProp(CallerProperty(Caller)).FValue := Value;
    DoAfterChange(Caller);
  end;
end;

procedure TpgFloatProp.Write(AStorage: TpgStorage);
begin
  AStorage.WriteFloat(FValue);
end;

{ TpgStringProp }

procedure TpgStringProp.CopyFrom(AProp: TpgProp);
begin
  inherited;
  if AProp is TpgStringProp then
    FValue := TpgStringProp(AProp).FValue;
end;

function TpgStringProp.GetAsString: Utf8String;
begin
  Result := FValue;
end;

function TpgStringProp.GetValue: Utf8String;
begin
  Result := FValue;
end;

procedure TpgStringProp.Read(AStorage: TpgStorage);
begin
  FValue := AStorage.ReadString;
end;

procedure TpgStringProp.SetAsString(const Value: Utf8String);
begin
  FValue := Value;
end;

procedure TpgStringProp.SetValue(const Value: Utf8String);
var
  Caller: TpgElement;
  MustWrite: boolean;
begin
  GetCaller(Caller, MustWrite);

  // Do we have to write?
  if (FValue <> Value) or MustWrite then
  begin
    DoBeforeChange(Caller);
    TpgStringProp(CallerProperty(Caller)).FValue := Value;
    DoAfterChange(Caller);
  end;
end;

procedure TpgStringProp.Write(AStorage: TpgStorage);
begin
  AStorage.WriteString(FValue);
end;

{ TpgBinaryProp }

procedure TpgBinaryProp.Read(AStorage: TpgStorage);
begin
  FValue := AStorage.ReadBinary;
end;

procedure TpgBinaryProp.SetBinary(First: pointer; Count: integer);
var
  Caller: TpgElement;
  MustWrite: boolean;
  Prop: TpgBinaryProp;
begin
  GetCaller(Caller, MustWrite);

  DoBeforeChange(Caller);
  Prop := TpgBinaryProp(CallerProperty(Caller));

  SetLength(Prop.FValue, Count);
  if Count > 0 then
    Move(First^, Prop.FValue[1], Count);

  DoAfterChange(Caller);
end;

procedure TpgBinaryProp.Write(AStorage: TpgStorage);
begin
  AStorage.WriteBinary(FValue);
end;

{ TpgRefProp }

procedure TpgRefProp.CopyFrom(AProp: TpgProp);
begin
  inherited;
  if AProp is TpgRefProp then
    DoSetReference(TpgRefProp(AProp).FReference);
end;

procedure TpgRefProp.DoSetReference(const Value: TpgElement);
begin
  FReference := Value;
  if assigned(Value) then
    FDocument := Value.Document
  else
    FDocument := nil;
end;

function TpgRefProp.GetReference: TpgElement;
begin
  Result := FReference;
end;

procedure TpgRefProp.Read(AStorage: TpgStorage);
// These will be dereferenced later on
begin
  // this looks fishy - must read a pointer, not integer
  integer(FReference) := AStorage.ReadInt;
  integer(FDocument) := AStorage.ReadInt;
end;

procedure TpgRefProp.ResolveReferences(AStorage: TpgStorage);
begin
  // Get the owner container
  FDocument := AStorage.ResolveDocumentId(integer(FDocument));
  // Get the element pointer
  FReference := FDocument.ElementById(integer(FReference));
end;

procedure TpgRefProp.SetReference(const Value: TpgElement);
var
  Caller: TpgElement;
  MustWrite: boolean;
  RefProp: TpgRefProp;
begin
  GetCaller(Caller, MustWrite);
  if (FReference <> Value) or MustWrite then
  begin
    DoBeforeChange(Caller);
    RefProp := TpgRefProp(CallerProperty(Caller));
    RefProp.DoSetReference(Value);
    DoAfterChange(Caller);
  end;
end;

procedure TpgRefProp.Write(AStorage: TpgStorage);
var
  ElementId, DocumentId: integer;
begin
  // fishy too!
  ElementId := 0;
  if assigned(FReference) then
    ElementId := FReference.Id;
  DocumentId := AStorage.ProduceDocumentId(FDocument);
  AStorage.WriteInt(ElementId);
  AStorage.WriteInt(DocumentId);
end;

{ TpgCountedRefProp }

destructor TpgCountedRefProp.Destroy;
begin
  DoSetReference(nil);
  inherited;
end;

procedure TpgCountedRefProp.DoSetReference(const Value: TpgElement);
var
  Ref: TpgElement;
begin
  if FReference <> Value then
  begin
    // Dereference old
    if assigned(FDocument) and (FReferenceId > 0) then
    begin
      Ref := FDocument.ElementById(FReferenceId);
      if assigned(Ref) then
        TpgRefElement(FReference).DecRef;
    end;
    // Set new reference
    FReference := Value;
    if assigned(Value) then
    begin
      FDocument := Value.Document;
      FReferenceId := FReference.Id;
    end else
    begin
      FDocument := nil;
      FReferenceId := 0;
    end;
    // Reference new
    if assigned(FReference) then
      TpgRefElement(FReference).IncRef;
  end;
end;

procedure TpgCountedRefProp.ResolveReferences(AStorage: TpgStorage);
begin
  // Get the owner container
  // fishy
  FDocument := AStorage.ResolveDocumentId(integer(FDocument));

  // Get the element pointer
  FReferenceId := longword(FReference);

  FReference := FDocument.ElementById(FReferenceId);
  if assigned(FReference) then
    TpgRefElement(FReference).IncRef
end;

{ TpgStyleProp }

function TpgStyleProp.GetReference: TpgStyle;
begin
  Result := TpgStyle(inherited GetReference);
end;

procedure TpgStyleProp.SetReference(const Value: TpgStyle);
begin
  inherited SetReference(Value);
end;

{ TpgStyleable }

function TpgStyleable.CheckReferenceProps(AId: longword;
  AInfo: TpgPropInfo; var AOwner: TpgElement): TpgProp;
var
  CloneElement: TpgElement;
  StyleElement: TpgStyle;
  Prop: TpgProp;
begin
  CloneElement := nil;
  StyleElement := nil;
  Result := nil;

  // Do we have a referenced element?
  Prop := LocalProps.ById(piClone);
  if assigned(Prop) then
    CloneElement := TpgRefProp(Prop).Reference;

  // Do we have a style element?
  Prop := LocalProps.byId(piStyle);
  if assigned(Prop) then
    StyleElement := TpgStyleProp(Prop).Reference;

  // Check style
  if assigned(StyleElement) then
  begin
    Result := StyleElement.CheckPropLocations(AId, AInfo, AOwner);
    if assigned(Result) then
      exit;
  end;

  // Check clone
  if assigned(CloneElement) then
    // Correct class?
    if CloneElement.InheritsFrom(AInfo.MinElementClass) then
      // Let the referenced element check
      Result := CloneElement.CheckPropLocations(AId, AInfo, AOwner);
end;

function TpgStyleable.FloatPropById(AId: longword): TpgFloatProp;
begin
  Result := TpgFloatProp(PropById(AId));
end;

function TpgStyleable.GetClone: TpgRefProp;
begin
  Result := RefPropbyId(piClone);
end;

function TpgStyleable.GetName: TpgStringProp;
begin
  Result := StringPropById(piName);
end;

function TpgStyleable.GetStyle: TpgStyleProp;
begin
  Result := TpgStyleProp(PropbyId(piStyle));
end;

function TpgStyleable.IntPropById(AId: longword): TpgIntProp;
begin
  Result := TpgIntProp(PropById(AId));
end;

function TpgStyleable.RefPropById(AId: longword): TpgRefProp;
begin
  Result := TpgRefProp(PropById(AId));
end;

function TpgStyleable.StringPropById(AId: longword): TpgStringProp;
begin
  Result := TpgStringProp(PropById(AId));
end;

{ TpgStyle }

function TpgStyle.CheckPropertyClass(AInfo: TpgPropInfo): boolean;
begin
  Result := pfStyle in AInfo.Flags;
end;

{ TpgRefElement }

procedure TpgRefElement.DecRef;
begin
  dec(FRefCount);
  if FRefCount <= 0 then
  begin
    // We do not free the ref element as before, just clear it. Freeing it
    // gives problems with the container's clear method, as it doesn't expect
    // the element to be freed through DecRef when doing a free on the total list
    // This may have the side effect of reference elements hanging around, but
    // the scene should be able to check for these (using the RefCount)
    Clear;
  end;
end;

procedure TpgRefElement.IncRef;
begin
  inc(FRefCount);
end;

initialization

  glPropInfoList := TpgPropInfoList.Create;
  glElementInfoList := TpgElementInfoList.Create;

  RegisterElement(eiElement, TpgElement, 'Element');
  RegisterProp(piElementList, TpgElementListProp, 'Elements', TpgElement, []);

  RegisterElement(eiStyleable, TpgStyleable, 'Styleable');
  RegisterElement(eiStyle,     TpgStyle,     'Style');

  RegisterProp(piClone, TpgRefProp, 'Clone', TpgStyleable, [pfInherit, pfStyle, pfStored]);
  RegisterProp(piStyle, TpgStyleProp, 'Style', TpgStyleable, [pfStyle, pfStored]);
  RegisterProp(piName, TpgStringProp, 'Name', TpgStyleable, [pfStored]);

finalization

  SetLength(glThreads, 0);
  FreeAndNil(glElementInfoList);
  FreeAndNil(glPropInfoList);

end.
