unit sdTranslations;
{
  unit sdTranslations: create translated components
  copyright (c) 2001-2010, Nils Haeck (www.simdesign.nl)
  v1.0 dd 10sep2010

  this unit is used by:
  - unfold
}

interface

uses
  Classes, Contnrs, SysUtils, sdSortedLists, Forms, StdCtrls, ComCtrls, Menus,
  ExtCtrls, Dialogs, Controls, {Speedbar,} Buttons, Math, ActnList;

type

  TTranslateComponentEvent = procedure (Sender: TObject; AComponent: TComponent) of object;

  TTransItem = class
  private
    FOriginal: string;        // Original text
    FTranslated: string;      // Translated text
    FIsUntranslated: boolean; // This item is untranslated in the translation file
  public
    property Original: string read FOriginal write FOriginal;
    property Translated: string read FTranslated write FTranslated;
    property IsUntranslated: boolean read FIsUntranslated write FIsUntranslated;
  end;

  TTranslator = class(TComponent)
  private
    FTranslation: TSortedList;
    FFolder: string;
    FLanguage: string;
    FOnTranslateComponent: TTranslateComponentEvent;
    function CompareTrans(Item1, Item2: TObject; Info: pointer): integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // This procedure must be called before any translations can be made. AFolder
    // contains the foldername of translation files (including backslash), ALanguage
    // is the name as it appears in the "tr_<language>.txt" file
    procedure LoadLanguage(AFolder, ALanguage: string);
    // Translate a string
    function Translate(ALine: string): string;
    // Save all untranslated strings to a file "untr_<language>.txt"
    procedure SaveUntranslated;
    procedure TranslateComponent(AComponent: TComponent);
    // The current language string selected
    property Language: string read FLanguage;
    // Set this event to translate special component from within the application
    property OnTranslateComponent: TTranslateComponentEvent read FOnTranslateComponent write FOnTranslateComponent;
  end;

// Translate a line ALine. The translation process uses the loaded translation file.
// This routine is named crypictally "Trl" to keep it short, not to disturb the
// source code too much when added in many places
function Trl(const ALine: string): string;

// Use this procedure to get a valid list of translations
procedure FindAvailableTranslations(AFolder: string; AList: TStrings);

implementation

var
  // Translator component reference
  glTranslator: TTranslator = nil;

function Trl(const ALine: string): string;
begin
  if assigned(glTranslator) then
    Result := glTranslator.Translate(ALine)
  else
    Result := ALine;
end;

function Cr2Tilde(ALine: string): string;
// Convert #13#10 to ~
var
  APos: integer;
begin
  // #13#10
  APos := pos(#13#10, ALine);
  while APos > 0 do begin
    Delete(ALine, Apos, 2);
    Insert('~', ALine, APos);
    APos := pos(#13#10, ALine);
  end;
  // #13 alone qualifies too
  APos := pos(#13, ALine);
  while APos > 0 do begin
    ALine[Apos] := '~';
    APos := pos(#13, ALine);
  end;
  Result := ALine;
end;

function Tilde2Cr(ALine: string): string;
// Convert ~ to #13#10
var
  APos: integer;
begin
  APos := pos('~', ALine);
  while APos > 0 do begin
    Delete(ALine, Apos, 1);
    Insert(#13#10, ALine, APos);
    APos := pos('~', ALine);
  end;
  Result := ALine;
end;

procedure FindAvailableTranslations(AFolder: string; AList: TStrings);
var
  S: TSearchRec;
  Res: integer;
begin
  AList.Clear;
  AList.Add('Original');
  Res := FindFirst(AFolder  + 'tr_*.txt', faAnyfile, S);
  while Res = 0 do begin
    AList.Add(copy(S.Name, 4, Length(S.Name) - 7));
    Res := FindNext(S);
  end;
  FindClose(S);
end;

function TTranslator.Translate(ALine: string): string;
// Translate ALine
var
  Index: integer;
  AItem: TTransItem;
  ASearch: string;
begin
  Result := ALine;
  if length(ALine) = 0 then exit;

  ASearch := Cr2Tilde(ALine);

  // Build a comparison item
  AItem := TTransItem.Create;
  AItem.Original := ASearch;

  // Do we find a direct translation
  if FTranslation.Find(AItem, Index) then begin
    Result := Tilde2Cr(TTransItem(FTranslation[Index]).Translated);
    AItem.Free;
  end else begin
    // No match.. we will add the item with original translation, as untranslated
    AItem.Translated := AItem.Original;
    AItem.IsUntranslated := True;
    FTranslation.Insert(Index, AItem);
  end;

end;

procedure TTranslator.LoadLanguage(AFolder, ALanguage: string);
// Find the file for ALanguage and parse it into our translation list
var
  i, Index: integer;
  AList: TStringList;
  AFileName: string;
  AItem: TTransItem;
begin
  FTranslation.Clear;
  FFolder := AFolder;
  FLanguage := ALanguage;

  try
    AList := TStringList.Create;
    try

      // Load the translation
      AFileName := Format('%str_%s.txt', [AFolder, ALanguage]);
      if FileExists(AFileName) then begin
        AList.LoadFromFile(AFileName);

        // Skip the lines that are empty or contain comments
        i := 0;
        while i < AList.Count do
          if (length(AList[i]) = 0) or (AList[i][1] = '#') then
            AList.Delete(i)
          else
            inc(i);

        // Add all the translations
        i := 0;
        while i < AList.Count do
          if AList[i][1] = '''' then begin
            // the ' signals an original string
            inc(i);
            if (i < AList.Count) and (AList[i][1] <> '''') then begin
              // Not directly another ' so this must be the translation
              AItem := TTransItem.Create;
              with AItem do begin
                Original   := Copy(AList[i - 1], 2, Length(AList[i - 1]));
                Translated := AList[i];
              end;
              // This inserts the item in correct position
              if not FTranslation.Find(AItem, Index) then begin
                FTranslation.Insert(Index, AItem)
              end else
                // Already exists..
                AItem.Free;
              inc(i);
            end;
          end else
            // Unrecognised string.. skip
            inc(i);
      end;

      // Load the untranslated file
      AFileName := Format('%suntr_%s.txt', [AFolder, ALanguage]);
      if FileExists(AFileName) then begin
        AList.LoadFromFile(AFileName);

        // Add all these items
        i := 0;
        while i < AList.Count do begin
          if AList[i][1] = '''' then begin
            // the ' signals an original string
            AItem := TTransItem.Create;
            with AItem do begin
              Original  := Copy(AList[i], 2, Length(AList[i]));
              inc(i);
              if i < AList.Count then
                Translated := AList[i];
              IsUntranslated := True;
            end;
            // We will add this item if it does not exist, in order not to forget
            // in next save of the untranslated items
            if not FTranslation.Find(AItem, Index) then
              FTranslation.Insert(Index, AItem)
            else
              AItem.Free;
          end;
          // Next line
          inc(i);
        end;
      end;

    finally
      AList.Free;
    end;
  except
    // some exception, make it silent
  end;
end;

procedure TTranslator.SaveUntranslated;
var
  i: integer;
  AList: TStringList;
begin
  // Find the untranslated items and put them in this file
  AList := TStringList.Create;
  try
    for i := 0 to FTranslation.Count - 1 do with TTransItem(FTranslation[i]) do
      if IsUntranslated then begin
        AList.Add('''' + Original);
        AList.Add(Translated);
      end;
    // Save
    if AList.Count > 0 then
      AList.SaveToFile(Format('%suntr_%s.txt', [FFolder, FLanguage]))
  finally
    AList.Free;
  end;
end;

procedure TTranslator.TranslateComponent(AComponent: TComponent);
var
  i, j: integer;
begin
  for i := 0 to AComponent.ComponentCount - 1 do
    // Recursive call
    TranslateComponent(AComponent.Components[i]);

  // Escape mechanism: do not translate components with tag = 999
  if AComponent.Tag = 999 then exit;

  // Translate properties
  if AComponent is TForm then
    TForm(AComponent).Caption := Translate(TForm(AComponent).Caption);

  if AComponent is TLabel then
    TLabel(AComponent).Caption := Translate(TLabel(AComponent).Caption);

  if AComponent is TButton then
    TButton(AComponent).Caption := Translate(TButton(AComponent).Caption);

  if AComponent is TCheckBox then
    TCheckbox(AComponent).Caption := Translate(TCheckBox(AComponent).Caption);

  if AComponent is TRadioButton then
    TRadioButton(AComponent).Caption := Translate(TRadioButton(AComponent).Caption);

  if AComponent is TTabSheet then
    TTabSheet(AComponent).Caption := Translate(TTabSheet(AComponent).Caption);

  if AComponent is TGroupBox then
    TGroupBox(AComponent).Caption := Translate(TGroupBox(AComponent).Caption);

  if AComponent is TMenuItem then
    TMenuItem(AComponent).Caption := Translate(TMenuItem(AComponent).Caption);

//  if AComponent is TSpeedItem then
//    TSpeedItem(AComponent).Hint:=Translate(TSpeedItem(AComponent).Hint);

  if AComponent is TSpeedButton then
    TSpeedButton(AComponent).Caption := Translate(TSpeedButton(AComponent).Caption);

  if AComponent is TEdit then
    TEdit(AComponent).Text:=Translate(TEdit(AComponent).Text);

  if AComponent is TComboBox then
    for j := 0 to TComboBox(AComponent).Items.Count-1 do
      TComboBox(AComponent).Items[j]:= Translate(TComboBox(AComponent).Items[j]);

  if AComponent is TRadioGroup then begin
    TRadioGroup(AComponent).Caption := Translate(TRadioGroup(AComponent).Caption);
    for j:=0 to TRadioGroup(AComponent).Items.Count-1 do
      TRadioGroup(AComponent).Items[j]:= Translate(TRadioGroup(AComponent).Items[j]);
  end;

  if AComponent is TOpenDialog then begin
    TOpenDialog(AComponent).Title:=Translate(TOpenDialog(AComponent).Title);
    TOpenDialog(AComponent).Filter:=Translate(TOpenDialog(AComponent).Filter);
  end;

  if AComponent is TSaveDialog then begin
    TSaveDialog(AComponent).Title:=Translate(TSaveDialog(AComponent).Title);
    TSaveDialog(AComponent).Filter:=Translate(TSaveDialog(AComponent).Filter);
  end;

  if AComponent is TToolButton then with TToolButton(AComponent) do begin
    Caption := Translate(Caption);
    Hint := Translate(Hint);
  end;

  if AComponent is TListview then with AComponent as TListview do begin
    for j:=0 to Columns.Count-1 do
      Columns[j].Caption := Translate(Columns[j].Caption);
  end;

  if AComponent is TStatusBar then with AComponent as TStatusBar do begin
    for j:=0 to Panels.Count-1 do
      Panels[j].Text := Translate(Panels[j].Text);
  end;

  if assigned(FOnTranslateComponent) then
    FOnTranslateComponent(Self, AComponent)

end;

{ TTranslator }

function TTranslator.CompareTrans(Item1, Item2: TObject; Info: pointer): integer;
// Compare two TTransItems and decide which one comes first or if they're equal
var
  S1, S2: string;
begin
  // Use case sensitivity
  Result := 0;
  if assigned(Item1) and assigned(Item2) then begin
    S1 := TTransItem(Item1).Original;
    S2 := TTransItem(Item2).Original;
    if S1 < S2 then
      Result := -1
    else if S1 = S2 then
      Result := 0
    else
      Result := 1;
  end;
end;

constructor TTranslator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTranslation := TSortedList.Create;
  FTranslation.OnCompare := CompareTrans;
  glTranslator := Self;
end;

destructor TTranslator.Destroy;
begin
  FreeAndNil(FTranslation);
  glTranslator := nil;
  inherited;
end;

end.
