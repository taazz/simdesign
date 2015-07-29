unit SHFileOp;
{
  Copyright © 1997 WinWright Consulting
  Written by Wayne Niddery

  You may use this code freely in any project, commercial included, as long
  as the this entire comment section, including copyright and credit, remains 
  intact. You may redistribute this code to others, and/or a compiled version
  thereof, as freeware only.
}

interface

uses Windows, ShellAPI, SysUtils, Classes;

type
  TSHFileOpCode = (foCopy, foMove, foRename, foDelete);

  TShellFileOp = class(TComponent)
  private
    FileOpStruct: TSHFileOpStruct;
    FHandle: THandle;
    FFilesOnly: boolean;
    FNoConfirmation: boolean;
    FAutoMakeDir: boolean;
    FUseRecycleBin: boolean;
    FRenameOnCollision: boolean;
    FAnimate: boolean;
    FProgressTitle: string;

    FileList: TStringList;
    FDestination: string;

    procedure BuildFlags;
    function BuildList(list: TStrings): pchar;
    procedure Cleanup;
    function Execute(Op: integer): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddTarget(const fname: string);
    procedure AddTargets(flist: TStrings);
    function CopyFiles(const dest: string): boolean;
    function MoveFiles(const dest: string): boolean;
    function RenameFiles(const dest: string): boolean;
    function DeleteFiles: boolean;
    function RecycleFiles: boolean;
    property ParentHandle: THandle read FHandle write FHandle;
  published
    // flags
    property FilesOnly: Boolean read FFilesOnly write FFilesOnly;
    property NoConfirmation: Boolean read FNoConfirmation write FNoConfirmation;
    property AutoMakeDir: Boolean read FAutoMakeDir write FAutoMakeDir;
    property RenameOnCollision: Boolean read FRenameOnCollision write FRenameOnCollision;
    property Animate: Boolean read FAnimate write FAnimate;
    property ProgressTitle: string read FProgressTitle write FProgressTitle;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('WinWright', [TShellFileOp]);
end;

constructor TShellFileOp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FileList := TStringList.Create;
  FileList.Sorted := True; FileList.Duplicates := dupIgnore;
end;

destructor TShellFileOp.Destroy;
begin
  FileList.Free;
  inherited Destroy;
end;

procedure TShellFileOp.BuildFlags;
begin
  with FileOpStruct do begin
    wnd := FHandle;
    fFlags := 0;
    if FFilesOnly then            fFlags := fFlags or FOF_FilesOnly;
    if FNoConfirmation then       fFlags := fFlags or FOF_NoConfirmation;
    if not FAnimate then          fFlags := fFlags or FOF_Silent;
    if FUseRecycleBin then        fFlags := fFlags or FOF_AllowUndo;

    if Length(FProgressTitle) > 0 then begin
      fFlags := fFlags or FOF_SimpleProgress;
      lpszProgressTitle := pchar(FProgressTitle);
    end;

    if wFunc <> FO_Delete then begin
      if FAutoMakeDir then        fFlags := fFlags or FOF_NoConfirmMkDir;
      if FRenameOnCollision then  fFlags := fFlags or FOF_RenameOnCollision;
    end;
    hNameMappings := nil;
  end;
end;

function TShellFileOp.BuildList(list: TStrings): pchar;
var i, len: integer;
    buffer, p: pchar;
begin
  buffer := nil;
  try
    // calculate needed buffer size
    len := 0;
    for i := 0 to pred(list.Count) do
      Inc(len, Length(list[i]) + 1);
    // get the memory
    Getmem(buffer, len + 1); p := buffer;
    // fill it
    for i := 0 to pred(list.Count) do begin
      len := Length(list[i]);
      MoveMemory(p, pchar(list[i]), len);
      Inc(p, len); p^ := #0; Inc(p);
    end;
    p^ := #0; // need double null at end!
  finally
    Result := buffer;
  end;
end;

function TShellFileOp.Execute(Op: integer): boolean;
begin
  if (FileList.Count > 0) then begin
    FileOpStruct.pFrom := BuildList(FileList);
    FileOpStruct.pTo   := pchar(FDestination);

    FileOpStruct.wFunc := Op;
    BuildFlags;

    SHFileOperation(FileOpStruct);

    Result := FileOpStruct.fAnyOperationsAborted;

    Cleanup;
  end else
    Result := False;
end;

procedure TShellFileOp.Cleanup;
begin
  Freemem(FileOpStruct.pFrom);
  FileList.Clear;
end;

function TShellFileOp.CopyFiles(const dest: string): boolean;
begin
  FDestination := dest;
  Result := Execute(FO_Copy);
end;

function TShellFileOp.MoveFiles(const dest: string): boolean;
begin
  FDestination := dest;
  Result := Execute(FO_Move);
end;

function TShellFileOp.RenameFiles(const dest: string): boolean;
begin
  FDestination := dest;
  Result := Execute(FO_Rename);
end;

function TShellFileOp.DeleteFiles: boolean;
begin
  FUseRecycleBin := False;
  Result := Execute(FO_Delete);
end;

function TShellFileOp.RecycleFiles: boolean;
begin
  FUseRecycleBin := True;
  Result := Execute(FO_Delete);
end;

procedure TShellFileOp.AddTarget(const fname: string);
begin
  FileList.Add(fname);
end;

procedure TShellFileOp.AddTargets(flist: TStrings);
begin
  FileList.AddStrings(flist);
end;

end.
