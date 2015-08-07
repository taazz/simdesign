{ unit sdPdfEncryption

  PDF document reader based on the open PDF specification
  5th edition, version 1.5

  This unit implements the objects as described in the PDF reference
  chapter 3.5: Encryption

  Author: Nils Haeck M.Sc.

  To do:
    - Encryption
    - decryption of strings (small issue here, mainly in other units)
    
  Changes:
    14Jan2004 - Created
    30Sep2004 - Added MD5/RC4 security handler (revision 2 and 3 compatible)

  copyright (c) 2004 - 2005 by Simdesign B.V.

}
unit sdPdfEncryption;

interface

uses
  Classes, SysUtils, sdPdfObjects, sdPdfUtil;

type

  // User actions (see table 3.20)
  TUserAction = (
    uaRead,                      // standard open and read
    uaPrintLowRes,               // print at low res (bit 3)
    uaModifyContents,            // bit 4
    uaCopyExtract,               // bit 5
    uaAddModifyAnnotFormFields,  // bit 6
    uaFillInFormFields,          // bit 9
    uaExtract,                   // bit 10
    uaAssemble,                  // bit 11
    uaPrintHiRes                 // bit 12
  );

  TPdfEncryptor = class;

  // Generic security handler (override to add your own type)
  TPdfSecurityHandler = class(TPersistent)
  private
    FEncryptor: TPdfEncryptor;
    FCanDecrypt: boolean;
    function GetDict: TPdfDictionary;
  public
    constructor Create(AEncryptor: TPdfEncryptor); virtual;
    procedure DecryptStream(GenNum, ObjNum: integer; S, D: TMemoryStream); virtual; abstract;
    function RequirePassword(Action: TUserAction): boolean;
    function CheckPassword(const Password: string; Action: TUserAction): boolean; virtual; abstract;
    function CanDecode: boolean; virtual; abstract;
    function GetAlgorithmName: string; virtual; abstract;
    function GetOEntry: string;
    function GetUEntry: string;
    function GetPEntry4byte: string;
    function Revision: integer;
    function GetFileID: string;
    property Dict: TPdfDictionary read GetDict;
  end;

  // Standard PDF security handler, can decode MD5/RC4, fixed and variable bitlength
  TPdfStandardSecurityHandler = class(TPdfSecurityHandler)
  private
    FKey: string;
  protected
    function GetAlgorithm: integer;
    function GetKeyByteLength: integer;
  public
    procedure DecryptStream(GenNum, ObjNum: integer; S, D: TMemoryStream); override;
    function CheckPassword(const Password: string; Action: TUserAction): boolean; override;
    function CanDecode: boolean; override;
    function GetAlgorithmName: string; override;
    function ComputeEncryptionKey(const Password: string): string;
    function ComputeUValue(const Password: string): string;
  end;

  TPdfEncryptor = class(TPersistent)
  private
    FDocument: TObject; // Pointer to parent TPdfDocument
    FDict: TPdfDictionary;
    FHandler: TPdfSecurityHandler;
    procedure SetDict(const Value: TPdfDictionary);
    function GetAlgorithmName: string; // pointer to the encryption dictionary
  public
    constructor Create(ADocument: TObject); virtual;
    destructor Destroy; override;
    procedure DecryptStream(GenNum, ObjNum: integer; S, D: TMemoryStream);
    function CanDecode: boolean;
    function CheckPassword(const Password: string; Action: TUserAction): boolean;
    function RequirePassword(Action: TUserAction): boolean;
    property Document: TObject read FDocument;
    property Dict: TPdfDictionary read FDict write SetDict;
    property AlgorithmName: string read GetAlgorithmName;
  end;

implementation

uses
  Dialogs,
  sdPdfDocument,
  DCPmd5, DCPrc4; // Cipher library for Delphi, hash.pas

const

  // padding string
  cPadString =
    #$28#$BF#$4E#$5E#$4E#$75#$8A#$41#$64#$00#$4E#$56#$FF#$FA#$01#$08 +
    #$2E#$2E#$00#$B6#$D0#$68#$3E#$80#$2F#$0C#$A9#$FE#$64#$53#$69#$7A;

{ TPdfEncryptor }

function TPdfEncryptor.CanDecode: boolean;
begin
  Result := False;
  if assigned(FHandler) then
    Result := FHandler.CanDecode;
end;

function TPdfEncryptor.CheckPassword(const Password: string; Action: TUserAction): boolean;
begin
  Result := False;
  if assigned(FHandler) then
    Result := FHandler.CheckPassword(Password, Action);
end;

constructor TPdfEncryptor.Create(ADocument: TObject);
begin
  inherited Create;
  FDocument := ADocument;
end;

procedure TPdfEncryptor.DecryptStream(GenNum, ObjNum: integer; S, D: TMemoryStream);
begin
  if assigned(FHandler) then
    FHandler.DecryptStream(GenNum, ObjNum, S, D)
  else
    raise EPdfError.CreateFmt(sPdfUnableToDecode, [AlgorithmName]);
end;

destructor TPdfEncryptor.Destroy;
begin
  FreeAndNil(FHandler);
  inherited;
end;

function TPdfEncryptor.GetAlgorithmName: string;
begin
  Result := 'Unknown';
  if assigned(FHandler) then
    Result := FHandler.GetAlgorithmName;
end;

function TPdfEncryptor.RequirePassword(Action: TUserAction): boolean;
begin
  Result := True;
  if assigned(FHandler) then
    Result := FHandler.RequirePassword(Action);
end;

procedure TPdfEncryptor.SetDict(const Value: TPdfDictionary);
var
  AFilter: string;
begin
  FDict := Value;
  if not assigned(FDict) then exit;
  // Find filter
  AFilter := FDict.StringByKey('Filter');
  if (AFilter = '') or (AFilter = 'Standard') then begin
    // Create the standard handler
    FHandler := TPdfStandardSecurityHandler.Create(Self);
  end else begin
    // to do: add additional security handlers for public key
  end;
end;

{ TPdfSecurityHandler }

constructor TPdfSecurityHandler.Create(AEncryptor: TPdfEncryptor);
begin
  inherited Create;
  FEncryptor := AEncryptor;
end;

function TPdfSecurityHandler.GetDict: TPdfDictionary;
begin
  Result := FEncryptor.Dict;
end;

function TPdfSecurityHandler.GetFileID: string;
var
  F: TPdfArray;
begin
  Result := '';
  F := TPdfDocument(FEncryptor.Document).FileID;
  if assigned(F) then
    Result := F.StringByIndex(0);
end;

function TPdfSecurityHandler.GetOEntry: string;
begin
  Result := Dict.StringByKeyDefault('O', '');
end;

function TPdfSecurityHandler.GetPEntry4byte: string;
var
  i: integer;
  P: cardinal;
begin
  SetLength(Result, 4);
  P := cardinal(Dict.IntegerByKeyDefault('P', 0));
  for i := 1 to 4 do begin
    Result[i] := chr(P and $FF);
    P := P shr 8;
  end;
end;

function TPdfSecurityHandler.GetUEntry: string;
begin
  Result := Dict.StringByKeyDefault('U', '');
end;

function TPdfSecurityHandler.RequirePassword(Action: TUserAction): boolean;
begin
  Result := not CheckPassword('', Action);
end;

function TPdfSecurityHandler.Revision: integer;
begin
  Result := Dict.IntegerByKeyDefault('R', 0);
end;

{ TPdfStandardSecurityHandler }

function TPdfStandardSecurityHandler.CanDecode: boolean;
begin
  CanDecode := GetAlgorithm in [1, 2];
end;

function HexToBinString(AHex: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to length(AHex) div 2 - 1 do
    Result := Result + chr(StrToInt('$' + copy(AHex, 1 + i * 2, 2)));
end;

function BinToHexString(ABin: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to length(ABin) do
    Result := Result + IntToHex(ord(ABin[i]), 2)
end;

function TPdfStandardSecurityHandler.CheckPassword(const Password: string;
  Action: TUserAction): boolean;
var
  UValue: string;
begin
  // Test with empty user password
  UValue := ComputeUValue(Password);

  // Compare with dict, if they equal the password is OK
  case Revision of
  2:  Result := UValue = GetUEntry;
  3:  Result := copy(Uvalue, 1, 16) = copy(GetUEntry, 1, 16);
  else
    Result := False;
  end;//case

  FCanDecrypt := Result;
end;

function TPdfStandardSecurityHandler.ComputeEncryptionKey(const Password: string): string;
var
  i: integer;
  Info, Pass, HashInput: string;
  Digest, Temp: string;
  MD5: TDCP_md5;
begin
  // Algorithm 3.2

  // Step 1: pad password
  Pass := copy(Password + cPadString, 1, 32);

  // Step 2: feed this to MD5 input
  HashInput := Pass;

  // Step 3: value of O entry, add to hash input
  Info := GetOEntry;
  if not length(Info) = 32 then
    raise EPdfError(sPdfInvalidEncEntry);
  HashInput := HashInput + Info;

  // Step 4: value of P entry, as 4byte, add to hash input
  HashInput := HashInput + GetPentry4byte;

  // Step 5:
  HashInput := HashInput + GetFileID;

  // Step 6: skipped - to do must check this
  {if Revision = 3 then
    HashInput := HashInput + #$FF#$FF#$FF#$FF;}

  // Step 7: Calculate digest
  MD5 := TDCP_md5.Create(nil);
  try
    MD5.Init;
    MD5.UpdateStr(HashInput);
    SetLength(Digest, 16);
    MD5.Final(Digest[1]);
  finally
    MD5.Free;
  end;

  // Step 8: rev 3 only
  if Revision = 3 then begin
    for i := 1 to 50 do begin
      Temp := Digest;
      MD5 := TDCP_md5.Create(nil);
      try
        MD5.Init;
        MD5.UpdateStr(Temp);
        MD5.Final(Digest[1]);
      finally
        MD5.Free;
      end;
    end;
  end;

  // Step 9: truncate to key length
  FKey := copy(Digest, 1, GetKeyByteLength);
end;

function TPdfStandardSecurityHandler.ComputeUValue(const Password: string): string;
var
  i, j: integer;
  RC4: TDCP_RC4;
  MD5: TDCP_md5;
  Digest: string;
  FXorKey: string;
begin
  case Revision of
  2: // Revision 2, algorithm 3.4
    begin
      // Step 1 Compute encryption password
      ComputeEncryptionKey(Password);
      // Step 2 Pass to RC4
      RC4 := TDCP_RC4.Create(nil);
      try
        SetLength(Result, length(cPadString));
        RC4.Init(FKey[1], length(FKey) * 8, nil);
        RC4.Encrypt(cPadString[1], Result[1], length(cPadString));
      finally
        RC4.Free;
      end;
    end;
  3: // Revision 3, algorithm 3.5
    begin

      // Step 1 Compute encryption password
      ComputeEncryptionKey(Password);
      MD5 := TDCP_md5.Create(nil);
      try
        MD5.Init;

        // Step 2, 3 pass padstring to MD5 and pass fileid
        MD5.UpdateStr(cPadString + GetFileID);

        SetLength(Digest, 16);
        MD5.Final(Digest[1]);
      finally
        MD5.Free;
      end;

      // Step 4: RC4 result from hash with encryption key
      RC4 := TDCP_RC4.Create(nil);
      try
        SetLength(Result, length(Digest));
        RC4.Init(FKey[1], length(FKey) * 8, nil);
        RC4.Encrypt(Digest[1], Result[1], length(Digest));
      finally
        RC4.Free;
      end;

      // Step 5: do 19 times this
      SetLength(FXorKey, length(FKey));
      for i := 1 to 19 do begin

        Digest := Result;
        RC4 := TDCP_RC4.Create(nil);
        try
          // XOR
          for j := 1 to length(FKey) do
            FXOrKey[j] := chr(ord(FKey[j]) xor i);
          // And run it
          RC4.Init(FXOrKey[1], length(FXOrKey) * 8, nil);
          RC4.Encrypt(Digest[1], Result[1], length(Digest));
        finally
          RC4.Free;
        end;

      end;
      // add 16 bytes of random padding
      for i := 1 to 16 do
        Result := Result + chr(random(256));
    end;
  else
    raise EPdfError.Create(sInvalidRevision);
  end;
end;

procedure TPdfStandardSecurityHandler.DecryptStream(GenNum, ObjNum: integer; S, D: TMemoryStream);
var
  i, ANum: integer;
  Digest, HashInput: string;
  MD5: TDCP_md5;
  RC4: TDCP_RC4;
begin
  if not FCanDecrypt then
    raise EPdfError.Create(sCannotDecryptContent);
  if not assigned(S) or not assigned(D) or (S.Size = 0) then exit;
  D.Size := S.Size;

  // Algorithm 3.1
  HashInput := FKey;

  // 3 low order bytes of object number
  ANum := ObjNum;
  for i := 1 to 3 do begin
    HashInput := HashInput + chr(ANum and $FF);
    ANum := ANum shr 8;
  end;

  // 2 low order bytes of generation number
  ANum := GenNum;
  for i := 1 to 2 do begin
    HashInput := HashInput + chr(ANum and $FF);
    ANum := ANum shr 8;
  end;

  // Step 7: Calculate digest
  MD5 := TDCP_md5.Create(nil);
  try
    MD5.Init;
    MD5.UpdateStr(HashInput);
    SetLength(Digest, 16);
    MD5.Final(Digest[1]);
  finally
    MD5.Free;
  end;

  // Use N + 5 bytes from Digest
  Digest := copy(Digest, 1, length(FKey) + 5);

  // Initialize RC4 decryption
  RC4 := TDCP_RC4.Create(nil);
  try
    RC4.Init(Digest[1], length(Digest) * 8, nil);
    // decrypt S to D
    RC4.Encrypt(S.Memory^, D.Memory^, S.Size);
  finally
    RC4.Free;
  end;

end;

function TPdfStandardSecurityHandler.GetAlgorithm: integer;
begin
  Result := Dict.IntegerByKeyDefault('V', -1);
end;

function TPdfStandardSecurityHandler.GetAlgorithmName: string;
begin
  case GetAlgorithm of
  0: Result := 'Undocumented Nr.0';
  1: Result := 'MD5/RC4 40 bit'; // Algorithm 3.1
  2: Result := 'MD5/RC4 variable bitlength';
  3: Result := 'US D.o.C.';
  4: Result := 'Custom Proprietary';
  else
    Result := 'Unknown';
  end;
end;

function TPdfStandardSecurityHandler.GetKeyByteLength: integer;
begin
  case GetAlgorithm of
  1: Result := 40 div 8;
  2: // Use the Length variable
    Result := Dict.IntegerByKeyDefault('Length', 40) div 8;
  else
    Result := 0;
  end;
end;

end.
