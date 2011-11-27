{ unit sdStreams64Platform

  Platform-dependent code for sdStreams

  currently only for M$ windows

  Date: 13nov2010
  Author: Nils Haeck
}
unit sdStreams64Platform;

{$ifdef lcl}{$MODE Delphi}{$endif}

interface

uses
  Windows, SysUtils;

{ aux functions }

function sdWin32Check(RetVal: boolean): boolean;
function sdGlobalAllocPtr(Bytes: Longint): Pointer;
function sdGlobalReAllocPtr(P: Pointer; Bytes: Longint): Pointer;
function sdSetEndOfFile(hFile: THandle): boolean;
function sdGlobalFreePtr(P: Pointer): THandle;
function sdMap_Create: LongWord;
function sdVirtualAlloc(lpvAddress: Pointer; dwSize: LongWord): Pointer;
function sdVirtualFree(lpAddress: Pointer; dwSize: LongWord): Boolean;


implementation

function sdWin32Check(RetVal: boolean): boolean;
begin
{$warnings off}
  Result := Win32Check(RetVal);
{$warnings on}
end;

function sdGlobalAllocPtr(Bytes: Longint): Pointer;
begin
{$ifdef lcl}
  Result := nil;
{$else}
{$warnings off}
  Result := GlobalAllocPtr(HeapAllocFlags, Bytes);
{$warnings on}
{$endif}
end;

function sdGlobalReAllocPtr(P: Pointer; Bytes: Longint): Pointer;
begin
{$ifdef lcl}
  Result := nil;
{$else}
{$warnings off}
  Result := GlobalReAllocPtr(P, Bytes, HeapAllocFlags);
{$warnings on}
{$endif}
end;

function sdSetEndOfFile(hFile: THandle): boolean;
begin
  Result := SetEndOfFile(hFile);
end;

function sdGlobalFreePtr(P: Pointer): THandle;
begin
{$ifdef lcl}
  Result := null;
{$else}
  Result := GlobalFreePtr(P);
{$endif}
end;

function sdMap_Create: LongWord;
var
  AInfo: TSystemInfo;
begin
  // Get system information with page size
  GetSystemInfo(AInfo);
  Result := AInfo.dwAllocationGranularity;
end;

function sdVirtualAlloc(lpvAddress: Pointer; dwSize: LongWord): Pointer;
begin
//    MEM_COMMIT,    // Directly "commit" the memory. aka make it ready to write/read to/from
//    PAGE_READWRITE // We want to read and write to the memory
  Result := VirtualAlloc(lpvAddress, dwSize, MEM_COMMIT, PAGE_READWRITE);
end;

function sdVirtualFree(lpAddress: Pointer; dwSize: DWORD): Boolean;
begin
// MEM_RELEASE
  Result := VirtualFree(lpAddress, dwSize, MEM_RELEASE);
end;

end.
