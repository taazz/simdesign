{ <b>Project</b>: Pyro<p>
  <b>Module</b>: Pyro Core<p>

  <b>Description:</b><p>
  CPU feature detection

  <b>Author</b>: Nils Haeck (n.haeck@simdesign.nl)<p>
  Copyright (c) 2006 SimDesign BV
}
unit pgCPUInfo;

interface

function HasMMX: Boolean;

procedure EMMS;

implementation

uses
  pgBlend;

type
  TCPUInstructionSet = (ciMMX, ciSSE, ciSSE2, ci3DNow, ci3DNowExt);

const

  CPUISChecks: Array[TCPUInstructionSet] of Cardinal =
    (  $800000, // ciMMX
      $2000000, // ciSSE
      $4000000, // ciSSE2
     $80000000, // ci3DNow
     $40000000  // c3DNowExt
     );

function CPUID_Available: Boolean;
asm
        MOV       EDX,False
        PUSHFD
        POP       EAX
        MOV       ECX,EAX
        XOR       EAX,$00200000
        PUSH      EAX
        POPFD
        PUSHFD
        POP       EAX
        XOR       ECX,EAX
        JZ        @1
        MOV       EDX,True
@1:     PUSH      EAX
        POPFD
        MOV       EAX,EDX
end;

function CPU_Signature: Integer;
asm
        PUSH    EBX
        MOV     EAX,1
        DW      $A20F   // CPUID
        POP     EBX
end;

function CPU_Features: Integer;
asm
        PUSH    EBX
        MOV     EAX,1
        DW      $A20F   // CPUID
        POP     EBX
        MOV     EAX,EDX
end;

function CPU_AMDExtensionsAvailable: Boolean;
asm
        PUSH    EBX
        MOV     @Result, True
        MOV     EAX, $80000000
        DW      $A20F   // CPUID
        CMP     EAX, $80000000
        JBE     @NOEXTENSION
        JMP     @EXIT
      @NOEXTENSION:
        MOV     @Result, False
      @EXIT:
        POP     EBX
end;

function CPU_AMDExtFeatures: Integer;
asm
        PUSH    EBX
        MOV     EAX, $80000001
        DW      $A20F   // CPUID
        POP     EBX
        MOV     EAX,EDX
end;

function HasInstructionSet(const InstructionSet: TCPUInstructionSet): Boolean;
begin
  Result := False;
  if not CPUID_Available then Exit;                   // no CPUID available
  if CPU_Signature shr 8 and $0F < 5 then Exit;       // not a Pentium class
  if (InstructionSet = ci3DNow) or
     (InstructionSet = ci3DNowExt) then
  begin
    if not CPU_AMDExtensionsAvailable or (CPU_AMDExtFeatures and CPUISChecks[InstructionSet] = 0) then
      Exit;
  end
  else
    if CPU_Features and CPUISChecks[InstructionSet] = 0 then
      Exit; // no MMX

  Result := True;
end;

procedure EMMS;
begin
  if glMMXActive then
  asm
    db $0F,$77               /// EMMS
  end;
end;

function HasMMX: Boolean;
begin
  Result := HasInstructionSet(ciMMX);
end;

end.
