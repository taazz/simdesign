//
// Heavily based on Delphi Free Stuff Include File
//                  by Brad Stowers: bstowers@pobox.com
//                  Delphi Free Stuff: http://www.delphifreestuff.com/
//
// 08/03/06 - ur - added Delphi 2006 detection
// 08/07/04 - LR - BCB5 and BCB6 detection
// 30/08/02 - EG - Delphi 7 detection
// 24/01/02 - EG - Linux/Kylix changes (Puthoon)
// 28/12/01 - EG - Added GLS_Graphics32_SUPPORT
// 27/07/01 - EG - Renamed and made GLScene-specific
// 26/06/01 - EG - Added Delphi6 stuff
//

{$DEFINE GLSCENE}

//
// Additionnal support & dependencies
//

// Activate support for Alex Denissov's Graphics32 library (http://www.g32.org)
{.$DEFINE GLS_Graphics32_SUPPORT}

// De-Activate assembly routine
{.$DEFINE GEOMETRY_NO_ASM}

//
// Platform Detection
//

{$IFDEF WIN32}
  {$DEFINE GLS_WIN32}
  {$DEFINE MSWINDOWS}
{$ELSE}
  {$IFNDEF LINUX}
    {$DEFINE GLS_WIN16}
    {$DEFINE MSWINDOWS}
  {$ENDIF LINUX}
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$DEFINE GLS_VCL}
{$ELSE}
  {$DEFINE GLS_CLX}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE GEOMETRY_NO_ASM}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE GEOMETRY_NO_ASM}
  {$TYPEINFO ON}
  {$INLINE ON}
{$ENDIF}

{$IFDEF CLR}
  {$DEFINE GEOMETRY_NO_ASM}
  {$UNSAFECODE ON}
{$ENDIF}

{$ifdef VER180}
  {$DEFINE GLS_COMPILER_10}
  {$DEFINE GLS_DELPHI_10}
  {$DEFINE GLS_DELPHI}       
{$endif}

{$ifdef VER170}
  {$DEFINE GLS_COMPILER_9}
  {$DEFINE GLS_DELPHI_9}
  {$DEFINE GLS_DELPHI}       
{$endif}

{$ifdef VER160}
  {$DEFINE GLS_COMPILER_8}
  {$DEFINE GLS_DELPHI_8}
  {$DEFINE GLS_DELPHI}       
{$endif}

{$ifdef VER150}
  {$DEFINE GLS_COMPILER_7}
  {$DEFINE GLS_DELPHI}
  {$DEFINE GLS_DELPHI_7}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$endif}

{$ifdef VER140}
  {$DEFINE GLS_COMPILER_6}
  {$DEFINE GLS_DELPHI_6}
  {$IFDEF BCB}
    {$DEFINE GLS_CPPB}
    {$DEFINE GLS_CPPB_6}
  {$ELSE}      
    {$DEFINE GLS_DELPHI}       
  {$ENDIF}
{$endif}

{$IFDEF VER130}
  {$DEFINE GLS_COMPILER_5} 
  {$DEFINE GLS_DELPHI_5}
  {$IFDEF BCB}
    {$DEFINE GLS_CPPB}
    {$DEFINE GLS_CPPB_5}
  {$ELSE}      
    {$DEFINE GLS_DELPHI}       
  {$ENDIF}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE GLS_COMPILER_4}
  {$DEFINE GLS_CPPB}
  {$DEFINE GLS_CPPB_4}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE GLS_COMPILER_4}
  {$DEFINE GLS_DELPHI}
  {$DEFINE GLS_DELPHI_4}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE GLS_COMPILER_3}
  {$DEFINE GLS_CPPB}
  {$DEFINE GLS_CPPB_3}
{$ENDIF}

{$IFDEF VER100}
  {$DEFINE GLS_COMPILER_3}
  {$DEFINE GLS_DELPHI}
  {$DEFINE GLS_DELPHI_3}
{$ENDIF}

{$IFDEF VER93}
  {$DEFINE GLS_COMPILER_2}  { C++B v1 compiler is really v2 }
  {$DEFINE GLS_CPPB}
  {$DEFINE GLS_CPPB_1}
{$ENDIF}

{$IFDEF VER90}
  {$DEFINE GLS_COMPILER_2}
  {$DEFINE GLS_DELPHI}
  {$DEFINE GLS_DELPHI_2}
{$ENDIF}

{$IFDEF VER80}
  {$DEFINE GLS_COMPILER_1}
  {$DEFINE GLS_DELPHI}
  {$DEFINE GLS_DELPHI_1}
{$ENDIF}

{$IFNDEF GLS_CPPB}
  {$IFNDEF GLS_DELPHI}
    { Don't know what the hell it is.  Could be new version, or could be old BP. }
    {$DEFINE GLS_UNKNOWN_COMPILER}
  {$ENDIF}
{$ENDIF}

{$IFDEF GLS_COMPILER_1}
  {$DEFINE GLS_COMPILER_1_UP}
{$ENDIF}

{$IFDEF GLS_COMPILER_2}
  {$DEFINE GLS_COMPILER_1_UP}
  {$DEFINE GLS_COMPILER_2_UP}
{$ENDIF}

{$IFDEF GLS_COMPILER_3}
  {$DEFINE GLS_COMPILER_1_UP}
  {$DEFINE GLS_COMPILER_2_UP}
  {$DEFINE GLS_COMPILER_3_UP}
{$ENDIF}

{$IFDEF GLS_COMPILER_4}
  {$DEFINE GLS_COMPILER_1_UP}
  {$DEFINE GLS_COMPILER_2_UP}
  {$DEFINE GLS_COMPILER_3_UP}
  {$DEFINE GLS_COMPILER_4_UP}
{$ENDIF}

{$IFDEF GLS_COMPILER_5}
  {$DEFINE GLS_COMPILER_1_UP}
  {$DEFINE GLS_COMPILER_2_UP}
  {$DEFINE GLS_COMPILER_3_UP}
  {$DEFINE GLS_COMPILER_4_UP}
  {$DEFINE GLS_COMPILER_5_UP}
{$ENDIF}

{$IFDEF GLS_COMPILER_6}
  {$DEFINE GLS_COMPILER_1_UP}
  {$DEFINE GLS_COMPILER_2_UP}
  {$DEFINE GLS_COMPILER_3_UP}
  {$DEFINE GLS_COMPILER_4_UP}
  {$DEFINE GLS_COMPILER_5_UP}
  {$DEFINE GLS_COMPILER_6_UP}
{$ENDIF}

{$IFDEF GLS_COMPILER_7}
  {$DEFINE GLS_COMPILER_1_UP}
  {$DEFINE GLS_COMPILER_2_UP}
  {$DEFINE GLS_COMPILER_3_UP}
  {$DEFINE GLS_COMPILER_4_UP}
  {$DEFINE GLS_COMPILER_5_UP}
  {$DEFINE GLS_COMPILER_6_UP}
  {$DEFINE GLS_COMPILER_7_UP}
{$ENDIF}

{$IFDEF GLS_COMPILER_8}
  {$DEFINE GLS_COMPILER_1_UP}
  {$DEFINE GLS_COMPILER_2_UP}
  {$DEFINE GLS_COMPILER_3_UP}
  {$DEFINE GLS_COMPILER_4_UP}
  {$DEFINE GLS_COMPILER_5_UP}
  {$DEFINE GLS_COMPILER_6_UP}
  {$DEFINE GLS_COMPILER_7_UP}
  {$DEFINE GLS_COMPILER_8_UP}
{$ENDIF}

{$IFDEF GLS_COMPILER_9}
  {$DEFINE GLS_COMPILER_1_UP}
  {$DEFINE GLS_COMPILER_2_UP}
  {$DEFINE GLS_COMPILER_3_UP}
  {$DEFINE GLS_COMPILER_4_UP}
  {$DEFINE GLS_COMPILER_5_UP}
  {$DEFINE GLS_COMPILER_6_UP}
  {$DEFINE GLS_COMPILER_7_UP}
  {$DEFINE GLS_COMPILER_8_UP}
  {$DEFINE GLS_COMPILER_9_UP}
{$ENDIF}

{$IFDEF GLS_COMPILER_10}
  {$DEFINE GLS_COMPILER_1_UP}
  {$DEFINE GLS_COMPILER_2_UP}
  {$DEFINE GLS_COMPILER_3_UP}
  {$DEFINE GLS_COMPILER_4_UP}
  {$DEFINE GLS_COMPILER_5_UP}
  {$DEFINE GLS_COMPILER_6_UP}
  {$DEFINE GLS_COMPILER_7_UP}
  {$DEFINE GLS_COMPILER_8_UP}
  {$DEFINE GLS_COMPILER_9_UP}
  {$DEFINE GLS_COMPILER_10_UP}
{$ENDIF}

{$IFDEF GLS_DELPHI_2}
  {$DEFINE GLS_DELPHI_2_UP}
{$ENDIF}

{$IFDEF GLS_DELPHI_3}
  {$DEFINE GLS_DELPHI_2_UP}
  {$DEFINE GLS_DELPHI_3_UP}
{$ENDIF}

{$IFDEF GLS_DELPHI_4}
  {$DEFINE GLS_DELPHI_2_UP}
  {$DEFINE GLS_DELPHI_3_UP}
  {$DEFINE GLS_DELPHI_4_UP}
{$ENDIF}

{$IFDEF GLS_DELPHI_5}
  {$DEFINE GLS_DELPHI_2_UP}
  {$DEFINE GLS_DELPHI_3_UP}
  {$DEFINE GLS_DELPHI_4_UP}
  {$DEFINE GLS_DELPHI_5_UP}
{$ENDIF}

{$IFDEF GLS_DELPHI_6}
  {$DEFINE GLS_DELPHI_2_UP}
  {$DEFINE GLS_DELPHI_3_UP}
  {$DEFINE GLS_DELPHI_4_UP}
  {$DEFINE GLS_DELPHI_5_UP}
  {$DEFINE GLS_DELPHI_6_UP}
{$ENDIF}

{$IFDEF GLS_DELPHI_7}
  {$DEFINE GLS_DELPHI_2_UP}
  {$DEFINE GLS_DELPHI_3_UP}
  {$DEFINE GLS_DELPHI_4_UP}
  {$DEFINE GLS_DELPHI_5_UP}
  {$DEFINE GLS_DELPHI_6_UP}
  {$DEFINE GLS_DELPHI_7_UP}
{$ENDIF}

{$IFDEF GLS_DELPHI_8}
  {$DEFINE GLS_DELPHI_2_UP}
  {$DEFINE GLS_DELPHI_3_UP}
  {$DEFINE GLS_DELPHI_4_UP}
  {$DEFINE GLS_DELPHI_5_UP}
  {$DEFINE GLS_DELPHI_6_UP}
  {$DEFINE GLS_DELPHI_7_UP}
  {$DEFINE GLS_DELPHI_8_UP}
{$ENDIF}

{$IFDEF GLS_DELPHI_9}
  {$DEFINE GLS_DELPHI_2_UP}
  {$DEFINE GLS_DELPHI_3_UP}
  {$DEFINE GLS_DELPHI_4_UP}
  {$DEFINE GLS_DELPHI_5_UP}
  {$DEFINE GLS_DELPHI_6_UP}
  {$DEFINE GLS_DELPHI_7_UP}
  {$DEFINE GLS_DELPHI_8_UP}
  {$DEFINE GLS_DELPHI_9_UP}
{$ENDIF}

{$IFDEF GLS_DELPHI_10}
  {$DEFINE GLS_DELPHI_2_UP}
  {$DEFINE GLS_DELPHI_3_UP}
  {$DEFINE GLS_DELPHI_4_UP}
  {$DEFINE GLS_DELPHI_5_UP}
  {$DEFINE GLS_DELPHI_6_UP}
  {$DEFINE GLS_DELPHI_7_UP}
  {$DEFINE GLS_DELPHI_8_UP}
  {$DEFINE GLS_DELPHI_9_UP}
  {$DEFINE GLS_DELPHI_10_UP}
{$ENDIF}

{$IFDEF GLS_CPPB_3}
  {$DEFINE GLS_CPPB_3_UP}
{$ENDIF}

{$IFDEF GLS_CPPB_4}
  {$DEFINE GLS_CPPB_3_UP}
  {$DEFINE GLS_CPPB_4_UP}
{$ENDIF}

{$IFDEF GLS_CPPB_5}
  {$DEFINE GLS_CPPB_3_UP}
  {$DEFINE GLS_CPPB_4_UP}
  {$DEFINE GLS_CPPB_5_UP}  
{$ENDIF}

{$IFDEF GLS_CPPB_6}
  {$DEFINE GLS_CPPB_3_UP}
  {$DEFINE GLS_CPPB_4_UP}
  {$DEFINE GLS_CPPB_5_UP}  
  {$DEFINE GLS_CPPB_6_UP} 
{$ENDIF}

{$IFDEF GLS_COMPILER_3_UP}
  {$DEFINE GLS_NO_COM_CLEANUP}
{$ENDIF}

{$IFDEF GLS_CPPB_3_UP}
  // C++Builder requires this if you use Delphi components in run-time packages.
  {$ObjExportAll On}
{$ENDIF}
