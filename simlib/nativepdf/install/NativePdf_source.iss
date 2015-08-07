; NativePdf "source" installer script

[Setup]
AppName=NativePdf + Pyro Source
AppVerName=NativePdf + Pyro Source v1.07
AppCopyright=Copyright (C) 2001-2010 SimDesign BV
DefaultDirName={pf}\SimDesign\NativePdf107
DefaultGroupName=SimDesign
LicenseFile=license_source.txt
;InfoAfterFile=readme_source.txt
DisableStartupPrompt=yes
OutputBaseFilename=NativePdf

[Components]
Name: "Delphi7"; Description: "Install for Delphi 7"; types: full compact;

[Tasks]
Name: "desktopicon"; Description: "Create a &desktop icon";

[Messages]
SelectTasksLabel2=Select the additional tasks you would like Setup to perform while installing [name], then click Next.%n%nYou must close all running instances of Delphi before you continue!

[Files]
Source: "..\..\pyro\License_source.txt"; DestDir: "{app}"; DestName: "License.txt";
Source: "..\..\pyro\versions.txt"; DestDir: "{app}"; DestName: "versions.txt";


; Example SVG file
;Source: "..\demos\ShapeDemo\exe\tiger.svg"; DestDir: "{app}\simlib\nativesvg\demos\ShapeDemo\exe"
;Source: "..\demos\ShapeDemo\exe\opacity01.svg"; DestDir: "{app}\simlib\nativesvg\demos\ShapeDemo\exe"
;Source: "..\demos\ShapeDemo\exe\PreserveAspectRatio.svg"; DestDir: "{app}\simlib\nativesvg\demos\ShapeDemo\exe"

; Demos
Source: "..\demos\pdfdemo\*.res"; DestDir: "{app}\simlib\nativepdf\demos\pdfdemo"
Source: "..\demos\pdfdemo\*.cfg"; DestDir: "{app}\simlib\nativepdf\demos\pdfdemo"
Source: "..\demos\pdfdemo\*.dof"; DestDir: "{app}\simlib\nativepdf\demos\pdfdemo"
Source: "..\demos\pdfdemo\*.dpr"; DestDir: "{app}\simlib\nativepdf\demos\pdfdemo"
Source: "..\demos\pdfdemo\*.pas"; DestDir: "{app}\simlib\nativepdf\demos\pdfdemo"
Source: "..\demos\pdfdemo\*.dfm"; DestDir: "{app}\simlib\nativepdf\demos\pdfdemo"
Source: "..\demos\pdfeditor\*.res"; DestDir: "{app}\simlib\nativepdf\demos\pdfeditor"
Source: "..\demos\pdfeditor\*.cfg"; DestDir: "{app}\simlib\nativepdf\demos\pdfeditor"
Source: "..\demos\pdfeditor\*.dof"; DestDir: "{app}\simlib\nativepdf\demos\pdfeditor"
Source: "..\demos\pdfeditor\*.dpr"; DestDir: "{app}\simlib\nativepdf\demos\pdfeditor"
Source: "..\demos\pdfeditor\*.pas"; DestDir: "{app}\simlib\nativepdf\demos\pdfeditor"
Source: "..\demos\pdfeditor\*.dfm"; DestDir: "{app}\simlib\nativepdf\demos\pdfeditor"

;Source: "..\demos\D7demos.bpg"; DestDir: "{app}\simlib\nativepdf\demos"

; Documentation
Source: "..\docu\NativePdf.chm"; DestDir: "{app}\simlib\nativepdf\docu"
Source: "..\..\pyro\docu\Pyro.chm"; DestDir: "{app}\simlib\pyro\docu"

; Sources NativePdf
Source: "..\*.pas"; DestDir: "{app}\simlib\nativepdf"

; Sources Pyro
Source: "..\..\pyro\source\engine\*.pas"; DestDir: "{app}\simlib\pyro\source\engine"
Source: "..\..\pyro\source\formats\*.pas"; DestDir: "{app}\simlib\pyro\source\formats"
Source: "..\..\pyro\source\gui\*.res"; DestDir: "{app}\simlib\pyro\source\gui"
Source: "..\..\pyro\source\gui\*.dfm"; DestDir: "{app}\simlib\pyro\source\gui"
Source: "..\..\pyro\source\gui\*.pas"; DestDir: "{app}\simlib\pyro\source\gui"

; Sources additional

Source: "..\..\nativexml\NativeXml.pas"; DestDir: "{app}\simlib\nativexml";
Source: "..\..\..\extlib\extmem\Fast\FastMM.pas"; DestDir: "{app}\extlib\extmem\Fast";
;Source: "..\..\bitmap\sdMapIterator.pas"; DestDir: "{app}\simlib\bitmap";
;Source: "..\..\bitmap\sdBitmapConversion.pas"; DestDir: "{app}\simlib\bitmap";
Source: "..\..\disk\sdFileList.pas"; DestDir: "{app}\simlib\disk";
Source: "..\..\general\SortedLists.pas"; DestDir: "{app}\simlib\general";
Source: "..\..\bitmap\sdBitmapResize.pas"; DestDir: "{app}\simlib\bitmap";


[Icons]
Name: "{userdesktop}\NativePdf v1.07"; Filename: "{app}"; Tasks: desktopicon







