object frmMain: TfrmMain
  Left = 584
  Top = 178
  Width = 993
  Height = 643
  Caption = 'Dynode'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mnuMain
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object splHor1: TSplitter
    Left = 185
    Top = 54
    Width = 4
    Height = 423
  end
  object splHor2: TSplitter
    Left = 772
    Top = 54
    Width = 4
    Height = 423
    Align = alRight
  end
  object cbButtons: TCoolBar
    Left = 0
    Top = 0
    Width = 977
    Height = 54
    AutoSize = True
    BandBorderStyle = bsNone
    Bands = <
      item
        Control = tbStandard
        ImageIndex = -1
        MinHeight = 24
        Width = 973
      end
      item
        Control = tbView
        ImageIndex = -1
        MinHeight = 24
        Width = 113
      end
      item
        Break = False
        Control = tbRun
        ImageIndex = -1
        Width = 860
      end>
    FixedSize = True
    object tbStandard: TToolBar
      Left = 9
      Top = 0
      Width = 192
      Height = 24
      Align = alNone
      AutoSize = True
      Caption = 'tbStandard'
      Flat = True
      Images = ilMain
      TabOrder = 0
      object btnNewItems: TToolButton
        Left = 0
        Top = 0
        Caption = 'New Items'
        ImageIndex = 0
      end
      object btnOpen: TToolButton
        Left = 23
        Top = 0
        Caption = '&Open...'
        ImageIndex = 0
        MenuItem = mnuOpen
        Style = tbsDropDown
      end
      object btnSave: TToolButton
        Left = 61
        Top = 0
        Caption = '&Save'
        MenuItem = mnuSave
      end
      object T1: TToolButton
        Left = 84
        Top = 0
        Width = 8
        ImageIndex = 1
        Style = tbsSeparator
      end
      object btnSaveAll: TToolButton
        Left = 92
        Top = 0
        Caption = 'Save All'
        MenuItem = mnuSaveAll
      end
      object btnOpenProject: TToolButton
        Left = 115
        Top = 0
        Caption = 'Open Project...'
        MenuItem = mnuOpenProject
      end
      object T2: TToolButton
        Left = 138
        Top = 0
        Width = 8
        ImageIndex = 2
        Style = tbsSeparator
      end
      object btnAddFile: TToolButton
        Left = 146
        Top = 0
        Caption = 'btnAddFile'
        ImageIndex = 0
      end
      object btnRemoveFile: TToolButton
        Left = 169
        Top = 0
        Caption = 'Remove from Project...'
        MenuItem = mnuRemovefromProject
      end
    end
    object tbView: TToolBar
      Left = 9
      Top = 25
      Width = 100
      Height = 24
      Align = alNone
      AutoSize = True
      Flat = True
      Images = ilMain
      TabOrder = 1
      object btnViewUnit: TToolButton
        Left = 0
        Top = 0
        Caption = 'Units...'
        MenuItem = mnuViewUnits
      end
      object btnViewForm: TToolButton
        Left = 23
        Top = 0
        Caption = 'btnViewForm'
        ImageIndex = 0
      end
      object btnToggleViewForm: TToolButton
        Left = 46
        Top = 0
        Caption = 'btnToggleViewForm'
        ImageIndex = 0
      end
      object T3: TToolButton
        Left = 69
        Top = 0
        Width = 8
        Caption = 'T3'
        ImageIndex = 1
        Style = tbsSeparator
      end
      object btnNewForm: TToolButton
        Left = 77
        Top = 0
        Caption = 'btnNewForm'
        ImageIndex = 0
      end
    end
    object tbRun: TToolBar
      Left = 122
      Top = 25
      Width = 847
      Height = 25
      Caption = 'tbRun'
      Flat = True
      Images = ilMain
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Transparent = False
      object btnRun: TToolButton
        Left = 0
        Top = 0
        Hint = 'Run (F9)'
        Caption = 'Run'
        ImageIndex = 0
        MenuItem = mnuRun
        ParentShowHint = False
        ShowHint = True
        Style = tbsDropDown
      end
      object btnPause: TToolButton
        Left = 38
        Top = 0
        Caption = 'Program Pause'
        MenuItem = mnuProgramPause
      end
      object T4: TToolButton
        Left = 61
        Top = 0
        Width = 8
        Caption = 'T4'
        ImageIndex = 1
        Style = tbsSeparator
      end
      object btnTraceInto: TToolButton
        Left = 69
        Top = 0
        Caption = 'Trace Into'
        MenuItem = mnuTraceInto
      end
      object btnStepOver: TToolButton
        Left = 92
        Top = 0
        Caption = 'Step Over'
        MenuItem = mnuStepover
      end
    end
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 54
    Width = 185
    Height = 423
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object splVer1: TSplitter
      Left = 0
      Top = 161
      Width = 185
      Height = 4
      Cursor = crVSplit
      Align = alTop
    end
    object vstObjects: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 185
      Height = 161
      Align = alTop
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Sans Serif'
      Header.Font.Style = []
      Header.MainColumn = -1
      TabOrder = 0
      Columns = <>
    end
    object vstProperties: TVirtualStringTree
      Left = 0
      Top = 165
      Width = 185
      Height = 258
      Align = alClient
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Sans Serif'
      Header.Font.Style = []
      Header.MainColumn = -1
      TabOrder = 1
      Columns = <>
    end
  end
  object sbMain: TStatusBar
    Left = 0
    Top = 566
    Width = 977
    Height = 19
    Panels = <>
  end
  object pcProjects: TPageControl
    Left = 189
    Top = 54
    Width = 583
    Height = 423
    ActivePage = tsProject
    Align = alClient
    TabOrder = 3
    object tsProject: TTabSheet
      Caption = 'tsProject'
      object sneCode: TSynEdit
        Left = 0
        Top = 0
        Width = 575
        Height = 395
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 0
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Lines.UnicodeStrings = 'sneCode'
        FontSmoothing = fsmNone
      end
    end
  end
  object tcDebug: TTabControl
    Left = 0
    Top = 477
    Width = 977
    Height = 89
    Align = alBottom
    TabOrder = 4
    TabPosition = tpBottom
    Tabs.Strings = (
      'Debug')
    TabIndex = 0
    object mmDebug: TMemo
      Left = 4
      Top = 4
      Width = 969
      Height = 63
      Align = alClient
      Lines.Strings = (
        'mmDebug')
      TabOrder = 0
    end
  end
  object pnlRgt: TPanel
    Left = 776
    Top = 54
    Width = 201
    Height = 423
    Align = alRight
    Caption = 'pnlRgt'
    TabOrder = 5
    object splVer2: TSplitter
      Left = 1
      Top = 101
      Width = 199
      Height = 3
      Cursor = crVSplit
      Align = alTop
    end
    object vstStructure: TVirtualStringTree
      Left = 1
      Top = 104
      Width = 199
      Height = 318
      Align = alClient
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Sans Serif'
      Header.Font.Style = []
      Header.MainColumn = -1
      TabOrder = 0
      Columns = <>
    end
    object vstProjectMngr: TVirtualStringTree
      Left = 1
      Top = 1
      Width = 199
      Height = 100
      Align = alTop
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Sans Serif'
      Header.Font.Style = []
      Header.MainColumn = -1
      TabOrder = 1
      Columns = <>
    end
  end
  object mnuMain: TMainMenu
    Images = ilMain
    Left = 656
    Top = 8
    object mnuFile: TMenuItem
      Caption = '&File'
      object mnuNew: TMenuItem
        Caption = 'New'
      end
      object mnuOpen: TMenuItem
        Caption = '&Open...'
        ImageIndex = 0
        OnClick = mnuOpenClick
      end
      object mnuOpenProject: TMenuItem
        Caption = 'Open Project...'
        ShortCut = 16506
      end
      object mnuReopen: TMenuItem
        Caption = 'Reopen'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuSave: TMenuItem
        Caption = '&Save'
        ShortCut = 16467
      end
      object mnuSaveAs: TMenuItem
        Caption = 'Save As...'
      end
      object mnuSaveProjectAs: TMenuItem
        Caption = 'Save Project As...'
      end
      object mnuSaveAll: TMenuItem
        Caption = 'Save All'
      end
      object mnuClose: TMenuItem
        Caption = 'Close'
      end
      object mnuCloseAll: TMenuItem
        Caption = 'Close All'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mnuUseUnit: TMenuItem
        Caption = 'Use Unit...'
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mnuPrint: TMenuItem
        Caption = 'Print...'
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object mnuExit: TMenuItem
        Caption = '&Exit'
      end
    end
    object mnuEdit: TMenuItem
      Caption = '&Edit'
      object mnuUndo: TMenuItem
        Caption = 'Undo'
        ShortCut = 16474
      end
      object mnuRedo: TMenuItem
        Caption = 'Redo'
        ShortCut = 49242
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object mnuCut: TMenuItem
        Caption = 'Cut'
        ShortCut = 16472
      end
      object mnuCopy: TMenuItem
        Caption = 'Copy'
        ShortCut = 16451
      end
      object mnuPaste: TMenuItem
        Caption = 'Paste'
        ShortCut = 16470
      end
      object mnuDelete: TMenuItem
        Caption = 'Delete'
        ShortCut = 16430
      end
      object mnuSelectAll: TMenuItem
        Caption = 'Select All'
        ShortCut = 16449
      end
    end
    object mnuSearch: TMenuItem
      Caption = '&Search'
      object mnuFind: TMenuItem
        Caption = 'Find...'
        ShortCut = 16454
      end
      object mnuFindinFiles: TMenuItem
        Caption = 'Find in Files...'
      end
      object mnuReplace: TMenuItem
        Caption = 'Replace...'
        ShortCut = 16466
      end
      object mnuSearchAgain: TMenuItem
        Caption = 'Search Again'
        ShortCut = 114
      end
      object mnuIncrementalSearch: TMenuItem
        Caption = 'Incremental Search'
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object mnuGotoLineNumber: TMenuItem
        Caption = 'Go to Line Number...'
      end
      object mnuFindError: TMenuItem
        Caption = 'Find Error...'
      end
      object N13: TMenuItem
        Caption = '-'
      end
      object mnuBrowseSymbol: TMenuItem
        Caption = 'Browse Symbol...'
      end
    end
    object mnuView: TMenuItem
      Caption = '&View'
      object mnuProjectManager: TMenuItem
        Caption = 'Project Manager'
        ShortCut = 49232
      end
      object mnuObjectInspector: TMenuItem
        Caption = 'Object Inspector'
      end
      object mnuAlignmentPalette: TMenuItem
        Caption = 'Alignment Palette'
      end
      object mnuBrowser: TMenuItem
        Caption = 'Browser'
      end
      object mnuCodeExplorer: TMenuItem
        Caption = 'Code Explorer'
      end
      object N14: TMenuItem
        Caption = '-'
      end
      object mnuToggle: TMenuItem
        Caption = 'Toggle Form/Unit'
        ShortCut = 123
      end
      object mnuViewUnits: TMenuItem
        Caption = 'Units...'
      end
      object mnuViewForms: TMenuItem
        Caption = 'Forms...'
      end
      object N15: TMenuItem
        Caption = '-'
      end
      object mnuNewEditWindow: TMenuItem
        Caption = 'New Edit Window...'
      end
      object N16: TMenuItem
        Caption = '-'
      end
      object mnuToolbars: TMenuItem
        Caption = 'Toolbars'
        object mnuTbrStandard: TMenuItem
          Caption = 'Standard'
          Checked = True
          RadioItem = True
        end
      end
    end
    object mnuProject: TMenuItem
      Caption = '&Project'
      object mnuAddtoProject: TMenuItem
        Caption = 'Add to Project...'
        ShortCut = 8314
      end
      object mnuRemovefromProject: TMenuItem
        Caption = 'Remove from Project...'
      end
      object mnuViewSource: TMenuItem
        Caption = 'View Source'
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object mnuCompile: TMenuItem
        Caption = 'Compile %s'
        ShortCut = 16504
      end
      object mnuBuild: TMenuItem
        Caption = 'Build %s'
        OnClick = mnuBuildClick
      end
      object mnuSyntaxcheck: TMenuItem
        Caption = 'Syntax check %s'
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object mnuCompileAllProjects: TMenuItem
        Caption = 'Compile All Projects'
      end
      object mnuBuildAllProjects: TMenuItem
        Caption = 'Build All Projects'
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object mnuOptions: TMenuItem
        Caption = 'Options...'
        ShortCut = 24698
        OnClick = mnuOptionsClick
      end
    end
    object Run1: TMenuItem
      Caption = 'Run'
      object mnuRun: TMenuItem
        Caption = 'Run'
        Hint = 'Run (F9)'
        ImageIndex = 0
        ShortCut = 120
      end
      object mnuAttachToProcess: TMenuItem
        Caption = 'Attach to Process...'
      end
      object mnuParameters: TMenuItem
        Caption = 'Parameters...'
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object mnuStepover: TMenuItem
        Caption = 'Step Over'
        ShortCut = 119
      end
      object mnuTraceInto: TMenuItem
        Caption = 'Trace Into'
        ShortCut = 118
      end
      object mnuTraceNextSL: TMenuItem
        Caption = 'Trace to Next Source Line'
        ShortCut = 8310
      end
      object mnuRuntoCursor: TMenuItem
        Caption = 'Run to Cursor'
        ShortCut = 115
      end
      object mnuRunUntilReturn: TMenuItem
        Caption = 'Run Until Return'
        ShortCut = 8311
      end
      object mnuShowExecutionPoint: TMenuItem
        Caption = 'Show Execution Point'
      end
      object mnuProgramPause: TMenuItem
        Caption = 'Program Pause'
      end
      object mnuProgramReset: TMenuItem
        Caption = 'Program Reset'
        ShortCut = 16497
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object mnuInspect: TMenuItem
        Caption = 'Inspect...'
      end
      object mnuEvaluateModify: TMenuItem
        Caption = 'Evaluate/Modify...'
        ShortCut = 16502
      end
      object mnuAddWatch: TMenuItem
        Caption = 'Add Watch...'
        ShortCut = 16500
      end
      object mnuAddBreakpoint: TMenuItem
        Caption = 'Add Breakpoint'
      end
    end
    object mnuComponent: TMenuItem
      Caption = '&Component'
    end
    object mnuDatabase: TMenuItem
      Caption = '&Database'
    end
    object mnuTools: TMenuItem
      Caption = '&Tools'
      object mnuEnvironmentOptions: TMenuItem
        Caption = 'Environment Options...'
      end
      object mnuEditorOptions: TMenuItem
        Caption = 'Editor Options...'
      end
      object mnuDebuggerOptions: TMenuItem
        Caption = 'Debugger Options...'
      end
      object mnuRepository: TMenuItem
        Caption = 'Repository...'
      end
      object mnuConfigureTools: TMenuItem
        Caption = 'Configure Tools...'
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object mnuSQLTool: TMenuItem
        Caption = 'SQL Tool'
      end
      object mnuImageEditor: TMenuItem
        Caption = 'Image Editor'
      end
    end
    object mnuWindow: TMenuItem
      Caption = '&Window'
    end
    object mnuHelp: TMenuItem
      Caption = '&Help'
    end
  end
  object ilMain: TImageList
    Left = 688
    Top = 8
    Bitmap = {
      494C010101000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00000000000000
      00000000000000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF00FFFFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000000000FF
      000000FF00000000000000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF0000000000000000000000000000FF
      000000FF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FF000000FF000000FF
      000000FF000000FF000000FF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000000FF000000FF000000FF
      000000FF000000FF000000FF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      000000FF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      000000FF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFF0000000000008000000000000000
      8000000000000000800000000000000080000000000000008000000000000000
      8000000000000000800000000000000080000000000000008000000000000000
      80000000000000008000000000000000C000000000000000E000000000000000
      FFC3000000000000FFC300000000000000000000000000000000000000000000
      000000000000}
  end
end
