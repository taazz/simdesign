object ItemView: TItemView
  Left = 0
  Top = 0
  Width = 618
  Height = 477
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  TabStop = True
  object ListView: TListView
    Left = 0
    Top = 26
    Width = 618
    Height = 395
    Align = alClient
    Columns = <
      item
        Caption = 'Col1'
      end
      item
        Caption = 'Col2'
      end
      item
        Caption = 'Col3'
      end
      item
        Caption = 'Col4'
      end>
    FullDrag = True
    HideSelection = False
    MultiSelect = True
    OwnerData = True
    RowSelect = True
    ParentShowHint = False
    PopupMenu = pmItems
    ShowHint = True
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = ListViewChange
    OnColumnClick = ListViewColumnClick
    OnContextPopup = ListViewContextPopup
    OnCustomDraw = DrawBackGround
    OnCustomDrawItem = ListViewCustomDrawItem
    OnData = ListviewData
    OnDataStateChange = ListViewDataStateChange
    OnDblClick = ListviewDblClick
    OnEdited = ListViewEdited
    OnEditing = ListViewEditing
    OnEnter = ListViewEnter
    OnInfoTip = ListViewInfoTip
    OnKeyDown = ListViewKeyDown
    OnMouseDown = ListViewMouseDown
    OnMouseMove = ListViewMouseMove
  end
  object cbItems: TControlBar
    Left = 0
    Top = 0
    Width = 618
    Height = 26
    Align = alTop
    AutoSize = True
    BevelEdges = []
    PopupMenu = pmToolbars
    TabOrder = 1
    object tbSorting: TToolBar
      Left = 11
      Top = 2
      Width = 158
      Height = 22
      HelpContext = 80
      Caption = 'tbSorting'
      EdgeInner = esNone
      EdgeOuter = esNone
      Images = ilMenu
      TabOrder = 0
      Transparent = True
      Wrapable = False
      object ToolButton16: TToolButton
        Left = 0
        Top = 2
        Action = SortRandom
      end
      object ToolButton17: TToolButton
        Left = 23
        Top = 2
        Width = 8
        Caption = 'ToolButton17'
        ImageIndex = 1
        Style = tbsSeparator
      end
      object ToolButton18: TToolButton
        Left = 31
        Top = 2
        Action = SortName
      end
      object ToolButton19: TToolButton
        Left = 54
        Top = 2
        Action = SortDate
      end
      object ToolButton20: TToolButton
        Left = 77
        Top = 2
        Action = SortSize
      end
      object ToolButton21: TToolButton
        Left = 100
        Top = 2
        Action = SortSeries
      end
      object ToolButton22: TToolButton
        Left = 123
        Top = 2
        Width = 8
        Caption = 'ToolButton22'
        ImageIndex = 5
        Style = tbsSeparator
      end
      object ToolButton23: TToolButton
        Left = 131
        Top = 2
        Action = SortOrder
      end
    end
    object tbViews: TToolBar
      Left = 182
      Top = 2
      Width = 118
      Height = 22
      HelpContext = 55
      Caption = 'tbViews'
      EdgeInner = esNone
      EdgeOuter = esNone
      Images = ilMenu
      TabOrder = 1
      Transparent = True
      Wrapable = False
      object ToolButton24: TToolButton
        Left = 0
        Top = 2
        Action = ViewList
      end
      object ToolButton25: TToolButton
        Left = 23
        Top = 2
        Action = ViewSmall
      end
      object ToolButton26: TToolButton
        Left = 46
        Top = 2
        Action = ViewLarge
      end
      object ToolButton27: TToolButton
        Left = 69
        Top = 2
        Action = ViewThumb
      end
      object ToolButton28: TToolButton
        Left = 92
        Top = 2
        Action = ViewDetail
      end
    end
    object tbItems: TToolBar
      Left = 313
      Top = 2
      Width = 96
      Height = 22
      Caption = 'tbItems'
      EdgeInner = esNone
      EdgeOuter = esNone
      Images = ilMenu
      TabOrder = 2
      Transparent = True
      Wrapable = False
      object ToolButton29: TToolButton
        Left = 0
        Top = 2
        Action = ItemDelete
      end
      object ToolButton30: TToolButton
        Left = 23
        Top = 2
        Action = ItemRemove
      end
      object ToolButton5: TToolButton
        Left = 46
        Top = 2
        Action = ItemDescribe
      end
      object ToolButton7: TToolButton
        Left = 69
        Top = 2
        Action = QuickSearch
      end
    end
    object tbTypes: TToolBar
      Left = 422
      Top = 2
      Width = 99
      Height = 22
      Caption = 'tbTypes'
      EdgeInner = esNone
      EdgeOuter = esNone
      Images = ilMenu
      TabOrder = 3
      Transparent = True
      Wrapable = False
      object ToolButton1: TToolButton
        Left = 0
        Top = 2
        Action = ShowFiles
      end
      object ToolButton2: TToolButton
        Left = 23
        Top = 2
        Action = ShowFolders
        Caption = '`'
      end
      object ToolButton3: TToolButton
        Left = 46
        Top = 2
        Action = ShowGroups
      end
      object ToolButton4: TToolButton
        Left = 69
        Top = 2
        Action = ShowSeries
      end
    end
  end
  object pnlEdit: TPanel
    Left = 0
    Top = 421
    Width = 618
    Height = 56
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    DesignSize = (
      618
      56)
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 14
      Height = 56
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        14
        56)
      object btnHideEdit: TSpeedButton
        Left = 0
        Top = 0
        Width = 14
        Height = 56
        Anchors = [akLeft, akTop, akRight, akBottom]
        Flat = True
        Glyph.Data = {
          2E020000424D2E0200000000000076000000280000000D000000370000000100
          040000000000B801000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
          D000DDDF77F77DDDD000DDDF87F87DDDD000DDDF87F87DDDD000DDDF87F87DDD
          D000DDDF87F87DDDD000DDDF87F87DDDD000DDDF87F87DDDD000DDDF87F87DDD
          D000DDDF87F87DDDD000DDDF87F87DDDD000DDDF87F87DDDD000DDDF87F87DDD
          D000DDDF87F87DDDD000DDDF87F87DDDD000DDDF87F87DDDD000DDDF87F87DDD
          D000DDDF87F87DDDD000DDDF87F87DDDD000DDDF87F87DDDD000DDDF87F87DDD
          D000DDDF87F87DDDD000DDDF87F87DDDD000DDDF87F87DDDD000DDDF87F87DDD
          D000DDDF87F87DDDD000DDDF87F87DDDD000DDDF87F87DDDD000DDDF87F87DDD
          D000DDDF87F87DDDD000DDDF87F87DDDD000DDDF87F87DDDD000DDDF87F87DDD
          D000DDDF87F87DDDD000DDDF87F87DDDD000DDDF87F87DDDD000DDDF87F87DDD
          D000DDDF87F87DDDD000DDDF87F87DDDD000DDDF87F87DDDD000DDDF87F87DDD
          D000DDDFF7FF7DDDD000DDDDDDDDDDDDD000D00000000000D000DF7777777770
          D000DF8888888870D000DF8008800870D000DF8800008870D000DF8880088870
          D000DF8800008870D000DF8008800870D000DF8888888870D000DF8888888870
          D000DFFFFFFFFFF0D000DDDDDDDDDDDDD000}
        OnClick = btnHideEditClick
      end
    end
    object pcEdit: TPageControl
      Left = 14
      Top = 0
      Width = 604
      Height = 57
      ActivePage = tsDescr
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabHeight = 14
      TabOrder = 1
      OnChange = pcEditChange
      object tsDescr: TTabSheet
        Caption = '&Description'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        object btnDescription: TBitBtn
          Left = 440
          Top = 5
          Width = 60
          Height = 24
          Caption = 'OK'
          Default = True
          TabOrder = 0
          OnClick = btnDescriptionClick
          Glyph.Data = {
            DE010000424DDE01000000000000760000002800000024000000120000000100
            0400000000006801000000000000000000001000000000000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3333333333333333333333330000333333333333333333333333F33333333333
            00003333344333333333333333388F3333333333000033334224333333333333
            338338F3333333330000333422224333333333333833338F3333333300003342
            222224333333333383333338F3333333000034222A22224333333338F338F333
            8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
            33333338F83338F338F33333000033A33333A222433333338333338F338F3333
            0000333333333A222433333333333338F338F33300003333333333A222433333
            333333338F338F33000033333333333A222433333333333338F338F300003333
            33333333A222433333333333338F338F00003333333333333A22433333333333
            3338F38F000033333333333333A223333333333333338F830000333333333333
            333A333333333333333338330000333333333333333333333333333333333333
            0000}
          NumGlyphs = 2
        end
        object cbbDescr: TComboBox
          Left = 6
          Top = 6
          Width = 427
          Height = 21
          ItemHeight = 13
          TabOrder = 1
        end
      end
      object tsRating: TTabSheet
        Caption = '&Rating'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ImageIndex = 1
        ParentFont = False
        object lblLower: TLabel
          Left = 56
          Top = 16
          Width = 22
          Height = 13
          Caption = 'Poor'
          OnClick = lblLowerClick
        end
        object lblHigher: TLabel
          Left = 200
          Top = 16
          Width = 43
          Height = 13
          Caption = 'Excellent'
          OnClick = lblHigherClick
        end
        object Label1: TLabel
          Left = 0
          Top = 0
          Width = 69
          Height = 13
          Caption = 'Quality Rating:'
        end
        object lblQualRating: TLabel
          Left = 0
          Top = 14
          Width = 47
          Height = 16
          Alignment = taCenter
          Caption = 'Various'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object slQualRating: TRxSlider
          Left = 80
          Top = 0
          Width = 121
          Height = 32
          Increment = 20
          MaxValue = 200
          TabOrder = 0
          Value = 100
          OnChange = slQualRatingChange
        end
        object btnQualOK: TBitBtn
          Left = 424
          Top = 5
          Width = 76
          Height = 24
          Caption = 'Apply'
          Default = True
          TabOrder = 1
          OnClick = btnQualOKClick
          Glyph.Data = {
            DE010000424DDE01000000000000760000002800000024000000120000000100
            0400000000006801000000000000000000001000000000000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3333333333333333333333330000333333333333333333333333F33333333333
            00003333344333333333333333388F3333333333000033334224333333333333
            338338F3333333330000333422224333333333333833338F3333333300003342
            222224333333333383333338F3333333000034222A22224333333338F338F333
            8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
            33333338F83338F338F33333000033A33333A222433333338333338F338F3333
            0000333333333A222433333333333338F338F33300003333333333A222433333
            333333338F338F33000033333333333A222433333333333338F338F300003333
            33333333A222433333333333338F338F00003333333333333A22433333333333
            3338F38F000033333333333333A223333333333333338F830000333333333333
            333A333333333333333338330000333333333333333333333333333333333333
            0000}
          NumGlyphs = 2
        end
      end
      object tsQSearch: TTabSheet
        Caption = 'Quick&Search'
        ImageIndex = 2
        object Label2: TLabel
          Left = 160
          Top = -1
          Width = 217
          Height = 33
          AutoSize = False
          Caption = 
            'Search while you type!'#13#10'Quicksearch will automatically jump to t' +
            'he first'#13#10'occurance of your text in the current view'
        end
        object Image1: TImage
          Left = 8
          Top = 10
          Width = 16
          Height = 16
          Picture.Data = {
            055449636F6E0000010001001010100000000000280100001600000028000000
            10000000200000000100040000000000C0000000000000000000000000000000
            0000000000000000000080000080000000808000800000008000800080800000
            80808000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
            FFFFFF000000000000000000000000000000000000000000000000000F000000
            000F00000F000000000F0000000000000000000000F000000F00000000F00080
            0F00000000F000800F0000000000000000000000000F00000F00000000000000
            0000000000000000000000000000F00000F00000000000000000000000000000
            00000000FFFF0000FFFF000007C1000007C1000007C100000101000000010000
            000100000001000080030000C1070000C1070000E38F0000E38F0000E38F0000
            FFFF0000}
        end
        object edQSearch: TEdit
          Left = 32
          Top = 8
          Width = 121
          Height = 22
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          OnChange = edQSearchChange
        end
        object chbSearchFileName: TCheckBox
          Left = 456
          Top = 0
          Width = 153
          Height = 17
          Caption = 'Search FileNames'
          Checked = True
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          State = cbChecked
          TabOrder = 1
        end
        object chbSearchDescription: TCheckBox
          Left = 456
          Top = 16
          Width = 153
          Height = 17
          Caption = 'Search Descriptions'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
        end
        object BitBtn1: TBitBtn
          Left = 368
          Top = 0
          Width = 81
          Height = 33
          Action = SearchShowAll
          Caption = 'Show All Results'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000
            0000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF000000000000000000000000000000000000000000FF00FF0000000000FFFF
            FF00000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF0000000000FFFFFF00000000000000000000000000FF00FF0000000000FFFF
            FF00000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF0000000000FFFFFF00000000000000000000000000FF00FF00000000000000
            00000000000000000000000000000000000000000000FF00FF00000000000000
            00000000000000000000000000000000000000000000FF00FF00000000000000
            0000FFFFFF00000000000000000000000000000000000000000000000000FFFF
            FF000000000000000000000000000000000000000000FF00FF00000000000000
            0000FFFFFF00000000000000000000000000C0C0C0000000000000000000FFFF
            FF000000000000000000000000000000000000000000FF00FF00000000000000
            0000FFFFFF00000000000000000000000000C0C0C0000000000000000000FFFF
            FF000000000000000000000000000000000000000000FF00FF00FF00FF000000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
            FF0000000000FFFFFF00000000000000000000000000FF00FF0000000000FFFF
            FF00000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF000000000000000000000000000000000000000000FF00FF00000000000000
            0000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00000000000000000000000000FF00FF00FF00FF00FF00FF000000
            00000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF0000000000FFFFFF0000000000FF00FF00FF00FF00FF00FF000000
            0000FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00000000000000000000000000FF00FF00FF00FF00FF00FF000000
            00000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
          Layout = blGlyphTop
          Spacing = -1
        end
      end
    end
  end
  object alItemView: TActionList
    Images = ilMenu
    OnUpdate = alItemViewUpdate
    Left = 16
    Top = 80
    object SortUnsorted: TAction
      Category = 'Sorting'
      Caption = '&Unsorted'
      OnExecute = SortUnsortedExecute
    end
    object SortRandom: TAction
      Category = 'Sorting'
      Caption = '&Randomize'
      Hint = 'Randomize list'
      ImageIndex = 0
      OnExecute = SortRandomExecute
    end
    object SortName: TAction
      Category = 'Sorting'
      Caption = 'By &Name'
      Hint = 'Sort on Name'
      ImageIndex = 1
      OnExecute = SortNameExecute
    end
    object SortNameNum: TAction
      Category = 'Sorting'
      Caption = 'By Name (N&umeric)'
      ImageIndex = 1
      OnExecute = SortNameNumExecute
    end
    object SortDate: TAction
      Category = 'Sorting'
      Caption = 'By &Date'
      Hint = 'Sort on Date'
      ImageIndex = 2
      OnExecute = SortDateExecute
    end
    object SortSize: TAction
      Category = 'Sorting'
      Caption = 'By &Size'
      Hint = 'Sort on Size'
      ImageIndex = 3
      OnExecute = SortSizeExecute
    end
    object SortSeries: TAction
      Category = 'Sorting'
      Caption = 'By &Series'
      Hint = 'Sort on Series'
      ImageIndex = 4
      OnExecute = SortSeriesExecute
    end
    object SortOrder: TAction
      Category = 'Sorting'
      Caption = 'SortDir'
      Hint = 'Switch Direction'
      ImageIndex = 5
      OnExecute = SortOrderExecute
    end
    object ViewList: TAction
      Category = 'View'
      Caption = 'ViewList'
      Hint = 'List'
      ImageIndex = 7
      ShortCut = 122
      OnExecute = ViewListExecute
    end
    object ViewSmall: TAction
      Category = 'View'
      Caption = 'ViewSmall'
      Hint = 'Small Icons'
      ImageIndex = 8
      ShortCut = 121
      OnExecute = ViewSmallExecute
    end
    object ViewLarge: TAction
      Category = 'View'
      Caption = 'ViewLarge'
      Hint = 'Large Icons'
      ImageIndex = 9
      ShortCut = 120
      OnExecute = ViewLargeExecute
    end
    object ViewThumb: TAction
      Category = 'View'
      Caption = 'ViewThumb'
      Hint = 'Thumbnails'
      ImageIndex = 10
      ShortCut = 119
      OnExecute = ViewThumbExecute
    end
    object ViewDetail: TAction
      Category = 'View'
      Caption = 'ViewDetail'
      Hint = 'Details'
      ImageIndex = 11
      ShortCut = 123
      OnExecute = ViewDetailExecute
    end
    object ItemDelete: TAction
      Category = 'Functions'
      Caption = 'Delete Item(s)'
      Enabled = False
      Hint = 'Delete Item(s)'
      ImageIndex = 12
      ShortCut = 46
      OnExecute = ItemDeleteExecute
    end
    object ItemRemove: TAction
      Category = 'Functions'
      Caption = 'Remove Item(s)'
      Enabled = False
      Hint = 'Remove Item(s)'
      ImageIndex = 13
      ShortCut = 8238
      OnExecute = ItemRemoveExecute
    end
    object ShowFiles: TAction
      Category = 'Show'
      Caption = 'Show Files'
      Checked = True
      Hint = 'Show Files'
      ImageIndex = 14
      OnExecute = ShowFilesExecute
    end
    object ShowFolders: TAction
      Category = 'Show'
      Caption = 'Show Folders'
      Hint = 'Show Folders'
      ImageIndex = 15
      OnExecute = ShowFoldersExecute
    end
    object ShowGroups: TAction
      Category = 'Show'
      Caption = 'Show Groups'
      Hint = 'Show Groups'
      ImageIndex = 16
      OnExecute = ShowGroupsExecute
    end
    object ShowSeries: TAction
      Category = 'Show'
      Caption = 'Show Series'
      Hint = 'Show Series'
      ImageIndex = 17
      OnExecute = ShowSeriesExecute
    end
    object SortingToolbar: TAction
      Category = 'Toolbar'
      Caption = 'Sorting Toolbar'
      Checked = True
      OnExecute = SortingToolbarExecute
    end
    object ViewsToolbar: TAction
      Category = 'Toolbar'
      Caption = 'Views Toolbar'
      Checked = True
      OnExecute = ViewsToolbarExecute
    end
    object ItemsToolbar: TAction
      Category = 'Toolbar'
      Caption = 'Items Toolbar'
      Checked = True
      OnExecute = ItemsToolbarExecute
    end
    object TypesToolbar: TAction
      Category = 'Toolbar'
      Caption = 'Types Toolbar'
      Checked = True
      OnExecute = TypesToolbarExecute
    end
    object ItemOpen: TAction
      Category = 'Keys'
      Caption = 'ItemOpen'
      OnExecute = ItemOpenExecute
    end
    object SelectAll: TAction
      Category = 'Select'
      Caption = '&All Items'
      ShortCut = 16449
      OnExecute = SelectAllExecute
    end
    object RotateLeft: TAction
      Category = 'Lossless'
      Caption = 'Rotate &Left'
      ImageIndex = 18
      OnExecute = RotateLeftExecute
    end
    object RotateRight: TAction
      Category = 'Lossless'
      Caption = 'Rotate &Right'
      ImageIndex = 19
      OnExecute = RotateRightExecute
    end
    object Rotate180: TAction
      Category = 'Lossless'
      Caption = 'Rotate 180 deg'
      Hint = 'Rotate 180 degrees'
      ImageIndex = 27
      OnExecute = Rotate180Execute
    end
    object FlipHor: TAction
      Category = 'Lossless'
      Caption = 'Flip &Horizontal'
      ImageIndex = 20
      OnExecute = FlipHorExecute
    end
    object FlipVer: TAction
      Category = 'Lossless'
      Caption = 'Flip &Vertical'
      ImageIndex = 21
      OnExecute = FlipVerExecute
    end
    object Properties: TAction
      Category = 'Item'
      Caption = '&Properties'
      ImageIndex = 25
      OnExecute = PropertiesExecute
    end
    object SelectInvert: TAction
      Category = 'Select'
      Caption = '&Invert Selection'
      Hint = 'Invert Selection'
      ShortCut = 16457
      OnExecute = SelectInvertExecute
    end
    object SelectDuplicates: TAction
      Category = 'Select'
      Caption = '&Duplicates'
      ShortCut = 16452
      OnExecute = SelectDuplicatesExecute
    end
    object SelectDupeInFolder: TAction
      Category = 'Select'
      Caption = 'Duplicates in &Folder'
      OnExecute = SelectDupeInFolderExecute
    end
    object SortFolder: TAction
      Category = 'Sorting'
      Caption = 'By &Folder'
      Hint = 'Sort on Folder'
      ImageIndex = 26
      OnExecute = SortFolderExecute
    end
    object SortDupeGroup: TAction
      Category = 'Sorting'
      Caption = 'By &Duplicate Group'
      OnExecute = SortDupeGroupExecute
    end
    object SortAscending: TAction
      Category = 'Sorting'
      Caption = '&Ascending'
      Checked = True
      ImageIndex = 5
      OnExecute = SortAscendingExecute
    end
    object SortDescending: TAction
      Category = 'Sorting'
      Caption = '&Descending'
      ImageIndex = 6
      OnExecute = SortDescendingExecute
    end
    object ItemDescribe: TAction
      Category = 'Functions'
      Caption = '&Describe Item(s)'
      Hint = 'Describe Item(s)'
      ImageIndex = 22
      ShortCut = 114
      OnExecute = ItemDescribeExecute
    end
    object SendToMySelection: TAction
      Category = 'SendTo'
      Caption = 'My &Selection'
      Hint = 'Send to My Selection'
      ImageIndex = 23
      OnExecute = SendToMySelectionExecute
    end
    object SendToEmail: TAction
      Category = 'SendTo'
      Caption = '&E-Mail'
      Hint = 'Send to E-Mail'
      ImageIndex = 24
      OnExecute = SendToEmailExecute
    end
    object SelectSmart: TAction
      Category = 'Select'
      Caption = '&Smart Series'
      Hint = 'Select a Series'
      ShortCut = 16467
      OnExecute = SelectSmartExecute
    end
    object Rename: TAction
      Category = 'Functions'
      Caption = 'Rename Item(s)'
      Hint = 'Rename Item(s)'
      ImageIndex = 28
      ShortCut = 113
      OnExecute = RenameExecute
    end
    object ChangeFiledate: TAction
      Category = 'Functions'
      Caption = 'Change &Filedate'
      Hint = 'Change Filedate'
      ImageIndex = 29
      OnExecute = ChangeFiledateExecute
    end
    object QuickSearch: TAction
      Category = 'Functions'
      Caption = 'Quick&Search'
      Hint = 'Click for QuickSearch'
      ImageIndex = 30
      ShortCut = 16454
      OnExecute = QuickSearchExecute
    end
    object ItemRate: TAction
      Category = 'Functions'
      Caption = 'ItemRate'
      ShortCut = 16466
      OnExecute = ItemRateExecute
    end
    object SearchShowAll: TAction
      Category = 'Functions'
      Caption = 'Show All Results'
      ImageIndex = 30
      OnExecute = SearchShowAllExecute
    end
    object SortSimilar: TAction
      Category = 'Sorting'
      Caption = 'By &Similarity'
      Hint = 'Sort by similarity'
      OnExecute = SortSimilarExecute
    end
    object RotateOri: TAction
      Category = 'Lossless'
      Caption = 'Rotate using &EXIF flag'
      ImageIndex = 31
      OnExecute = RotateOriExecute
    end
  end
  object ilMenu: TImageList
    Left = 48
    Top = 80
    Bitmap = {
      494C010120002200040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000009000000001002000000000000090
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
      0000808080008000000080000000800000008000000080000000808080000000
      0000000000000000000000000000000000008080000080800000808000008080
      0000808000008080000080800000808000008080000080800000808000008080
      0000808000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      0000800000008000000080000000800000008000000080000000800000008000
      00000000000000000000000000000000000080800000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0080800000FFFFFF0080800000FFFFFF00FFFFFF00FFFFFF008080
      00008080000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000800000008000
      0000800000000000000000000000000000000000000000000000800000008000
      00008000000000000000000000000000000080800000FFFFFF00808000008080
      0000FFFFFF00808000008080000080800000FFFFFF008080000080800000FFFF
      FF008080000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000080808000800000008000
      0000000000000000000000000000000000000000000000000000000000008000
      0000800000008080800000000000000000008080000080800000FFFFFF00FFFF
      FF00FFFFFF00808000008080000080800000FFFFFF008080000080800000FFFF
      FF008080000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000800000000000
      0000000000000000000000000000008000000080000000000000000000008080
      8000800000008000000000000000000000008080000080800000808000008080
      0000FFFFFF00808000008080000080800000FFFFFF008080000080800000FFFF
      FF008080000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000080000000800000000000
      0000000000000000000000800000008000000080000000800000000000000000
      00008000000080000000000000000000000080800000FFFFFF00FFFFFF00FFFF
      FF0080800000808000008080000080800000FFFFFF00FFFFFF00FFFFFF008080
      00008080000000000000000000000000000000000000FFFFFF00FFFFFF00FF00
      0000FF000000FFFFFF00FF000000FFFFFF00FF000000FFFFFF00FF000000FFFF
      FF00FF000000FFFFFF0000000000000000000000000000000000FFFFFF000000
      00000000000000000000C0C0C0000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000080000000800000000000
      0000000000000000000000800000008000000080000000800000000000000000
      0000800000008000000000000000000000008080000080800000808000008080
      000080800000808000008080000080800000FFFFFF0080800000808000008080
      00008080000000000000000000000000000000000000FFFFFF00FF000000FFFF
      FF00FF000000FFFFFF00FF000000FFFFFF00FF000000FFFFFF00FF000000FFFF
      FF00FF000000FFFFFF0000000000000000000000000000000000FFFFFF000000
      00000000000000000000C0C0C0000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000080000000800000000000
      0000000000000000000000000000008000000080000000000000000000000000
      0000000000000000000000000000000000008080000080800000808000008080
      000080800000808000008080000080800000FFFFFF0080800000808000008080
      00008080000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FF000000FFFFFF00FF000000FF000000FF000000FFFFFF00FF000000FF00
      0000FF000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000800000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000800000008000000000000000000000008080000080800000808000008080
      0000808000008080000080800000808000008080000080800000808000008080
      00008080000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FF000000FFFFFF00FF000000FFFFFF00FF000000FFFFFF00FF000000FF00
      0000FF000000FFFFFF000000000000000000000000000000000000000000FFFF
      FF000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000080808000800000008000
      0000000000000000000080000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080000080800000808000008080
      0000808000008080000080800000808000008080000080800000808000008080
      00008080000000000000000000000000000000000000FFFFFF00FF000000FF00
      0000FF000000FFFFFF00FF000000FF000000FF000000FFFFFF00FF000000FFFF
      FF00FF000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800000008000
      0000800000000000000080000000800000000000000000000000000000008000
      0000808080000000000000000000000000008080000080800000808000008080
      0000808000008080000080800000808000008080000080800000808000008080
      00008080000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      0000800000008000000080000000800000008000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080008000000080000000800000008000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF00000080000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF00000080000000FF000000FF00000080800000FF0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000800000000000000000000000FF0000008080000080800000FF00
      0000000000008000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080000000000080808000000000000000000000000000000000000000
      0000800080008000800080008000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800000000000000000000000
      00000000000000000000000000000000000000000000FF00000080800000FFFF
      00008000000080000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800080008000800080008000800080008000800000000000000000000000
      00000000000000000000000000000000000080808000FFFFFF00FFFFFF0000FF
      FF00FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF00FF000000FFFF
      0000FFFF000080000000FFFFFF000000000000000000FFFFFF00808080008080
      800080808000FFFFFF00808080008080800080808000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF008080
      8000000000000000000000000000808080000000000000000000000000000000
      0000800080008000800080008000800080008000800080008000800080000000
      00000000000000000000000000000000000080808000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FF000000FFFF
      0000FFFF00008000000000FFFF000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000080808000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF000000FF000000FF00
      0000FF00000080000000FFFFFF000000000000000000FFFFFF00808080008080
      800080808000FFFFFF00808080008080800080808000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000FFFF00FFFFFF00FFFF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000080808000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF00FFFFFF0000FFFF000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000080808000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFF
      FF0000FFFF00FFFFFF00FFFFFF000000000000000000FFFFFF00808080000000
      000080808000FFFFFF008080800000000000C0C0C00000000000000000000000
      0000000000000000000000000000800000000000000000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C0000000000000000000000000000000000080808000FFFFFF00800000008000
      000080000000800000008000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF000000FF000000800000FFFF000000000000000000FFFFFF0000000000C0C0
      C00000000000FFFFFF0000000000C0C0C00000000000C0C0C000000000000000
      0000000000000000000080000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000C0C0C000C0C0
      C0000000000000000000000000000000000080808000FFFFFF00FF000000FF00
      0000FF00000080000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000FF000000FF00FFFFFF00000000008000000080000000800000000000
      0000C0C0C00000000000C0C0C00000000000C0C0C00000000000C0C0C000C0C0
      C000C0C0C000000000008000000080000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000000000000000
      000000000000000000000000000000000000000000000000000000000000C0C0
      C0000000000000000000000000000000000080808000FFFFFF00FF000000FF00
      00008080000080000000FFFFFF0000FFFF00FFFFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00000000008000000080000000800000008000
      000000000000C0C0C00000000000C0C0C00000000000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C00080000000800000000000000080808000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000000000000000
      0000000000008000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800080808000FF000000FF00
      0000808000008080000080000000808080008080800080808000808080008080
      8000808080008080800080808000000000000000000000000000000000000000
      00000000000000000000C0C0C00000000000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C00080000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800000000000
      0000000000008000000080000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF0000000000
      0000FF00000080800000FFFF0000800000000000000000000000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C0000000000080000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      0000800000008000000080000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF000000FFFF0000FFFF00008000000080000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000800000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF0000FFFF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000000000000000000000000000000000000000000000000
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
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C000C0C0C00000000000000000000000000000000000800080008000
      8000800080008000800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800080000000000000000000000000008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C0C0
      C000C0C0C000C0C0C00000000000000000000000000000000000800080008000
      8000800080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800080008000
      8000800080000000000000000000000000008080800000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C0C0
      C000C0C0C000C0C0C00000000000000000000000000000000000800080008000
      8000800080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008000800080008000800080008000
      8000800080000000000000000000000000008080800000000000FFFFFF00FFFF
      FF00000000000000000000000000FFFFFF00000000000000000000000000FFFF
      FF00FFFFFF000000000000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C000C0C0C00000000000000000000000000000000000800080008000
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080008000800080008000800080008000800080008000
      8000800080000000000000000000000000008080800000000000FFFFFF000000
      0000FFFFFF00FFFFFF0000000000FFFFFF0000000000FFFFFF00FFFFFF000000
      0000FFFFFF000000000000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C000C0C0C00000000000000000000000000000000000800080008000
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800000000000FFFFFF00FFFF
      FF00000000000000000000000000FFFFFF0000000000FFFFFF00FFFFFF000000
      0000FFFFFF000000000000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C00000000000000000000000000000000000800080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800000000000FFFFFF000000
      0000FFFFFF00FFFFFF0000000000FFFFFF0000000000FFFFFF00FFFFFF000000
      0000FFFFFF000000000000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C00000000000000000000000000000000000800080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800000000000FFFFFF00FFFF
      FF000000000000000000FFFFFF00FFFFFF00000000000000000000000000FFFF
      FF00FFFFFF000000000000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000FFFFFF000000000000000000000000000000
      000000000000FFFFFF00FFFF0000FFFFFF00FFFF000000000000000000000000
      0000800000008000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C0000000000000000000000000008080800000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000FFFFFF000000000000000000000000000000
      000000000000FFFF0000FFFFFF00FFFF0000FFFFFF0000000000800000008000
      0000800000008000000080000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C0000000000000000000000000008080800000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000800000008000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C0000000000000000000000000008080800000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000800000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0000000000000000000000000008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008080800080808000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000808080008080800080808000FFFFFF000000000000000000000000000000
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
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF00000000000000000000000000C0C0C000808080008080
      8000808080008080800080808000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000080008000800080008000
      800080008000000000000000000000000000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000000000000000000000000000000000000000000000000000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C0000000000000000000000000008000
      800080008000800080008000800000000000000000000000000000FFFF000080
      80000080800000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000080
      80000080800000FFFF000000000000000000C0C0C00080808000808080008080
      8000808080008080800080808000808080008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000800080008000
      800080008000000000000000000000000000C0C0C000C0C0C000C0C0C0000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C000C0C0C000C0C0C0000000000000000000000000008000
      800080008000800080000000000000000000000000000000000000FFFF000080
      80000080800000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000080
      80000080800000FFFF000000000000000000C0C0C000C0C0C000808080008080
      8000808080008080800080808000808080008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000800080008000
      800080008000000000000000000000000000C0C0C00000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C0C0C0000000000000000000000000008000
      800080008000800080000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF000000000000000000C0C0C00080808000808080008080
      80008080800080808000808080008080800080808000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000000000000000008000
      8000800080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      800080008000000000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF000000000000000000C0C0C000C0C0C000808080008080
      80008080800080808000808080008080800080808000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000000000000000008000
      8000800080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      800080008000000000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF000000000000000000C0C0C00080808000808080008080
      80008080800080808000808080008080800080808000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000000000000000000000
      0000800080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      800000000000000000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF000000000000000000C0C0C000C0C0C000808080008080
      80008080800080808000808080008080800080808000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000000000000000000000
      0000800080000000000000000000000000000000000000000000000000000000
      0000000000000000000080000000000000000000000080000000000000000000
      0000000000000000000000000000000000000000000000000000000000008000
      800000000000000000000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF000000000000000000C0C0C00080808000808080008080
      80008080800080808000808080008080800080808000FF000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800000000000
      0000000000000000000080000000000000000000000080000000000000000000
      0000000000008000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF000000000000000000C0C0C000C0C0C000808080008080
      8000808080008080800080808000808080008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000800000000000
      0000000000008000000000000000000000000000000000000000800000000000
      0000000000008000000080000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF00000000
      00000000FF000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF000000000000000000C0C0C00080808000808080000000
      0000000000000000000080808000808080008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008000000080000000800000008000
      0000800000000000000000000000000000000000000000000000000000008000
      0000800000008000000080000000800000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF00000000
      00000000FF000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000008080800000000000000000008080
      8000808080008080800000000000000000008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080000000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000080000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      0000000000000000FF000000000000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF000000000000000000000000000000000080808000808080008080
      8000808080008080800080808000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000800000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000808080008080800000000000000000000000000000000000000000000000
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
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C6008484
      8400848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C6C6C600C6C6C600C6C6C600C6C6C6008484
      8400848484000000000000000000000000000000000000000000000000000000
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF00000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      000000000000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      000000000000C6C6C600C6C6C60000840000C6C6C600C6C6C600C6C6C6008484
      84008484840000000000000000000000000000000000000000000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      00000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF00000000000000000000000000000000008484
      8400C6C6C600C6C6C60000840000008400000084000000840000C6C6C6008484
      84008484840084848400000000000000000000000000000000000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000FF0000000000000000000000000000000000000000000000
      000000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000000000FF
      FF0000000000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00000000000000000000000000000000008484
      8400C6C6C60000840000C6C6C60000840000C6C6C60000840000C6C6C6008484
      8400848484008484840000000000000000000000000000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      00000000FF00000000000000000000000000000000000000000000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFF
      FF000000000000FFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000000000000000000000000084848400C6C6
      C600C6C6C60000840000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C6008484
      8400848484008484840000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF00000000000000000000000000000000000000
      FF000000FF00000000000000000000000000000000000000000000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000FFFF000000000000FF
      FF0000000000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF0000000000000000000000000084848400C6C6
      C600008400000084000000840000C6C6C6000084000000840000C6C6C6008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF0000000000000000000000FF000000
      FF000000000000000000000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF0000000000FFFF
      FF000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000000000000000000000000084848400C6C6
      C600C6C6C60000840000C6C6C600C6C6C6000084000000840000C6C6C6008484
      8400848484008484840084848400000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF000000FF000000
      00000000000000000000000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000FFFF000000000000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084848400C6C6
      C600C6C6C600C6C6C6000084000000840000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00848484008484840084848400000000000000000000000000000000000000
      00000000000000000000000000000000FF000000FF000000FF00000000000000
      00000000000000000000000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000000000FFFF
      FF0000FFFF000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      0000FFFFFF000000000000000000000000000000000084848400C6C6C600C6C6
      C600C6C6C600C6C6C600FFFFFF00FFFFFF008484840084848400848484008484
      8400FFFFFF00FFFFFF0084848400000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF000000FF000000
      00000000000000000000000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000FFFF00000000000000
      0000000000008080800000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000084848400C6C6C600C6C6
      C600FFFFFF00FFFFFF008484840084848400FFFFFF0084848400FFFFFF00FFFF
      FF0084848400FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF0000000000000000000000FF000000
      FF000000000000000000000000000000000000000000FFFFFF0000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF000000
      0000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000000000FFFFFF000000
      0000000000000000000000000000000000000000000084848400FFFFFF00FFFF
      FF008484840084848400FFFFFF00FFFFFF00FFFFFF0084848400FFFFFF00FFFF
      FF00848484008484840084848400000000000000000000000000000000000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      FF000000FF0000000000000000000000000000000000FFFFFF0000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000084848400848484008484
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF0084848400FFFFFF00FFFFFF00FFFF
      FF008484840000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      00000000FF000000FF00000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      840084848400FFFFFF00FFFFFF0084848400FFFFFF00FFFFFF00848484008484
      84000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840084848400FFFFFF008484840084848400000000000000
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
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000FFFFFF008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000848484008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084000000FFFF
      FF00FFFFFF008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000084
      000000840000FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084000000FFFF
      FF00FFFFFF008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000084
      000000FF000000FF0000FFFFFF00000000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084000000FFFF
      FF00840000008400000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF0000FF000000FF0000FFFF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084000000FFFFFF00840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000000000000000000000000000008484840000000000000000000000
      000000000000848484000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFF0000FFFF0000000000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF000000FF000000
      FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000848484008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084000000FFFF
      FF00FFFFFF008400000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084000000FFFF
      FF00FFFFFF008400000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000084000000FFFFFF008400
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084000000FFFF
      FF00840000008400000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000000000000000000000
      0000000000008484840000000000000000000000000000000000000000000000
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
      0000000000000000000000000000000000000000000084000000840000008400
      0000000000000000000000000000000000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000FFFFFF008400
      0000000000000000000000000000000000000000000084000000FFFFFF008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400848484008484
      84008484840084848400848484008484840084848400FFFFFF00000000000000
      0000000000000000000000000000000000000000000084000000840000000000
      0000000000000000000000000000000000000000000084000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400000000000000000000000000848484000000000000000000000000000000
      000084848400C6C6C600FFFFFF00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084848400C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400C6C6C600C6C6C600FFFFFF000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084848400C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400C6C6C600C6C6C600C6C6C600FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400C6C6C600C6C6C600C6C6C600C6C6C600FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000000000000000000000000000000000000000000084000000840000008400
      000000000000000000000000000000000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400C6C6C600C6C6C600C6C6C600C6C6C600FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400C6C6C600C6C6C600C6C6C600FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000FFFFFF008400
      0000000000000000000000000000000000000000000084000000FFFFFF008400
      000000000000000000000000000000000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF00000000000000000000000000
      000000000000000000000000000000000000000000000000000084848400C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400C6C6C600C6C6C600FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000000000
      0000000000000000000000000000000000000000000084000000840000000000
      000000000000000000000000000000000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF00000000000000000000000000
      000000000000000000000000000000000000000000000000000084848400C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400C6C6C600FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400848484008484
      84008484840084848400848484008484840084848400FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000000000000000000000000000000000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000FFFFFF008400
      0000000000000000000000000000000000000000000084000000FFFFFF008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000000000
      0000000000000000000000000000000000000000000084000000840000000000
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
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000008400000084000000
      8400000084000000840000008400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C600C6C6C60000000000FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000008400000084000000
      0000000000000000000000008400000000000000000000000000848484000000
      0000848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      84000000000084848400000000000000000000000000000000000084840000FF
      FF000084840000FFFF000084840000FFFF000000000000000000000000000000
      000084848400000000008484840000000000000000000000000000000000C6C6
      C600C6C6C600C6C6C600C6C6C60000000000FFFFFF000000000000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000084000000
      8400000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      000000000000000000008484840000000000000000000084840000FFFF000084
      840000FFFF000084840000FFFF000084840000FFFF0000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C60000000000FFFFFF000000000000000000FFFF
      FF000000000000000000FFFFFF00000000000000000000000000000000000000
      8400000084000000000000000000000000000000000084848400000000000000
      00000000000084848400000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000084848400000000000000000000FFFF000084840000FF
      FF000084840000FFFF000084840000FFFF000084840000000000000000008484
      84000000000000000000000000008484840000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C60000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000FFFFFF00000000000000000000000000000000000000
      0000000084000000840000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000848400000000000000
      00000000000000000000000000000000000000FFFF0000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C6000000
      000000000000C6C6C600C6C6C60000000000FFFFFF000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000008400000000000000
      0000000000000000840000008400000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF00848484000000000000000000000000000084840000FF
      FF000084840000FFFF000084840000FFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C6000000
      000000000000C6C6C600C6C6C60000000000FFFFFF000000000000000000FFFF
      FF000000000000000000FFFFFF00000000000000000000008400000084000000
      8400000084000000840000008400000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FF00
      0000FF000000FFFFFF00FF000000FFFFFF00FF000000FFFFFF00FF000000FFFF
      FF0000000000FFFFFF000000000000000000000000000084840000FFFF000084
      840000FFFF000084840000FFFF000084840000FFFF0000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C6000000000000000000FFFFFF00FFFFFF00FFFF
      FF000000000000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FF000000FFFF
      FF00FF000000FFFFFF00FF000000FFFFFF00FF000000FFFFFF00FF000000FFFF
      FF0000000000FFFFFF0000000000000000000000000000FFFF000084840000FF
      FF000084840000FFFF000084840000FFFF000084840000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C600C6C6
      C600C6C6C6000000000000000000FFFFFF00FFFFFF0000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000084000000840000008400
      0000000000008400000084000000840000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FF000000FFFFFF00FF000000FF000000FF000000FFFFFF00FF000000FF00
      000000000000FFFFFF0000000000000000000000000000848400000000000000
      00000000000000000000000000000000000000FFFF0000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C6000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000FFFFFF00000000000000000084848400840000000000
      0000000000000000000084000000848484000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FF000000FFFFFF00FF000000FFFFFF00FF000000FFFFFF00FF000000FF00
      000000000000FFFFFF000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF000000000000000000FFFFFF00FFFFFF000000000000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FF000000FF00
      0000FF000000FFFFFF00FF000000FF000000FF000000FFFFFF00FF000000FFFF
      FF0000000000FFFFFF0000000000000000000000000000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000FFFFFF00FFFFFF000000000000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000848484008400
      0000000000008400000084848400000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF000000000000000000000000000000000000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400840000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000900000000100010000000000800400000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFFFFFFFFFFFFE4FFFFFFFFF01F
      0003000107C1E00F0003000107C1C7C70003000107C18FE30003000101019E63
      0003000100019C330003000100019C330003000100019E7F0003000180038FF3
      00030001C1078DFF00030001C107C4E700030001E38FE07FFFE40001E38FF07F
      FFFFFFFFE38FFCFFFFFFFFFFFFFFFDFFFC3FFFFFFFFFE7FFF81FFFFFFFFBE1FF
      FB0B001F8011E07F0000001F0001E01F0000001F0000E0070000001F0000E003
      0000001F000BFFFF0000001F000BF0070000001E000BF80700000004001BFE07
      0000000080FBBF870000000080FBBBE70000F800FFFBD9FFD0DFFC00FFFBE0FF
      F81FFE04FFFBF9FFFE3FFFFFFFFBFBFFFFFFFFFFFFFFFFFF8181FFF30000FFFF
      8181FFC37FFE03FFC183FF03000203FFC183FC03000203FFE187F003000203FF
      E187E003000203FFF18FFFFF000203FFF18FFFFF00020037F99FE0030002F033
      F99FF0030002F001FDBFFC0300020033FFFFFF03000203F7FFFFFFC3000203FF
      FFFFFFF3000003FFFFFFFFFFFFFFFFFFFFFFE3FFFFFFFFFF800180FF02000040
      80010000020180408001002A8207E04180010000821FF84180010000C27FFE43
      80010000C3FFFFC380010000E3FFFFC780010000E3FDBFC780010000F3DDBBCF
      80010000F39BD9CF8001002AFB07E0DF80010000FF9FF9FFC003007FFFDFFBFF
      C00380FFFFFFFFFFFFFFE3FFFFFFFFFFFF87FFFFFC00FFFFFE03EFFDFC00F800
      F803C7FDF000F000E003C3FBF000E000E001C3F3C000C000E001E1E7C0008000
      C001F0C700000000C001F80F00000000C000FC1F00000001C000FC1F00000003
      8000F80F000300078000E0C70003000F8001C1E3000F001F8007C3F1000F80FF
      E00FC7FD003F80FFF83FFFFF003FFFFFFFFFFFFFFFFFFFFFE3FFFF81FFC06111
      E23FFFFFFFFFFFFFE7FFFFC3FFC06111FFFFFFC3FFC0FFFFFFFFFFC303C06111
      FF1FFFC3FFC0FFFFFF1181C703C06111FF3FFFFF03C0FFFFFFFFC3FF03C06111
      FFFFC3FF03FFFFFF8FFFC3FF03FF000088FFC3FF03FFFFFF9FFFC7FF03FFE111
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFFF3FFFB8F8F
      0031FFFDFFFB8888AAB1FBFB803B9F9F0020F1FDC03BFFFF0020F0F3C07FFFFF
      003BE0FFE0718F8F003BE071E0FB8888003BC07BF0FD9F9F003BC03DF1F3FFFF
      003B8033FBFFFFFFAABBFFFFFFF38F8F003BFFFBFFFD8888FFFBFFFBFFFB9F9F
      FFFBFFFBFFFDFFFFFFFBFFFBFFF3FFFFFE7FFFFFFFFFFFFFF81F81EFFFF7C0FB
      E0039DC7000180718000CFC7000100310000E783000100200000F38300010020
      0000B9EF0001003B000081EF0001003B0000FFEF0001003B000088EF0001003B
      00009CEF0001003B0000C1EF0001003B0000C9EF0001807B8003E3EF0001C0FB
      E00FE3EFFFF7FFFBFC3FF7EFFFF7FFFB00000000000000000000000000000000
      000000000000}
  end
  object pmToolbars: TPopupMenu
    Left = 112
    Top = 80
    object SortDate1: TMenuItem
      Action = SortingToolbar
    end
    object SortDir1: TMenuItem
      Action = ViewsToolbar
    end
    object SortName1: TMenuItem
      Action = ItemsToolbar
    end
    object TypesToolbar1: TMenuItem
      Action = TypesToolbar
    end
  end
  object dftFiles: TDropFileTarget
    Dragtypes = [dtCopy, dtMove]
    GetDataOnEnter = False
    OnDrop = dftFilesDrop
    OnGetDropEffect = dftFilesGetDropEffect
    ShowImage = True
    Left = 16
    Top = 152
  end
  object dfsFiles: TDropFileSource
    Dragtypes = [dtCopy, dtMove, dtLink]
    OnFeedback = dfsFilesFeedback
    ImageIndex = 0
    ShowImage = False
    ImageHotSpotX = 16
    ImageHotSpotY = 16
    Left = 56
    Top = 152
  end
  object ddmFiles: TDropDummy
    Dragtypes = []
    GetDataOnEnter = False
    ShowImage = True
    Left = 96
    Top = 152
  end
  object tmrShowItems: TTimer
    Enabled = False
    Interval = 5
    OnTimer = tmrShowItemsTimer
    Left = 144
    Top = 80
  end
  object pmItems: TPopupMenu
    Images = ilMenu
    Left = 176
    Top = 80
    object DeleteItems1: TMenuItem
      Action = ItemDelete
    end
    object RemoveItems1: TMenuItem
      Action = ItemRemove
    end
    object miShell: TMenuItem
      Caption = '&Shell'
      object Open1: TMenuItem
        Caption = 'Open'
        Default = True
      end
    end
    object SendTo1: TMenuItem
      Caption = 'Send &To'
      object MySelection1: TMenuItem
        Action = SendToMySelection
      end
      object SendToEmail1: TMenuItem
        Action = SendToEmail
      end
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object SortList1: TMenuItem
      Caption = 'Sort &List'
      object Unsorted1: TMenuItem
        Action = SortUnsorted
      end
      object SortName2: TMenuItem
        Action = SortName
      end
      object ByDate1: TMenuItem
        Action = SortDate
      end
      object BySize1: TMenuItem
        Action = SortSize
      end
      object BySeries1: TMenuItem
        Action = SortSeries
      end
      object ByFolder1: TMenuItem
        Action = SortFolder
      end
      object ByDuplicateGroup1: TMenuItem
        Action = SortDupeGroup
      end
      object BySimilarity1: TMenuItem
        Action = SortSimilar
      end
      object ByNameNumeric1: TMenuItem
        Action = SortNameNum
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Ascending1: TMenuItem
        Action = SortAscending
      end
      object Descending1: TMenuItem
        Action = SortDescending
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Randomize1: TMenuItem
        Action = SortRandom
      end
    end
    object Select1: TMenuItem
      Caption = '&Select'
      object SelectAll1: TMenuItem
        Action = SelectAll
      end
      object Duplicates1: TMenuItem
        Action = SelectDuplicates
      end
      object DuplicatesinFolder1: TMenuItem
        Action = SelectDupeInFolder
      end
      object SmartSeries1: TMenuItem
        Action = SelectSmart
      end
    end
    object Lossless1: TMenuItem
      Caption = '&Lossless'
      object RotateLeft1: TMenuItem
        Action = RotateLeft
      end
      object RotateRight1: TMenuItem
        Action = RotateRight
      end
      object Rotate180deg1: TMenuItem
        Action = Rotate180
      end
      object FlipHorizontal1: TMenuItem
        Action = FlipHor
      end
      object FlipVertical1: TMenuItem
        Action = FlipVer
      end
      object RotateusingEXIFori1: TMenuItem
        Action = RotateOri
      end
    end
    object RenameItems1: TMenuItem
      Action = Rename
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Properties2: TMenuItem
      Action = Properties
    end
  end
end
