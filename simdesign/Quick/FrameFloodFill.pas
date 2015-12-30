unit FrameFloodFill;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameToolWin, StdCtrls, ExtCtrls, FrameSelectColor, ComCtrls,
  FrameThreshold;

type

  TfrFloodFill = class(TfrToolWin)
    frFloodFillColor: TfrSelectColor;
    btnPreview: TButton;
    btnAccept: TButton;
    btnCancel: TButton;
    frThreshold: TfrThreshold;
    gbPosition: TGroupBox;
    btnPick: TButton;
    lbXPos: TLabel;
    lbXVal: TLabel;
    lbYPos: TLabel;
    lbYVal: TLabel;
    procedure btnAcceptClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnPickClick(Sender: TObject);
  private
    FOnPick: TNotifyEvent;
    FOnAccept: TNotifyEvent;
    FOnCancel: TNotifyEvent;
  published
    property OnPick: TNotifyEvent read FOnPick write FOnPick;
    property OnAccept: TNotifyEvent read FOnAccept write FOnAccept;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
  end;

var
  frFloodFill: TfrFloodFill;

implementation

{$R *.dfm}

procedure TfrFloodFill.btnAcceptClick(Sender: TObject);
begin
  if assigned(FOnAccept) then FOnAccept(Self);
  btnAccept.Enabled := False;
  btnCancel.Enabled := False;
end;

procedure TfrFloodFill.btnCancelClick(Sender: TObject);
begin
  if assigned(FOnCancel) then FOnCancel(Self);
  btnAccept.Enabled := False;
  btnCancel.Enabled := False;
end;

procedure TfrFloodFill.btnPickClick(Sender: TObject);
begin
  if assigned(FOnPick) then FOnPick(Self);
end;

end.
