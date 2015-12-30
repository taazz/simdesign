unit FrameReplaceColor;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameToolWin, StdCtrls, ExtCtrls, FrameSelectColor, ComCtrls,
  FrameThreshold;

type

  TfrReplaceColor = class(TfrToolWin)
    frSourceColor: TfrSelectColor;
    frTargetColor: TfrSelectColor;
    btnPreview: TButton;
    btnAccept: TButton;
    btnCancel: TButton;
    frThreshold: TfrThreshold;
    procedure btnPreviewClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FOnPreview: TNotifyEvent;
    FOnAccept: TNotifyEvent;
    FOnCancel: TNotifyEvent;
  published
    property OnPreview: TNotifyEvent read FOnPreview write FOnPreview;
    property OnAccept: TNotifyEvent read FOnAccept write FOnAccept;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
  end;

var
  frReplaceColor: TfrReplaceColor;

implementation

{$R *.dfm}

procedure TfrReplaceColor.btnPreviewClick(Sender: TObject);
begin
  if assigned(FOnPreview) then FOnPreview(Self);
  btnAccept.Enabled := True;
  btnCancel.Enabled := True;
end;

procedure TfrReplaceColor.btnAcceptClick(Sender: TObject);
begin
  if assigned(FOnAccept) then FOnAccept(Self);
  btnAccept.Enabled := False;
  btnCancel.Enabled := False;
end;

procedure TfrReplaceColor.btnCancelClick(Sender: TObject);
begin
  if assigned(FOnCancel) then FOnCancel(Self);
  btnAccept.Enabled := False;
  btnCancel.Enabled := False;
end;

end.
