unit FrameToolWin;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ImgList;

type
  TfrToolWin = class(TFrame)
    pnlTop: TPanel;
    pbTitle: TPaintBox;
    lbTitle: TLabel;
    procedure pbTitlePaint(Sender: TObject);
  private
    FImages: TImageList;
    FImageIndex: TImageIndex;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
  published
    property Title: string read GetTitle write SetTitle;
    property Images: TImageList read FImages write FImages;
    property ImageIndex: TImageIndex read FImageIndex write FImageIndex;
  end;

implementation

{$R *.dfm}

function TfrToolWin.GetTitle: string;
begin
  Result := lbTitle.Caption;
end;

procedure TfrToolWin.pbTitlePaint(Sender: TObject);
begin
  pbTitle.Canvas.Brush.Style := bsSolid;
  pbTitle.Canvas.Brush.Color := clAppWorkspace;
  pbTitle.Canvas.FillRect(Rect(
    0, 0, pbTitle.Width, pbTitle.Height));
  // Image
  if assigned(FImages) and (FImageIndex >= 0) then
    FImages.Draw(pbTitle.Canvas, 1, 1, FImageIndex, True);
end;

procedure TfrToolWin.SetTitle(const Value: string);
begin
  lbTitle.Caption := Value;
end;

end.
