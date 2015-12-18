unit MailBoxMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient,
  IdMessageClient,  IdSMTP, IdExplicitTLSClientServerBase, IdSMTPBase;

type
  TfrmMailbox = class(TForm)
    IdSMTP1: TIdSMTP;
    lbName: TLabel;
    edName: TEdit;
    edPassword: TEdit;
    lbPassword: TLabel;
    btnCheckEmail: TButton;
    Memo1: TMemo;
    procedure IdSMTP1Connected(Sender: TObject);
    procedure btnCheckEmailClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMailbox: TfrmMailbox;

implementation

{$R *.dfm}

procedure TfrmMailbox.btnCheckEmailClick(Sender: TObject);
begin
//
end;

procedure TfrmMailbox.IdSMTP1Connected(Sender: TObject);
begin
//
end;

end.
