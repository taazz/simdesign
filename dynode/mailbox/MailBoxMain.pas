unit MailBoxMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient,
  IdMessageClient,  IdSMTP, IdExplicitTLSClientServerBase, IdSMTPBase;

type
  TfrmMailbox = class(TForm)
    Memo1: TMemo;
    IdSMTP1: TIdSMTP;
    lbName: TLabel;
    edName: TEdit;
    edPassword: TEdit;
    lbPassword: TLabel;
    btnConnect: TButton;
    procedure IdSMTP1Connected(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMailbox: TfrmMailbox;

implementation

{$R *.dfm}

procedure TfrmMailbox.btnConnectClick(Sender: TObject);
begin
//
end;

procedure TfrmMailbox.IdSMTP1Connected(Sender: TObject);
begin
//
end;

end.
