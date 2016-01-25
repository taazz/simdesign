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
    lbPassword: TLabel;
    btnCheckMail: TButton;
    procedure IdSMTP1Connected(Sender: TObject);
    procedure btnCheckMailClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMailbox: TfrmMailbox;

implementation

{$R *.dfm}

procedure TfrmMailbox.btnCheckMailClick(Sender: TObject);
// test now
var
  // variáveis e objetos necessários para o envio
  IdSSLIOHandlerSocket: TIdSSLIOHandlerSocketOpenSSL;
  IdSMTP: TIdSMTP;
  IdMessage: TIdMessage;
  IdText: TIdText;
  sAnexo: string;
begin
  // instanciação dos objetos
  IdSSLIOHandlerSocket := TIdSSLIOHandlerSocketOpenSSL.Create(Self);
  IdSMTP := TIdSMTP.Create(Self);
  IdMessage := TIdMessage.Create(Self);

  try
    // Configuração do protocolo SSL (TIdSSLIOHandlerSocketOpenSSL)
    IdSSLIOHandlerSocket.SSLOptions.Method := sslvSSLv23;
    IdSSLIOHandlerSocket.SSLOptions.Mode := sslmClient;

    // Configuração do servidor SMTP (TIdSMTP)
    IdSMTP.IOHandler := IdSSLIOHandlerSocket;
    IdSMTP.UseTLS := utUseImplicitTLS;
    IdSMTP.AuthType := satDefault;
    IdSMTP.Port := 465;
    IdSMTP.Host := 'smtp.gmail.com';
    IdSMTP.Username := 'MYLOGIN@gmail.com';
    IdSMTP.Password := 'MYPASS';

    // Configuração da mensagem (TIdMessage)
    IdMessage.From.Address := 'MYLOGIN@gmail.com';
    IdMessage.From.Name := 'John Smith';
    IdMessage.ReplyTo.EMailAddresses := IdMessage.From.Address;
    IdMessage.Recipients.Add.Text := 'receiver@example.com';
    IdMessage.Subject := 'Hello World';
    IdMessage.Encoding := meMIME;

    // Configuração do corpo do email (TIdText)
    IdText := TIdText.Create(IdMessage.MessageParts);
    IdText.Body.Add('The body of the e-mail goes here');
    IdText.ContentType := 'text/plain; charset=iso-8859-1';


    // Conexão e autenticação
    try
      IdSMTP.Connect;
      IdSMTP.Authenticate;
    except
      on E:Exception do
      begin
        MessageDlg('Cannot authenticate: ' +
          E.Message, mtWarning, [mbOK], 0);
        Exit;
      end;
    end;

    // Envio da mensagem
    try
      IdSMTP.Send(IdMessage);
      MessageDlg('Message sent successfully!', mtInformation, [mbOK], 0);
    except
      On E:Exception do
      begin
        MessageDlg('Error while sending a message: ' +
          E.Message, mtWarning, [mbOK], 0);
      end;
    end;
  finally
    // liberação dos objetos da memória
    FreeAndNil(IdMessage);
    FreeAndNil(IdSSLIOHandlerSocket);
    FreeAndNil(IdSMTP);
  end;
end;

procedure TfrmMailbox.IdSMTP1Connected(Sender: TObject);
begin
//
end;

end.
