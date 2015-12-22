unit SmartViewMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, VirtualTrees;

type
  TfrmSmartview = class(TForm)
    VirtualDrawTree1: TVirtualDrawTree;
    VirtualStringTree1: TVirtualStringTree;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSmartview: TfrmSmartview;

implementation

{$R *.dfm}

end.
