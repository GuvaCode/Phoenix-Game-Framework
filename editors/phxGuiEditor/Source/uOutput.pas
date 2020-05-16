unit uOutput;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SynEditHighlighter, SynHighlighterPas,
  SynEdit;

type
  TFrmOutput = class(TForm)
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmOutput: TFrmOutput;

implementation

{$R *.dfm}

end.
