unit uCharacter.Search;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{$IFnDEF FPC}
uses
  Vcl.StdCtrls, Vcl.Dialogs, Vcl.Forms, Vcl.Controls, Vcl.Graphics, System.Classes, System.Variants, System.SysUtils, Winapi.Messages, Winapi.Windows;
{$ELSE}
{$ENDIF}


type
  TFrmCharacterSearch = class(TForm)
    edChar: TRadioButton;
    edNumber: TRadioButton;
    edSearch: TEdit;
    btnCancel: TButton;
    btnOk: TButton;
    procedure UpdateButtons(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCharacterSearch: TFrmCharacterSearch;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TFrmCharacterSearch.FormCreate(Sender: TObject);
begin
  UpdateButtons(Sender);
end;

procedure TFrmCharacterSearch.UpdateButtons(Sender: TObject);
begin
  if edChar.Checked then
  begin
    btnOk.Enabled:= Length(edSearch.Text) = 1;
  end;
  if edNumber.Checked then
  begin
    btnOk.Enabled:= Length(edSearch.Text) > 0;
  end;

end;



end.
