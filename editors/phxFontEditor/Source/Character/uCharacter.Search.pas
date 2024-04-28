unit uCharacter.Search;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls;

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

{$R *.dfm}

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
