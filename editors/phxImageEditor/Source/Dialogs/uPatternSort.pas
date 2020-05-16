unit uPatternSort;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFrmPatternSort = class(TForm)
    cbSortType: TComboBox;
    Label1: TLabel;
    btnOk: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmPatternSort: TFrmPatternSort;

implementation

{$R *.dfm}

procedure TFrmPatternSort.FormCreate(Sender: TObject);
begin
  cbSortType.ItemIndex:= 0;
end;

end.
