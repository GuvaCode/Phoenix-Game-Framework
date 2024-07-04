unit uPattern.Sort;

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
    function GetSortType: Integer;
    { Private declarations }
  public
    function Execute: Boolean;

    property SortType: Integer read GetSortType;
  end;

var
  FrmPatternSort: TFrmPatternSort;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
function TFrmPatternSort.Execute: Boolean;
begin
  Result:= ShowModal = mrOk;
end;

procedure TFrmPatternSort.FormCreate(Sender: TObject);
begin
  cbSortType.ItemIndex:= 0;
end;

function TFrmPatternSort.GetSortType: Integer;
begin
  Result:= cbSortType.ItemIndex;
end;

end.
