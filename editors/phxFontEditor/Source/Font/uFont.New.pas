unit uFont.New;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ComCtrls, StdCtrls, ImgList;

type

//------------------------------------------------------------------------------
TNewMode = (
  nmNone,
  nmEmpty,
  nmGenerator
);

//------------------------------------------------------------------------------
TFrmFontNew = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    ListView1: TListView;
    ToolBarImages: TImageList;
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListView1DblClick(Sender: TObject);
  private
    function GetMode: TNewMode;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean;

    property Mode: TNewMode read GetMode;
  end;

var
  FrmFontNew: TFrmFontNew;

implementation

{$R *.dfm}

// TFrmFontNew
//------------------------------------------------------------------------------
constructor TFrmFontNew.Create(AOwner: TComponent);
begin
  inherited;

  ListView1SelectItem(Self, nil, False);

end;

//------------------------------------------------------------------------------
function TFrmFontNew.Execute: Boolean;
begin
  Result:= ShowModal = mrOk;
end;

//------------------------------------------------------------------------------
function TFrmFontNew.GetMode: TNewMode;
begin
  case ListView1.ItemIndex of
    0  : Result:= nmEmpty;
    1  : Result:= nmGenerator;
    else Result:= nmNone;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmFontNew.ListView1DblClick(Sender: TObject);
begin
  ModalResult:= mrOk;
end;

//------------------------------------------------------------------------------
procedure TFrmFontNew.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  btnOk.Enabled:= ListView1.ItemIndex >= 0;
end;

end.
