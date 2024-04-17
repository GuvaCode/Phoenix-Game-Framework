unit uImage.New;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ComCtrls, StdCtrls, ImgList;

type

//------------------------------------------------------------------------------
TNewMode = (
  newEmpty,
  newTexture,
  newPacker
);

//------------------------------------------------------------------------------
TNewDialog = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    ListView1: TListView;
    WizardImages: TImageList;
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
  NewDialog: TNewDialog;

implementation

{$R *.dfm}

{ TFrmFontNew }

//------------------------------------------------------------------------------
constructor TNewDialog.Create(AOwner: TComponent);
begin
  inherited;

  ListView1SelectItem(Self, nil, False);
end;

//------------------------------------------------------------------------------
function TNewDialog.Execute: Boolean;
begin
  Result:= ShowModal = mrOk;
end;

//------------------------------------------------------------------------------
function TNewDialog.GetMode: TNewMode;
begin
  case ListView1.ItemIndex of
    0  : Result:= newEmpty;
    1  : Result:= newTexture;
    2  : Result:= newPacker;
    else Result:= newEmpty;
  end;
end;

//------------------------------------------------------------------------------
procedure TNewDialog.ListView1DblClick(Sender: TObject);
begin
  ModalResult:= mrOk;
end;

//------------------------------------------------------------------------------
procedure TNewDialog.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  btnOk.Enabled:= ListView1.ItemIndex >= 0;
end;

end.
