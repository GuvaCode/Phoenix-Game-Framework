unit uDialog.New;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ImgList;

type

//------------------------------------------------------------------------------
TNewMode = (
  nmEmpty,
  nmTexture,
  nmPacker
);

//------------------------------------------------------------------------------
TNewDialog = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    ListView1: TListView;
    ToolBarImages: TImageList;
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
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
    0  : Result:= nmEmpty;
    1  : Result:= nmTexture;
    2  : Result:= nmPacker;
    else Result:= nmEmpty;
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
