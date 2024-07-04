unit uElementName;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  phxTypes,

  phxGraphics,
  phxSkin,
  phxSkinEx;



type
  TFrmElementName = class(TForm)
    Label1: TLabel;
    edElementControl: TComboBox;
    btnCancel: TButton;
    btnOkey: TButton;
    edElementPart: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    edElementName: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure edElementControlChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edElementPartChange(Sender: TObject);
    procedure edElementPartDropDown(Sender: TObject);
  private
    FElementName: String;
    FElementControl: String;
    FElementPart: String;

    procedure edElementControlEnum;

    procedure SetElementName(const Value: String);
    procedure SetElementControl(const Value: String);
    procedure SetElementPart(const Value: String);
  public
    function Execute: Boolean;

    Property ElementControl: String read FElementControl write SetElementControl;
    Property ElementPart: String read FElementPart write SetElementPart;
    Property ElementName: String read FElementName write SetElementName;
  end;

var
  FrmElementName: TFrmElementName;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
procedure TFrmElementName.FormCreate(Sender: TObject);
begin
  edElementControlEnum;
end;

//------------------------------------------------------------------------------
function TFrmElementName.Execute: Boolean;
begin
 // edElementControl.Text:= FElementControl;
 // edElementPart   .Text:= FElementPart;
  //edElementName   .Text:= FElementName;

  Result:= ShowModal = mrOk;
end;

//------------------------------------------------------------------------------
procedure TFrmElementName.FormShow(Sender: TObject);
begin
  edElementControl.SetFocus;
end;

//------------------------------------------------------------------------------
procedure TFrmElementName.edElementControlEnum;
var Control: TPHXThemedControl;
begin
  edElementControl.Items.BeginUpdate;
  edElementControl.Items.Clear;

  for Control := Low(TPHXThemedControl) to High(TPHXThemedControl) do
  begin
    edElementControl.Items.Add(ControlToString(Control));
  end;
  edElementControl.Items.EndUpdate;
end;


//------------------------------------------------------------------------------
procedure TFrmElementName.edElementControlChange(Sender: TObject);
var Value: String;
begin
  Value:= edElementControl.Text;

  if FElementControl <> Value then
  begin
    FElementControl:= Value;

    edElementPart.Text:= '';
  end;
{
  if edElementPart.Items.Count > 0 then
  begin
    FElementPart:= edElementPart.Items[0];

    edElementPart.Text:= FElementPart;
  end;  }
end;

//------------------------------------------------------------------------------
procedure TFrmElementName.edElementPartDropDown(Sender: TObject);
var Part: TPHXThemedPart;
var Name: String;
begin
  edElementPart.Items.BeginUpdate;
  edElementPart.Items.Clear;

  for Part := Low(TPHXThemedPart) to High(TPHXThemedPart) do
  begin
    Name:= PartToString(Part);

    if GetControlName(Name) = ElementControl then
    begin
      edElementPart.Items.Add( GetPartName(Name) );
    end;
  end;
  edElementPart.Items.EndUpdate;
end;

//------------------------------------------------------------------------------
procedure TFrmElementName.edElementPartChange(Sender: TObject);
begin
  FElementName:= edElementControl.Text + '.' + edElementPart.Text;

  edElementName.Text:= FElementName;
end;



//------------------------------------------------------------------------------
procedure TFrmElementName.SetElementControl(const Value: String);
begin
  if FElementControl <> Value then
  begin
    FElementControl:= Value;

    edElementControl.Text:= Value;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmElementName.SetElementPart(const Value: String);
begin
  if FElementPart <> Value then
  begin
    FElementPart:= Value;

    edElementPart.Text:= Value;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmElementName.SetElementName(const Value: String);
begin
  if FElementName <> Value then
  begin
    FElementName:= Value;

    FElementControl:= GetControlName(Value);
    FElementPart   := GetPartName(Value);

    edElementName   .Text:= Value;
    edElementControl.Text:= FElementControl;
    edElementPart   .Text:= FElementPart;
 end;
end;

end.
