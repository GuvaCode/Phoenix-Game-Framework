unit uNewDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TypInfo,

  phxGraphics;

type
  TFrmNewDialog = class(TForm)
    edName: TEdit;
    Button1: TButton;
    edWidth: TComboBox;
    edHeight: TComboBox;
    edFormat: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
  private
    function GetFormat: TPHXPixelFormat;
    function GetHeight: Integer;
    function GetName: String;
    function GetWidth: Integer;

    procedure SetFormat(const Value: TPHXPixelFormat);
    procedure SetHeight(const Value: Integer);
    procedure SetName(const Value: String); reintroduce;
    procedure SetWidth(const Value: Integer);
  public
    property Name: String read GetName write SetName;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Format: TPHXPixelFormat read GetFormat write SetFormat;
  end;

var
  FrmNewDialog: TFrmNewDialog;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
procedure TFrmNewDialog.FormCreate(Sender: TObject);
var Index: TPHXPixelFormat;
begin
  edFormat.Items.Clear;
  edFormat.Items.BeginUpdate;

  for Index:= Low(TPHXPixelFormat) to High(TPHXPixelFormat) do
  begin
    edFormat.Items.Add(  GetEnumName(TypeInfo(TPHXPixelFormat), Integer(Index)) );
  end;
  edFormat.Items.EndUpdate;

  Name   := '';
  Width  := 256;
  Height := 256;
  Format := pfRGBA;
end;


{$REGION 'Property getters'}

//------------------------------------------------------------------------------
function TFrmNewDialog.GetName: String;
begin
  Result:= edName.Text;
end;

//------------------------------------------------------------------------------
function TFrmNewDialog.GetWidth: Integer;
begin
  Result:= StrToIntDef(edWidth.Text, 256);
end;

//------------------------------------------------------------------------------
function TFrmNewDialog.GetHeight: Integer;
begin
  Result:= StrToIntDef(edHeight.Text, 256);
end;
  
//------------------------------------------------------------------------------
function TFrmNewDialog.GetFormat: TPHXPixelFormat;
begin
  Result:= TPHXPixelFormat( edFormat.ItemIndex );
end;

{$ENDREGION}

{$REGION 'Propery setters'}

//------------------------------------------------------------------------------
procedure TFrmNewDialog.SetName(const Value: String);
begin
  edName.Text:= Value;
end;

//------------------------------------------------------------------------------
procedure TFrmNewDialog.SetWidth(const Value: Integer);
begin
  edWidth.Text:= IntToStr(Value);
end;

//------------------------------------------------------------------------------
procedure TFrmNewDialog.SetHeight(const Value: Integer);
begin
  edHeight.Text:= IntToStr(Value);
end;

//------------------------------------------------------------------------------
procedure TFrmNewDialog.SetFormat(const Value: TPHXPixelFormat);
begin
  edFormat.ItemIndex:= Ord(Value);
end;

{$ENDREGION}


end.
