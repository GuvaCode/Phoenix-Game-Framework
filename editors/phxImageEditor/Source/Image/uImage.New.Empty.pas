unit uImage.New.Empty;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  phxDevice,
  phxGraphics;

type

//------------------------------------------------------------------------------
TFrmImageEmpty = class(TForm)
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
  private
    function GetImageName: String;
    function GetImageWidth: Integer;
    function GetImageHeight: Integer;
    function GetImageFormat: TPHXPixelFormat;

    procedure SetImageName(const Value: String);
    procedure SetImageWidth(const Value: Integer);
    procedure SetImageHeight(const Value: Integer);
    procedure SetImageFormat(const Value: TPHXPixelFormat);
  public
    constructor Create(AOwner: TComponent); override;

    function Execute: Boolean;

    property ImageName: String read GetImageName write SetImageName;
    property ImageWidth: Integer read GetImageWidth write SetImageWidth;
    property ImageHeight: Integer read GetImageHeight write SetImageHeight;
    property ImageFormat: TPHXPixelFormat read GetImageFormat write SetImageFormat;
  end;

var
  FrmImageEmpty: TFrmImageEmpty;

implementation

{$R *.dfm}

Uses TypInfo;

// TFrmImageEmpty
//==============================================================================
constructor TFrmImageEmpty.Create(AOwner: TComponent);
var Index: TPHXPixelFormat;
begin
  inherited;
  edFormat.Items.Clear;
  edFormat.Items.BeginUpdate;

  for Index:= Low(TPHXPixelFormat) to High(TPHXPixelFormat) do
  begin
    edFormat.Items.Add(  GetEnumName(TypeInfo(TPHXPixelFormat), Integer(Index)) );
  end;
  edFormat.Items.EndUpdate;

  ImageName   := '';
  ImageWidth  := 256;
  ImageHeight := 256;
  ImageFormat := pfRGBA;
end;

//------------------------------------------------------------------------------
function TFrmImageEmpty.Execute: Boolean;
begin
  Result:= ShowModal = mrOk;
end;



{$REGION 'Property getters'}

//------------------------------------------------------------------------------
function TFrmImageEmpty.GetImageName: String;
begin
  Result:= edName.Text;
end;

//------------------------------------------------------------------------------
function TFrmImageEmpty.GetImageWidth: Integer;
begin
  Result:= StrToIntDef(edWidth.Text, 256);
end;

//------------------------------------------------------------------------------
function TFrmImageEmpty.GetImageHeight: Integer;
begin
  Result:= StrToIntDef(edHeight.Text, 256);
end;
  
//------------------------------------------------------------------------------
function TFrmImageEmpty.GetImageFormat: TPHXPixelFormat;
begin
  Result:= TPHXPixelFormat( edFormat.ItemIndex );
end;

{$ENDREGION}

{$REGION 'Propery setters'}

//------------------------------------------------------------------------------
procedure TFrmImageEmpty.SetImageName(const Value: String);
begin
  edName.Text:= Value;
end;

//------------------------------------------------------------------------------
procedure TFrmImageEmpty.SetImageWidth(const Value: Integer);
begin
  edWidth.Text:= IntToStr(Value);
end;

//------------------------------------------------------------------------------
procedure TFrmImageEmpty.SetImageHeight(const Value: Integer);
begin
  edHeight.Text:= IntToStr(Value);
end;

//------------------------------------------------------------------------------
procedure TFrmImageEmpty.SetImageFormat(const Value: TPHXPixelFormat);
begin
  edFormat.ItemIndex:= Ord(Value);
end;

{$ENDREGION}


end.
