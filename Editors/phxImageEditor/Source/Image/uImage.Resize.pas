unit uImage.Resize;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  phxDevice,
  phxGraphics;

type

//------------------------------------------------------------------------------
TFrmImageResize = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    edheight: TComboBox;
    edWidth: TComboBox;
    edFormat: TComboBox;
    Label3: TLabel;
    Button2: TButton;
    Button1: TButton;
  private
    function GetImageWidth: Integer;
    function GetImageHeight: Integer;
    function GetImageFormat: TPHXPixelFormat;

    procedure SetImageWidth(const Value: Integer);
    procedure SetImageHeight(const Value: Integer);
    procedure SetImageFormat(const Value: TPHXPixelFormat);
  public
    constructor Create(AOwner: TComponent); override;

    function Execute: Boolean;

    property ImageWidth: Integer read GetImageWidth write SetImageWidth;
    property ImageHeight: Integer read GetImageHeight write SetImageHeight;
    property ImageFormat: TPHXPixelFormat read GetImageFormat write SetImageFormat;
  end;

var
  FrmImageResize: TFrmImageResize;

implementation

{$R *.dfm}

Uses TypInfo;

// TFrmImageResize
//==============================================================================
constructor TFrmImageResize.Create(AOwner: TComponent);
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

  ImageWidth  := 256;
  ImageHeight := 256;
  ImageFormat := pfRGBA;
end;

//------------------------------------------------------------------------------
function TFrmImageResize.Execute: Boolean;
begin
  Result:= ShowModal = mrOk;
end;


{$REGION 'Property getters'}


//------------------------------------------------------------------------------
function TFrmImageResize.GetImageWidth: Integer;
begin
  Result:= StrToIntDef(edWidth.Text, 256);
end;

//------------------------------------------------------------------------------
function TFrmImageResize.GetImageHeight: Integer;
begin
  Result:= StrToIntDef(edHeight.Text, 256);
end;

//------------------------------------------------------------------------------
function TFrmImageResize.GetImageFormat: TPHXPixelFormat;
begin
  Result:= TPHXPixelFormat( edFormat.ItemIndex );
end;

{$ENDREGION}

{$REGION 'Propery setters'}


//------------------------------------------------------------------------------
procedure TFrmImageResize.SetImageWidth(const Value: Integer);
begin
  edWidth.Text:= IntToStr(Value);
end;

//------------------------------------------------------------------------------
procedure TFrmImageResize.SetImageHeight(const Value: Integer);
begin
  edHeight.Text:= IntToStr(Value);
end;

//------------------------------------------------------------------------------
procedure TFrmImageResize.SetImageFormat(const Value: TPHXPixelFormat);
begin
  edFormat.ItemIndex:= Ord(Value);
end;

{$ENDREGION}


end.
