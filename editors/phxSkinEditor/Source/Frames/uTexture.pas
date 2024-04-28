unit uTexture;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,

  phxTypes,

  phxTexture,
  phxGraphics,
  phxSkin,
  phxDevice;

type
  TFrmTexture = class(TForm)
    edWrapS: TComboBox;
    Label1: TLabel;
    edWrapT: TComboBox;
    Label2: TLabel;
    edFilterMin: TComboBox;
    edFilterMag: TComboBox;
    cbMipmaps: TCheckBox;
    Panel2: TPanel;
    Panel1: TPanel;
    btnOk: TButton;
    Button4: TButton;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    FTexture: TPHXTexture;

    procedure ImageToForm;
    procedure FormToImage;

    procedure SetTexture(const Value: TPHXTexture);
  public
    function Execute(const Texture: TPHXTexture): Boolean;

    property Texture: TPHXTexture read FTexture write SetTexture;
  end;

var
  FrmTexture: TFrmTexture;

implementation

{$R *.dfm}
(*

// Possible values for the texure minifying and magnification functions
//------------------------------------------------------------------------------
TPHXTextureFilter = (
  // GL_NEAREST
  tfNearest,
  // GL_LINEAR
  tfLinear,
  // GL_NEAREST_MIPMAP_NEAREST
  tfNearestMipmapNearest,
  // GL_LINEAR_MIPMAP_NEAREST
  tfLinearMipmapNearest,
  // GL_NEAREST_MIPMAP_LINEAR
  tfNearestMipmapLinear,
  // GL_LINEAR_MIPMAP_LINEAR
  tfLinearMipmapLinear
  );

// The texture minifying function is used whenever the pixel being textured maps
// to an area greater than one texture element.
TPHXTextureFilterMin = tfNearest .. tfLinearMipmapLinear;
// The texture magnification function is used when the pixel being textured maps
// to an area less than or equal to one texture element
TPHXTextureFilterMag = tfNearest .. tfLinear;

// Specifies the type of the texture wrapping to use.
//------------------------------------------------------------------------------
TPHXTextureWrap = (
  // GL_REPEAT
  twRepeat,
  // GL_CLAMP
  twClamp
 );

    // Generate mipmaps for the texture
    property Mipmaps: Boolean read FMipmaps write FMipmaps;
    // Texture wrap
    property WrapS: TPHXTextureWrap read FWrapS write FWrapS;
    // Texture wrap
    property WrapT: TPHXTextureWrap read FWrapT write FWrapT;
    // Texure minifying function
    property FilterMin: TPHXTextureFilterMin read FFilterMin write FFilterMin;
    // magnification minifying function
    property FilterMag: TPHXTextureFilterMag read FFilterMag write FFilterMag;
*)

//------------------------------------------------------------------------------
function TextureWrapToString(const Value: TPHXTextureWrap): String;
begin
  case Value of
    twRepeat: Result:='Repeat';
    twClamp : Result:='Clamp';
  end;
end;

//------------------------------------------------------------------------------
function StringToTextureWrap(const Value: String): TPHXTextureWrap;
var Index: TPHXTextureWrap;
begin
  for Index := Low(TPHXTextureWrap) to High(TPHXTextureWrap) do
  begin
    if SameText(Value, TextureWrapToString(Index)) then
    begin
      Result:= Index;
      Exit;
    end;

  end;
  Result:= twRepeat;
end;

//------------------------------------------------------------------------------
function TextureFilterToString(const Value: TPHXTextureFilter): String;
begin
  case Value of
    tfNearest             : Result:='Nearest';
    tfLinear              : Result:='Linear';
    tfNearestMipmapNearest: Result:='NearestMipmapNearest';
    tfNearestMipmapLinear : Result:='NearestMipmapLinear';
    tfLinearMipmapNearest : Result:='LinearMipmapNearest';
    tfLinearMipmapLinear  : Result:='LinearMipmapLinear';
  end;
end;

//------------------------------------------------------------------------------
function StringToTextureFilter(const Value: String): TPHXTextureFilter;
var TextureFilter: TPHXTextureFilter;
begin
  for TextureFilter := Low(TPHXTextureFilter) to High(TPHXTextureFilter) do
  begin
    if SameText(Value, TextureFilterToString(TextureFilter)) then
    begin
      Result:= TextureFilter;
      Exit;
    end;

  end;
  Result:= tfNearest;
end;


// http://gregs-blog.com/2008/01/17/opengl-texture-filter-parameters-explained/
//------------------------------------------------------------------------------
procedure TFrmTexture.FormCreate(Sender: TObject);
var TextureFilter: TPHXTextureFilter;
begin
  edFilterMin.Items.BeginUpdate;
  edFilterMin.Items.Clear;
  for TextureFilter := Low(TPHXTextureFilterMin) to High(TPHXTextureFilterMin) do
  begin
    edFilterMin.Items.Add( TextureFilterToString(TextureFilter));
  end;
  edFilterMin.Items.EndUpdate;

  edFilterMag.Items.BeginUpdate;
  edFilterMag.Items.Clear;
  for TextureFilter := Low(TPHXTextureFilterMag) to High(TPHXTextureFilterMag) do
  begin
    edFilterMag.Items.Add( TextureFilterToString(TextureFilter));
  end;
  edFilterMag.Items.EndUpdate;
end;

//------------------------------------------------------------------------------
function TFrmTexture.Execute(const Texture: TPHXTexture): Boolean;
begin
  SetTexture(Texture);

  if ShowModal = mrOk then
  begin
    FormToImage;

    Result:= True;
  end else
  begin
    Result:= False;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTexture.FormToImage;
begin
  Texture.Settings.Mipmaps:= cbMipmaps.Checked;

  Texture.Settings.WrapS:= StringToTextureWrap(edWrapS.Text);
  Texture.Settings.WrapS:= StringToTextureWrap(edWrapT.Text);

  Texture.Settings.FilterMin:= StringToTextureFilter(edFilterMin.Text);
  Texture.Settings.FilterMag:= StringToTextureFilter(edFilterMag.Text);

end;

//------------------------------------------------------------------------------
procedure TFrmTexture.ImageToForm;
begin
  cbMipmaps.Checked:= Texture.Settings.Mipmaps;

  edWrapS.Text:= TextureWrapToString(Texture.Settings.WrapS);
  edWrapT.Text:= TextureWrapToString(Texture.Settings.WrapS);

  edFilterMin.Text:= TextureFilterToString(Texture.Settings.FilterMin);
  edFilterMag.Text:= TextureFilterToString(Texture.Settings.FilterMag);
end;

//------------------------------------------------------------------------------
procedure TFrmTexture.SetTexture(const Value: TPHXTexture);
begin
  FTexture := Value;

  ImageToForm;
end;



end.
