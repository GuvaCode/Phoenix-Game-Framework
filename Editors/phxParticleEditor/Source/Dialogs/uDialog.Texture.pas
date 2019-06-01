unit uDialog.Texture;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  Generics.Collections,

  phxTexture,

  phxGraphics,
  phxGraphicsEx;

type

//------------------------------------------------------------------------------
TTextureDialog = class(TForm)
    PaintBox1: TPaintBox;
    ScrollBar1: TScrollBar;
    Panel1: TPanel;
    Label1: TLabel;
    btnOkey: TButton;
    btnCancel: TButton;
    cbTransparent: TCheckBox;
    OpenImageDialog: TOpenDialog;
  private
    FTextures: TList<TPHXTexture>;
    FSelected: String;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute: Boolean;

    property Textures: TList<TPHXTexture> read FTextures write FTextures;
    property Selected: String read FSelected write FSelected;
  end;

var
  TextureDialog: TTextureDialog;

implementation

{$R *.dfm}

// TTextureDialog
//==============================================================================
constructor TTextureDialog.Create(AOwner: TComponent);
var Index: Integer;
var List : TPHXGraphicFormats;
var Filter: String;
begin
  inherited;
  List:= GraphicFormats;

  Filter:= 'All supported texture formats|';
  for Index := 0 to List.Count - 1 do
  begin
    Filter:=Filter + ';*' + String(List.Items[Index].Extension);
  end;
  Filter:= Filter + '|';

  for Index := 0 to List.Count - 1 do
  begin
    Filter:=Filter + Format('%s|*.%s|', [List.Items[Index].Extension, List.Items[Index].Extension]);
  end;

  OpenImageDialog.Filter:= Filter;
end;

//------------------------------------------------------------------------------
destructor TTextureDialog.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
function TTextureDialog.Execute: Boolean;
begin

end;

//------------------------------------------------------------------------------
end.
