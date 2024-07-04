unit uTextures;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, Menus,

  System.Generics.Collections,

  phxDevice,
  phxTexture,
  phxGraphics,
  phxGraphicsEx,

  uActions;

type
  TFrmTextures = class(TForm)
    lwTextures: TListView;
    Splitter1: TSplitter;
    GroupBox1: TGroupBox;
    PaintBox1: TPaintBox;
    PopupMenu1: TPopupMenu;
    Loadtexture1: TMenuItem;
    procedure PaintBox1Paint(Sender: TObject);
    procedure lwTexturesClick(Sender: TObject);
    procedure Loadtexture1Click(Sender: TObject);
    procedure lwTexturesKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    FTexture: TPHXTexture;
    FPreview: TBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure lwTexturesUpdate;

    Property Texture: TPHXTexture read FTexture write FTexture;
  end;


var FrmTextures: TFrmTextures;

implementation


{$R *.dfm}

//------------------------------------------------------------------------------
constructor TFrmTextures.Create(AOwner: TComponent);
begin
  inherited;
  FPreview:= TBitmap.Create;
end;

//------------------------------------------------------------------------------
destructor TFrmTextures.Destroy;
begin
  FPreview.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TFrmTextures.FormShow(Sender: TObject);
begin
  lwTexturesUpdate;
end;
//------------------------------------------------------------------------------
procedure TFrmTextures.lwTexturesClick(Sender: TObject);
var Index: Integer;
begin
  Index:= lwTextures.ItemIndex;

  if (Index >= 0) and (Index < ModActions.Textures.Count) then
  begin
    FTexture:= ModActions.Textures[ lwTextures.Items[Index].Caption ];

    FTexture.ToBitmap(FPreview);

 //   phxGraphicUtils.DrawTexture(FTexture, FPreview);
  end else
  begin
    FPreview.Width:= 0;
    FPreview.Height:= 0;
  end;

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmTextures.lwTexturesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  lwTexturesClick(Sender);
end;

//------------------------------------------------------------------------------
procedure TFrmTextures.lwTexturesUpdate;
var Item   : TListItem;
var Name: String;
begin
  lwTextures.Items.BeginUpdate;
  lwTextures.Items.Clear;
  for Name in ModActions.Textures.Keys do
  begin
    Item:= lwTextures.Items.Add;
    Item.Caption:= Name;
  end;
  lwTextures.Items.EndUpdate;
end;

//------------------------------------------------------------------------------
procedure TFrmTextures.PaintBox1Paint(Sender: TObject);
var Rect: TRect;
var ScaleX: Single;
var ScaleY: Single;
var X,Y: Integer;
var W,H: Integer;
begin
  with PaintBox1.Canvas do
  begin
    Brush.Color:= clWhite;

    FillRect(ClipRect);

    if (FPreview.Width > 0) and (FPreview.Height > 0) then
    begin
      ScaleX:= PaintBox1.Width  / FPreview.Width;
      ScaleY:= PaintBox1.Height / FPreview.Height;

      if ScaleX <= ScaleY then
      begin
        W:= Round(FPreview.Width  * ScaleX) - 8;
        H:= Round(FPreview.Height * ScaleX) - 8;
      end else
      begin
        W:= Round(FPreview.Width  * ScaleY) - 8;
        H:= Round(FPreview.Height * ScaleY) - 8;
      end;
      X:= (PaintBox1.Width  - W) div 2;
      Y:= (PaintBox1.Height - H) div 2;

      Rect.Left  := X;
      Rect.Top   := Y;
      Rect.Right := X + W;
      Rect.Bottom:= Y + H;

      StretchDraw(Rect, FPreview);

    end;

  end;
end;


//------------------------------------------------------------------------------
procedure TFrmTextures.Loadtexture1Click(Sender: TObject);
begin
  ModActions.actToolTextureLoad.Execute;
end;



end.
