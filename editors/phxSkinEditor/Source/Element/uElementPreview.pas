unit uElementPreview;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ExtCtrls, ImgList, ComCtrls, ToolWin, StdCtrls, Menus,

  phxTypes,

  phxGraphics,
  phxGraphicsEx,

  phxSkin,
  phxSkinEx;

type
  TFrmElementPreview = class(TFrame)
    PaintBox1: TPaintBox;
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    cbShowSizingRegion: TToolButton;
    cbShowSizingMargins: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    PopupMenu1: TPopupMenu;
    btnShowMargins: TMenuItem;
    btnShowFixedMargins: TMenuItem;
    btnShowOuterMargins: TMenuItem;
    btnShowContentMargins: TMenuItem;
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,  Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;   Shift: TShiftState; X, Y: Integer);
    procedure cbShowSizingMarginsClick(Sender: TObject);
    procedure cbShowSizingRegionClick(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnShowMarginsClick(Sender: TObject);
    procedure btnShowFixedMarginsClick(Sender: TObject);
    procedure btnShowOuterMarginsClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure btnShowContentMarginsClick(Sender: TObject);
  private
    FSkin      : TPHXSkin;
    FElement   : TPHXSkinElement;
    FBuffer    : TBitmap;
    FBackground: TBitmap;
    FZoom      : Single;
    FOnChanged: TNotifyEvent;

    function GetDragArea(X,Y: Integer): Integer;

    procedure SetElement(const Value: TPHXSkinElement);
    procedure SetSkin(const Value: TPHXSkin);
    procedure SetZoom(const Value: Single);
  protected
    Margin : TRect;
//    Borders: TRect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // The current skin
    property Skin: TPHXSkin read FSkin write SetSkin;
    // The current element
    property Element: TPHXSkinElement read FElement write SetElement;
    // The zoom level
    property Zoom: Single read FZoom write SetZoom;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
constructor TFrmElementPreview.Create(AOwner: TComponent);
begin
  inherited;

  FBuffer:= TBitmap.Create;
  FZoom  := 5.0;

  FBackground:= CreateTransparentImage(4);
end;

//------------------------------------------------------------------------------
destructor TFrmElementPreview.Destroy;
begin
  FBuffer.Free;
  FBackground.Free;
  inherited;
end;



//------------------------------------------------------------------------------
procedure TFrmElementPreview.PaintBox1Paint(Sender: TObject);
var DstRect: TRect;
var SrcRect: TRect;
begin
  with PaintBox1.Canvas do
  begin
    Brush.Style:= bsSolid;
    Brush.Color:= clWhite;

   // FillRect(ClipRect);

    Pen.Color:= clSilver;
    Pen.Style:= psSolid;

    Rectangle(ClipRect);
  end;

  if Element = nil then
  begin
    Exit;
  end;

  // Calculate the destination rect for the texture
  DstRect.Left  := (PaintBox1.Width  -  Round(Element.Width  * Zoom)) div 2;
  DstRect.Top   := (PaintBox1.Height -  Round(Element.Height * Zoom)) div 2;

  DstRect.Right := DstRect.Left + Round(Element.Width  * Zoom);
  DstRect.Bottom:= DstRect.Top  + Round(Element.Height * Zoom);

  // Calculate the source rect for the texture
  SrcRect.Left  := Element.Bounds.Left;
  SrcRect.Top   := Element.Bounds.Top;
  SrcRect.Right := Element.Bounds.Right;
  SrcRect.Bottom:= Element.Bounds.Bottom;

  if btnShowMargins.Checked then
  begin
    Margin.Left  := DstRect.Left;
    Margin.Top   := DstRect.Top;
    Margin.Right := DstRect.Right;
    Margin.Bottom:= DstRect.Bottom;
  end;
  if btnShowFixedMargins.Checked then
  begin
    Margin.Left  := DstRect.Left   + Round(Element.Margins.Left   * Zoom);
    Margin.Top   := DstRect.Top    + Round(Element.Margins.Top    * Zoom);
    Margin.Right := DstRect.Right  - Round(Element.Margins.Right  * Zoom);
    Margin.Bottom:= DstRect.Bottom - Round(Element.Margins.Bottom * Zoom);
  end;
  if btnShowOuterMargins.Checked then
  begin
    Margin.Left  := DstRect.Left   - Round(Element.Shadow.Left   * Zoom);
    Margin.Top   := DstRect.Top    - Round(Element.Shadow.Top    * Zoom);
    Margin.Right := DstRect.Right  + Round(Element.Shadow.Right  * Zoom);
    Margin.Bottom:= DstRect.Bottom + Round(Element.Shadow.Bottom * Zoom);
  end;
  if btnShowContentMargins.Checked then
  begin
    Margin.Left  := DstRect.Left   + Round(Element.TextPadding.Left   * Zoom);
    Margin.Top   := DstRect.Top    + Round(Element.TextPadding.Top    * Zoom);
    Margin.Right := DstRect.Right  - Round(Element.TextPadding.Right  * Zoom);
    Margin.Bottom:= DstRect.Bottom - Round(Element.TextPadding.Bottom * Zoom);
  end;

  (*
  Margin.Left  := DstRect.Left   + Round(Element.FixedMargins.Left   * Zoom);
  Margin.Top   := DstRect.Top    + Round(Element.FixedMargins.Top    * Zoom);
  Margin.Right := DstRect.Right  - Round(Element.FixedMargins.Right  * Zoom);
  Margin.Bottom:= DstRect.Bottom - Round(Element.FixedMargins.Bottom * Zoom);

  Borders.Left  := DstRect.Left   - Round(Element.OuterMargins.Left   * Zoom);
  Borders.Top   := DstRect.Top    - Round(Element.OuterMargins.Top    * Zoom);
  Borders.Right := DstRect.Right  + Round(Element.OuterMargins.Right  * Zoom);
  Borders.Bottom:= DstRect.Bottom + Round(Element.OuterMargins.Bottom * Zoom);
  *)

  with PaintBox1.Canvas do
  begin
    CopyRect(DstRect, FBuffer.Canvas, SrcRect);
//    StretchDraw(Rect, FBuffer);

    Brush.Style:= bsClear;

    if cbShowSizingRegion.Down then
    begin
      // Draw sizing region
      Brush.Style:= bsDiagCross;
      Brush.Color:= $00FF901E;

      Pen.Style  := psClear;

      if Element.Margins.Left <> 0 then
      begin
        Rectangle(DstRect.Left, DstRect.Top, Margin.Left, DstRect.Bottom);
      end;
      if Element.Margins.Right <> 0 then
      begin
        Rectangle(Margin.Right, DstRect.Top, DstRect.Right, DstRect.Bottom);
      end;
      if Element.Margins.Top <> 0 then
      begin
        Rectangle(Margin.Left, DstRect.Top, Margin.Right, Margin.Top);
      end;
      if Element.Margins.Bottom <> 0 then
      begin
        Rectangle(Margin.Left, Margin.Bottom, Margin.Right, DstRect.Bottom);
      end;
    end;

    if cbShowSizingMargins.Down then
    begin
      // Draw margin lines
      Pen.Color  := clBlack;
      Pen.Style  := psDot;

      if Element.Margins.Left <> 0 then
      begin
        MoveTo(Margin.Left , 2);
        LineTo(Margin.Left , PaintBox1.Height-2);
      end;
      if Element.Margins.Right <> 0 then
      begin
        MoveTo(Margin.Right        , 2);
        LineTo(Margin.Right        , PaintBox1.Height-2);
      end;
      if Element.Margins.Top <> 0 then
      begin
        MoveTo(2                , Margin.Top);
        LineTo(PaintBox1.Width-2, Margin.Top);
      end;
      if Element.Margins.Bottom <> 0 then
      begin
        MoveTo(2                , Margin.Bottom);
        LineTo(PaintBox1.Width-2, Margin.Bottom);
      end;

      // Draw margin boxes
      Brush.Style:= bsSolid;
      Brush.Color:= clBlack;
      Pen.Style  := psSolid;

      if Element.Margins.Left <> 0 then
      begin
        Rectangle(Margin.Left - 1, 2                   , Margin.Left + 2, 2 + 3);
        Rectangle(Margin.Left - 1, PaintBox1.Height - 2, Margin.Left + 2, PaintBox1.Height - 2 - 3);
      end;
      if Element.Margins.Right <> 0 then
      begin
        Rectangle(Margin.Right - 1, 2                   , Margin.Right + 2, 2 + 3);
        Rectangle(Margin.Right - 1, PaintBox1.Height - 2, Margin.Right + 2, PaintBox1.Height - 2 - 3);
      end;

      if Element.Margins.Top <> 0 then
      begin
        Rectangle(2                  , Margin.Top-1,                   2 + 3, Margin.Top + 2);
        Rectangle(PaintBox1.Width - 2, Margin.Top-1, PaintBox1.Width - 2 - 3, Margin.Top + 2);
      end;
      if Element.Margins.Bottom <> 0 then
      begin
        Rectangle(2                  , Margin.Bottom-1,                   2 + 3, Margin.Bottom + 2);
        Rectangle(PaintBox1.Width - 2, Margin.Bottom-1, PaintBox1.Width - 2 - 3, Margin.Bottom + 2);
      end;
    end;

  end;
end;

const DRAG_BORDER_LEFT   = 5;
const DRAG_BORDER_RIGHT  = 6;
const DRAG_BORDER_TOP    = 7;
const DRAG_BORDER_BOTTOM = 8;

const DRAG_FIXED_MARGIN_LEFT   = 1;
const DRAG_FIXED_MARGIN_RIGHT  = 2;
const DRAG_FIXED_MARGIN_TOP    = 3;
const DRAG_FIXED_MARGIN_BOTTOM = 4;

//------------------------------------------------------------------------------
function TFrmElementPreview.GetDragArea(X, Y: Integer): Integer;
begin
  Result:= 0;
  if btnShowMargins.Checked then
  begin
    // Left margin
    if (Margin.Left  - 2 < X) and (Margin.Left  + 2 > X) then
    begin
      Result:= DRAG_BORDER_LEFT;
    end else
    // Right margin
    if (Margin.Right - 2 < X) and (Margin.Right + 2 > X) then
    begin
      Result:= DRAG_BORDER_RIGHT;
    end else
    // Top margin
    if (Margin.Top    - 2 < Y) and (Margin.Top    + 2 > Y) then
    begin
      Result:= DRAG_BORDER_TOP
    end else
    // Bottom margin
    if (Margin.Bottom - 2 < Y) and (Margin.Bottom + 2 > Y) then
    begin
      Result:= DRAG_BORDER_BOTTOM;
    end;

  end else
  if btnShowFixedMargins.Checked then
  begin
    // Left margin
    if (Margin.Left  - 2 < X) and (Margin.Left  + 2 > X) then
    begin
      Result:= DRAG_FIXED_MARGIN_LEFT;
    end else
    // Right margin
    if (Margin.Right - 2 < X) and (Margin.Right + 2 > X) then
    begin
      Result:= DRAG_FIXED_MARGIN_RIGHT;
    end else
    // Top margin
    if (Margin.Top    - 2 < Y) and (Margin.Top    + 2 > Y) then
    begin
      Result:= DRAG_FIXED_MARGIN_TOP
    end else
    // Bottom margin
    if (Margin.Bottom - 2 < Y) and (Margin.Bottom + 2 > Y) then
    begin
      Result:= DRAG_FIXED_MARGIN_BOTTOM;
    end;
  end else
  if btnShowOuterMargins.Checked then
  begin

  end;

end;




var DownArea: Integer;
var DownPos : TPoint;

//------------------------------------------------------------------------------
procedure TFrmElementPreview.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DownArea:= GetDragArea(X, Y);

  DownPos.X:= X;
  DownPos.Y:= Y;
end;

//------------------------------------------------------------------------------
procedure TFrmElementPreview.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var HoverArea: Integer;
var DeltaX: Integer;
var DeltaY: Integer;
begin
  if not Assigned(Element) then
  begin
    PaintBox1.Cursor:= crDefault;

    Exit;
  end;

  if ssLeft in Shift then
  begin
    DeltaX:= Round( (X - DownPos.X) / Zoom);
    DeltaY:= Round( (Y - DownPos.Y) / Zoom);

    case DownArea of
      // Left margin
      DRAG_FIXED_MARGIN_LEFT:
      begin
        Element.Margins:= TRecti.Create(
          Element.Margins.Left + DeltaX,
          Element.Margins.Top,
          Element.Margins.Right,
          Element.Margins.Bottom
        );
      end;
      // Right margin
      DRAG_FIXED_MARGIN_RIGHT:
      begin
        Element.Margins:= TRecti.Create(
          Element.Margins.Left,
          Element.Margins.Top,
          Element.Margins.Right - DeltaX,
          Element.Margins.Bottom
        );
      end;
      DRAG_FIXED_MARGIN_TOP:
      begin
        Element.Margins:= TRecti.Create(
          Element.Margins.Left,
          Element.Margins.Top + DeltaY,
          Element.Margins.Right,
          Element.Margins.Bottom
        );
      end;
      DRAG_FIXED_MARGIN_BOTTOM:
      begin
        Element.Margins:= TRecti.Create(
          Element.Margins.Left,
          Element.Margins.Top,
          Element.Margins.Right,
          Element.Margins.Bottom  - DeltaY
        );
      end;
      // Left margin
      DRAG_BORDER_LEFT:
      begin
        Element.Bounds:= TRecti.Create(
          Element.Bounds.Left + DeltaX,
          Element.Bounds.Top,
          Element.Bounds.Right,
          Element.Bounds.Bottom
        );
      end;
      // Right margin
      DRAG_BORDER_RIGHT:
      begin
        Element.Bounds:= TRecti.Create(
          Element.Bounds.Left,
          Element.Bounds.Top,
          Element.Bounds.Right - DeltaX,
          Element.Bounds.Bottom
        );
      end;
      DRAG_BORDER_TOP:
      begin
        Element.Bounds:= TRecti.Create(
          Element.Bounds.Left,
          Element.Bounds.Top + DeltaY,
          Element.Bounds.Right,
          Element.Bounds.Bottom
        );
      end;
      DRAG_BORDER_BOTTOM:
      begin
        Element.Bounds:= TRecti.Create(
          Element.Bounds.Left,
          Element.Bounds.Top,
          Element.Bounds.Right,
          Element.Bounds.Bottom  - DeltaY
        );
      end;
    end;
    PaintBox1.Invalidate;

    DownPos.X:= X;
    DownPos.Y:= Y;
  end else
  begin
    HoverArea:= GetDragArea(X, Y);

    case HoverArea of
      1..2, 5..6:
      begin
        PaintBox1.Cursor:= crSizeWE;
      end;
      3..4, 7..8:
      begin
        PaintBox1.Cursor:= crSizeNS;
      end;
      else
      begin
        PaintBox1.Cursor:= crDefault;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmElementPreview.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnChanged) then OnChanged(Self);
end;


//------------------------------------------------------------------------------
procedure TFrmElementPreview.FrameResize(Sender: TObject);
begin
  SetElement(Element);
end;

//------------------------------------------------------------------------------
procedure TFrmElementPreview.SetElement(const Value: TPHXSkinElement);
var ZoomX: Single;
var ZoomY: Single;
begin
  FElement := Value;

  if Assigned(FElement) then
  begin
    // Adjust zoom factor
    if (Element.Width > 0) and (Element.Height > 0) then
    begin
      ZoomX:= (PaintBox1.Width  - 20) / (Element.Width  + Element.Shadow.Left + Element.Shadow.Right);
      ZoomY:= (PaintBox1.Height - 20) / (Element.Height + Element.Shadow.Top  + Element.Shadow.Bottom);

      if ZoomX <= ZoomY then
      begin
        FZoom:= ZoomX;
      end else
      begin
        FZoom:= ZoomY;
      end;
    end;

   // TPHXSkinFactory.DefaultElements(ComboBox1.Items, ExtractControlName(Element.Name) );
  end;

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmElementPreview.SetSkin(const Value: TPHXSkin);
begin
  FSkin := Value;

  if Assigned(FSkin) then
  begin
    FSkin.Draw(FBuffer, FBackground);
  end;

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmElementPreview.SetZoom(const Value: Single);
begin
  FZoom := Value;

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmElementPreview.cbShowSizingMarginsClick(Sender: TObject);
begin
  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmElementPreview.cbShowSizingRegionClick(Sender: TObject);
begin
  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmElementPreview.btnShowMarginsClick(Sender: TObject);
begin
  btnShowMargins       .Checked:= True;
  btnShowFixedMargins  .Checked:= False;
  btnShowOuterMargins  .Checked:= False;
  btnShowContentMargins.Checked:= False;

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmElementPreview.btnShowFixedMarginsClick(Sender: TObject);
begin
  btnShowMargins       .Checked:= False;
  btnShowFixedMargins  .Checked:= True;
  btnShowOuterMargins  .Checked:= False;
  btnShowContentMargins.Checked:= False;


  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmElementPreview.btnShowOuterMarginsClick(Sender: TObject);
begin
  btnShowMargins       .Checked:= False;
  btnShowFixedMargins  .Checked:= False;
  btnShowOuterMargins  .Checked:= True;
  btnShowContentMargins.Checked:= False;

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmElementPreview.btnShowContentMarginsClick(Sender: TObject);
begin
  btnShowMargins       .Checked:= False;
  btnShowFixedMargins  .Checked:= False;
  btnShowOuterMargins  .Checked:= False;
  btnShowContentMargins.Checked:= True;

  PaintBox1.Invalidate;
end;





end.
