unit phxColorEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  Vcl.ExtCtrls, Vcl.StdCtrls,

  Generics.Collections,

  phxTypes;


type

TPHXColorEditor = class(TGraphicControl)
  private
    FSelected: TColor4f;
    FButtonDown : Boolean;
    FButtonHover: Boolean;
    FOnChanged: TNotifyEvent;

    procedure SetSelected(const Value: TColor4f);
    procedure SetSelectedRed(const Value: Single);
    procedure SetSelectedAlpha(const Value: Single);
    procedure SetSelectedBlue(const Value: Single);
    procedure SetSelectedGreen(const Value: Single);
    function GetButtonRect: TRect;
    { Private declarations }
  protected
    procedure Paint; override;


    procedure DoChanged;
    
    procedure ButtonPressed;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;

    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;

    Property ButtonRect: TRect read GetButtonRect;
  public
    constructor Create(AOwner: TComponent); override;

    { Public declarations }
    Property Selected: TColor4f read FSelected write SetSelected;

    property Color;
  published
    property Anchors;

    Property Red: Single read FSelected.Red write SetSelectedRed;
    Property Green: Single read FSelected.Green write SetSelectedGreen;
    Property Blue: Single read FSelected.Blue write SetSelectedBlue;
    Property Alpha: Single read FSelected.Alpha write SetSelectedAlpha;

    Property OnChange: TNotifyEvent read FOnChanged write FOnChanged;
  end;

THLSColorPanel = class;


TfrmColorDialog = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    cbRed: TScrollBar;
    cbGreen: TScrollBar;
    cbBlue: TScrollBar;
    cbAlpha: TScrollBar;
    GroupBox2: TGroupBox;
    btnOkey: TButton;
    btnCancel: TButton;
    cbStandardColors: TColorBox;
    Panel1: TPanel;
    edRed: TEdit;
    edGreen: TEdit;
    edBlue: TEdit;
    edAlpha: TEdit;
    GroupBox3: TGroupBox;
    edScriptColor: TEdit;
    procedure JvFullColorPanel1ColorChange(Sender: TObject);
    procedure cbAlphaChange(Sender: TObject);
    procedure JvColorComboBox1Change(Sender: TObject);
    procedure edRedChange(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure cbStandardColorsGetColors(Sender: TCustomColorBox;
      Items: TStrings);
    procedure btnOkeyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FColor: TColor4f;
    FColorPanel: THLSColorPanel;

    procedure SetColor(const R,G,B, A: Byte); overload;

    Procedure BindInputEvents(Enabled: Boolean);
    procedure SetColor(const Value: TColor4f); overload;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    Property Color: TColor4f read FColor write SetColor;
  end;

THLSColorPanel = class(TGraphicControl)
  private
    FBuffer: TBitmap;
  //  FWidth: Integer;
  //  FHeight: Integer;
 //   procedure SetHeight(const Value: Integer);
  //  procedure SetWidth(const Value: Integer);

    procedure ResizeBuffer;
    procedure DrawBuffer;


    function ConvertFromColor(AColor: TColor): Cardinal;
    function ConvertToColor(AColor: Cardinal): TColor;
//    procedure SetHeight(const Value: Integer);
//    procedure SetWidth(const Value: Integer);
  protected
    procedure Paint; override;
    procedure Resize; override;

    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;  Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  //  property Width: Integer read FWidth write SetWidth;
  //  property Height: Integer read FHeight write SetHeight;
  end;


var
  frmColorDialog: TfrmColorDialog;


procedure Register;

implementation

{$R *.dfm}

uses Themes;

procedure Register;
begin
  RegisterComponents('Phoenix', [TPHXColorEditor]);
end;

const COLOR_TRANSPARENT_1 = clWhite;
const COLOR_TRANSPARENT_2 = clSilver;

procedure PaintColor(canvas: TCanvas; Color: TColor4f; R: TRect);
var IX, IY: Integer;
var PX, PY: IntegeR;
var ColorA: TColor;
var ColorB: TColor;
var Position: Single;
var ARect: TRect;

const TransparentSize = 4;
begin

  ColorA:= RGB(
      Round(Color.Red   * Color.Alpha * 255 + GetRValue(COLOR_TRANSPARENT_1) * ( 1- Color.Alpha)),
      Round(Color.Green * Color.Alpha * 255 + GetGValue(COLOR_TRANSPARENT_1) * ( 1- Color.Alpha)),
      Round(Color.Blue  * Color.Alpha * 255 + GetBValue(COLOR_TRANSPARENT_1) * ( 1- Color.Alpha))
      )
      ;
  ColorB:= RGB(
      Round(Color.Red   * Color.Alpha * 255 + GetRValue(COLOR_TRANSPARENT_2) * ( 1- Color.Alpha)),
      Round(Color.Green * Color.Alpha * 255 + GetGValue(COLOR_TRANSPARENT_2) * ( 1- Color.Alpha)),
      Round(Color.Blue  * Color.Alpha * 255 + GetBValue(COLOR_TRANSPARENT_2) * ( 1- Color.Alpha))
      )
      ;

  Canvas.Brush.Style:= bsSolid;

  IY:= 0;
  PY:= R.Top;
  while PY < R.Bottom do
  begin
    IX:= 0;
    PX:= R.Left;
    while PX < R.Right do
    begin
      Position:= PX / (R.Right - R.Left);{
  ColorA:= RGB(
      Round(Red   * Position * 255) + Round(GetRValue(COLOR_TRANSPARENT_1) * ( 1- Position)),
      Round(Green * Position * 255) + Round(GetGValue(COLOR_TRANSPARENT_1) * ( 1- Position)),
      Round(Blue  * Position * 255) + Round(GetBValue(COLOR_TRANSPARENT_1) * ( 1- Position))
      )
      ;
  ColorB:= RGB(
      Round(Red   * Position * 255) + Round(GetRValue(COLOR_TRANSPARENT_2) * ( 1- Position)),
      Round(Green * Position * 255) + Round(GetGValue(COLOR_TRANSPARENT_2) * ( 1- Position)),
      Round(Blue  * Position * 255) + Round(GetBValue(COLOR_TRANSPARENT_2) * ( 1- Position))
      )
      ;      }
      if (IX + IY) mod 2 = 0 then
      begin
        Canvas.Brush.Color:= ColorA;
      end else
      begin
        Canvas.Brush.Color:= ColorB;
      end;

      ARect:= Rect(PX, PY, PX + TransparentSize, PY + TransparentSize);

      if ARect.Right  > R.Right  then ARect.Right := R.Right;
      if ARect.Bottom > R.Bottom then ARect.Bottom:= R.Bottom;


      Canvas.FillRect(ARect);

      Inc(PX, TransparentSize);
      Inc(IX);
    end;
    Inc(PY, TransparentSize);
    Inc(IY);
  end;
  {
  Canvas.Brush.Style:= bsClear;

  Canvas.Pen.Color:= clBlack;

  Canvas.Rectangle(0,0, Width, Height);

  Canvas.Pen.Color:= clWhite;

  Canvas.Rectangle(1,1, Width-1, Height-1);
  Canvas.Rectangle(2,2, Width-2, Height-2);
  }


end;

{ TPHXColorEditor }


constructor TPHXColorEditor.Create(AOwner: TComponent);
begin
  inherited;
 // FSelected:= clrWhite;
end;

//------------------------------------------------------------------------------
procedure TPHXColorEditor.DoChanged;
begin
  if Assigned(OnChange) then OnChange(Self);
end;

//------------------------------------------------------------------------------
procedure TPHXColorEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  FButtonDown:= PtInRect(ButtonRect, Point(X,Y)) and (Button = TMouseButton.mbLeft);

  Invalidate;
end;
//------------------------------------------------------------------------------
procedure TPHXColorEditor.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  FButtonHover:= PtInRect(ButtonRect, Point(X,Y));

  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPHXColorEditor.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FButtonDown:= False;

  if PtInRect(ButtonRect, Point(X,Y)) then ButtonPressed;


  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPHXColorEditor.Paint;
var
  Details: TThemedElementDetails;
  R: TRect;
  BtnWidth, BtnHeight: Integer;
begin
 if ThemeServices.ThemesEnabled then
  begin
    Details := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal);
    Details := ThemeServices.GetElementDetails(teEditRoot);

    ThemeServices.DrawElement(Canvas.Handle, Details,   ClientRect);

    if Enabled then
    begin
      if FButtonHover then
      begin
        if FButtonDown then
        begin
          Details := ThemeServices.GetElementDetails(tbPushButtonPressed);
        end else
        begin
          Details := ThemeServices.GetElementDetails(tbPushButtonHot);
        end;
      end else
      begin
        Details := ThemeServices.GetElementDetails(tbPushButtonNormal);
      end;
    end else
    begin
      Details := ThemeServices.GetElementDetails(tbPushButtonDisabled);
    end;

    R := Bounds(Width - Height+1, 1, Height-2, Height-2);

    ThemeServices.DrawElement(Canvas.Handle, Details,   R);
  end else
  begin
    BtnWidth := GetSystemMetrics(SM_CXMENUCHECK);
    BtnHeight := GetSystemMetrics(SM_CYMENUCHECK);
    R := ClientRect;

    DrawFrameControl(Canvas.Handle, R, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_CHECKED);
  end;

  R := Bounds(4, 4, Width-Height-6, Height-8);

  PaintColor(Canvas, Selected, R);

  Canvas.Brush.Style:= bsClear;
  Canvas.Pen.Color:= clBlack;

  R := Bounds(3, 3, Width-Height-4, Height-6);

  Canvas.Rectangle( R.Left,  R.Top,  R.Right,  R.Bottom);
end;

//------------------------------------------------------------------------------
procedure TPHXColorEditor.ButtonPressed;
var Dialog: TfrmColorDialog;
begin
  Dialog:= TfrmColorDialog.Create(Self);
  try
    Dialog.Color:= Selected;
    if Dialog.ShowModal = mrOk then
    begin
      Selected:= Dialog.Color;

      DoChanged;

      Invalidate;
    end;
  finally
    Dialog.Free;
  end;

end;
//------------------------------------------------------------------------------
procedure TPHXColorEditor.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
   Message.Result:= 1;
end;


//------------------------------------------------------------------------------
function TPHXColorEditor.GetButtonRect: TRect;
begin
  Result := Bounds(Width - Height+1, 1, Height-2, Height-2);
end;

//------------------------------------------------------------------------------
procedure TPHXColorEditor.SetSelected(const Value: TColor4f);
begin
  FSelected := Value;


  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPHXColorEditor.SetSelectedAlpha(const Value: Single);
begin
  FSelected.Alpha := Value;

  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPHXColorEditor.SetSelectedBlue(const Value: Single);
begin
  FSelected.Blue := Value;

  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPHXColorEditor.SetSelectedGreen(const Value: Single);
begin
  FSelected.Green := Value;

  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPHXColorEditor.SetSelectedRed(const Value: Single);
begin
  FSelected.Red := Value;

  Invalidate;
end;



var  SelectedColors: TList<TColor>;




// TfrmColorDialog
//==============================================================================
constructor TfrmColorDialog.Create(AOwner: TComponent);
begin
  inherited;
  FColorPanel:= THLSColorPanel.Create(Self);
//  FColorPanel.Width := PaintBox1.Width;
//  FColorPanel.Height:= PaintBox1.Height;
  FColorPanel.Parent:= Panel1;
  FColorPanel.Align := alClient;
  FColorPanel.OnMouseDown:= PaintBox1MouseDown;
  FColorPanel.OnMouseMove:= PaintBox1MouseMove;
end;

//------------------------------------------------------------------------------
destructor TfrmColorDialog.Destroy;
begin
//  FColorPanel.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TfrmColorDialog.BindInputEvents(Enabled: Boolean);
begin
  if Enabled then
  begin
    cbRed  .OnChange:= cbAlphaChange;
    cbGreen.OnChange:= cbAlphaChange;
    cbBlue .OnChange:= cbAlphaChange;
    cbAlpha.OnChange:= cbAlphaChange;

    edRed  .OnChange:= edRedChange;
    edGreen.OnChange:= edRedChange;
    edBlue .OnChange:= edRedChange;
    edAlpha.OnChange:= edRedChange;

  //  JvFullColorPanel1.OnColorChange:= JvFullColorPanel1ColorChange;

    cbStandardColors.OnChange:= JvColorComboBox1Change;
  end else
  begin
    cbRed  .OnChange:= nil;
    cbGreen.OnChange:= nil;
    cbBlue .OnChange:= nil;
    cbAlpha.OnChange:= nil;

    edRed  .OnChange:= nil;
    edGreen.OnChange:= nil;
    edBlue .OnChange:= nil;
    edAlpha.OnChange:= nil;

  // JvFullColorPanel1.OnColorChange:= nil;

    cbStandardColors.OnChange:= nil;
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmColorDialog.JvFullColorPanel1ColorChange(Sender: TObject);
var Color: TColor;
var R,G,B: Byte;
begin
 // Color:= JvFullColorPanel1.ColorSpace.ConvertToColor(JvFullColorPanel1.FullColor);

  R:= GetRValue(Color);
  G:= GetGValue(Color);
  B:= GetBValue(Color);

  SetColor(R,G,B, cbAlpha.Position);
end;


//------------------------------------------------------------------------------
procedure TfrmColorDialog.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var R,G,B: Byte;
var Color: TColor;
begin


  if (X < FColorPanel.FBuffer.Width) and (Y < FColorPanel.FBuffer.Height) then
  begin
    Color:= FColorPanel.FBuffer.Canvas.Pixels[X,Y];

    R:= GetRValue(Color);
    G:= GetGValue(Color);
    B:= GetBValue(Color);

    SetColor(R,G,B, cbAlpha.Position);
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmColorDialog.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var R,G,B: Byte;
var Color: TColor;
begin
  if (ssLeft in Shift) and (X < FColorPanel.FBuffer.Width) and (Y < FColorPanel.FBuffer.Height) then
  begin
    Color:= FColorPanel.FBuffer.Canvas.Pixels[X,Y];

    R:= GetRValue(Color);
    G:= GetGValue(Color);
    B:= GetBValue(Color);

    SetColor(R,G,B, cbAlpha.Position);
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmColorDialog.JvColorComboBox1Change(Sender: TObject);
var R,G,B: Byte;
var Color: TColor;
begin
  Color:= cbStandardColors.Selected;

  R:= GetRValue(Color);
  G:= GetGValue(Color);
  B:= GetBValue(Color);

  SetColor(R,G,B, cbAlpha.Position);
end;



//------------------------------------------------------------------------------
procedure TfrmColorDialog.cbAlphaChange(Sender: TObject);
var R,G,B,A: Byte;
begin

  R:= cbRed  .Position;
  G:= cbGreen.Position;
  B:= cbBlue .Position;
  A:= cbAlpha.Position;

  SetColor(R,G,B,A);
end;



//------------------------------------------------------------------------------
procedure TfrmColorDialog.cbStandardColorsGetColors(Sender: TCustomColorBox; Items: TStrings);
var Index: Integer;
var Color: TColor;
begin
  for Color in SelectedColors do
  begin
    Items.InsertObject(0, Vcl.Graphics.ColorToString(Color and $00FFFFFF), TObject(Color and $00FFFFFF));
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmColorDialog.edRedChange(Sender: TObject);
var R,G,B,A: Byte;
begin
  R:= StrToIntDef(edRed  .Text, 0);
  G:= StrToIntDef(edGreen.Text, 0);
  B:= StrToIntDef(edBlue .Text, 0);
  A:= StrToIntDef(edAlpha.Text, 0);

  SetColor(R,G,B,A);
end;

procedure TfrmColorDialog.FormCreate(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------
procedure TfrmColorDialog.btnOkeyClick(Sender: TObject);
var R,G,B: Byte;
var Color: TColor;
begin
  R:= StrToIntDef(edRed  .Text, 0);
  G:= StrToIntDef(edGreen.Text, 0);
  B:= StrToIntDef(edBlue .Text, 0);

  Color:= RGB(R,G,B);

  if not SelectedColors.Contains(Color) then
  begin
    SelectedColors.Add(Color);
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmColorDialog.SetColor(const R, G, B, A: Byte);
var Color: TColor;
begin
  Color:= RGB(R,G,B);

  FColor.Red   := R / 255;
  FColor.Green := G / 255;
  FColor.Blue  := B / 255;
  FColor.Alpha := A / 255;

  BindInputEvents(False);

  cbStandardColors.Selected:= Color;

  edScriptColor.Text:= '0x' + IntToHex(Color, 8);

  //  JvFullColorPanel1.FullColor:=  JvFullColorPanel1.ColorSpace.ConvertFromColor( Color );

//  JvFullColorPanel1.v

  cbRed  .Position:= R;
  cbGreen.Position:= G;
  cbBlue .Position:= B;
  cbAlpha.Position:= A;

  edRed  .Text:= IntToStr(R);
  edGreen.Text:= IntToStr(G);
  edBlue .Text:= IntToStr(B);
  edAlpha.Text:= IntToStr(A);

  BindInputEvents(True);
end;

//------------------------------------------------------------------------------
procedure TfrmColorDialog.SetColor(const Value: TColor4f);
var R,G,B,A: Byte;
begin
  FColor := Value;

  R:= Trunc(Value.Red   * 255);
  G:= Trunc(Value.Green * 255);
  B:= Trunc(Value.Blue  * 255);
  A:= Trunc(Value.Alpha * 255);

  SetColor(R,G,B,A);{

  BindInputEvents(False);

  JvFullColorPanel1.FullColor:=  JvFullColorPanel1.ColorSpace.ConvertFromColor( RGB(R,G,B) );

  cbRed  .Position:= R;
  cbGreen.Position:= G;
  cbBlue .Position:= B;
  cbAlpha.Position:= A;

  edRed  .Value:= R;
  edGreen.Value:= G;
  edBlue .Value:= B;
  edAlpha.Value:= A;

  BindInputEvents(True);
  }
end;

type
  TJvFullColorArray = array [0..MaxListSize - 1] of Cardinal;
  PJvFullColorArray = ^TJvFullColorArray;

  TJvAxisIndex = (
  axIndex0,
  axIndex1 ,
  axIndex2
  );

const
  HLS_MIN = 0;
  HLS_MAX = 240;

const RGB_MAX = 255;

function SetAxisValue(AColor: Cardinal; AAxis: TJvAxisIndex; NewValue: Byte): Cardinal;
begin
  case AAxis of
    axIndex0:
      AColor := (AColor and $FFFFFF00) or  NewValue;
    axIndex1:
      AColor := (AColor and $FFFF00FF) or (NewValue shl  8);
    axIndex2:
      AColor := (AColor and $FF00FFFF) or (NewValue shl 16);
  end;
  Result := AColor;
end;

function RGBToBGR(Value: Cardinal): Cardinal;
begin
  Result :=
   ((Value and $00FF0000) shr 16) or
    (Value and $0000FF00) or
   ((Value and $000000FF) shl 16);
end;

procedure SplitColorParts(AColor: Cardinal; var Part1, Part2, Part3: Integer);
begin
  Part1 :=  AColor         and $000000FF;
  Part2 := (AColor shr 8)  and $000000FF;
  Part3 := (AColor shr 16) and $000000FF;
end;

function JoinColorParts(const Part1, Part2, Part3: Integer): Cardinal;
begin
  Result :=
     (Part1 and $000000FF) or
    ((Part2 and $000000FF) shl 8) or
    ((Part3 and $000000FF) shl 16);
end;

 const
  HLS_MAX_HALF = HLS_MAX / 2.0;
  HLS_MAX_ONE_THIRD = HLS_MAX / 3.0;
  HLS_MAX_TWO_THIRDS = (HLS_MAX * 2.0) / 3.0;
  HLS_MAX_SIXTH = HLS_MAX / 6.0;
  HLS_MAX_TWELVETH = HLS_MAX / 12.0;

function ConvertFromColor(AColor: TColor): Cardinal;
var
  Hue, Lightness, Saturation: Double;
  Red, Green, Blue: Integer;
  ColorMax, ColorMin, ColorDiff, ColorSum: Double;
  RedDelta, GreenDelta, BlueDelta: Extended;
begin
  SplitColorParts(AColor, Red, Green, Blue);

  if Red > Green then
    ColorMax := Red
  else
    ColorMax := Green;
  if Blue > ColorMax then
    ColorMax := Blue;
  if Red < Green then
    ColorMin := Red
  else
    ColorMin := Green;
  if Blue < ColorMin then
    ColorMin := Blue;
  ColorDiff := ColorMax - ColorMin;
  ColorSum := ColorMax + ColorMin;

  Lightness := (ColorSum * HLS_MAX + RGB_MAX) / (2.0 * RGB_MAX);
  if ColorMax = ColorMin then
    AColor := (Round(Lightness) shl 8) or (2 * HLS_MAX div 3)
  else
  begin
    if Lightness <= HLS_MAX_HALF then
      Saturation := (ColorDiff * HLS_MAX + ColorSum / 2.0) / ColorSum
    else
      Saturation := (ColorDiff * HLS_MAX + ((2.0 * RGB_MAX - ColorMax - ColorMin) / 2.0)) /
        (2.0 * RGB_MAX - ColorMax - ColorMin);

    RedDelta := ((ColorMax - Red) * HLS_MAX_SIXTH + ColorDiff / 2.0) / ColorDiff;
    GreenDelta := ((ColorMax - Green) * HLS_MAX_SIXTH + ColorDiff / 2.0) / ColorDiff;
    BlueDelta := ((ColorMax - Blue) * HLS_MAX_SIXTH + ColorDiff / 2.0) / ColorDiff;

    if Red = ColorMax then
      Hue := BlueDelta - GreenDelta
    else
    if Green = ColorMax then
      Hue := HLS_MAX_ONE_THIRD + RedDelta - BlueDelta
    else
      Hue := 2.0 * HLS_MAX_ONE_THIRD + GreenDelta - RedDelta;

    if Hue < 0 then
      Hue := Hue + HLS_MAX;
    if Hue > HLS_MAX then
      Hue := Hue - HLS_MAX;

    AColor :=
      JoinColorParts(Cardinal(Round(Hue)), Cardinal(Round(Lightness)), Cardinal(Round(Saturation)));
  end;
  Result := AColor and $00FFFFFF;
end;

function ConvertToColor(AColor: Cardinal): TColor;
var
  Red, Green, Blue: Double;
  Magic1, Magic2: Double;
  Hue, Lightness, Saturation: Integer;

  function HueToRGB(Lightness, Saturation, Hue: Double): Integer;
  var
    ResultEx: Double;
  begin
    if Hue < 0 then
      Hue := Hue + HLS_MAX;
    if Hue > HLS_MAX then
      Hue := Hue - HLS_MAX;

    if Hue < HLS_MAX_SIXTH then
      ResultEx := Lightness + ((Saturation - Lightness) * Hue + HLS_MAX_TWELVETH) / HLS_MAX_SIXTH
    else
    if Hue < HLS_MAX_HALF then
      ResultEx := Saturation
    else
    if Hue < HLS_MAX_TWO_THIRDS then
      ResultEx := Lightness + ((Saturation - Lightness) * (HLS_MAX_TWO_THIRDS - Hue) + HLS_MAX_TWELVETH) / HLS_MAX_SIXTH
    else
      ResultEx := Lightness;
    Result := Round(ResultEx);
  end;

  function RoundColor(Value: Double): Integer;
  begin
    if Value > RGB_MAX then
      Result := RGB_MAX
    else
      Result := Round(Value);
  end;

begin
  SplitColorParts(AColor, Hue, Lightness, Saturation);

  if Saturation = 0 then
  begin
    Red := (Lightness * RGB_MAX) / HLS_MAX;
    Green := Red;
    Blue := Red;
  end
  else
  begin
    if Lightness <= HLS_MAX_HALF then
      Magic2 := (Lightness * (HLS_MAX + Saturation) + HLS_MAX_HALF) / HLS_MAX
    else
      Magic2 := Lightness + Saturation - ((Lightness * Saturation) + HLS_MAX_HALF) / HLS_MAX;

    Magic1 := 2 * Lightness - Magic2;

    Red := (HueToRGB(Magic1, Magic2, Hue + HLS_MAX_ONE_THIRD) * RGB_MAX + HLS_MAX_HALF) / HLS_MAX;
    Green := (HueToRGB(Magic1, Magic2, Hue) * RGB_MAX + HLS_MAX_HALF) / HLS_MAX;
    Blue := (HueToRGB(Magic1, Magic2, Hue - HLS_MAX_ONE_THIRD) * RGB_MAX + HLS_MAX_HALF) / HLS_MAX;
  end;

  Result :=  JoinColorParts(RoundColor(Red), RoundColor(Green), RoundColor(Blue))  and $00FFFFFF;
end;








// THLSColorPanel
//------------------------------------------------------------------------------
constructor THLSColorPanel.Create(AOwner: TComponent);
begin
  inherited;
  FBuffer:= TBitmap.Create;
end;

//------------------------------------------------------------------------------
destructor THLSColorPanel.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure THLSColorPanel.Resize;
begin
  inherited;
  ResizeBuffer;
end;

//------------------------------------------------------------------------------
procedure THLSColorPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var R,G,B: Byte;
var Color: TColor;
begin
    inherited;
  if (X < FBuffer.Width) and (Y < FBuffer.Height) then
  begin
    Color:= FBuffer.Canvas.Pixels[X,Y];

    R:= GetRValue(Color);
    G:= GetGValue(Color);
    B:= GetBValue(Color);

   // SetColor(R,G,B, cbAlpha.Position);
  end;
end;

//------------------------------------------------------------------------------
procedure THLSColorPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var R,G,B: Byte;
var Color: TColor;
begin
  inherited;
  if (ssLeft in Shift) and (X < FBuffer.Width) and (Y < FBuffer.Height) then
  begin
    Color:= FBuffer.Canvas.Pixels[X,Y];

    R:= GetRValue(Color);
    G:= GetGValue(Color);
    B:= GetBValue(Color);

  //  SetColor(R,G,B, cbAlpha.Position);
  end;

end;

//------------------------------------------------------------------------------
procedure THLSColorPanel.ResizeBuffer;
begin
  FBuffer.Width      := Width;
  FBuffer.Height     := Height;
  FBuffer.PixelFormat:= pf32Bit;

  DrawBuffer;
end;

//------------------------------------------------------------------------------
procedure THLSColorPanel.DrawBuffer;
var
  AxisX, AxisY: TJvAxisIndex;
  IndexX, IndexY: Integer;
  MinX, MaxX, MinY, MaxY: Integer;
  RangeX, RangeY: Integer;
  TempColor: Cardinal;
  Line: PJvFullColorArray;
begin
  if (FBuffer.Width = 0) or (FBuffer.Height = 0) or (Width = 0) or (Height = 0) then
    Exit;

  AxisX := axIndex0;
  AxisY := axIndex1;

//  with ColorSpace do
//  begin
    MinX := HLS_MIN;
    MaxX := HLS_MAX;
    RangeX := HLS_MAX - HLS_MIN;
    MinY := HLS_MIN;
    MaxY := HLS_MAX;
    RangeY := HLS_MAX - HLS_MIN;


    TempColor := SetAxisValue(0, axIndex2, HLS_MAX);
    with FBuffer do
    begin
      Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(Rect(0, 0, Width-1, Height-1));
      for IndexY := 0 to Height-1 do
      begin
        Line := ScanLine[IndexY];

        TempColor := SetAxisValue(TempColor, AxisY, (RangeY * IndexY) div (Height - 1) + MinY);
        for IndexX := 0 to Width-1 do
        begin
          TempColor := SetAxisValue(TempColor, AxisX, (RangeX * IndexX) div (Width - 1) + MinX);
          // (outchy) don't remove, Bitmap colors are stocked as (MSB) 00RRGGBB (LSB)
          // Delphi TColor is (MSB) 00BBGGRR (LSB)
          Line[IndexX] := RGBToBGR(ConvertToColor(TempColor));
        end;
      end;
    end;
//  inherited DrawBuffer;
end;


procedure THLSColorPanel.Paint;
begin
  inherited;
  Canvas.Draw(0,0, FBuffer);
end;

//------------------------------------------------------------------------------
function THLSColorPanel.ConvertFromColor(AColor: TColor): Cardinal;
var
  Hue, Lightness, Saturation: Double;
  Red, Green, Blue: Integer;
  ColorMax, ColorMin, ColorDiff, ColorSum: Double;
  RedDelta, GreenDelta, BlueDelta: Extended;
begin
  SplitColorParts(AColor, Red, Green, Blue);

  if Red > Green then
    ColorMax := Red
  else
    ColorMax := Green;
  if Blue > ColorMax then
    ColorMax := Blue;
  if Red < Green then
    ColorMin := Red
  else
    ColorMin := Green;
  if Blue < ColorMin then
    ColorMin := Blue;
  ColorDiff := ColorMax - ColorMin;
  ColorSum := ColorMax + ColorMin;

  Lightness := (ColorSum * HLS_MAX + RGB_MAX) / (2.0 * RGB_MAX);
  if ColorMax = ColorMin then
    AColor := (Round(Lightness) shl 8) or (2 * HLS_MAX div 3)
  else
  begin
    if Lightness <= HLS_MAX_HALF then
      Saturation := (ColorDiff * HLS_MAX + ColorSum / 2.0) / ColorSum
    else
      Saturation := (ColorDiff * HLS_MAX + ((2.0 * RGB_MAX - ColorMax - ColorMin) / 2.0)) /
        (2.0 * RGB_MAX - ColorMax - ColorMin);

    RedDelta := ((ColorMax - Red) * HLS_MAX_SIXTH + ColorDiff / 2.0) / ColorDiff;
    GreenDelta := ((ColorMax - Green) * HLS_MAX_SIXTH + ColorDiff / 2.0) / ColorDiff;
    BlueDelta := ((ColorMax - Blue) * HLS_MAX_SIXTH + ColorDiff / 2.0) / ColorDiff;

    if Red = ColorMax then
      Hue := BlueDelta - GreenDelta
    else
    if Green = ColorMax then
      Hue := HLS_MAX_ONE_THIRD + RedDelta - BlueDelta
    else
      Hue := 2.0 * HLS_MAX_ONE_THIRD + GreenDelta - RedDelta;

    if Hue < 0 then
      Hue := Hue + HLS_MAX;
    if Hue > HLS_MAX then
      Hue := Hue - HLS_MAX;

    AColor :=
      JoinColorParts(Cardinal(Round(Hue)), Cardinal(Round(Lightness)), Cardinal(Round(Saturation)));
  end;
  Result := AColor and $00FFFFFF;
end;

//------------------------------------------------------------------------------
function THLSColorPanel.ConvertToColor(AColor: Cardinal): TColor;
var
  Red, Green, Blue: Double;
  Magic1, Magic2: Double;
  Hue, Lightness, Saturation: Integer;

  function HueToRGB(Lightness, Saturation, Hue: Double): Integer;
  var
    ResultEx: Double;
  begin
    if Hue < 0 then
      Hue := Hue + HLS_MAX;
    if Hue > HLS_MAX then
      Hue := Hue - HLS_MAX;

    if Hue < HLS_MAX_SIXTH then
      ResultEx := Lightness + ((Saturation - Lightness) * Hue + HLS_MAX_TWELVETH) / HLS_MAX_SIXTH
    else
    if Hue < HLS_MAX_HALF then
      ResultEx := Saturation
    else
    if Hue < HLS_MAX_TWO_THIRDS then
      ResultEx := Lightness + ((Saturation - Lightness) * (HLS_MAX_TWO_THIRDS - Hue) + HLS_MAX_TWELVETH) / HLS_MAX_SIXTH
    else
      ResultEx := Lightness;
    Result := Round(ResultEx);
  end;

  function RoundColor(Value: Double): Integer;
  begin
    if Value > RGB_MAX then
      Result := RGB_MAX
    else
      Result := Round(Value);
  end;

begin
  SplitColorParts(AColor, Hue, Lightness, Saturation);

  if Saturation = 0 then
  begin
    Red := (Lightness * RGB_MAX) / HLS_MAX;
    Green := Red;
    Blue := Red;
  end
  else
  begin
    if Lightness <= HLS_MAX_HALF then
      Magic2 := (Lightness * (HLS_MAX + Saturation) + HLS_MAX_HALF) / HLS_MAX
    else
      Magic2 := Lightness + Saturation - ((Lightness * Saturation) + HLS_MAX_HALF) / HLS_MAX;

    Magic1 := 2 * Lightness - Magic2;

    Red := (HueToRGB(Magic1, Magic2, Hue + HLS_MAX_ONE_THIRD) * RGB_MAX + HLS_MAX_HALF) / HLS_MAX;
    Green := (HueToRGB(Magic1, Magic2, Hue) * RGB_MAX + HLS_MAX_HALF) / HLS_MAX;
    Blue := (HueToRGB(Magic1, Magic2, Hue - HLS_MAX_ONE_THIRD) * RGB_MAX + HLS_MAX_HALF) / HLS_MAX;
  end;

  Result :=  JoinColorParts(RoundColor(Red), RoundColor(Green), RoundColor(Blue))  and $00FFFFFF;
end;
     {
//------------------------------------------------------------------------------
procedure THLSColorPanel.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;

    ResizeBuffer;
  end;
end;

//------------------------------------------------------------------------------
procedure THLSColorPanel.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;

    ResizeBuffer;
  end;
end;
}
initialization
  SelectedColors:= TList<TColor>.Create;
finalization
  SelectedColors.Free;
end.
