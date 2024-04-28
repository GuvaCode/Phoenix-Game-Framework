unit uImage.Patterns;

{$MODE Delphi}

interface

uses
  Messages, SysUtils, Variants, Classes, Types,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, Math,

  Generics.Collections,

  phxTypes,
  phxMath,
  phxImage,
  phxImageEx,

  phxGraphicsEx;

const PATTERN_VSPACE = 4;
const PATTERN_HSPACE = 4;

type

//------------------------------------------------------------------------------
TPHXImageViewType = (
  vtImage,
  vtPatterns
  );

//------------------------------------------------------------------------------
TFrmPatterns = class(TFrame)
    ScrollBox1: TScrollBox;
    PaintBox1: TPaintBox;
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ScrollBox1Resize(Sender: TObject);
  private
    FImage: TPHXImage;
    FBuffer    : TBitmap;
    FBackground: TBitmap;

    FPatternRects: TList<TRect>;

//    FGridSize: TSizei;
    FPatternIndex: Integer;

    //
    procedure PlacePatterns;

    //procedure CalculateGridSize;

   // function GetColumnCount: Integer;
   // function GetRowCount: Integer;
    function GetPatternName: String;

    procedure SetImage(const Value: TPHXImage);
    procedure SetPatternIndex(const Value: Integer);
    function GetOnPatternDblClick: TNotifyEvent;
    procedure SetOnPatternDblClick(const Value: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // The image
    Property Image: TPHXImage read FImage write SetImage;

    property PatternIndex: Integer read FPatternIndex write SetPatternIndex;
    property PatternName : String  read GetPatternName;

    property OnPatternDblClick: TNotifyEvent read GetOnPatternDblClick write SetOnPatternDblClick;

   // property ColumnWidth: Integer read FGridSize.Width write FGridSize.Width;
   // property ColumnCount: Integer read GetColumnCount;

  //  property RowHeight: Integer read FGridSize.Height write FGridSize.Height;
   // property RowCount: Integer read GetRowCount;
  end;

implementation
uses LCLIntf, LCLType;
{$R *.lfm}

//TFrmPatterns
//------------------------------------------------------------------------------
constructor TFrmPatterns.Create(AOwner: TComponent);
begin
  inherited;
  FBuffer    := TBitmap.Create;
  FBackground:= CreateTransparentImage(4);

  FPatternRects:= TList<TRect>.Create;
end;
//------------------------------------------------------------------------------
destructor TFrmPatterns.Destroy;
begin
  FPatternRects.Free;

  FBuffer.Free;
  FBackground.Free;
  inherited;
end;
     {
//------------------------------------------------------------------------------
function TFrmPatterns.GetColumnCount: Integer;
begin
  Result:= (PaintBox1.ClientWidth - PATTERN_HSPACE) div (ColumnWidth + PATTERN_HSPACE);
end;

//------------------------------------------------------------------------------
function TFrmPatterns.GetRowCount: Integer;
begin
  if Assigned(Image) then
  begin
    Result:= (Image.Patterns.Count div GetColumnCount);
  end else
  begin
    Result:= 0;
  end;
end;    }

//------------------------------------------------------------------------------
function TFrmPatterns.GetPatternName: String;
begin
  if Assigned(FImage) and (PatternIndex >= 0) and (PatternIndex < Image.Patterns.Count) then
  begin
    Result:= String( Image.Patterns[PatternIndex].Name );
  end else
  begin
    Result:= '';
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatterns.PlacePatterns;
var Index      : Integer;
var Pattern    : TPHXPattern;
var X, Y       : Integer;
var Rect       : TRect;
var Row        : Integer;
var W,H    : Integer;
begin
  FPatternRects.Clear;

  X  := PATTERN_HSPACE div 2;
  Y  := PATTERN_VSPACE div 2;
  Row:= 0;
  for Index := 0 to Image.Patterns.Count - 1 do
  begin
    Pattern:= Image.Patterns.List^[Index];

    W:= Max( PaintBox1.Canvas.TextWidth( String(Pattern.Name) ), Pattern.Width) + PATTERN_HSPACE;
    H:= Pattern.Height + PaintBox1.Canvas.TextHeight( String(Pattern.Name) );

    // Move to the next line
    if (X + W + GetSystemMetrics(SM_CXVSCROLL) > ScrollBox1.ClientWidth) then
    begin
      X:= PATTERN_HSPACE div 2;
      Y:= Y + Row + PATTERN_VSPACE;
      Row:= 0;
    end;

    Rect.Left  := X;
    Rect.Top   := Y;
    Rect.Right := X + W;
    Rect.Bottom:= Y + H;

    FPatternRects.Add(Rect);
    Row:= Max(Row, H);
    X:= X + W + PATTERN_VSPACE;
  end;
  PaintBox1.Height:= Y + Row + PATTERN_VSPACE;
  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmPatterns.SetImage(const Value: TPHXImage);
begin
  FImage := Value;

  if Assigned(FImage) then
  begin
    Image.Draw(FBuffer, FBackground);
    // Image.Draw(FBuffer, Graphics.ColorToRGB(clWindow));
    //CalculateGridSize;

    PlacePatterns;
  end;

  PaintBox1.Invalidate;
end;


//------------------------------------------------------------------------------
procedure TFrmPatterns.SetPatternIndex(const Value: Integer);
begin
  if FPatternIndex <> Value then
  begin
    FPatternIndex := Value;

    PaintBox1.Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatterns.ScrollBox1Resize(Sender: TObject);
begin
  if Assigned(FImage) then
  begin
   // CalculateGridSize;

    PlacePatterns;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmPatterns.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Index: Integer;
begin
  inherited;

  if Button = TMouseButton.mbLeft then
  begin
    for Index := 0 to FPatternRects.Count - 1 do
    begin
      if PtInRect(FPatternRects[Index], Point(X,Y)) then
      begin
        SetPatternIndex(Index);

        Exit;
      end;

    end;
    SetPatternIndex(-1);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatterns.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if (PatternIndex >= 0) and (ssLeft in Shift) then
  begin
    PaintBox1.BeginDrag(True);
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmPatterns.PaintBox1Paint(Sender: TObject);
var Index      : Integer;
var Pattern    : TPHXPattern;
var Rect       : TRect;
var X,Y        : Integer;
var Name       : String;
begin
  with PaintBox1.Canvas do
  begin
    Font.Style:= [fsBold];

    Brush.Style:= bsClear;

    if Assigned(Image) then
    begin

      if Image.Patterns.Count <> FPatternRects.Count then
      begin
        PlacePatterns;
      end;

      for Index := 0 to Image.Patterns.Count - 1 do
      begin
        Pattern:= Image.Patterns.List^[Index];
        Rect   := FPatternRects[Index];

        Name:= String(Pattern.Name);

        //W:= Rect.Right- Rect.Left;
        //H:= Rect.Width- Rect.Left;

        TextOut(Rect.Left + (Rect.Width - TextWidth(Name)) div 2, Rect.Top, Name);

        // Draw the pattern image
        X:= Rect.Left + (Rect.Width  - Pattern.Width                    ) div 2;
        Y:= Rect.Top  + (Rect.Height - Pattern.Height - TextHeight(Name)) div 2 + TextHeight(Name);

        Image.DrawPattern(PaintBox1.Canvas, FBuffer, X, Y, Index);

        if Index = FPatternIndex then
        begin
          Pen.Color:= clRed;

          Rectangle(Rect.Left - 1, Rect.Top - 1, Rect.Right + 1, Rect.Bottom + 1);
        end;
      end;
    end;
  end;
end;

function TFrmPatterns.GetOnPatternDblClick: TNotifyEvent;
begin
  Result:= PaintBox1.OnDblClick;
end;

procedure TFrmPatterns.SetOnPatternDblClick(const Value: TNotifyEvent);
begin
  PaintBox1.OnDblClick:= value;
end;





end.
