unit uAnimation.Frames;

interface

uses
  Messages, SysUtils, Variants, Classes, LCLType, Types,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, ToolWin, ImgList, ActnList,

  Generics.Collections,

  Math,

  phxTypes,

  phxGraphicsEx,

  phxImage,
  phxImageEx,

  uActions,

  uImage.Patterns,

  uAnimation.Frame;

type

//------------------------------------------------------------------------------

{ TFrmAnimationFrames }

TFrmAnimationFrames = class(TFrame)
    ListImages: TImageList;
    ToolBar1: TToolBar;
    btnFrameAdd: TToolButton;
    btnFrameDel: TToolButton;
    ToolButton1: TToolButton;
    btnFrameDec: TToolButton;
    btnFrameInc: TToolButton;
    ActionList1: TActionList;
    actFrameAdd: TAction;
    actFrameDel: TAction;
    ScrollBox1: TScrollBox;
    PaintBox1: TPaintBox;
    procedure FrameResize(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1DragOver(Sender, Source: TObject; X, Y: Integer;   State: TDragState; var Accept: Boolean);
    procedure PaintBox1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure actFrameAddExecute(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure actFrameDelExecute(Sender: TObject);
    procedure actFrameDelUpdate(Sender: TObject);

  private
    FAnimation : TPHXAnimation;
    FImage     : TPHXImage;
    FSelected  : Integer;
    FBuffer    : TBitmap;
    FBackground: TBitmap;
    FPatterns  : TFrmPatterns;

    FrameRects: TList<TRect>;
    FEditor: TFrmAnimationFrame;

//    ColumnWidth: Integer;

    procedure PlaceFrames;

//    function GetPatternWidth(const Frame: TPHXAnimationFrame): Integer;
   // function GetPatternHeight(const Frame: TPHXAnimationFrame): Integer;

    procedure DrawPattern(const Rect: TRect; const PatternIndex: Integer);

    procedure SetAnimation(const Value: TPHXAnimation);
    procedure SetSelected(const Value: Integer);
    procedure SetPatterns(const Value: TFrmPatterns);
    procedure SetEditor(const Value: TFrmAnimationFrame);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure KeyDown(var Key: Word; Shift: TShiftState);override;

    // The animation
    property Animation: TPHXAnimation read FAnimation write SetAnimation;
    property Image: TPHXImage read FImage;

    property Selected : Integer read FSelected write SetSelected;

    property Patterns: TFrmPatterns read FPatterns write SetPatterns;
    property Editor: TFrmAnimationFrame read FEditor write SetEditor;
  end;

implementation

{$R *.lfm}

// TFrmAnimationFrames
//==============================================================================
constructor TFrmAnimationFrames.Create(AOwner: TComponent);
begin
  inherited;
  FBuffer    := TBitmap.Create;
  FBackground:= CreateTransparentImage(4);

  FrameRects:= TList<TRect>.Create;

  ScrollBox1.DoubleBuffered:= True;

  with PaintBox1.Canvas do
  begin
    Font.Style := [fsBold];
  end;
end;

//------------------------------------------------------------------------------
destructor TFrmAnimationFrames.Destroy;
begin
  FBuffer.Free;
  FBackground.Free;

  FrameRects.Free;
  inherited;
end;


//------------------------------------------------------------------------------
procedure TFrmAnimationFrames.PlaceFrames;
var Index      : Integer;
var Frame      : TPHXAnimationFrame;
var Pattern    : TPHXPattern;
var X, Y       : Integer;
var Row        : Integer;
var Rect       : TRect;
var W,H       : Integer;
begin
  FrameRects.Clear;

  X  := PATTERN_HSPACE div 2;
  Y  := PATTERN_VSPACE div 2;
  Row:= PATTERN_VSPACE;
  for Index := 0 to Animation.Frames.Count - 1 do
  begin
    Frame:= Animation.Frames.List^[Index];

    W:= PaintBox1.Canvas.TextWidth ( String(Frame.Name) ) + PATTERN_HSPACE;
    H:= PaintBox1.Canvas.TextHeight( String(Frame.Name) ) + 4+4;

    if Assigned(Image) and (Frame.Pattern >= 0) and (Frame.Pattern < Image.Patterns.Count) then
    begin
      Pattern:= Image.Patterns.List^[Frame.Pattern];

      W:= Max(W, Pattern.Width + PATTERN_HSPACE);

      H:= H + Pattern.Height + 4;
    end else
    begin
      H:= H + 24;
    end;

    Rect.Left  := X;
    Rect.Top   := Y;
    Rect.Right := X + W;
    Rect.Bottom:= Y + H;

    FrameRects.Add(Rect);

    Row:= Max(Row, Rect.Bottom);

    X:= X + W + PATTERN_VSPACE;
  end;
  PaintBox1.Width := X;
  PaintBox1.Height:= Row;

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationFrames.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  if Assigned(Animation) then
  begin
    if Key = LCLType.VK_LEFT then
    begin
      SetSelected( Max(0, FSelected - 1));
    end;
    if Key = LCLType.VK_RIGHT then
    begin
      SetSelected( Min(FSelected + 1, Animation.Frames.Count - 1));
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationFrames.ScrollBar1Change(Sender: TObject);
begin
  PaintBox1.Invalidate;
end;


//------------------------------------------------------------------------------
procedure TFrmAnimationFrames.PaintBox1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Source = FPatterns.PaintBox1;

  Accept:= True;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationFrames.PaintBox1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  actFrameAdd.Execute
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationFrames.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Index: Integer;
begin
  inherited;

  SetFocus;

  if Button = TMouseButton.mbLeft then
  begin
    for Index := 0 to FrameRects.Count - 1 do
    begin
      if PtInRect(FrameRects[Index], Point(X,Y)) then
      begin
        SetSelected(Index);

        Exit;
      end;

    end;
    SetSelected(-1);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationFrames.DrawPattern(const Rect: TRect; const PatternIndex: Integer);
var Pattern: TPHXPattern;
var X,Y: Integer;
begin
  if Assigned(Animation.Image) and ( PatternIndex >= 0) and (PatternIndex < Animation.Image.Patterns.Count) then
  begin
    Pattern:= Animation.Image.Patterns.List^[PatternIndex];

    X:= Rect.Left + ((Rect.Right  - Rect.Left) - Pattern.Width ) div 2;
    Y:= Rect.Top  + ((Rect.Bottom - Rect.Top ) - Pattern.Height) div 2;

   // Animation.Image.DrawPattern(PaintBox1.Canvas, FBuffer, X,Y, PatternIndex);
    Animation.Image.DrawPattern(PaintBox1.Canvas, FBuffer, X,Y, PatternIndex);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationFrames.PaintBox1Paint(Sender: TObject);
var Index: Integer;
var Frame_: TPHXAnimationFrame;
var Rect : TRect;
var R    : TRect;
begin
  with PaintBox1.Canvas do
  begin
    Brush.Style := bsClear;
    Brush.Color := ScrollBox1.Color;

    FillRect(PaintBox1.ClientRect);

    Font.Style := [fsBold];

    if Assigned(Animation) then
    begin

      if Animation.Frames.Count <> FrameRects.Count then
      begin
        PlaceFrames;
      end;

      for Index := 0 to Animation.Frames.Count - 1 do
      begin
        Frame_:= Animation.Frames[Index];
        Rect := FrameRects[Index];

        TextOut(Rect.Left + 2, Rect.Top + 4, String(Frame_.Name));

       // Rect.Top:= Rect.Top + TextHeight(String(Frame.Name)) + 8;

        if Assigned(Animation.Image) and (Frame_.Pattern >= 0) and (Frame_.Pattern < Animation.Image.Patterns.Count) then
        begin
          R.Left  := Rect.Left + 2;
          R.Top   := Rect.Top  + TextHeight(String(Frame_.Name)) + 2;
          R.Right := Rect.Right + 2;
          R.Bottom:= Rect.Bottom + 2;

          DrawPattern(R , Frame_.Pattern);
        end;

        if Index = FSelected then
        begin
          Pen.Color := clRed;
          Pen.Style := psSolid;

          Brush.Style := bsClear;

          Rectangle(Rect.Left, Rect.Top,  Rect.Right, Rect.Bottom);
        end;
      end;
    end;
  end;
end;

procedure TFrmAnimationFrames.FrameResize(Sender: TObject);
begin
  PaintBox1.Invalidate;
end;


{$REGION 'Animation Actions'}

//------------------------------------------------------------------------------
procedure TFrmAnimationFrames.actFrameDelUpdate(Sender: TObject);
begin
  actFrameAdd.Enabled:= Assigned(Animation);
  actFrameDel.Enabled:= Assigned(Animation) and (FSelected >= 0) and (FSelected < Animation.Frames.Count);
end;


var FrameCounter: Integer = 1;

//------------------------------------------------------------------------------
procedure TFrmAnimationFrames.actFrameAddExecute(Sender: TObject);
var Time: Single;
begin
  if not Assigned(Animation) then Exit;

  if Animation.FrameRate = 0 then
  begin
    Time:= 0.0;
  end
  else
  begin
    Time:=1 / Animation.FrameRate;
  end;

  if Assigned(FPatterns.Image) and (FPatterns.PatternIndex >= 0) then
  begin
    FAnimation.Frames.Add(FPatterns.PatternName, Time, FPatterns.PatternIndex);
  end else
  begin
    FAnimation.Frames.Add( Format('Frame%d', [FrameCounter]), Time, -1);

    Inc(FrameCounter);
  end;

  SetAnimation(FAnimation);

  SetSelected(FAnimation.Frames.Count - 1);

  ScrollBox1.HorzScrollBar.Position:= ScrollBox1.HorzScrollBar.Range;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationFrames.actFrameDelExecute(Sender: TObject);
begin
  if Assigned(Animation) and (FSelected >= 0) and (FSelected < FAnimation.Frames.Count) then
  begin
    FAnimation.Frames.Delete(FSelected);

    FSelected:= Max(0, FSelected-1);

    ModActions.Document.Changed;
  end;
end;



{$ENDREGION}



//------------------------------------------------------------------------------
procedure TFrmAnimationFrames.SetAnimation(const Value: TPHXAnimation);
begin
  if FAnimation <> Value then
  begin
    FAnimation := Value;
    FSelected  := -1;
  end;

  if Assigned(FAnimation) then
  begin
    FImage:= FAnimation.Image;

    PlaceFrames;
  end else
  begin
    FImage:= nil;
  end;

  if Assigned(Image) then
  begin
    //Image.Draw(FBuffer, FBackground);//Graphics.ColorToRGB(clWindow));
    //FAnimation.Image.Draw(FBuffer, Graphics.ColorToRGB(clWindow) );

    FAnimation.Image.Draw(FBuffer, FBackground);
  end else
  begin
    FBuffer.Width := 0;
    FBuffer.Height:= 0;
  end;


  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationFrames.SetEditor(const Value: TFrmAnimationFrame);
begin
  FEditor := Value;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationFrames.SetPatterns(const Value: TFrmPatterns);
begin
  if Assigned(FPatterns) then
  begin
    FPatterns.OnPatternDblClick:= nil;
  end;

  FPatterns := Value;

  if Assigned(FPatterns) then
  begin
    FPatterns.OnPatternDblClick:= actFrameAddExecute;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationFrames.SetSelected(const Value: Integer);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;

    if Assigned(FEditor) then
    begin
      FEditor.Frame:= FSelected;
    end;

    PaintBox1.Invalidate;
  end;
end;


end.
